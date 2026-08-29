##########################################################
# 05_predict_labor_income.R
#
# Section 3. Out-of-sample prediction. Chunks 1-7 are the
# training sample; chunks 8-10 are held out. Model choice
# is by validation RMSE on log labour income. LOOCV for
# the winning model uses the OLS leverage formula from
# class rather than n refits.
#
# Outputs: figures and .tex tables used by pred_equipo_03.
##########################################################

drop_unused_levels <- function(d) {
  d |> mutate(across(where(is.factor), droplevels))
}

align_to_train <- function(newdata, train) {
  for (nm in names(train)) {
    if (is.factor(train[[nm]]) && nm %in% names(newdata)) {
      newdata[[nm]] <- factor(as.character(newdata[[nm]]), levels = levels(train[[nm]]))
    }
  }
  newdata
}

db_train <- geih |> filter(bin_train == 1) |> drop_unused_levels()
db_valid <- geih |> filter(bin_train == 0) |> align_to_train(db_train)

# Rows whose factor level is unseen in training cannot be
# scored by lm(); they are rare and are dropped from the
# validation comparison only.
n_valid_raw <- nrow(db_valid)
db_valid <- db_valid |>
  filter(!is.na(cat_educ), !is.na(cat_relab), !is.na(cat_size_firm), !is.na(cat_estrato))
n_valid_dropped <- n_valid_raw - nrow(db_valid)

formulas <- list(
  s1_uncond = num_log_income ~ num_age + num_age2,
  s1_cond   = num_log_income ~ num_age + num_age2 + num_hours + cat_relab,
  s2_uncond = num_log_income ~ bin_female,
  s2_pref   = num_log_income ~ bin_female + num_age + num_age2 + cat_educ +
    num_hours + cat_relab,
  p_mincer  = num_log_income ~ bin_female + num_age + num_age2 + cat_educ +
    num_hours + cat_relab + bin_formal,
  p_firm    = num_log_income ~ bin_female + num_age + num_age2 + cat_educ +
    num_hours + cat_relab + bin_formal + cat_size_firm + cat_estrato,
  p_returns = num_log_income ~ bin_female * (num_age + num_age2 + cat_educ + num_hours) +
    cat_relab + bin_formal,
  p_flex    = num_log_income ~ bin_female + num_age + num_age2 + I(num_age^3) +
    num_hours + num_hours2 + cat_educ + cat_relab + bin_formal + cat_size_firm,
  p_hh      = num_log_income ~ bin_female * (num_age + num_age2 + num_minors) +
    bin_head + cat_educ + num_hours + cat_relab + bin_formal,
  p_overfit = num_log_income ~ bin_female * (poly(num_age, 5) + cat_educ + num_hours) +
    cat_relab + bin_formal + cat_size_firm + num_minors + bin_head + cat_estrato
)

model_labels <- c(
  s1_uncond = "S1: Unconditional age profile",
  s1_cond   = "S1: Conditional age profile",
  s2_uncond = "S2: Unconditional gender gap",
  s2_pref   = "S2: Preferred gender gap",
  p_mincer  = "P1: Add formality",
  p_firm    = "P2: Add firm size and estrato",
  p_returns = "P3: Gender-specific returns",
  p_flex    = "P4: Cubic age and quadratic hours",
  p_hh      = "P5: Household structure",
  p_overfit = "P6: Flexible age, education, and hours"
)

fit_and_score <- function(formula, train, valid) {
  model <- lm(formula, data = train)
  tibble(
    n_train = nobs(model),
    rmse_train = caret::RMSE(pred = fitted(model), obs = model$model[[1]]),
    rmse_valid = rmse_newdata(model, valid),
    r2_train = summary(model)$r.squared
  )
}

message("Estimating prediction models on chunks 1-7...")
scores <- imap_dfr(formulas, function(frm, nm) {
  out <- fit_and_score(frm, db_train, db_valid)
  out$id <- nm
  out$label <- model_labels[[nm]]
  out$section <- if (startsWith(nm, "s1")) {
    "Section 1"
  } else if (startsWith(nm, "s2")) {
    "Section 2"
  } else {
    "Additional"
  }
  out
}) |>
  arrange(rmse_valid)

best_id <- scores$id[1]
best_label <- scores$label[1]
m_best <- lm(formulas[[best_id]], data = db_train)

rmse_loocv <- loocv_rmse_ols(m_best)
rmse_valid_best <- scores$rmse_valid[1]
rmse_train_best <- scores$rmse_train[1]

scores_tex <- scores |>
  mutate(
    Model = label,
    Origin = section,
    `Training RMSE` = fmt_num(rmse_train, 4),
    `Validation RMSE` = fmt_num(rmse_valid, 4),
    `Training R$^2$` = fmt_num(r2_train, 3)
  ) |>
  select(Model, Origin, `Training RMSE`, `Validation RMSE`, `Training R$^2$`)

write_booktabs(scores_tex, file.path(path_tables, "tab_prediction_rmse.tex"), align = "llccc")

loocv_tex <- tibble(
  Model = best_label,
  `Training RMSE` = fmt_num(rmse_train_best, 4),
  `LOOCV RMSE` = fmt_num(rmse_loocv, 4),
  `Validation RMSE` = fmt_num(rmse_valid_best, 4)
)
write_booktabs(loocv_tex, file.path(path_tables, "tab_loocv_vs_validation.tex"), align = "lccc")

# Variable importance: permutation of validation inputs.
# We shuffle one covariate (or a factor) at a time, keep
# the fitted model fixed, and record the increase in
# validation RMSE. This uses the same loss we used to
# select the model and does not reward in-sample fit.
permute_rmse <- function(model, valid, var, outcome = "num_log_income", seed = 4107) {
  set.seed(seed)
  d <- valid
  d[[var]] <- sample(d[[var]])
  if (var == "num_age" && "num_age2" %in% names(d)) {
    d$num_age2 <- d$num_age^2
  }
  if (var == "num_hours" && "num_hours2" %in% names(d)) {
    d$num_hours2 <- d$num_hours^2
  }
  rmse_newdata(model, d, outcome)
}

vars_to_permute <- c(
  "num_age", "num_hours", "num_minors",
  "bin_female", "bin_formal", "bin_head", "cat_educ", "cat_relab",
  "cat_size_firm", "cat_estrato"
)
vars_to_permute <- intersect(vars_to_permute, all.vars(formulas[[best_id]]))

base_valid <- rmse_valid_best
importance <- map_dfr(vars_to_permute, function(v) {
  tibble(
    variable = v,
    rmse_permuted = permute_rmse(m_best, db_valid, v),
    delta_rmse = rmse_permuted - base_valid
  )
}) |>
  arrange(desc(delta_rmse))

importance <- importance |>
  mutate(
    label = recode(
      variable,
      num_age = "Age",
      num_age2 = "Age squared",
      num_hours = "Weekly hours",
      num_hours2 = "Hours squared",
      num_minors = "Minors in household",
      bin_female = "Female",
      bin_formal = "Formality",
      bin_head = "Household head",
      cat_educ = "Education",
      cat_relab = "Employment type",
      cat_size_firm = "Firm size",
      cat_estrato = "Socioeconomic stratum"
    )
  )

imp_tex <- importance |>
  mutate(
    Variable = label,
    `Validation RMSE (permuted)` = fmt_num(rmse_permuted, 4),
    `Increase in RMSE` = fmt_num(delta_rmse, 4)
  ) |>
  select(Variable, `Validation RMSE (permuted)`, `Increase in RMSE`)

write_booktabs(imp_tex, file.path(path_tables, "tab_variable_importance.tex"), align = "lcc")

top_var <- importance$variable[1]
top_label <- importance$label[1]

fig_importance <- importance |>
  mutate(label = fct_reorder(label, delta_rmse)) |>
  ggplot(aes(x = delta_rmse, y = label)) +
  geom_col(fill = col_navy, width = 0.7) +
  labs(
    title = "Variable importance in the selected model",
    subtitle = "Increase in validation RMSE after permuting the covariate, model held fixed.",
    x = "Increase in validation RMSE",
    y = NULL,
    caption = "Source: Own calculations, GEIH 2018 (Bogotá sample). Training: chunks 1–7; validation: chunks 8–10."
  )

ggsave(file.path(path_figures, "fig_variable_importance.pdf"), fig_importance,
       width = 7.2, height = 4.6)
ggsave(file.path(path_figures, "fig_variable_importance.png"), fig_importance,
       width = 7.2, height = 4.6, dpi = 300)

# How predictions depend on the leading covariate: grid the
# variable, hold other covariates at training-sample typical
# values, and plot the model's predicted log income.
typical_row <- db_train |>
  summarise(
    across(where(is.numeric), ~ median(.x, na.rm = TRUE)),
    across(where(is.factor), ~ names(sort(table(.x), decreasing = TRUE))[1])
  )
for (nm in names(db_train)) {
  if (is.factor(db_train[[nm]]) && nm %in% names(typical_row)) {
    typical_row[[nm]] <- factor(typical_row[[nm]], levels = levels(db_train[[nm]]))
  }
}

make_grid <- function(varname, data, typical) {
  grid <- typical[rep(1, 80), ]
  if (is.numeric(data[[varname]])) {
    grid[[varname]] <- seq(quantile(data[[varname]], 0.02, na.rm = TRUE),
                           quantile(data[[varname]], 0.98, na.rm = TRUE),
                           length.out = 80)
    if (varname == "num_age") grid$num_age2 <- grid$num_age^2
    if (varname == "num_hours") grid$num_hours2 <- grid$num_hours^2
  } else {
    levs <- levels(data[[varname]])
    grid <- typical[rep(1, length(levs)), ]
    grid[[varname]] <- factor(levs, levels = levs)
  }
  grid$pred <- predict(m_best, newdata = grid)
  grid$x <- grid[[varname]]
  grid
}

pd_grid <- make_grid(top_var, db_train, typical_row)

if (is.numeric(db_train[[top_var]])) {
  fig_pd <- ggplot(pd_grid, aes(x = x, y = pred)) +
    geom_line(colour = col_navy, linewidth = 1) +
    labs(
      title = paste("How predicted log income varies with", tolower(top_label)),
      subtitle = "Other covariates held at training-sample medians / modes.",
      x = top_label,
      y = "Predicted log monthly labour income",
      caption = "Source: Own calculations, GEIH 2018 (Bogotá sample)."
    )
} else {
  fig_pd <- ggplot(pd_grid, aes(x = x, y = pred)) +
    geom_col(fill = col_navy, width = 0.7) +
    labs(
      title = paste("How predicted log income varies with", tolower(top_label)),
      subtitle = "Other covariates held at training-sample medians / modes.",
      x = top_label,
      y = "Predicted log monthly labour income",
      caption = "Source: Own calculations, GEIH 2018 (Bogotá sample)."
    ) +
    theme(axis.text.x = element_text(angle = 25, hjust = 1))
}

ggsave(file.path(path_figures, "fig_prediction_partial_dependence.pdf"), fig_pd,
       width = 7.2, height = 4.4)
ggsave(file.path(path_figures, "fig_prediction_partial_dependence.png"), fig_pd,
       width = 7.2, height = 4.4, dpi = 300)

# Where the model fails: validation residuals against
# predicted values. Systematic fans at the top of the
# distribution are the relevant diagnostic for a tax
# authority looking for misreporting among high earners.
db_valid <- db_valid |>
  mutate(
    pred = predict(m_best, newdata = db_valid),
    resid = num_log_income - pred
  )

fig_resid <- ggplot(db_valid, aes(x = pred, y = resid)) +
  geom_hline(yintercept = 0, colour = col_slate, linewidth = 0.4) +
  geom_point(alpha = 0.15, colour = col_navy, size = 0.7) +
  geom_smooth(se = FALSE, colour = col_terra, linewidth = 0.8, method = "loess", span = 0.4) +
  labs(
    title = "Validation residuals against predicted log income",
    subtitle = "Selected model. A fan at high predicted values is income the model cannot see.",
    x = "Predicted log monthly labour income",
    y = "Validation residual",
    caption = "Source: Own calculations, GEIH 2018 (Bogotá sample). Validation: chunks 8–10."
  )

ggsave(file.path(path_figures, "fig_validation_residuals.pdf"), fig_resid,
       width = 7.2, height = 4.4)
ggsave(file.path(path_figures, "fig_validation_residuals.png"), fig_resid,
       width = 7.2, height = 4.4, dpi = 300)

fig_rmse <- scores |>
  mutate(label = fct_reorder(label, rmse_valid),
         best = id == best_id) |>
  ggplot(aes(x = rmse_valid, y = label, fill = best)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = c("TRUE" = col_terra, "FALSE" = col_navy), guide = "none") +
  labs(
    title = "Validation RMSE by specification",
    subtitle = "Lower is better. Selected model highlighted.",
    x = "Validation RMSE (log monthly labour income)",
    y = NULL,
    caption = "Source: Own calculations, GEIH 2018 (Bogotá sample). Training: chunks 1–7; validation: chunks 8–10."
  )

ggsave(file.path(path_figures, "fig_validation_rmse.pdf"), fig_rmse,
       width = 7.4, height = 5.0)
ggsave(file.path(path_figures, "fig_validation_rmse.png"), fig_rmse,
       width = 7.4, height = 5.0, dpi = 300)

saveRDS(list(
  scores = scores,
  best_id = best_id,
  best_label = best_label,
  m_best = m_best,
  rmse_loocv = rmse_loocv,
  rmse_valid = rmse_valid_best,
  rmse_train = rmse_train_best,
  importance = importance,
  top_var = top_var,
  top_label = top_label,
  n_valid_dropped = n_valid_dropped
), file.path(path_temp, "section3_models.rds"))
