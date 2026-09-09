##########################################################
# 04_estimate_gender_income_gap.R
#
# Section 2. Unconditional and conditional gender gaps.
# The control set is a modelling choice: we start with
# predetermined human-capital variables and then add job
# attributes. The preferred specification for the tax-
# authority's prediction problem (M6) includes job
# attributes as legitimate predictors, even though they
# are flagged as potential bad controls for the causal
# interpretation of the gender coefficient. The gender
# coefficient is recovered by OLS and by Frisch-Waugh-
# Lovell, with analytical and bootstrap standard errors.
#
# Outputs: figures and .tex tables used by gap_equipo_03.
##########################################################

# 0) Descriptive statistics ---------------------------------------------------

# Before any estimation, we obtain descriptive statistics to motivate the
# analysis. Here: a visual and tabular comparison of income by gender.

# Create a non-numeric (text) gender variable
geih <- geih |>
  mutate(cat_gender = ifelse(bin_female == 1, "Woman", "Man"))

# Table of descriptive statistics of income by gender
tab_income_by_gender <- geih |>
  group_by(cat_gender) |>
  summarise(
    num_n           = n(),
    num_mean_log    = mean(num_log_income, na.rm = TRUE),
    num_sd_log      = sd(num_log_income, na.rm = TRUE),
    num_mean_income = mean(num_income, na.rm = TRUE),
    .groups = "drop"
  )

tab_income_by_gender

# Density plot of log-income by gender
cat_gender_colors <- c("Man" = "#0072B2", "Woman" = "#CC79A7")  # Colour assigned to each gender

# Plot the density
fig_income_density <- geih |>
  ggplot(aes(x = num_log_income, fill = cat_gender, color = cat_gender)) +
  geom_density(alpha = 0.35, linewidth = 0.8) +
  scale_fill_manual(values = cat_gender_colors) +
  scale_color_manual(values = cat_gender_colors) +
  coord_cartesian(xlim = c(9, 17)) +
  labs(
    x = "Log(monthly labour income)", y = "Density",
    fill = "Gender", color = "Gender",
    title = "Distribution of labour income by gender",
    caption = "Source: GEIH 2018, Bogotá."
  ) +
  theme_classic() +
  theme(
    plot.title   = element_text(hjust = 0.5, face = "bold", margin = margin(b = 10)),
    plot.caption = element_text(hjust = 0.5, size = 8, color = "grey40", margin = margin(t = 12))
  )

ggsave(file.path(path_figures, "fig_gap_income_density.pdf"), fig_income_density,
       width = 7.2, height = 4.4)
ggsave(file.path(path_figures, "fig_gap_income_density.png"), fig_income_density,
       width = 7.2, height = 4.4, dpi = 300)

# 1) First, we estimate the unconditional gap ----------------------------------

# Define the formula
f_gap_uncond <- num_log_income ~ bin_female

# Estimate the model
m_gap_uncond <- lm(f_gap_uncond, data = geih)

# 2) Next, we estimate the conditional gap -------------------------------------

# For this part, we test several models, starting with the simplest and
# moving to more complex ones. Here we classify the controls available to
# us, following Cinelli, Forney, Pearl (2022) as a reference.

# Nodes: position and role (X, Z, or Y) within each pattern
nodes <- tribble(
  ~cat_pattern,  ~cat_role, ~num_x, ~num_y,
  "1. Neutral",  "X",       1,      0.0,
  "1. Neutral",  "Y",       3,      0.0,
  "1. Neutral",  "Z",       2,      0.65,
  "2. Mediator", "X",       1,      0.0,
  "2. Mediator", "Z",       2,      0.0,
  "2. Mediator", "Y",       3,      0.0,
  "3. Collider", "X",       1,      0.5,
  "3. Collider", "Y",       3,      0.5,
  "3. Collider", "Z",       2,      0.0,
  "4. Modifier", "Z",       2,      0.65,
  "4. Modifier", "X",       1,      0.0,
  "4. Modifier", "Y",       3,      0.0
) |>
  mutate(cat_pattern = fct_inorder(cat_pattern))

# Arrows between nodes
edges <- tribble(
  ~cat_pattern,  ~num_x, ~num_y, ~num_xend, ~num_yend, ~cat_line,
  "1. Neutral",    1,     0.0,    3,         0.0,       "solid",
  "1. Neutral",    2,     0.5,    2.85,      0.06,      "solid",
  "2. Mediator",   1,     0.0,    1.85,      0.0,       "solid",
  "2. Mediator",   2.15,  0.0,    3,         0.0,       "solid",
  "3. Collider",   1.15,  0.35,   1.85,      0.06,      "solid",
  "3. Collider",   2.85,  0.35,   2.15,      0.06,      "solid",
  "4. Modifier",   1,     0.0,    3,         0.0,       "solid",
  "4. Modifier",   2,     0.5,    2,         0.03,      "dashed"
) |>
  mutate(cat_pattern = fct_inorder(cat_pattern))

# Which real variables fall into each pattern
captions <- tribble(
  ~cat_pattern,  ~str_caption,                          ~str_verdict,
  "1. Neutral",  "age, stratum",                        "Good for causality and prediction",
  "2. Mediator", "educ., hours, occupation, formality",  "Bad for causality, good for prediction",
  "3. Collider", "household head",                       "Bad for causality and prediction",
  "4. Modifier", "minors in household",                  "Requires modeling the interaction"
) |>
  mutate(cat_pattern = fct_inorder(cat_pattern))

# Only the Z node is coloured by verdict; X and Y stay neutral
cat_pattern_colors <- c(
  "1. Neutral"  = "#3B8BD4",
  "2. Mediator" = "#D85A30",
  "3. Collider" = "#E24B4A",
  "4. Modifier" = "#D4537E"
)

nodes <- nodes |>
  mutate(cat_fill = if_else(cat_role == "Z", as.character(cat_pattern), "generic"))

fig_causal_diagram <- ggplot() +
  geom_segment(
    data = edges,
    aes(x = num_x, y = num_y, xend = num_xend, yend = num_yend, linetype = cat_line),
    arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
    linewidth = 0.6, color = "grey30"
  ) +
  geom_label(
    data = nodes,
    aes(x = num_x, y = num_y, label = cat_role, fill = cat_fill),
    color = "white", fontface = "bold", size = 5.5,
    label.size = 0, label.padding = unit(0.3, "lines")
  ) +
  geom_text(
    data = captions,
    aes(x = 2, y = -0.35, label = str_caption),
    size = 3.8, color = "grey30"
  ) +
  geom_text(
    data = captions,
    aes(x = 2, y = -0.56, label = str_verdict),
    size = 3.4, color = "grey15", fontface = "italic"
  ) +
  scale_fill_manual(values = c(cat_pattern_colors, generic = "grey70"), guide = "none") +
  scale_linetype_manual(values = c(solid = "solid", dashed = "dashed"), guide = "none") +
  coord_cartesian(xlim = c(0.5, 3.5), ylim = c(-0.72, 1.0)) +
  facet_wrap(~ cat_pattern, ncol = 2) +
  labs(
    title = "Classification of candidate controls for the gender gap",
    caption = "Source: Cinelli, C., Forney, A., & Pearl, J. (2022). A Crash Course in Good and Bad Controls.\nJournal of Sociological Methods and Research (Technical Report R-493)."
  ) +
  theme_void() +
  theme(
    strip.text   = element_text(face = "bold", size = 13),
    plot.title   = element_text(hjust = 0.5, face = "bold", size = 16, margin = margin(b = 8)),
    plot.caption = element_text(hjust = 0.5, size = 8, color = "grey40", margin = margin(t = 8)),
    plot.margin  = margin(t = 12, r = 10, b = 8, l = 10)
  )

ggsave(file.path(path_figures, "fig_gap_controls_dag.pdf"), fig_causal_diagram,
       width = 10, height = 5.62)
ggsave(file.path(path_figures, "fig_gap_controls_dag.png"), fig_causal_diagram,
       width = 10, height = 5.62, dpi = 300)

fig_causal_diagram

# We now assess the "bad" controls empirically
tab_balance <- geih |>
  mutate(
    bin_domestic = as.numeric(cat_relab == "Domestic worker"),
    bin_tertiary = as.numeric(cat_educ == "Tertiary"),  
    num_estrato  = as.numeric(as.character(cat_estrato))
  ) |>
  select(bin_female, num_age, num_estrato, bin_tertiary,
         num_hours, bin_domestic, bin_formal) |>
  pivot_longer(cols = -bin_female, names_to = "cat_variable", values_to = "num_value") |>
  group_by(cat_variable) |>
  mutate(num_sd_total = sd(num_value, na.rm = TRUE)) |>
  group_by(cat_variable, bin_female, num_sd_total) |>
  summarise(num_mean = mean(num_value, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(
    names_from  = bin_female,
    values_from = num_mean,
    names_glue  = "num_mean_{ifelse(bin_female == 1, 'female', 'male')}"
  ) |>
  mutate(
    num_smd = (num_mean_female - num_mean_male) / num_sd_total,
    # Each variable is on a different scale (years, hours, 0/1 proportions), 
    # so raw differences between women and men can't be compared side by
    # side. Dividing by each variable's own standard deviation puts
    # every difference in the same unit.
    cat_label = case_match(cat_variable,
                           "num_age"      ~ "Age",
                           "num_estrato"  ~ "Stratum",
                           "bin_tertiary" ~ "Tertiary education",
                           "num_hours"    ~ "Hours worked",
                           "bin_domestic" ~ "Domestic work",
                           "bin_formal"   ~ "Formality"
    ),
    cat_label = fct_reorder(cat_label, abs(num_smd))
  )

tab_balance

fig_balance <- ggplot(tab_balance, aes(x = num_smd, y = cat_label)) +
  geom_vline(xintercept = 0, linetype = "solid", color = "grey60") +
  geom_segment(aes(x = 0, xend = num_smd, y = cat_label, yend = cat_label),
               linewidth = 0.8, color = "#0072B2") +
  geom_point(size = 4, color = "#0072B2") +
  labs(
    x = "Standardized difference", y = NULL, # Note: the difference is women - men
    title = "Gender gap in candidate controls",
    caption = "Source: GEIH 2018, Bogotá."
  ) +
  theme_classic() +
  theme(
    plot.title   = element_text(hjust = 0.5, face = "bold", margin = margin(b = 10)),
    plot.caption = element_text(hjust = 0.5, size = 8, color = "grey40")
  )

ggsave(file.path(path_figures, "fig_gap_balance.pdf"), fig_balance,
       width = 7.2, height = 4.4)
ggsave(file.path(path_figures, "fig_gap_balance.png"), fig_balance,
       width = 7.2, height = 4.4, dpi = 300)

# With this evidence, we define the different specifications
f_gap_m2 <- num_log_income ~ bin_female + num_age + num_age2 # Control for age
f_gap_m3 <- num_log_income ~ bin_female + num_age + num_age2 + cat_estrato # Age + socioeconomic stratum
f_gap_m4 <- num_log_income ~ bin_female + num_age + num_age2 + cat_estrato + cat_educ # Age + stratum + education
f_gap_m5 <- num_log_income ~ bin_female + num_age + num_age2 + cat_estrato + cat_educ + bin_formal # Age + stratum + education + formality
f_gap_m6 <- num_log_income ~ bin_female + num_age + num_age2 + cat_estrato + cat_educ + bin_formal + num_hours + cat_relab # Age + stratum + education + formality + weekly hours + employment type

# Estimate the different models
m_gap_m2 <- lm(f_gap_m2, data = geih)
m_gap_m3 <- lm(f_gap_m3, data = geih)
m_gap_m4 <- lm(f_gap_m4, data = geih)
m_gap_m5 <- lm(f_gap_m5, data = geih)
m_gap_m6 <- lm(f_gap_m6, data = geih)

# 3) Next, we estimate the conditional gap via FWL -----------------------------

# We compare FWL with the standard regression; the coefficient should match.
# M6 is the preferred specification for the tax authority's prediction
# problem: job attributes are useful predictors of expected income even
# though they are flagged as potential bad controls for a *causal*
# reading of the gender coefficient (see fig_gap_controls_dag.pdf).

# Build a list containing all the models
list_specs <- list(
  list(name = "M1: Unconditional",              model = m_gap_uncond, controls = ~1),
  list(name = "M2: + Age",                      model = m_gap_m2,     controls = ~ num_age + num_age2),
  list(name = "M3: + Stratum",                  model = m_gap_m3,     controls = ~ num_age + num_age2 + cat_estrato),
  list(name = "M4: + Education",                model = m_gap_m4,     controls = ~ num_age + num_age2 + cat_estrato + cat_educ),
  list(name = "M5: + Formality",                model = m_gap_m5,     controls = ~ num_age + num_age2 + cat_estrato + cat_educ + bin_formal),
  list(name = "M6: + Hours/Occupation (preferred)", model = m_gap_m6, controls = ~ num_age + num_age2 + cat_estrato + cat_educ + bin_formal + num_hours + cat_relab)
)

# Obtain FWL for the preferred specification (M6)
m_gap_fwl <- fwl_gender(geih, list_specs[[6]]$controls)

# 4) Next, we report the analytical and bootstrap standard errors --------------

tab_gap_final <- map_dfr(list_specs, function(spec) {
  message("  -> ", spec$name) # Print the model name
  se_boot <- bootstrap_fwl_se(geih, spec$controls) # Extract the bootstrap SE
  coef_f  <- coef(spec$model)["bin_female"] # Extract the bin_female coefficient
  tibble(
    cat_specification  = spec$name,
    num_coef_female    = coef_f,
    num_se_analytical  = summary(spec$model)$coefficients["bin_female", "Std. Error"],
    num_se_bootstrap   = se_boot,
    num_pct_gap        = 100 * (exp(coef_f) - 1),
    num_r2             = summary(spec$model)$r.squared,
    num_n              = nobs(spec$model)
  )
})

tab_gap_final

# Verify that FWL matches the ols regression
ols_coef    <- unname(coef(m_gap_m6)["bin_female"])
fwl_coef    <- unname(coef(m_gap_fwl)["x_tilde"])
ols_se      <- unname(summary(m_gap_m6)$coefficients["bin_female", "Std. Error"])
fwl_se_boot <- tab_gap_final$num_se_bootstrap[
  tab_gap_final$cat_specification == "M6: + Hours/Occupation (preferred)"
]

tab_fwl_check <- tibble(
  `OLS female coefficient` = fmt_num(ols_coef, 6),
  `FWL coefficient`        = fmt_num(fwl_coef, 6),
  `Analytical SE`          = fmt_num(ols_se, 3),
  `FWL bootstrap SE`       = fmt_num(fwl_se_boot, 3),
  `Absolute difference`    = fmt_num(abs(ols_coef - fwl_coef), 8)
)

write_booktabs(tab_fwl_check, file.path(path_tables, "tab_fwl_check.tex"), align = "ccccc")

# 5) Next, we build the comparison table with all components -------------------

tab_gap_tex <- tab_gap_final |>
  mutate(
    Specification   = cat_specification,
    `Female coef.`   = fmt_num(num_coef_female, 3),
    `Analytical SE`  = fmt_num(num_se_analytical, 3),
    `Bootstrap SE`   = fmt_num(num_se_bootstrap, 3),
    `Gap (\\%)`      = fmt_num(num_pct_gap, 1),
    `R$^2$`          = fmt_num(num_r2, 3),
    N                = fmt_num(num_n, 0)
  ) |>
  select(Specification, `Female coef.`, `Analytical SE`, `Bootstrap SE`,
         `Gap (\\%)`, `R$^2$`, N)

write_booktabs(tab_gap_tex, file.path(path_tables, "tab_gender_gap.tex"), align = "lcccccc")

# 6) Now, the age-income profiles ----------------------------------------------

# Define the model with a gender-by-age interaction, built on the
# preferred (M6) control set: age, stratum, education, formality,
# hours, and employment type.
f_gap_profile <- num_log_income ~ bin_female * num_age + bin_female * num_age2 +
  cat_estrato + cat_educ + bin_formal + num_hours + cat_relab

# Estimate the model
m_gap_profile <- lm(f_gap_profile, data = geih)

# Extract the peak ages
peaks_gender <- peaks_by_gender(m_gap_profile) # Returns the pair of peak ages for men and women
peaks_gender$peak_men
peaks_gender$peak_women

# Plot the predicted profiles

# Find the most common category for each control variable
modal_estrato <- names(sort(table(geih$cat_estrato), decreasing = TRUE))[1]
modal_educ    <- names(sort(table(geih$cat_educ), decreasing = TRUE))[1]
modal_formal  <- as.integer(names(sort(table(geih$bin_formal), decreasing = TRUE))[1])
modal_relab   <- names(sort(table(geih$cat_relab), decreasing = TRUE))[1]

# For a numeric variable (hours), use the median instead of the mode
median_hours  <- median(geih$num_hours)

# Build a table of hypothetical people
grid_profile <- expand.grid(
  num_age    = seq(18, 80, by = 1),
  bin_female = c(0, 1),
  KEEP.OUT.ATTRS = FALSE
) |>
  as_tibble() |>
  mutate(
    num_age2    = num_age^2,
    cat_estrato = factor(modal_estrato, levels = levels(geih$cat_estrato)),
    cat_educ    = factor(modal_educ, levels = levels(geih$cat_educ)),
    bin_formal  = modal_formal,
    num_hours   = median_hours,
    cat_relab   = factor(modal_relab, levels = levels(geih$cat_relab)),
    cat_gender  = if_else(bin_female == 1, "Woman", "Man")
  )

# Ask the already-fitted model to predict log-income 
grid_profile$num_pred <- predict(m_gap_profile, newdata = grid_profile)

fig_gap_profiles <- ggplot(grid_profile, aes(x = num_age, y = num_pred, color = cat_gender)) +
  geom_line(linewidth = 1.1) +
  geom_vline(xintercept = peaks_gender$peak_men, color = cat_gender_colors["Man"],
             linetype = "dotted", linewidth = 0.6) +
  geom_vline(xintercept = peaks_gender$peak_women, color = cat_gender_colors["Woman"],
             linetype = "dotted", linewidth = 0.6) +
  scale_color_manual(values = cat_gender_colors) +
  labs(
    x = "Age", y = "Predicted log(monthly labour income)",
    color = "Gender",
    title = "Predicted age-income profiles by gender",
    subtitle = paste0("Stratum = ", modal_estrato, "; education = ", modal_educ,
                      "; formality = ", modal_formal, "; hours = ", median_hours,
                      "; occupation = ", modal_relab, ". Dotted lines: peak age."),
    caption = "Source: GEIH 2018, Bogotá."
  ) +
  theme_classic() +
  theme(
    plot.title    = element_text(hjust = 0.5, face = "bold", margin = margin(b = 5)),
    plot.subtitle = element_text(hjust = 0.5, size = 8, color = "grey30"),
    plot.caption  = element_text(hjust = 0.5, size = 8, color = "grey40")
  )

ggsave(file.path(path_figures, "fig_gap_profiles.pdf"), fig_gap_profiles,
       width = 7.2, height = 4.4)
ggsave(file.path(path_figures, "fig_gap_profiles.png"), fig_gap_profiles,
       width = 7.2, height = 4.4, dpi = 300)

# Build the confidence intervals
ci_gender <- bootstrap_peaks_by_gender(geih, f_gap_profile)
ci_gender$men
ci_gender$women

# Build the final table
tab_peaks_gender <- tibble(
  Group = c("Men", "Women"),
  `Peak age` = c(fmt_num(peaks_gender$peak_men, 1), fmt_num(peaks_gender$peak_women, 1)),
  `95\\% bootstrap CI` = c(
    paste0("[", fmt_num(ci_gender$men[1], 1), ", ", fmt_num(ci_gender$men[2], 1), "]"),
    paste0("[", fmt_num(ci_gender$women[1], 1), ", ", fmt_num(ci_gender$women[2], 1), "]")
  )
)

write_booktabs(tab_peaks_gender, file.path(path_tables, "tab_gender_peaks.tex"), align = "lcc")

# 7) Save the models -----------------------------------------------------------

saveRDS(list(
  uncond    = m_gap_uncond,
  m2        = m_gap_m2,
  m3        = m_gap_m3,
  m4        = m_gap_m4,
  m5        = m_gap_m5,
  m6        = m_gap_m6,
  fwl       = m_gap_fwl,
  profile   = m_gap_profile,
  gap_table = tab_gap_final,
  peaks     = peaks_gender,
  ci_peaks  = ci_gender
), file.path(path_temp, "section2_models.rds"))