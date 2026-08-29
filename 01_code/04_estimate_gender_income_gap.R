##########################################################
# 04_estimate_gender_income_gap.R
#
# Section 2. Unconditional and conditional gender gaps.
# The control set is a modelling choice: we start with
# predetermined human-capital variables and then add job
# attributes, flagging the latter as potential bad
# controls. The preferred gender coefficient is recovered
# by OLS and by Frisch-Waugh-Lovell, with analytical and
# bootstrap standard errors.
#
# Outputs: figures and .tex tables used by gap_equipo_03.
##########################################################

f_gap_uncond <- num_log_income ~ bin_female
f_gap_hc     <- num_log_income ~ bin_female + num_age + num_age2 + cat_educ
f_gap_pref   <- num_log_income ~ bin_female + num_age + num_age2 + cat_educ +
  num_hours + cat_relab
f_gap_job    <- num_log_income ~ bin_female + num_age + num_age2 + cat_educ +
  num_hours + cat_relab + bin_formal + cat_size_firm

m_gap_uncond <- lm(f_gap_uncond, data = geih)
m_gap_hc     <- lm(f_gap_hc, data = geih)
m_gap_pref   <- lm(f_gap_pref, data = geih)
m_gap_job    <- lm(f_gap_job, data = geih)

# FWL on the preferred specification. The residual-on-
# residual coefficient must match the OLS female coefficient.
f_controls_pref <- ~ num_age + num_age2 + cat_educ + num_hours + cat_relab
m_fwl_pref <- fwl_gender(geih, f_controls_pref)
fwl_coef <- unname(coef(m_fwl_pref)[["x_tilde"]])
ols_coef <- unname(coef(m_gap_pref)[["bin_female"]])
ols_se   <- unname(sqrt(diag(vcov(m_gap_pref)))[["bin_female"]])

message("Bootstrapping FWL standard error (Section 2)...")
fwl_se_boot <- bootstrap_fwl_se(geih, f_controls_pref)

# Gender-specific life-cycle profiles, preferred controls.
f_gap_profile <- num_log_income ~ bin_female * (num_age + num_age2) +
  cat_educ + num_hours + cat_relab
m_gap_profile <- lm(f_gap_profile, data = geih)
peaks_g <- peaks_by_gender(m_gap_profile)

message("Bootstrapping gender-specific peak ages (Section 2)...")
ci_g <- bootstrap_peaks_by_gender(geih, f_gap_profile)

pct <- function(b) 100 * (exp(b) - 1)

gap_table <- tibble(
  Specification = c(
    "Unconditional",
    "Human capital (age, education)",
    "Preferred (add hours and employment type)",
    "Job attributes (add formality and firm size)"
  ),
  `Female coefficient` = c(
    coef(m_gap_uncond)[["bin_female"]],
    coef(m_gap_hc)[["bin_female"]],
    coef(m_gap_pref)[["bin_female"]],
    coef(m_gap_job)[["bin_female"]]
  ),
  `Analytical SE` = c(
    sqrt(diag(vcov(m_gap_uncond)))[["bin_female"]],
    sqrt(diag(vcov(m_gap_hc)))[["bin_female"]],
    ols_se,
    sqrt(diag(vcov(m_gap_job)))[["bin_female"]]
  ),
  `Bootstrap SE` = c(NA_real_, NA_real_, fwl_se_boot, NA_real_),
  `Gap in percent` = pct(c(
    coef(m_gap_uncond)[["bin_female"]],
    coef(m_gap_hc)[["bin_female"]],
    coef(m_gap_pref)[["bin_female"]],
    coef(m_gap_job)[["bin_female"]]
  )),
  `R2` = c(
    summary(m_gap_uncond)$r.squared,
    summary(m_gap_hc)$r.squared,
    summary(m_gap_pref)$r.squared,
    summary(m_gap_job)$r.squared
  ),
  N = c(nobs(m_gap_uncond), nobs(m_gap_hc), nobs(m_gap_pref), nobs(m_gap_job))
)

gap_tex <- gap_table |>
  mutate(
    `Female coefficient` = fmt_num(`Female coefficient`, 3),
    `Analytical SE` = fmt_num(`Analytical SE`, 3),
    `Bootstrap SE` = ifelse(is.na(`Bootstrap SE`), "---", fmt_num(`Bootstrap SE`, 3)),
    `Gap in percent` = fmt_num(`Gap in percent`, 1),
    `R$^2$` = fmt_num(`R2`, 3),
    N = fmt_num(N, 0)
  ) |>
  select(Specification, `Female coefficient`, `Analytical SE`, `Bootstrap SE`,
         `Gap in percent`, `R$^2$`, N)

write_booktabs(gap_tex, file.path(path_tables, "tab_gender_gap.tex"), align = "lcccccc")

modelsummary(
  list(
    "Unconditional" = m_gap_uncond,
    "Human capital" = m_gap_hc,
    "Preferred" = m_gap_pref,
    "Job attributes" = m_gap_job
  ),
  output = file.path(path_tables, "tab_gender_gap_coefs.tex"),
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  coef_omit = "cat_relab|cat_educ|cat_size_firm",
  coef_rename = c(
    "(Intercept)" = "Intercept",
    bin_female = "Female",
    num_age = "Age",
    num_age2 = "Age squared",
    num_hours = "Weekly hours",
    bin_formal = "Formal"
  ),
  fmt = 3,
  notes = "Education, employment-type, and firm-size indicators omitted from the display. Full estimates in the repository."
)

fwl_check <- tibble(
  `OLS female coefficient` = fmt_num(ols_coef, 6),
  `FWL coefficient` = fmt_num(fwl_coef, 6),
  `Analytical SE` = fmt_num(ols_se, 4),
  `FWL bootstrap SE` = fmt_num(fwl_se_boot, 4),
  `Absolute difference` = fmt_num(abs(ols_coef - fwl_coef), 8)
)
write_booktabs(fwl_check, file.path(path_tables, "tab_fwl_check.tex"), align = "ccccc")

peak_gender_tex <- tibble(
  Group = c("Men", "Women"),
  `Peak age` = c(fmt_num(peaks_g$peak_men, 1), fmt_num(peaks_g$peak_women, 1)),
  `95\\% bootstrap CI` = c(
    paste0("[", fmt_num(ci_g$men[1], 1), ", ", fmt_num(ci_g$men[2], 1), "]"),
    paste0("[", fmt_num(ci_g$women[1], 1), ", ", fmt_num(ci_g$women[2], 1), "]")
  )
)
write_booktabs(peak_gender_tex, file.path(path_tables, "tab_gender_peaks.tex"), align = "lcc")

saveRDS(list(
  uncond = m_gap_uncond,
  hc = m_gap_hc,
  pref = m_gap_pref,
  job = m_gap_job,
  fwl = m_fwl_pref,
  profile = m_gap_profile,
  gap_table = gap_table,
  fwl_coef = fwl_coef,
  fwl_se_boot = fwl_se_boot,
  ols_coef = ols_coef,
  ols_se = ols_se,
  peaks = peaks_g,
  ci_peaks = ci_g
), file.path(path_temp, "section2_models.rds"))

# Descriptive motivation: the raw gap is not an artefact of
# a handful of ages or of part-time work alone.
fig_gap_by_educ <- geih |>
  mutate(sex = if_else(bin_female == 1, "Women", "Men")) |>
  group_by(cat_educ, sex) |>
  summarise(mean_income = mean(num_income), .groups = "drop") |>
  ggplot(aes(x = cat_educ, y = mean_income / 1e6, fill = sex)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = c("Men" = col_navy, "Women" = col_terra)) +
  labs(
    title = "Mean monthly labour income by education and gender",
    x = "Highest educational attainment",
    y = "Mean monthly labour income (million COP)",
    caption = "Source: Own calculations, GEIH 2018 (Bogotá sample)."
  ) +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

ggsave(file.path(path_figures, "fig_gap_by_education.pdf"), fig_gap_by_educ,
       width = 7.2, height = 4.4)
ggsave(file.path(path_figures, "fig_gap_by_education.png"), fig_gap_by_educ,
       width = 7.2, height = 4.4, dpi = 300)

modal_relab <- names(sort(table(geih$cat_relab), decreasing = TRUE))[1]
median_hours <- median(geih$num_hours)
modal_educ <- names(sort(table(geih$cat_educ), decreasing = TRUE))[1]

grid_g <- expand.grid(
  num_age = seq(18, 80, by = 1),
  bin_female = c(0, 1),
  KEEP.OUT.ATTRS = FALSE
) |>
  as_tibble() |>
  mutate(
    num_age2 = num_age^2,
    num_hours = median_hours,
    cat_relab = factor(modal_relab, levels = levels(geih$cat_relab)),
    cat_educ = factor(modal_educ, levels = levels(geih$cat_educ)),
    sex = if_else(bin_female == 1, "Women", "Men")
  )

grid_g$pred <- predict(m_gap_profile, newdata = grid_g)

fig_gap_profiles <- ggplot(grid_g, aes(x = num_age, y = pred, colour = sex)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = peaks_g$peak_men, colour = col_navy,
             linetype = "dotted", linewidth = 0.4) +
  geom_vline(xintercept = peaks_g$peak_women, colour = col_terra,
             linetype = "dotted", linewidth = 0.4) +
  scale_colour_manual(values = c("Men" = col_navy, "Women" = col_terra)) +
  labs(
    title = "Predicted age--income profiles by gender",
    subtitle = paste0("Preferred controls. Hours at the median; education = ",
                      modal_educ, "; employment type = ", modal_relab, "."),
    x = "Age",
    y = "Predicted log monthly labour income",
    caption = "Source: Own calculations, GEIH 2018 (Bogotá sample). Dotted lines mark implied peaks."
  )

ggsave(file.path(path_figures, "fig_gap_profiles.pdf"), fig_gap_profiles,
       width = 7.2, height = 4.4)
ggsave(file.path(path_figures, "fig_gap_profiles.png"), fig_gap_profiles,
       width = 7.2, height = 4.4, dpi = 300)

fig_gap_profiles_cop <- grid_g |>
  ggplot(aes(x = num_age, y = exp(pred) / 1e6, colour = sex)) +
  geom_line(linewidth = 1) +
  scale_colour_manual(values = c("Men" = col_navy, "Women" = col_terra)) +
  labs(
    title = "Predicted monthly labour income by age and gender",
    subtitle = "Exponentiated fitted values from the preferred conditional specification.",
    x = "Age",
    y = "Predicted monthly labour income (million COP)",
    caption = "Source: Own calculations, GEIH 2018 (Bogotá sample)."
  )

ggsave(file.path(path_figures, "fig_gap_profiles_cop.pdf"), fig_gap_profiles_cop,
       width = 7.2, height = 4.4)
ggsave(file.path(path_figures, "fig_gap_profiles_cop.png"), fig_gap_profiles_cop,
       width = 7.2, height = 4.4, dpi = 300)
