##########################################################
# 03_estimate_age_income_profile.R
#
# Section 1. Unconditional and conditional age-income
# profiles. The quadratic is the empirical counterpart of
# a Ben-Porath life-cycle human-capital model: investment
# is front-loaded, so earnings rise and then flatten or
# fall. Hours and employment type are the only additional
# controls, as required.
#
# Outputs: figures and .tex tables used by age_equipo_03.
##########################################################

f_age_uncond <- num_log_income ~ num_age + num_age2
f_age_cond   <- num_log_income ~ num_age + num_age2 + num_hours + cat_relab

m_age_uncond <- lm(f_age_uncond, data = geih)
m_age_cond   <- lm(f_age_cond, data = geih)

peak_uncond <- extract_peak_age(m_age_uncond)
peak_cond   <- extract_peak_age(m_age_cond)

message("Bootstrapping peak-age intervals (Section 1)...")
ci_uncond <- bootstrap_peak_age(geih, f_age_uncond)
ci_cond   <- bootstrap_peak_age(geih, f_age_cond)

age_gof <- tibble(
  specification = c("Unconditional", "Conditional"),
  peak_age = c(peak_uncond, peak_cond),
  ci_low = c(ci_uncond[1], ci_cond[1]),
  ci_high = c(ci_uncond[2], ci_cond[2]),
  r2 = c(summary(m_age_uncond)$r.squared, summary(m_age_cond)$r.squared),
  adj_r2 = c(summary(m_age_uncond)$adj.r.squared, summary(m_age_cond)$adj.r.squared),
  n = c(nobs(m_age_uncond), nobs(m_age_cond))
)

modelsummary(
  list("Unconditional" = m_age_uncond, "Conditional" = m_age_cond),
  output = file.path(path_tables, "tab_age_income_coefs.tex"),
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  coef_rename = c(
    "(Intercept)" = "Intercept",
    num_age = "Age",
    num_age2 = "Age squared",
    num_hours = "Weekly hours"
  ),
  fmt = 4,
  escape = FALSE,
  title = "Age--labour income profiles, employed adults in Bogot\\'a."
)

age_gof_tex <- age_gof |>
  mutate(
    `Peak age` = fmt_num(peak_age, 1),
    `95\\% bootstrap CI` = paste0("[", fmt_num(ci_low, 1), ", ", fmt_num(ci_high, 1), "]"),
    `R$^2$` = fmt_num(r2, 3),
    `Adj. R$^2$` = fmt_num(adj_r2, 3),
    N = fmt_num(n, 0),
    Specification = specification
  ) |>
  select(Specification, `Peak age`, `95\\% bootstrap CI`, `R$^2$`, `Adj. R$^2$`, N)

write_booktabs(age_gof_tex, file.path(path_tables, "tab_age_income_peaks.tex"), align = "lccccc")

saveRDS(list(
  uncond = m_age_uncond,
  cond = m_age_cond,
  gof = age_gof
), file.path(path_temp, "section1_models.rds"))

# Binned means motivate the quadratic before any regression.
age_bins <- geih |>
  mutate(age_int = num_age) |>
  group_by(age_int) |>
  summarise(
    mean_log_w = mean(num_log_income),
    n = n(),
    .groups = "drop"
  ) |>
  filter(n >= 20)

fig_age_bins <- ggplot(age_bins, aes(x = age_int, y = mean_log_w)) +
  geom_point(colour = col_navy, alpha = 0.7, size = 1.6) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = TRUE,
              colour = col_terra, fill = col_terra, alpha = 0.15, linewidth = 0.8) +
  labs(
    title = "Mean log labour income by age",
    subtitle = "Employed adults, Bogotá 2018. Bins with fewer than 20 observations omitted.",
    x = "Age",
    y = "Mean log monthly labour income",
    caption = "Source: Own calculations, GEIH 2018 (Bogotá sample)."
  )

ggsave(file.path(path_figures, "fig_age_binned_means.pdf"), fig_age_bins,
       width = 7.2, height = 4.4)
ggsave(file.path(path_figures, "fig_age_binned_means.png"), fig_age_bins,
       width = 7.2, height = 4.4, dpi = 300)

# Predicted profiles. Conditional path holds hours at the
# sample median and employment type at the modal category,
# so the two curves differ only through the age coefficients
# and through the shift implied by those controls.
modal_relab <- names(sort(table(geih$cat_relab), decreasing = TRUE))[1]
median_hours <- median(geih$num_hours)

age_grid <- tibble(
  num_age = seq(18, 80, by = 1),
  num_age2 = num_age^2,
  num_hours = median_hours,
  cat_relab = factor(modal_relab, levels = levels(geih$cat_relab))
)

age_grid$pred_uncond <- predict(m_age_uncond, newdata = age_grid)
age_grid$pred_cond   <- predict(m_age_cond, newdata = age_grid)

fig_age_profiles <- age_grid |>
  pivot_longer(c(pred_uncond, pred_cond), names_to = "spec", values_to = "pred") |>
  mutate(spec = recode(spec,
                       pred_uncond = "Unconditional",
                       pred_cond = "Conditional (hours and employment type)")) |>
  ggplot(aes(x = num_age, y = pred, colour = spec, linetype = spec)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = peak_uncond, colour = col_navy, linetype = "dotted", linewidth = 0.4) +
  geom_vline(xintercept = peak_cond, colour = col_terra, linetype = "dotted", linewidth = 0.4) +
  scale_colour_manual(values = c("Unconditional" = col_navy,
                                 "Conditional (hours and employment type)" = col_terra)) +
  labs(
    title = "Predicted log labour income over the life cycle",
    subtitle = paste0("Conditional profile at median hours (", round(median_hours),
                      " per week) and modal employment type (", modal_relab, ")."),
    x = "Age",
    y = "Predicted log monthly labour income",
    caption = "Source: Own calculations, GEIH 2018 (Bogotá sample). Dotted lines mark implied peaks."
  )

ggsave(file.path(path_figures, "fig_age_profiles.pdf"), fig_age_profiles,
       width = 7.2, height = 4.4)
ggsave(file.path(path_figures, "fig_age_profiles.png"), fig_age_profiles,
       width = 7.2, height = 4.4, dpi = 300)

# Levels (COP), more interpretable for a tax-authority audience.
fig_age_profiles_cop <- age_grid |>
  mutate(
    Unconditional = exp(pred_uncond),
    Conditional = exp(pred_cond)
  ) |>
  pivot_longer(c(Unconditional, Conditional), names_to = "spec", values_to = "pred_cop") |>
  ggplot(aes(x = num_age, y = pred_cop / 1e6, colour = spec)) +
  geom_line(linewidth = 1) +
  scale_colour_manual(values = c("Unconditional" = col_navy, "Conditional" = col_terra)) +
  labs(
    title = "Predicted monthly labour income over the life cycle",
    subtitle = "Exponentiated fitted values. Conditional profile at median hours and modal employment type.",
    x = "Age",
    y = "Predicted monthly labour income (million COP)",
    caption = "Source: Own calculations, GEIH 2018 (Bogotá sample)."
  )

ggsave(file.path(path_figures, "fig_age_profiles_cop.pdf"), fig_age_profiles_cop,
       width = 7.2, height = 4.4)
ggsave(file.path(path_figures, "fig_age_profiles_cop.png"), fig_age_profiles_cop,
       width = 7.2, height = 4.4, dpi = 300)
