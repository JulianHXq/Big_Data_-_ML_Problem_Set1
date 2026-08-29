##########################################################
# 06_write_slide_macros.R
#
# Writes a LaTeX file of macros so the three Beamer decks
# read numbers from the estimates rather than from
# hand-copied text.
##########################################################

s1 <- readRDS(file.path(path_temp, "section1_models.rds"))
s2 <- readRDS(file.path(path_temp, "section2_models.rds"))
s3 <- readRDS(file.path(path_temp, "section3_models.rds"))

pct_gap <- function(b) 100 * (exp(b) - 1)

n_total <- nrow(geih)
n_train <- sum(geih$bin_train == 1)
n_valid <- sum(geih$bin_train == 0)
share_female <- mean(geih$bin_female)
median_income <- median(geih$num_income)
mean_income <- mean(geih$num_income)
median_hours <- median(geih$num_hours)
mean_age <- mean(geih$num_age)

macros <- list(
  nsample = fmt_num(n_total, 0),
  ntrain = fmt_num(n_train, 0),
  nvalid = fmt_num(n_valid, 0),
  sharefemale = fmt_num(100 * share_female, 1),
  medianincome = fmt_num(median_income, 0),
  meanincome = fmt_num(mean_income, 0),
  medianhours = fmt_num(median_hours, 0),
  meanage = fmt_num(mean_age, 1),
  peakuncond = fmt_num(s1$gof$peak_age[1], 1),
  peakuncondlo = fmt_num(s1$gof$ci_low[1], 1),
  peakuncondhi = fmt_num(s1$gof$ci_high[1], 1),
  peakcond = fmt_num(s1$gof$peak_age[2], 1),
  peakcondlo = fmt_num(s1$gof$ci_low[2], 1),
  peakcondhi = fmt_num(s1$gof$ci_high[2], 1),
  rtwoageu = fmt_num(s1$gof$r2[1], 3),
  rtwoagec = fmt_num(s1$gof$r2[2], 3),
  gapuncond = fmt_num(pct_gap(s2$gap_table$`Female coefficient`[1]), 1),
  gaphc = fmt_num(pct_gap(s2$gap_table$`Female coefficient`[2]), 1),
  gappref = fmt_num(pct_gap(s2$gap_table$`Female coefficient`[3]), 1),
  gapjob = fmt_num(pct_gap(s2$gap_table$`Female coefficient`[4]), 1),
  gapcoef = fmt_num(s2$ols_coef, 3),
  gapse = fmt_num(s2$ols_se, 3),
  gapseboot = fmt_num(s2$fwl_se_boot, 3),
  fwlcoef = fmt_num(s2$fwl_coef, 3),
  peakmen = fmt_num(s2$peaks$peak_men, 1),
  peakmenlo = fmt_num(s2$ci_peaks$men[1], 1),
  peakmenhi = fmt_num(s2$ci_peaks$men[2], 1),
  peakwomen = fmt_num(s2$peaks$peak_women, 1),
  peakwomenlo = fmt_num(s2$ci_peaks$women[1], 1),
  peakwomenhi = fmt_num(s2$ci_peaks$women[2], 1),
  bestmodel = s3$best_label,
  bestshort = if (startsWith(s3$best_id, "p_overfit")) {
    "P6: polinomio flexible"
  } else if (startsWith(s3$best_id, "p_firm")) {
    "P2: firma y estrato"
  } else {
    s3$best_label
  },
  gapuncondabs = fmt_num(abs(pct_gap(s2$gap_table$`Female coefficient`[1])), 1),
  gaphcabs = fmt_num(abs(pct_gap(s2$gap_table$`Female coefficient`[2])), 1),
  gapprefabs = fmt_num(abs(pct_gap(s2$gap_table$`Female coefficient`[3])), 1),
  bestvalid = fmt_num(s3$rmse_valid, 4),
  bestloocv = fmt_num(s3$rmse_loocv, 4),
  besttrain = fmt_num(s3$rmse_train, 4),
  topvar = s3$top_label,
  topdelta = fmt_num(s3$importance$delta_rmse[1], 4)
)

write_macros(macros, file.path(path_tables, "results_macros.tex"))
message("Wrote LaTeX macros to 02_outputs/tables/results_macros.tex")
