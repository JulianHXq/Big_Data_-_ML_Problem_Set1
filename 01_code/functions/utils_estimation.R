##########################################################
# utils_estimation.R
#
# Shared estimation helpers. Each function encodes one
# operation used in more than one section, so the three
# analysis scripts do not copy-paste algebra.
##########################################################

#' Implied peak age of a quadratic age profile.
#'
#' For log(w) = a + b1 * age + b2 * age^2, the vertex is
#' at age* = -b1 / (2 * b2). A concave profile (b2 < 0)
#' is an inverted-U, as predicted by a standard Ben-Porath
#' human-capital model.
peak_age_from_coefs <- function(beta_age, beta_age2) {
  if (is.na(beta_age) || is.na(beta_age2) || beta_age2 == 0) {
    return(NA_real_)
  }
  -beta_age / (2 * beta_age2)
}

extract_peak_age <- function(model, name_age = "num_age", name_age2 = "num_age2") {
  b <- coef(model)
  peak_age_from_coefs(b[[name_age]], b[[name_age2]])
}

#' Nonparametric bootstrap CI for the implied peak age.
#'
#' We bootstrap the whole estimation sample rather than
#' using the delta method because age* is a nonlinear
#' function of two coefficients, and because the problem
#' set asks explicitly for a bootstrap interval.
bootstrap_peak_age <- function(data,
                               formula,
                               name_age = "num_age",
                               name_age2 = "num_age2",
                               n_boot = 1000,
                               seed = 4107,
                               probs = c(0.025, 0.975)) {
  set.seed(seed)
  n <- nrow(data)
  peaks <- replicate(n_boot, {
    idx <- sample.int(n, n, replace = TRUE)
    fit <- try(lm(formula, data = data[idx, , drop = FALSE]), silent = TRUE)
    if (inherits(fit, "try-error")) {
      return(NA_real_)
    }
    extract_peak_age(fit, name_age, name_age2)
  })
  stats::quantile(peaks, probs = probs, na.rm = TRUE, names = FALSE)
}

#' Gender-specific peaks from female * (age + age2).
#'
#' Men (female = 0):  -b_age / (2 * b_age2)
#' Women (female = 1): -(b_age + b_f_age) / (2 * (b_age2 + b_f_age2))
peaks_by_gender <- function(model) {
  b <- coef(model)
  b_age <- b[["num_age"]]
  b_age2 <- b[["num_age2"]]
  b_f_age <- b[["bin_female:num_age"]]
  b_f_age2 <- b[["bin_female:num_age2"]]
  if (is.na(b_f_age)) b_f_age <- 0
  if (is.na(b_f_age2)) b_f_age2 <- 0
  list(
    peak_men = peak_age_from_coefs(b_age, b_age2),
    peak_women = peak_age_from_coefs(b_age + b_f_age, b_age2 + b_f_age2)
  )
}

bootstrap_peaks_by_gender <- function(data,
                                      formula,
                                      n_boot = 1000,
                                      seed = 4107) {
  set.seed(seed)
  n <- nrow(data)
  draws <- replicate(n_boot, {
    idx <- sample.int(n, n, replace = TRUE)
    fit <- try(lm(formula, data = data[idx, , drop = FALSE]), silent = TRUE)
    if (inherits(fit, "try-error")) {
      return(c(NA_real_, NA_real_))
    }
    p <- peaks_by_gender(fit)
    c(p$peak_men, p$peak_women)
  })
  list(
    men = stats::quantile(draws[1, ], probs = c(0.025, 0.975), na.rm = TRUE, names = FALSE),
    women = stats::quantile(draws[2, ], probs = c(0.025, 0.975), na.rm = TRUE, names = FALSE)
  )
}

#' Frisch-Waugh-Lovell coefficient on bin_female.
#'
#' Residualise log income and the female indicator on the
#' same controls, then regress residual-on-residual without
#' an intercept. FWL recovers the partialled-out OLS
#' coefficient; it does not change the interpretation.
fwl_gender <- function(data, controls_formula) {
  f_y <- update(controls_formula, num_log_income ~ .)
  f_x <- update(controls_formula, bin_female ~ .)
  y_tilde <- resid(lm(f_y, data = data))
  x_tilde <- resid(lm(f_x, data = data))
  lm(y_tilde ~ 0 + x_tilde)
}

bootstrap_fwl_se <- function(data,
                             controls_formula,
                             n_boot = 1000,
                             seed = 4107) {
  set.seed(seed)
  n <- nrow(data)
  coefs <- replicate(n_boot, {
    idx <- sample.int(n, n, replace = TRUE)
    fit <- try(fwl_gender(data[idx, , drop = FALSE], controls_formula), silent = TRUE)
    if (inherits(fit, "try-error")) {
      return(NA_real_)
    }
    unname(coef(fit)[["x_tilde"]])
  })
  sd(coefs, na.rm = TRUE)
}

#' Exact OLS LOOCV RMSE via the leverage formula from class.
#'
#' RMSE_LOOCV = sqrt(mean((e_i / (1 - h_ii))^2))
#'
#' A brute-force loop would refit the model n times. For OLS
#' the leave-one-out residual is the in-sample residual scaled
#' by 1 / (1 - h_ii), so the calculation is a single lm().
loocv_rmse_ols <- function(model) {
  e <- residuals(model)
  h <- hatvalues(model)
  h <- pmin(h, 1 - 1e-12)
  sqrt(mean((e / (1 - h))^2))
}

rmse_newdata <- function(model, newdata, outcome = "num_log_income") {
  pred <- predict(model, newdata = newdata)
  caret::RMSE(pred = pred, obs = newdata[[outcome]])
}

fmt_num <- function(x, digits = 2) {
  formatC(x, format = "f", digits = digits, big.mark = ",")
}

fmt_pct <- function(x, digits = 1) {
  paste0(formatC(100 * x, format = "f", digits = digits), "\\%")
}

tex_escape <- function(x) {
  x <- as.character(x)
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("%", "\\\\%", x)
  x <- gsub("&", "\\\\&", x)
  x <- gsub("_", "\\\\_", x)
  x <- gsub("#", "\\\\#", x)
  x
}

write_macros <- function(macros, path) {
  lines <- vapply(names(macros), function(nm) {
    sprintf("\\newcommand{\\%s}{%s}", nm, tex_escape(macros[[nm]]))
  }, character(1))
  writeLines(lines, con = path)
}

write_booktabs <- function(df, path, align = NULL) {
  if (is.null(align)) {
    align <- paste(rep("l", ncol(df)), collapse = "")
  }
  header <- paste(names(df), collapse = " & ")
  rows <- apply(as.matrix(df), 1, function(r) paste(r, collapse = " & "))
  tex <- c(
    paste0("\\begin{tabular}{", align, "}"),
    "\\toprule",
    paste0(header, " \\\\"),
    "\\midrule",
    paste0(rows, " \\\\"),
    "\\bottomrule",
    "\\end{tabular}"
  )
  writeLines(tex, con = path)
}
