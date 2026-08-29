##########################################################
# 02_construct_analysis_sample.R
#
# Builds a single analysis sample used in all three
# sections. Household-level variables are constructed
# before the individual-level restriction, so the count of
# minors is not conditional on employment.
#
# Sample rule (problem set, unless otherwise stated):
# employed (ocu == 1) and age >= 18.
#
# Additional restrictions, applied uniformly:
# 1. Positive reported labour income (y_total_m > 0),
#    because the outcome is log(w).
# 2. Positive hours (totalHoursWorked > 0), required by
#    the Section 1 conditional profile.
# 3. Non-missing employment type (relab), required by the
#    same specification.
#
# Outputs:
# - 00_data/geih_analysis.rds
# - 02_outputs/tables/sample_flow.rds
##########################################################

text_vars <- c(
  id_building        = "directorio",
  id_household       = "secuencia_p",
  id_person          = "orden",
  num_age            = "age",
  bin_male           = "sex",
  bin_employed       = "ocu",
  bin_formal         = "formal",
  cat_educ           = "maxEducLevel",
  num_hours          = "totalHoursWorked",
  cat_relab          = "relab",
  cat_relationship   = "p6050",
  num_income         = "y_total_m",
  cat_estrato        = "estrato1",
  bin_college        = "college",
  bin_self_employed  = "cuentaPropia",
  cat_size_firm      = "sizeFirm",
  cat_oficio         = "oficio",
  id_chunk           = "id_chunk"
)

geih_hh <- geih_raw |>
  select(all_of(text_vars)) |>
  mutate(
    across(c(num_age, bin_male, bin_employed, bin_formal, cat_educ,
             num_hours, cat_relab, cat_relationship, cat_estrato,
             bin_college, bin_self_employed, cat_size_firm, cat_oficio,
             id_chunk), as.numeric),
    num_income = as.numeric(num_income)
  )

# Minors must be counted on the full household, including
# children who will later be dropped from the analysis.
geih_hh <- geih_hh |>
  mutate(bin_minor = if_else(num_age < 18, 1, 0)) |>
  group_by(id_building, id_household) |>
  mutate(num_minors = sum(bin_minor, na.rm = TRUE)) |>
  ungroup() |>
  select(-bin_minor)

n_raw <- nrow(geih_hh)

# relab follows the GEIH position-in-employment recode
# used to construct the hosted sample (labels.html does
# not list it; values are those of p6430).
relab_labels <- c(
  "1" = "Private employee",
  "2" = "Public employee",
  "3" = "Domestic worker",
  "4" = "Self-employed",
  "5" = "Employer",
  "6" = "Unpaid family worker",
  "7" = "Unpaid worker, other household",
  "8" = "Day labourer",
  "9" = "Other"
)

educ_labels <- c(
  "1" = "None",
  "2" = "Preschool",
  "3" = "Primary incomplete",
  "4" = "Primary complete",
  "5" = "Secondary incomplete",
  "6" = "Secondary complete",
  "7" = "Tertiary",
  "9" = "Missing/DK"
)

size_labels <- c(
  "1" = "Self-employed",
  "2" = "2-5 workers",
  "3" = "6-10 workers",
  "4" = "11-50 workers",
  "5" = ">50 workers"
)

geih <- geih_hh |>
  filter(bin_employed == 1, num_age >= 18) |>
  mutate(
    bin_head = if_else(cat_relationship == 1, 1, 0),
    bin_female = if_else(bin_male == 0, 1, 0),
    cat_educ = if_else(is.na(cat_educ) | cat_educ == 9, 9, cat_educ),
    cat_educ = factor(cat_educ, levels = c(1, 2, 3, 4, 5, 6, 7, 9),
                      labels = educ_labels),
    cat_educ = fct_relevel(cat_educ, "None"),
    cat_relab = factor(cat_relab, levels = 1:9, labels = relab_labels),
    cat_size_firm = factor(cat_size_firm, levels = 1:5, labels = size_labels),
    cat_estrato = factor(cat_estrato),
    bin_formal = as.integer(bin_formal),
    bin_college = as.integer(bin_college == 1),
    bin_self_employed = as.integer(bin_self_employed == 1)
  )

n_employed_adult <- nrow(geih)

# Unpaid positions mechanically produce missing or zero
# labour income. They are not the tax-authority population
# of people with reportable earnings, so they are dropped
# rather than imputed.
geih <- geih |>
  filter(
    !is.na(num_income), num_income > 0,
    !is.na(num_hours), num_hours > 0,
    !is.na(cat_relab),
    !is.na(num_age),
    !is.na(bin_formal),
    !is.na(cat_size_firm),
    !is.na(cat_estrato),
    !is.na(bin_head)
  )
n_reporters <- nrow(geih)

geih <- geih |>
  mutate(
    num_log_income = log(num_income),
    num_age2 = num_age^2,
    num_hours2 = num_hours^2,
    bin_train = if_else(id_chunk <= 7, 1, 0)
  )

# Hours above 18 per day, seven days a week, are treated as
# reporting errors. This is a data-quality filter, not an
# outlier rule based on residuals.
geih <- geih |> filter(num_hours <= 126)

sample_flow <- tibble(
  step = c(
    "Raw GEIH chunks (all household members)",
    "Employed and age >= 18",
    "Positive labour income, hours, and employment type",
    "Drop implausible hours (>126 per week)"
  ),
  n = c(n_raw, n_employed_adult, n_reporters, nrow(geih))
)

saveRDS(geih, file.path(path_data, "geih_analysis.rds"))
saveRDS(sample_flow, file.path(path_tables, "sample_flow.rds"))

desc_tex <- tibble(
  Statistic = c(
    "Observations",
    "Women (percent)",
    "Mean age",
    "Median weekly hours",
    "Median monthly labour income (COP)",
    "Mean monthly labour income (COP)",
    "Formal (percent)",
    "Tertiary education (percent)"
  ),
  Value = c(
    fmt_num(nrow(geih), 0),
    fmt_num(100 * mean(geih$bin_female), 1),
    fmt_num(mean(geih$num_age), 1),
    fmt_num(median(geih$num_hours), 0),
    fmt_num(median(geih$num_income), 0),
    fmt_num(mean(geih$num_income), 0),
    fmt_num(100 * mean(geih$bin_formal), 1),
    fmt_num(100 * mean(geih$cat_educ == "Tertiary"), 1)
  )
)
write_booktabs(desc_tex, file.path(path_tables, "tab_descriptives.tex"), align = "lc")

db_train <- geih |> filter(bin_train == 1)
db_valid <- geih |> filter(bin_train == 0)

message(
  "Analysis sample: ", nrow(geih),
  " | train (chunks 1-7): ", nrow(db_train),
  " | validation (chunks 8-10): ", nrow(db_valid)
)
