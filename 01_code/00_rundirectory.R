##########################################################
# Master script
#
# Running this file reproduces all results in the repository.
#
# From an interactive R session, with the working directory
# at the project root:
#   source("01_code/00_rundirectory.R")
#
# Or from the command line:
#   Rscript 01_code/00_rundirectory.R
#
# Authors:
# - Julian Herrera
# - Andres Silva
# - Valentina Vera
#
# Course: MECA 4107 — Big Data and Machine Learning
#         for Applied Economics, Universidad de los Andes
##########################################################

args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grepl("^--file=", args)])
if (length(file_arg) == 1 && nzchar(file_arg)) {
  setwd(dirname(dirname(normalizePath(file_arg))))
} else if (file.exists("01_code/00_setup.R")) {
  # already at the project root
} else if (requireNamespace("here", quietly = TRUE)) {
  setwd(here::here())
}

source("01_code/00_setup.R")
source("01_code/functions/utils_estimation.R")
source("01_code/01_scrape_geih_chunks.R")
source("01_code/02_construct_analysis_sample.R")
source("01_code/03_estimate_age_income_profile.R")
source("01_code/04_estimate_gender_income_gap.R")
source("01_code/05_predict_labor_income.R")
source("01_code/06_write_slide_macros.R")
source("01_code/07_compile_slides.R")

message("Replication finished. Tables and figures are in 02_outputs/.")
sessioninfo::session_info()
