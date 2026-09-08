##########################################################
# 00_setup.R
# Cargar los paquetes, resuelve conflictos de nombres y define
#
# 00_rundirectory.R antes que otros codigos.
##########################################################

if (!require(pacman)) {
  install.packages("pacman")
  library(pacman)
}

p_load(
  tidyverse,    # wrangling and ggplot2
  here,         # project-relative paths
  rvest,        # static HTML / included tables
  httr,         # replicate the dynamic-page request
  jsonlite,     # JSON responses, if needed
  caret,        # RMSE and resampling helpers (class workflow)
  boot,         # nonparametric bootstrap
  modelsummary, # publication tables in .tex
  gt,           # descriptive tables
  scales,       # axis labels
  broom,        # tidy coefficient tables
  conflicted,   # make function conflicts explicit
  sessioninfo
)

conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("RMSE", "caret")

options(scipen = 999)
set.seed(4107)

path_data      <- here("00_data")
path_temp      <- here("03_temp")
path_figures   <- here("02_outputs", "figures")
path_tables    <- here("02_outputs", "tables")
path_slides    <- here("04_slides")

dir.create(path_data, recursive = TRUE, showWarnings = FALSE)
dir.create(path_temp, recursive = TRUE, showWarnings = FALSE)
dir.create(path_figures, recursive = TRUE, showWarnings = FALSE)
dir.create(path_tables, recursive = TRUE, showWarnings = FALSE)
dir.create(path_slides, recursive = TRUE, showWarnings = FALSE)

# Colores para los graficos de la entrega del Set problem 1.
col_navy   <- "#3a5e8c"
col_terra  <- "#b85c38"
col_slate  <- "#5b6b73"
col_gold   <- "#c4a35a"

# Definir un tema de ggplot2 para los graficos de la entrega del Set problem 1.
theme_bdml <- function(base_size = 12) {
  theme_classic(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", colour = "#1b365d", size = base_size + 1),
      plot.subtitle = element_text(colour = col_slate, size = base_size - 1),
      plot.caption = element_text(hjust = 0, size = 8, colour = col_slate),
      axis.title = element_text(colour = "#1b365d"),
      legend.position = "bottom",
      legend.title = element_blank()
    )
}

theme_set(theme_bdml())
