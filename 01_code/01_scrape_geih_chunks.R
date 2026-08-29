##########################################################
# 01_scrape_geih_chunks.R
#
# Scrapes the 10 GEIH 2018 Bogotá chunks hosted at
# https://ignaciomsarmiento.github.io/GEIH2018_sample/
# and the variable dictionary.
#
# The listing pages (page1.html, ..., page10.html) are
# dynamic: the HTML downloaded by read_html() does not
# contain the table. JavaScript inserts it from
# pages/geih_page_N.html via a w3-include-html request.
# Following the class workflow for dynamic pages, we
# replicate that request rather than parsing an empty
# shell.
#
# Politeness:
# - 15-second pause between chunks
# - cache to 00_data/ so later runs do not hit the server
#
# Outputs:
# - 00_data/geih_raw.rds
# - 00_data/dictionary.rds
# - 00_data/scrape_log.rds
##########################################################

path_raw <- file.path(path_data, "geih_raw.rds")
path_dic <- file.path(path_data, "dictionary.rds")
path_log <- file.path(path_data, "scrape_log.rds")

base_url <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/"
chunk_pause_sec <- 15

scrape_dictionary <- function(base_url) {
  dic_url <- paste0(base_url, "dictionary.html")
  message("Scraping dictionary: ", dic_url)
  dic_html <- read_html(dic_url)
  dictionary <- dic_html |>
    html_element(xpath = "/html/body/div/div/div[2]/table") |>
    html_table()
  if (nrow(dictionary) == 0) {
    dictionary <- dic_html |>
      html_element("table") |>
      html_table()
  }
  dictionary |>
    rename(variable = 1, description = 2) |>
    mutate(across(everything(), as.character))
}

#' Resolve the table URL hidden behind the dynamic include.
#'
#' pageN.html only contains
#'   <div w3-include-html="pages/geih_page_N.html"></div>
#' The browser then GETs that file. We do the same with rvest.
resolve_chunk_table_url <- function(page_url) {
  page_html <- read_html(page_url)
  include_path <- page_html |>
    html_element("[w3-include-html]") |>
    html_attr("w3-include-html")
  if (is.na(include_path) || include_path == "") {
    stop("Could not find w3-include-html on ", page_url)
  }
  url_absolute(include_path, page_url)
}

scrape_one_chunk <- function(chunk_id, base_url) {
  page_url <- paste0(base_url, "page", chunk_id, ".html")
  message("Resolving dynamic include for chunk ", chunk_id, ": ", page_url)
  table_url <- resolve_chunk_table_url(page_url)
  message("Fetching table: ", table_url)

  table_html <- read_html(table_url)
  chunk <- table_html |>
    html_element("table") |>
    html_table()
  if (nrow(chunk) == 0) {
    stop("Empty table for chunk ", chunk_id)
  }

  # tableHTML inserts an unnamed row-index column. dplyr cannot
  # mutate a frame whose names are NA or empty, so drop/repair
  # names before any tidyverse verb.
  nm <- names(chunk)
  drop_first <- is.na(nm[1]) || !nzchar(trimws(nm[1])) ||
    nm[1] %in% c("X1", "Var.1", "...1")
  if (drop_first) {
    chunk <- chunk[, -1, drop = FALSE]
    nm <- names(chunk)
  }
  empty_nm <- is.na(nm) | !nzchar(trimws(nm))
  if (any(empty_nm)) {
    nm[empty_nm] <- paste0("v", which(empty_nm))
    names(chunk) <- nm
  }

  chunk <- as_tibble(chunk) |>
    mutate(across(where(is.character), ~ ifelse(.x %in% c("NA", "na", ""), NA_character_, .x))) |>
    mutate(id_chunk = as.integer(chunk_id), .before = 1)
  chunk
}

if (file.exists(path_raw) && file.exists(path_dic)) {
  message("Cache found in 00_data/. Skipping scrape. ",
          "Delete geih_raw.rds to force a new download.")
  geih_raw <- readRDS(path_raw)
  dictionary <- readRDS(path_dic)
} else {
  scrape_started <- Sys.time()
  dictionary <- scrape_dictionary(base_url)
  saveRDS(dictionary, path_dic)
  Sys.sleep(chunk_pause_sec)

  chunks <- vector("list", 10)
  for (i in 1:10) {
    chunks[[i]] <- scrape_one_chunk(i, base_url)
    if (i < 10) {
      message("Pausing ", chunk_pause_sec, " seconds before the next chunk.")
      Sys.sleep(chunk_pause_sec)
    }
  }

  geih_raw <- bind_rows(chunks)
  saveRDS(geih_raw, path_raw)

  scrape_log <- tibble(
    scraped_at = scrape_started,
    finished_at = Sys.time(),
    n_rows = nrow(geih_raw),
    n_cols = ncol(geih_raw),
    n_chunks = n_distinct(geih_raw$id_chunk),
    pause_seconds = chunk_pause_sec,
    source = base_url
  )
  saveRDS(scrape_log, path_log)
  message("Saved ", nrow(geih_raw), " rows to ", path_raw)
}

stopifnot(n_distinct(geih_raw$id_chunk) == 10)
message("Raw GEIH: ", nrow(geih_raw), " rows, ", ncol(geih_raw), " columns.")
