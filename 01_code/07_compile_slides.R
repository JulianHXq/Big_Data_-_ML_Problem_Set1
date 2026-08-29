##########################################################
# 07_compile_slides.R
#
# Compiles the three Beamer decks. Requires pdflatex.
# Called last by 00_rundirectory.R when pdflatex is found.
##########################################################

find_pdflatex <- function() {
  candidates <- c(
    Sys.which("pdflatex"),
    "C:/Users/USUARIO/AppData/Local/Programs/MiKTeX/miktex/bin/x64/pdflatex.exe",
    file.path(Sys.getenv("LOCALAPPDATA"), "Programs/MiKTeX/miktex/bin/x64/pdflatex.exe"),
    file.path(Sys.getenv("APPDATA"), "TinyTeX/bin/windows/pdflatex.exe")
  )
  candidates <- candidates[nzchar(candidates)]
  hit <- candidates[file.exists(candidates)]
  if (length(hit) == 0) {
    return(NA_character_)
  }
  hit[[1]]
}

pdflatex <- find_pdflatex()
if (is.na(pdflatex)) {
  message("pdflatex not found. Compile 04_slides/*.tex manually.")
} else {
  decks <- c("age_equipo_03.tex", "gap_equipo_03.tex", "pred_equipo_03.tex")
  old <- setwd(path_slides)
  on.exit(setwd(old), add = TRUE)
  for (deck in decks) {
    message("Compiling ", deck)
    for (pass in 1:2) {
      status <- system2(pdflatex, c("-interaction=nonstopmode", deck), stdout = TRUE, stderr = TRUE)
      if (!is.null(attr(status, "status")) && attr(status, "status") != 0 && pass == 2) {
        warning("pdflatex failed on ", deck)
      }
    }
  }
  message("Slide PDFs written to 04_slides/.")
}
