# Predicting Labor Income
## Problem Set 1 — MECA 4107

**Julián Herrera, Andrés Silva y Valentina Vera**  
Grupo 3 · Big Data and Machine Learning para Economía Aplicada · Universidad de los Andes · 2026-20

El repositorio replica el análisis de ingreso laboral en Bogotá (GEIH 2018) que responde: *qué puede decirle un modelo de ingreso a la autoridad tributaria sobre quién podría estar subdeclarando, y dónde se rompen sus predicciones.*

---

## Replicación

Con el directorio de trabajo en la raíz del repositorio:

```r
source("01_code/00_rundirectory.R")
```

Desde la terminal:

```bash
Rscript 01_code/00_rundirectory.R
```

Ese único comando:

1. Extrae los 10 chunks de la [página del curso](https://ignaciomsarmiento.github.io/GEIH2018_sample/) (o lee el cache en `00_data/` si ya existe).
2. Construye la muestra de análisis (ocupados de 18 años o más, con ingreso y horas positivos).
3. Estima el perfil edad–ingreso, la brecha de género y los modelos predictivos.
4. Escribe figuras (`.pdf`/`.png`) y tablas (`.tex`) en `02_outputs/`.

Las diapositivas se compilan después, desde `04_slides/`:

```bash
pdflatex age_equipo_03.tex
pdflatex gap_equipo_03.tex
pdflatex pred_equipo_03.tex
```

Los PDF para Bloque Neón quedan en `04_slides/` con los nombres pedidos: `age_equipo_03.pdf`, `gap_equipo_03.pdf`, `pred_equipo_03.pdf`.

---

## Scraping, una sola vez

Las páginas `page1.html`–`page10.html` son dinámicas: el HTML inicial no trae la tabla. JavaScript la inserta desde `pages/geih_page_N.html` (`w3-include-html`). El scraper replica esa solicitud, espera **15 segundos** entre chunks y guarda `00_data/geih_raw.rds`.

Correr de nuevo el master **no** vuelve a pegarle a la página. Para forzar una descarga nueva, borrar `00_data/geih_raw.rds`.

---

## Código

| Script | Responsabilidad |
|---|---|
| `00_rundirectory.R` | Master. Solo llama al resto. |
| `00_setup.R` | Paquetes (`pacman`), rutas con `here`, tema de figuras. |
| `functions/utils_estimation.R` | Pico de edad, bootstrap, FWL, LOOCV por leverage. |
| `01_scrape_geih_chunks.R` | Scraping educado y cache. |
| `02_construct_analysis_sample.R` | Muestra única para las tres secciones. |
| `03_estimate_age_income_profile.R` | Sección 1. |
| `04_estimate_gender_income_gap.R` | Sección 2, incluyendo FWL. |
| `05_predict_labor_income.R` | Sección 3: validación, LOOCV, importancia. |
| `06_write_slide_macros.R` | Números que leen las diapositivas. |
| `07_compile_slides.R` | Compila los tres PDF Beamer si encuentra `pdflatex`. |

---

## Salidas

Todo se genera en `02_outputs/`. No se edita a mano.

- Figuras: `02_outputs/figures/`
- Tablas: `02_outputs/tables/` (`.tex` para Beamer, no capturas de R)
- Macros: `02_outputs/tables/results_macros.tex`
- Diapositivas: `04_slides/age_equipo_03.pdf`, `gap_equipo_03.pdf`, `pred_equipo_03.pdf`

---

## Decisiones de muestra (las mismas en las tres secciones)

- Unidad: ocupado (`ocu == 1`) de 18 años o más, como pide el enunciado.
- Ingreso: `y_total_m > 0`. El outcome es $\log(w)$; ceros y missing no son imputables sin una historia de selección, y no son la población con ingreso reportable.
- Horas: `totalHoursWorked > 0` y no mayores a 126 por semana (18 horas × 7 días). Lo segundo es error de reporte, no un recorte por residuales.
- Variables de hogar (número de menores) se construyen **antes** de restringir a ocupados.
- Entrenamiento / validación: chunks 1–7 / 8–10, sobre esa misma muestra.

---

## Software

- R 4.6.1 (o 4.x)
- Paquetes (instalación automática vía `pacman::p_load`): `tidyverse`, `here`, `rvest`, `httr`, `jsonlite`, `caret`, `boot`, `modelsummary`, `gt`, `scales`, `broom`, `conflicted`, `sessioninfo`
- LaTeX (para las diapositivas): `pdflatex` con `beamer`, `booktabs`, `tikz`

Semilla: `4107` (código del curso).
