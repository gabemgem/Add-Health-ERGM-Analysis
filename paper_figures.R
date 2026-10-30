source("utils/project_setup.R")

# =============================================================================
# paper_figures.R
# Purpose:  Caterpillar plots of per-community ERGM / STERGM estimates for the
#           paper, without knitting results_report.Rmd. One panel per term;
#           each row is a community (sorted by its estimate) with its 95% CI.
#           Filled blue = CI excludes 0 (p < .05); open grey = not significant.
#           Featured communities are ringed in orange and labelled.
#           Reads only the aggregate coefficient CSVs (no individual data).
# Inputs:   {RESULTS_ROOT}/.../ergm_coefs.csv, ergm_fit_info.csv,
#           stergm_coefs.csv, stergm_fit_info.csv (the report bundle; files
#           are found by name anywhere under RESULTS_ROOT)
# Outputs:  {OUT_DIR}/<name>_part1.pdf / .png, etc. (maximum six panels per part)
# Date:     2026-10-21
# =============================================================================

# =============================================================================
# CONFIG
# =============================================================================
RESULTS_ROOT <- Sys.getenv("SUPPLEMENT_RESULTS_ROOT", "reference_results")  # downloaded report bundle (same as results_report.Rmd)
OUT_DIR      <- "outputs/figures_paper"

# Default highlighted communities: ids (labelled "A", "B", ... in order) or
# ids named with the label to print, e.g. c("12" = "A", "45" = "B", "67" = "C").
# Print pseudonyms, not school ids. c() = no highlighting.
FEATURED <- c("406"="406","408"="408","428"="428","477"="477")

# Row order, one set of figures per entry: "estimate" (each panel sorted by
# its own estimates) and/or "community" (rows sorted by community id, the same
# in every panel, so a community can be followed across terms; files end in
# "_by_id"). A figure's own `order` overrides this.
ORDERS <- c("estimate", "community")
# Print community ids beside the rows in the "community" set. They are school
# ids, so keep FALSE for figures that go in the paper.
ROW_LABELS <- FALSE

# One entry per figure:
#   name     file name (without extension)
#   type     "ergm" or "stergm"
#   model    model id from config/ergm_model_desc.csv or config/stergm_model_desc.csv
#   part     "ergm", "formation" or "dissolution" (persistence)
#   terms    NULL = all terms, or raw term names to choose and order the panels
#            (print_terms() below lists them)
#   featured optional; overrides FEATURED for this figure
#   order    optional; overrides ORDERS for this figure
#   ncol, width, height  optional layout (defaults 3 panels per row, 7 in wide,
#            height fitted to the number of panel rows)
FIGURES <- list(
  list(name = "ergm_m14d",         type = "ergm",   model = "m14d", part = "ergm"),
  list(name = "stergm_m7f3d3_formation", type = "stergm", model = "m7f3d3", part = "formation"),
  list(name = "stergm_m7f3d3_dissolution", type = "stergm", model = "m7f3d3", part = "dissolution"),
  list(name = "stergm_m7f4d3_formation", type = "stergm", model = "m7f4d3", part = "formation"),
  list(name = "stergm_m7f4d3_dissolution", type = "stergm", model = "m7f4d3", part = "dissolution")
)
# =============================================================================

# Run from this folder (like results_report.Rmd), wherever the bundle was downloaded.
suppressPackageStartupMessages(library(tidyverse))
source("utils/report_helpers.R")

idx <- result_index(RESULTS_ROOT)
results <- list(
  ergm   = list(coefs = read_result(idx, "ergm_coefs.csv"),   info = read_result(idx, "ergm_fit_info.csv")),
  stergm = list(coefs = read_result(idx, "stergm_coefs.csv"), info = read_result(idx, "stergm_fit_info.csv"))
)

# Raw term names available for one model and part, to use in `terms`.
print_terms <- function(type, model, part) {
  unique(filter(results[[type]]$coefs, model_id == model, part == !!part)$term)
}

for (fig in FIGURES) {
  res <- results[[fig$type]]
  if (is.null(res$coefs)) { message(fig$name, ": no ", fig$type, " coefficients found under ", RESULTS_ROOT); next }
  for (fig_order in if (is.null(fig$order)) ORDERS else fig$order) {
    name <- paste0(fig$name, if (fig_order == "community") "_by_id" else "")
    r <- save_caterpillar(
      res$coefs, fig$model, fig$part, file.path(OUT_DIR, name),
      terms      = fig$terms,
      featured   = featured_labels(if (is.null(fig$featured)) FEATURED else fig$featured),
      ncol       = if (is.null(fig$ncol)) 3 else fig$ncol,
      width      = if (is.null(fig$width)) 7 else fig$width,
      height     = fig$height,
      info       = res$info,
      order      = fig_order,
      row_labels = ROW_LABELS)
    if (is.null(r)) { message(name, ": no usable estimates for ", fig$model, " / ", fig$part); next }
    message(sprintf("%s: %s communities shown of %s fit -> %s", name, r$n_shown, r$n_fit,
                    paste(r$files, collapse = ", ")))
  }
}
