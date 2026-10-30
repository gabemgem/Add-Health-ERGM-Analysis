source("utils/project_setup.R")

# Run from the supplement root to export tables without knitting the report.
# A nonblank paper_name in config selects a model and names its column.
RESULTS_ROOT <- Sys.getenv("SUPPLEMENT_RESULTS_ROOT", "reference_results")
OUT_DIR <- "outputs/tables_paper"
COMMUNITIES <- c("406" = "A", "408" = "B", "428" = "C", "477" = "D")
DIGITS <- 2

source("utils/report_helpers.R")
source("utils/model_pipeline.R")
source("utils/paper_table_helpers.R")

export_paper_tables(RESULTS_ROOT, OUT_DIR, COMMUNITIES, digits = DIGITS)
