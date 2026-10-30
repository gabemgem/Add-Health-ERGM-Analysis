# One entry point for saved-result reproduction or licensed-data refitting.
source("utils/project_setup.R")
args <- commandArgs(trailingOnly = TRUE)
mode <- sub("^--mode=", "", args[grepl("^--mode=", args)])
if (!length(mode)) mode <- "reference"
if (length(mode) != 1L || !mode %in% c("reference", "full", "check"))
  stop("Usage: Rscript run_analysis.R --mode=reference|full|check [--no-report]")
if (any(!grepl("^--mode=(reference|full|check)$|^--no-report$", args)))
  stop("Unrecognized argument: ", paste(args, collapse = " "))
source("scripts/check_inputs.R")
check_supplement(check_raw = identical(mode, "full"))
if (mode == "check") quit(status = 0)
dir.create("outputs", showWarnings = FALSE)
writeLines(c(paste("Mode:", mode), capture.output(sessionInfo())),
           file.path("outputs", paste0("sessionInfo_", mode, ".txt")))
if (mode == "full") {
  for (script in c("01_data_prep.R", "02_descriptives.R", "02b_network_structure.R",
                   "03_run_ergm_models.R", "04_run_stergm_models.R", "make_report_bundle.R")) {
    message("Running ", script)
    source(script, local = new.env(parent = globalenv()))
  }
}
Sys.setenv(SUPPLEMENT_RESULTS_ROOT = if (mode == "full") "results/report_bundle" else "reference_results")
if ("--no-report" %in% args) {
  source("paper_tables.R", local = new.env(parent = globalenv()))
  source("paper_figures.R", local = new.env(parent = globalenv()))
} else {
  if (!nzchar(rmarkdown::find_pandoc()$dir)) stop("Pandoc is required to render the report; see README.md.")
  rmarkdown::render("results_report.Rmd", output_dir = "outputs",
                    envir = new.env(parent = globalenv()), quiet = TRUE)
}
message("Completed ", mode, " workflow. See outputs/.")
