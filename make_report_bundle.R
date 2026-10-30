source("utils/project_setup.R")

# =============================================================================
# make_report_bundle.R
# Purpose:  Collect every file results_report.Rmd needs for download. All
#           files are aggregate CSVs (coefficients, fit status, GOF tables,
#           community-level counts) or PNG images (GOF plots, summary
#           figures); no .rds files and no individual-level data.
#           Output:
#             results/report_bundle/      flat folder with every CSV and PNG.
#                                         Download this folder; point
#                                         RESULTS_ROOT in results_report.Rmd at it.
# Inputs:   results/{ergm_ver}/ergm_*.csv, results/{stergm_ver}/stergm_*.csv,
#           results/{ergm_ver|stergm_ver}/<model>/figures/*_gof.png,
#           results/community_covariates.csv,
#           results/{descriptives_ver}/all_communities_descriptives*.csv, desc_*.csv,
#           results/{figures_ver}/*.png
# Outputs:  results/report_bundle/ with a checksum manifest.
# Date:     2026-10-19
# =============================================================================

# =============================================================================
# CONFIG
# =============================================================================
ergm_ver           <- "ergm_models"
stergm_ver         <- "stergm_models"
descriptives_ver   <- "descriptives"
figures_ver        <- "figures_report"
include_gof_images <- TRUE    # per-community GOF plots (one PNG per model x community)

# =============================================================================

library(here)

RESULTS_DIR <- here::here("results")
BUNDLE_DIR  <- file.path(RESULTS_DIR, "report_bundle")

csv_files <- c(
  list.files(file.path(RESULTS_DIR, ergm_ver), pattern = "^ergm_.*\\.csv$", full.names = TRUE),
  list.files(file.path(RESULTS_DIR, stergm_ver), pattern = "^stergm_.*\\.csv$", full.names = TRUE),
  file.path(RESULTS_DIR, "community_covariates.csv"),
  list.files(file.path(RESULTS_DIR, descriptives_ver),
             pattern = "^(all_communities_descriptives.*|desc_.*)\\.csv$", full.names = TRUE)
)
png_files <- c(
  list.files(file.path(RESULTS_DIR, figures_ver), pattern = "\\.png$", full.names = TRUE),
  if (include_gof_images) list.files(file.path(RESULTS_DIR, c(ergm_ver, stergm_ver)), pattern = "_gof\\.png$",
                                     recursive = TRUE, full.names = TRUE)
)
csv_files <- csv_files[file.exists(csv_files)]
required <- c("ergm_models_run.csv", "ergm_coefs.csv", "ergm_fit_info.csv",
              "stergm_models_run.csv", "stergm_coefs.csv", "stergm_fit_info.csv",
              "community_covariates.csv", "all_communities_descriptives_network_directed_full.csv")
if (!all(required %in% basename(csv_files)))
  stop("Missing report inputs: ", paste(setdiff(required, basename(csv_files)), collapse = ", "))
files <- c(csv_files, png_files)
if (anyDuplicated(basename(files))) stop("Duplicate file names would collide in the bundle: ",
                                         paste(unique(basename(files)[duplicated(basename(files))]), collapse = ", "))

# ---- Flat folder with everything ----
unlink(BUNDLE_DIR, recursive = TRUE)
dir.create(BUNDLE_DIR, recursive = TRUE)
ok <- file.copy(files, file.path(BUNDLE_DIR, basename(files)), overwrite = TRUE)
if (!all(ok)) stop("Could not copy: ", paste(basename(files)[!ok], collapse = ", "))
utils::write.csv(data.frame(file = basename(files), md5 = unname(tools::md5sum(files))),
                 file.path(BUNDLE_DIR, "checksums.csv"), row.names = FALSE)

# ---- Manifest ----
# Data rows per CSV. A result table with no columns is written as an empty
# (0-byte) file, which read.csv() cannot read; count it as 0 rows.
csv_rows <- function(f) {
  if (file.size(f) == 0) return(0L)
  tryCatch(nrow(utils::read.csv(f, check.names = FALSE)), error = function(e) 0L)
}
empty <- basename(csv_files[file.size(csv_files) == 0])
if (length(empty)) message("Empty CSV files (bundled, 0 rows): ", paste(empty, collapse = ", "))
manifest <- data.frame(
  file = basename(files),
  rows = c(vapply(csv_files, csv_rows, integer(1)),
           rep(NA_integer_, length(png_files))),
  kb = round(file.size(files) / 1024),
  modified = format(file.mtime(files), "%Y-%m-%d %H:%M"))
writeLines(c(paste("Report bundle written", format(Sys.time(), "%Y-%m-%d %H:%M")),
             sprintf("%d CSV files, %d images, %.1f MB in report_bundle/", length(csv_files), length(png_files),
                     sum(file.size(files)) / 1024^2), "",
             utils::capture.output(print(manifest, row.names = FALSE))),
           file.path(RESULTS_DIR, "report_bundle_manifest.txt"))
file.copy(file.path(RESULTS_DIR, "report_bundle_manifest.txt"), BUNDLE_DIR, overwrite = TRUE)

message(sprintf("Bundled %d CSVs and %d images into %s", length(csv_files), length(png_files), BUNDLE_DIR))
