source("utils/project_setup.R")

# =============================================================================
# 03_run_ergm_models.R
# Purpose:  Fit every wave 1 directed ERGM defined in config/ergm_model_desc.csv to every
#           community, then pool the per-community estimates with a
#           random-effects meta-analysis (two-step multi-network method).
#           Communities whose results are already saved for the same model
#           definition, data and settings are skipped, so the script can be
#           re-run after adding models or after an interrupted run.
# Inputs:   config/ergm_model_desc.csv
#           data/{input_ver}/community_{cid}_network_directed_w1.rds
#           data/{input_ver}/community_{cid}_nodal_covariates.rds   (shared-club edgecov)
#           data/{input_ver}/all_communities_descriptives.csv
# Outputs:  results/{output_ver}/<model_id>/community_{cid}_{info,coefs,corr,gof}.csv
#           results/{output_ver}/<model_id>/community_{cid}_model.rds   (stays on this machine)
#           results/{output_ver}/<model_id>/logs/community_{cid}.log
#           results/{output_ver}/ergm_{models_run,fit_info,coefs,gof,coef_corr,
#                                      meta,meta_regression,selection}.csv
#           results/community_covariates.csv
#           Every file needed by results_report.Rmd is CSV/text; collect them
#           with make_report_bundle.R.
# Prerequisite: 01_data_prep.R.
# Seed:     20240102 (settings$seed)
# Date:     2026-10-19
# =============================================================================

# =============================================================================
# CONFIG
# =============================================================================
input_ver     <- "processed"     # Subdirectory under data/ for network inputs
output_ver    <- "ergm_models"   # Subdirectory under results/ for outputs
only_models   <- NULL            # e.g. c("m14c", "m14d") to run a subset of config/ergm_model_desc.csv
force_rerun   <- FALSE           # TRUE = refit every community even if results exist
n_workers     <- as.integer(Sys.getenv("SUPPLEMENT_WORKERS", "1"))   # communities fit in parallel
if (is.na(n_workers) || n_workers < 1L) stop("SUPPLEMENT_WORKERS must be a positive integer.")
message(paste0("Running ERGM pipeline with ", n_workers, " parallel workers."))
time_limit    <- 21600            # seconds before one community's fit is killed
exclude_small <- TRUE            # primary meta-analysis excludes small communities (n < 50)

settings <- list()               # overrides of default_model_settings(), e.g. list(run_gof = FALSE)

# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(network)
  library(here)
})
source(here::here("utils", "model_pipeline.R"))
source(here::here("utils", "meta_trust.R"))

ctx <- list(
  type = "ergm",
  data_dir = here::here("data", input_ver),
  out_root = here::here("results", output_ver),
  utils_dir = here::here("utils"),
  settings = modifyList(default_model_settings(), settings),
  n_workers = n_workers, time_limit = time_limit, force_rerun = force_rerun
)
dir.create(ctx$out_root, recursive = TRUE, showWarnings = FALSE)
set.seed(ctx$settings$seed)

ctx$communities <- read_csv(file.path(ctx$data_dir, "all_communities_descriptives.csv"), show_col_types = FALSE)
ctx$covariates <- community_covariates(ctx$data_dir, ctx$communities)

defs <- read_ergm_models(here::here("config", "ergm_model_desc.csv"))
if (!is.null(only_models)) defs <- filter(defs, model_id %in% only_models)

out <- run_all_models(defs, ctx, exclude_small)

message("=== ERGMs complete ===")
print(count(out$info, model_id, status))
