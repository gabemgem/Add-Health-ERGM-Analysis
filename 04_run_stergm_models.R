source("utils/project_setup.R")

# =============================================================================
# 04_run_stergm_models.R
# Purpose:  Fit every directed STERGM defined in config/stergm_model_desc.csv (CMLE,
#           wave 1 -> wave 2) to every community, then pool formation and
#           dissolution estimates separately with random-effects
#           meta-analysis. Communities with up-to-date saved results are
#           skipped.
#
#           tergm >= 4.0 fits the dissolution sub-model as PERSISTENCE
#           (Persist() terms): estimate > 0 = arc more likely to persist,
#           < 0 = more likely to dissolve. Files keep the label "dissolution".
#           A model row with blank formation_terms (or dissolution_terms) is
#           a dissolution-only (formation-only) build-up step: the other
#           sub-model is fit as edges-only and not reported.
# Inputs:   config/stergm_model_desc.csv
#           data/{input_ver}/community_{cid}_network_directed_w{1,2}.rds
#           data/{input_ver}/community_{cid}_nodal_covariates.rds   (shared-club edgecov)
#           data/{input_ver}/all_communities_descriptives.csv
# Outputs:  results/{output_ver}/<model_id>/community_{cid}_{info,coefs,corr,gof}.csv
#           results/{output_ver}/<model_id>/community_{cid}_model.rds   (stays on this machine)
#           results/{output_ver}/<model_id>/logs/community_{cid}.log
#           results/{output_ver}/stergm_{models_run,fit_info,coefs,gof,coef_corr,
#                                        meta,meta_regression,selection}.csv
#           results/community_covariates.csv
# Prerequisite: 01_data_prep.R.
# Seed:     20240102 (settings$seed)
# Date:     2026-10-19
# =============================================================================

# =============================================================================
# CONFIG
# =============================================================================
input_ver     <- "processed"       # Subdirectory under data/ for network inputs
output_ver    <- "stergm_models"   # Subdirectory under results/ for outputs
only_models   <- NULL              # e.g. c("m7f3d3") to run a subset of config/stergm_model_desc.csv
force_rerun   <- FALSE             # TRUE = refit every community even if results exist
n_workers     <- as.integer(Sys.getenv("SUPPLEMENT_WORKERS", "1"))
if (is.na(n_workers) || n_workers < 1L) stop("SUPPLEMENT_WORKERS must be a positive integer.")
message(paste0("Running the STERGM pipeline with ", n_workers, " parallel workers."))
time_limit    <- 18000              # seconds before one community's fit is killed
exclude_small <- TRUE              # primary meta-analysis excludes small communities (n < 50)

settings <- list()                 # overrides of default_model_settings()

# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(network)
  library(tergm)
  library(here)
})
source(here::here("utils", "model_pipeline.R"))
source(here::here("utils", "meta_trust.R"))

ctx <- list(
  type = "stergm",
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

defs <- read_stergm_models(here::here("config", "stergm_model_desc.csv"))
if (!is.null(only_models)) defs <- filter(defs, model_id %in% only_models)

out <- run_all_models(defs, ctx, exclude_small)

message("=== STERGMs complete ===")
print(count(out$info, model_id, part, status))
