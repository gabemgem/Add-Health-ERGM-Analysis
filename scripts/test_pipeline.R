# Synthetic integration checks; no licensed data are read.
source("utils/project_setup.R")
source("scripts/check_inputs.R")
check_supplement()
suppressPackageStartupMessages(library(tidyverse))
source("utils/model_pipeline.R")
source("utils/network_helpers.R")
source("utils/composite_helpers.R")
source("utils/disclosure.R")
source("utils/report_helpers.R")
source("utils/paper_table_helpers.R")
source("utils/meta_trust.R")
descriptives <- read_result(result_index("reference_results"), "all_communities_descriptives_network_directed_full.csv")
stopifnot(all(c("community_id", "wave", "ideation_prev_w1", "race_asian_pi") %in% names(descriptives)),
          nrow(descriptives) == 32L)
stopifnot(is_suppressed(intToUtf8(167)),
          identical(fmt_disc(intToUtf8(167)), intToUtf8(167)),
          any(is_suppressed(descriptives$ideation_prev_w1)))
project <- getwd()
scratch <- tempfile("supplement-test-")
dir.create(scratch)

# Confirm the selected paper tables exactly match the existing ERGM exports.
defs <- read_ergm_models("config/ergm_model_desc.csv")
cc <- read_csv_quiet("reference_results/ergm_coefs.csv")
ii <- read_csv_quiet("reference_results/ergm_fit_info.csv")
for (cid in c("406", "408", "428", "477")) {
  actual <- build_paper_table(cc, ii, defs, cid, "ergm")$data
  # Frozen manuscript table content, kept separately from regenerated outputs.
  expected <- as.data.frame(readr::read_csv(paste0("tests/expected/ergm_ergm_community_", cid, ".csv"),
                                           show_col_types = FALSE))
  comparison <- all.equal(actual, expected, check.attributes = FALSE)
  if (!isTRUE(comparison)) stop("Paper table mismatch for ", cid, ": ", paste(comparison, collapse = "; "))
}

# Build the six raw input files with artificial people and survey responses.
n <- 60L
ids <- seq_len(n)
w1 <- data.frame(AID = ids, COMMID = rep(c(901, 902), each = n/2),
                 SCID = rep(c(901, 902), each = n/2), SSCID = 0, BIO_SEX = rep(1:2, n/2),
                 H1GI20 = rep(7:12, length.out = n), H1GI9 = 1, H1GI4 = 0)
w2 <- data.frame(AID = ids)
for (wave in 1:2) {
  d <- if (wave == 1) w1 else w2
  for (stem in c("FS", "PR", "SU", "RM", "RF", "TO", "HS")) {
    k <- switch(stem, FS = 19, PR = 8, SU = 7, RM = 9, RF = 9, TO = 61, HS = 7)
    for (j in seq_len(k)) d[[paste0("H", wave, stem, j)]] <- rep(c(0, 1), length.out = n)
  }
  for (j in 1:8) d[[paste0("H", wave, "PR", j)]] <- 3
  if (wave == 1) w1 <- d else w2 <- d
}
school <- data.frame(AID = ids, S1 = 15, S44 = 0)
for (j in 1:33) school[[paste0("S44A", j)]] <- as.integer(ids %% 3 == j %% 3)
friend <- data.frame(AID = ids)
for (j in 1:5) {
  friend[[paste0("MF_AID", j)]] <- ((ids - 1L + j) %% 30L) + 1L + 30L * (ids > 30L)
  friend[[paste0("FF_AID", j)]] <- 0
}
raw <- file.path(scratch, "raw")
source("scripts/check_inputs.R")
objects <- list(w1, w2, school, data.frame(SCID = c(901, 902), SAT_SCHL = 1), friend, friend)
for (j in seq_along(raw_files)) {
  target <- file.path(raw, raw_files[j])
  dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
  haven::write_xpt(objects[[j]], target)
}
file.copy(c("AddHealthSupplement.Rproj", "01_data_prep.R", "02_descriptives.R", "02b_network_structure.R",
            "make_report_bundle.R"), scratch)
dir.create(file.path(scratch, "utils"))
file.copy(list.files("utils", full.names = TRUE), file.path(scratch, "utils"))
old_raw <- Sys.getenv("RAW_DATA_ROOT", unset = NA_character_)
Sys.setenv(RAW_DATA_ROOT = raw)
setwd(scratch)
tryCatch({
  for (script in c("01_data_prep.R", "02_descriptives.R", "02b_network_structure.R"))
    source(script, local = new.env(parent = globalenv()))
  net <- readRDS("data/processed/community_901_network_directed_w1.rds")
  cv <- readRDS("data/processed/community_901_nodal_covariates.rds")
  stopifnot(network::is.directed(net), network::network.size(net) == 30L,
            network::network.edgecount(net) == 150L,
            all(paste0("S44A", 1:33) %in% names(cv)))
  sc <- build_shared_club_edgecov(net, cv)
  stopifnot(all(sc$any == (sc$count > 0)), sc$any[1, 4] == 1, sc$any[1, 2] == 0)
  # Wave II must use its own ideation values, even when baseline attributes exist.
  plot_net <- net
  network::set.vertex.attribute(plot_net, "ideation_binary_w1", rep(0, 30L))
  network::set.vertex.attribute(plot_net, "ideation_binary_w2", rep(1, 30L))
  plot_ig <- net_to_igraph(plot_net)
  stopifnot(all(network_ideation(plot_ig, 1) == 0), all(network_ideation(plot_ig, 2) == 1))
  stopifnot(!row_passes(c(yes = 9, no = 21)), row_passes(c(yes = 10, no = 20)))
  # Direct worker exercise, including CSVs and temporal persistence labels.
  settings <- default_model_settings()
  settings$run_gof <- TRUE
  settings$n_gof_sim <- 3L
  settings$gof_plots <- FALSE
  settings$eval_loglik <- FALSE
  for (type in c("ergm", "stergm")) {
    out <- file.path(scratch, "results", paste0("synthetic_", type))
    dir.create(out, recursive = TRUE, showWarnings = FALSE)
    w1_path <- file.path(scratch, "data/processed/community_901_network_directed_w1.rds")
    w2_path <- file.path(scratch, "data/processed/community_901_network_directed_w2.rds")
    # Artificial wave II has independently sampled arcs, ensuring formation/dissolution.
    set.seed(11)
    m <- matrix(rbinom(30L * 30L, 1, .18), 30L)
    diag(m) <- 0
    nw2 <- network::network(m, directed = TRUE)
    network::network.vertex.names(nw2) <- network::network.vertex.names(net)
    saveRDS(nw2, w2_path)
    job <- list(settings = settings, type = type, model_id = "synthetic", community_id = "901",
                spec_hash = "synthetic", model_dir = out, min_size = 20,
                w1_path = w1_path, w2_path = w2_path, terms = "edges",
                formation_terms = "edges", dissolution_terms = "edges")
    run_community_job(job)
    coefficients <- read_csv_quiet(community_files(out, "901")$coefs)
    stopifnot(nrow(coefficients) >= 1L, all(is.finite(coefficients$estimate)))
    fit_info <- read_csv_quiet(community_files(out, "901")$info)
    stopifnot(identical(fit_info$gof, "done"))
    if (type == "stergm") stopifnot(setequal(coefficients$part, c("formation", "dissolution")),
                                    any(grepl("Persist", coefficients$term)))
    h <- spec_hash("edges", w1_path, settings)
    changed <- settings; changed$seed <- settings$seed + 1L
    stopifnot(!identical(h, spec_hash("edges", w1_path, changed)))
  }
  # Exercise actual background workers, output aggregation and cache recovery.
  communities <- readr::read_csv("data/processed/all_communities_descriptives.csv", show_col_types = FALSE) |>
    dplyr::filter(community_id == 901)
  for (type in c("ergm", "stergm")) {
    csv <- file.path(scratch, paste0(type, "_audit.csv"))
    readr::write_csv(tibble(model_id = "audit", terms = "edges", formation_terms = "edges",
                            dissolution_terms = "edges", moderators = ""), csv)
    definitions <- if (type == "ergm") read_ergm_models(csv) else read_stergm_models(csv)
    ctx <- list(type = type, out_root = file.path(scratch, "results", paste0(type, "_models")),
                utils_dir = file.path(scratch, "utils"), data_dir = file.path(scratch, "data/processed"),
                settings = settings, communities = communities, n_workers = 1L,
                time_limit = 60, force_rerun = FALSE)
    dir.create(ctx$out_root, recursive = TRUE, showWarnings = FALSE)
    ctx$covariates <- community_covariates(ctx$data_dir, communities)
    result <- run_all_models(definitions, ctx)
    stopifnot(nrow(result$coefs) > 0, all(result$info$status == "fit"))
    # A valid but underpowered meta-analysis must still support the report.
    pooled <- meta_analyze(result$coefs, ctx$covariates)
    stopifnot(nrow(assess_trust(pooled, nrow(communities))) > 0)
    paths <- community_files(file.path(ctx$out_root, "audit"), "901")
    first_fit <- file.info(paths$info)$mtime
    run_all_models(definitions, ctx)
    stopifnot(identical(file.info(paths$info)$mtime, first_fit))
    file.remove(paths$coefs)
    recovered <- run_all_models(definitions, ctx)
    stopifnot(nrow(recovered$coefs) > 0, file.exists(paths$coefs))
  }
  source("make_report_bundle.R", local = new.env(parent = globalenv()))
  bundled <- read.csv("results/report_bundle/checksums.csv")
  stopifnot(all(file.exists(file.path("results/report_bundle", bundled$file))),
            identical(unname(tools::md5sum(file.path("results/report_bundle", bundled$file))), bundled$md5))
}, finally = {
  setwd(project)
  here::i_am("AddHealthSupplement.Rproj")
  if (is.na(old_raw)) Sys.unsetenv("RAW_DATA_ROOT") else Sys.setenv(RAW_DATA_ROOT = old_raw)
})
message("PASS: manuscript tables, synthetic preparation/descriptives, wave-specific plots, directed networks, clubs, suppression, ERGM/STERGM workers, GOF, pooling, background scheduling, cache recovery and bundle checksums.")
