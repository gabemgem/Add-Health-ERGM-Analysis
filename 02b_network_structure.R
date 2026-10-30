source("utils/project_setup.R")

# =============================================================================
# 02b_network_structure.R
# Purpose:  Directed network structure per community and wave: dyad census,
#           triad census compared with its U|MAN expectation (random networks
#           with the same mutual / asymmetric / null dyad counts), and
#           in/out-degree distributions. Wave 2 uses only people observed at
#           wave 2 (in_wave2 = 1), so attrition does not appear as isolates.
#           All outputs are community-level counts (CSV) and summary images,
#           read by results_report.Rmd via make_report_bundle.R.
# Inputs:   data/{input_ver}/community_{cid}_network_directed_w{1,2}.rds
#           data/{input_ver}/all_communities_descriptives.csv
# Outputs:  results/{output_ver}/desc_dyad_census.csv
#           results/{output_ver}/desc_triad_census.csv
#           results/{output_ver}/desc_degree_distribution.csv
#           results/{figures_ver}/desc_triad_census_oe.png
#           results/{figures_ver}/desc_degree_distribution.png
# Seed:     20240104 (U|MAN simulations)
# Date:     2026-10-19
# =============================================================================

# =============================================================================
# CONFIG
# =============================================================================
input_ver   <- "processed"       # Subdirectory under data/ for network inputs
output_ver  <- "descriptives"    # Subdirectory under results/ for CSVs
figures_ver <- "figures_report"  # Subdirectory under results/ for images bundled with the report
n_sim       <- 100               # U|MAN simulations per community and wave

# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(network)
  library(sna)
  library(here)
})
source(here::here("utils", "structure_helpers.R"))

INPUT_DIR   <- here::here("data", input_ver)
OUTPUT_DIR  <- here::here("results", output_ver)
FIGURES_DIR <- here::here("results", figures_ver)
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(FIGURES_DIR, recursive = TRUE, showWarnings = FALSE)

community_ids <- read_csv(file.path(INPUT_DIR, "all_communities_descriptives.csv"),
                          show_col_types = FALSE)$community_id

res <- list()
for (cid in community_ids) {
  for (wave in 1:2) {
    path <- file.path(INPUT_DIR, sprintf("community_%s_network_directed_w%d.rds", cid, wave))
    if (!file.exists(path)) next
    t0 <- Sys.time()
    res[[length(res) + 1]] <- network_structure(readRDS(path), cid, wave, n_sim)
    message(sprintf("Community %s wave %d: %.1f s", cid, wave, as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  }
}

dyads   <- bind_rows(lapply(res, `[[`, "dyads"))
triads  <- bind_rows(lapply(res, `[[`, "triads"))
degrees <- bind_rows(lapply(res, `[[`, "degrees"))
write_csv(dyads, file.path(OUTPUT_DIR, "desc_dyad_census.csv"))
write_csv(triads, file.path(OUTPUT_DIR, "desc_triad_census.csv"))
write_csv(degrees, file.path(OUTPUT_DIR, "desc_degree_distribution.csv"))

# ---- Summary images (the report draws its own versions from the CSVs) ----
oe <- triads |>
  group_by(wave, type) |>
  summarise(ratio = sum(observed) / sum(expected), .groups = "drop") |>
  left_join(triad_types, by = "type") |>
  mutate(type = factor(type, levels = rev(triad_types$type)))
p_oe <- ggplot(oe, aes(ratio, type, colour = class)) +
  geom_vline(xintercept = 1, colour = "grey40") +
  geom_segment(aes(x = 1, xend = ratio, yend = type), linewidth = 0.8) +
  geom_point(size = 2.5) +
  scale_x_log10() +
  facet_wrap(~ paste("Wave", wave)) +
  labs(x = "Observed / expected under U|MAN (log scale), pooled over communities", y = "Triad type", colour = NULL) +
  theme_minimal(base_size = 11) + theme(legend.position = "bottom")
ggsave(file.path(FIGURES_DIR, "desc_triad_census_oe.png"), p_oe, width = 9, height = 6, dpi = 150)

p_deg <- degree_plot(degrees, max_degree = 20)
ggsave(file.path(FIGURES_DIR, "desc_degree_distribution.png"), p_deg, width = 9, height = 6, dpi = 150)

message("=== Network structure descriptives complete: ", n_distinct(dyads$community_id), " communities ===")
