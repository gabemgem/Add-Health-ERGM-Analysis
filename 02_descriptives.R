source("utils/project_setup.R")

# =============================================================================
# 02_descriptives.R
# Purpose:  Per-community descriptive network statistics and visualizations.
#           Networks are directed (from 01_data_prep.R).
# Inputs:   data/{input_ver}/community_{cid}_network_directed_w{1,2}.rds
#           data/{input_ver}/community_{cid}_nodal_covariates.rds
#           data/{input_ver}/all_communities_descriptives.csv
# Outputs:  results/{output_ver}/community_{cid}_descriptives.csv
#           results/{output_ver}/all_communities_descriptives_network_directed_full.csv
#           results/{figures_ver}/community_{cid}_network_w1.png  (etc.)
# Date:     2026-03-23
# =============================================================================

# =============================================================================
# CONFIG
# =============================================================================
input_ver   <- "processed"      # Subdirectory under data/ for input files
output_ver  <- "descriptives"   # Subdirectory under results/ for output CSVs
figures_ver <- "figures_report"        # Subdirectory under results/ for plots
write_unsuppressed <- TRUE
plot_community_figures <- TRUE

# =============================================================================

# .Renviron is read by project_setup.R when present.

library(tidyverse)
library(network)
library(sna)
library(ggplot2)
library(here)

source(here::here("utils", "network_helpers.R"))
source(here::here("utils", "plot_helpers.R"))
source(here::here("utils", "disclosure.R"))

INPUT_DIR   <- here::here("data",    input_ver)
OUTPUT_DIR  <- here::here("results", output_ver)
FIGURES_DIR <- here::here("results", figures_ver)
dir.create(OUTPUT_DIR,  showWarnings = FALSE, recursive = TRUE)
dir.create(FIGURES_DIR, showWarnings = FALSE, recursive = TRUE)

race_levels <- c(race_white="White",race_black="Black",race_hispanic="Hispanic",
                 race_asian_pi="Asian/PI",race_american_indian="American Indian",
                 race_other="Other")
grade_levels <- setNames(7:12, paste0("grade_", 7:12))

network_groups <- function(net, wave) {
  n <- network::network.size(net)
  e <- network::network.edgecount(net)
  deg <- sna::degree(net, gmode="digraph")
  comp <- tryCatch(sna::component.dist(net, connected="weak"), error=function(err) list(csize=integer(0)))
  iso <- sum(deg == 0)
  lc <- if (length(comp$csize)) max(comp$csize) else NA_integer_
  g <- list(
    network = list(values = c(n_nodes = n, n_edges_arcs=e,
                              density=if (n>1) e / (n*(n-1)) else NA_real_,
                              avg_degree = mean(deg),
                              clustering = unname(tryCatch(sna::gtrans(net, mode="digraph"),error=function(err) NA_real_)),
                              reciprocity = unname(tryCatch(sna::grecip(net, measure="edgewise"), error=function(err) NA_real_))),
                   counts = c(n=n), ok = n >= MIN_CELL, quantity = TRUE),
    isolates = list(values=c(n_isolates=iso, n_components=length(comp$csize)),
                    counts=c(yes=iso, no=n-iso), ok=row_passes(c(iso, n-iso))),
    largest=list(values=c(largest_component=lc),
                 count = c(yes=lc, no=n-lc), ok=!is.na(lc) && row_passes(c(lc, n-lc))))
  setNames(g, paste0(names(g), "_w", wave))
}

sample_groups <- function(cv) {
  bin <- function(col, x) disc_binary(col, x)
  qty <- function(prefix, x, sd = TRUE) disc_quantity(c(setNames(list(mean), paste0(prefix, "_mean")),
                                                        if(sd) setNames(list(stats::sd), paste0(prefix, "_sd"))), x)
  club_cols <- intersect(paste0("S44A", 1:33), names(cv))
  club_m <- if (length(club_cols)) as.matrix(cv[club_cols]) else matrix(NA_real_, nrow(cv), 0)
  no_club_data <- if(length(club_cols)) rowSums(!is.na(club_m)) == 0 else rep(TRUE, nrow(cv))
  n_clubs <- ifelse(no_club_data, NA_real_, rowSums(club_m == 1, na.rm=TRUE))
  list(
    female = bin("female_share", ifelse(is.na(cv$gender), NA, cv$gender == 2)),
    age = qty("age", cv$age),
    grade = disc_categorical(grade_levels, cv$grade_w1),
    grade_miss = bin("grade_missing", is.na(cv$grade_w1)),
    race = disc_categorical(race_levels, cv$race_ethnicity,
                            extra = list(race_heterogeneity = function(p) 1 - sum(p^2))),
    race_miss = bin("race_missing", is.na(cv$race_ethnicity)),
    par_edu = qty("par_edu_w1", cv$par_edu_w1),
    welfare = bin("welfare_share_w1", cv$welfare_w1),
    ideation_w1 = bin("ideation_prev_w1", cv$ideation_binary_w1),
    ideation_w2 = bin("ideation_prev_w2", cv$ideation_binary_w2),
    onset = bin("ideation_onset_rate", ifelse(cv$ideation_binary_w1 %in% 0, cv$ideation_binary_w2, NA)),
    remission = bin("ideation_remission_rate", ifelse(cv$ideation_binary_w1 %in% 1, 1-cv$ideation_binary_w2, NA)),
    attempt_w1 = bin("attempt_prev_w1", cv$attempt_binary_w1),
    attempt_w2 = bin("attempt_prev_w2", cv$attempt_binary_w2),
    exposure_w1 = bin("any_exposure_prev_w1", cv$any_exposure_w1),
    exposure_w2 = bin("any_exposure_prev_w2", cv$any_exposure_w2),
    peer_death_w1 = bin("peer_death_exposure_prev_w1", cv$peer_death_exposure_w1),
    peer_death_w2 = bin("peer_death_exposure_prev_w2", cv$peer_death_exposure_w2),
    cesd_w1 = qty("cesd_w1", cv$cesd_w1),
    cesd_w2 = qty("cesd_w2", cv$cesd_w2),
    hopeless_w1 = qty("hopelessness_w1", cv$hopelessness_w1, sd = TRUE),
    hopeless_w2 = qty("hopelessness_w2", cv$hopelessness_w2, sd = TRUE),
    lonely_w1 = qty("loneliness_w1", cv$loneliness_w1, sd = TRUE),
    lonely_w2 = qty("loneliness_w2", cv$loneliness_w2, sd = TRUE),
    support_w1 = qty("social_support_w1", cv$social_support_w1, sd = TRUE),
    support_w2 = qty("social_support_w2", cv$social_support_w2, sd = TRUE),
    substance_w1 = qty("substance_use_prev_w1", cv$substance_use_w1),
    substance_w2 = qty("substance_use_prev_w2", cv$substance_use_w2),
    attrition = bin("wave2_attrition", !(cv$in_wave2 %in% 1)),
    cesd_miss = bin("cesd_missing_w1", is.na(cv$cesd_w1)),
    support_miss = bin("social_support_missing_w1", is.na(cv$social_support_w1)),
    club_miss = bin("club_data_missing", no_club_data),
    club_any = bin("club_any_share", n_clubs > 0),
    clubs = disc_quantity(c(clubs_mean = mean), n_clubs)
  )
}

community_summary <- read_csv(file.path(INPUT_DIR, "all_communities_descriptives.csv"),
                               show_col_types = FALSE)
community_ids <- community_summary$community_id
net_rows <- list(); sample_rows <- list(); cells <- list()

for (cid in community_ids) {

  w1_path <- file.path(INPUT_DIR, paste0("community_", cid, "_network_directed_w1.rds"))
  w2_path <- file.path(INPUT_DIR, paste0("community_", cid, "_network_directed_w2.rds"))

  if (!file.exists(w1_path) || !file.exists(w2_path)) {
    warning("Missing network files for community ", cid, " — skipping.")
    next
  }

  net_w1 <- readRDS(w1_path)
  net_w2 <- readRDS(w2_path)
  covars <- readRDS(file.path(INPUT_DIR, paste0("community_", cid, "_nodal_covariates.rds")))
  
  for (w in 1:2) {
    r <- disc_collect(cid, network_groups(if (w==1) net_w1 else net_w2, w), wave=w)
    net_rows[[length(net_rows) + 1]] <- mutate(r$values, wave=w, .after=community_id)
    cells[[length(cells) + 1]] <- r$cells
  }
  r<- disc_collect(cid, sample_groups(covars))
  sample_rows[[length(sample_rows) + 1]] <- r$values
  cells[[length(cells)+1]]<-r$cells
  
  if (plot_community_figures) {
    tryCatch({
      plot_network_ideation(net_w1,
                            label       = paste0("community_", cid, "_network_w1"),
                            wave_label  = "Wave 1",
                            figures_dir = FIGURES_DIR)
      plot_network_ideation(net_w2,
                            label       = paste0("community_", cid, "_network_w2"),
                            wave_label  = "Wave 2",
                            figures_dir = FIGURES_DIR, wave = 2L)
      plot_network_sidebyside(net_w1, net_w2,
                              label       = paste0("community_", cid),
                              figures_dir = FIGURES_DIR)
    }, error = function(e) {
      warning("Plotting failed for community ", cid, ": ", conditionMessage(e))
    })
  }

  message("Community ", cid, " descriptives complete.")
}

cells <- disc_column_checks(bind_rows(cells))
all_desc <- bind_rows(net_rows) %>%
  left_join(bind_rows(sample_rows), by="community_id") %>%
  arrange(community_id, wave)
all_desc_out <- disc_apply(all_desc, cells)

# Output name matches what results_report.Rmd reads.
write_csv(all_desc_out, file.path(OUTPUT_DIR, "all_communities_descriptives_network_directed_full.csv"), na="")

if (write_unsuppressed) {
  unsup_path <- file.path(INPUT_DIR, "descriptives_UNSUPPRESSED_do_not_export.csv")
  write_csv(all_desc, unsup_path, na = "")
  message("Unsuppressed descriptives (do not export): ", unsup_path)
}

for(cid in unique(all_desc_out$community_id)) {
  write_csv(filter(all_desc_out, community_id==cid),
            file.path(OUTPUT_DIR, paste0("community_", cid, "_descriptives.csv")), na="")
}

suppression <- cells %>%
  distinct(community_id, group, ok) %>%
  group_by(table = group) %>%
  summarise(communities = n(), blanked=sum(!ok), .groups="drop") %>%
  arrange(desc(blanked))
write_csv(suppression, file.path(OUTPUT_DIR, "descriptives_suppression_summary.csv"))
message("Tables blanked for at least one community (", SUPPRESSED, "): ",
        paste(sprintf("%s %d/%d", suppression$table, suppression$blanked, suppression$communities)[suppression$blanked > 0],
              collapse="; "))

# ---- Summary figures ----
p_size <- ggplot(community_summary, aes(x = n_nodes)) +
  geom_histogram(binwidth = 20, fill = "#4575b4", color = "white") +
  labs(title = "Distribution of Community Sizes",
       x = "Number of Nodes", y = "Count") +
  theme_minimal(base_size = 13)

ggsave(file.path(FIGURES_DIR, "community_size_distribution.png"),
       p_size, width = 7, height = 5, dpi = 300)

p_ideation <- all_desc_out %>%
  filter(wave == 1) %>%
  mutate(ideation_prev_w1 = suppressWarnings(as.numeric(ideation_prev_w1))) %>%
  filter(!is.na(ideation_prev_w1)) %>%
  ggplot(aes(x = reorder(as.character(community_id), ideation_prev_w1),
             y = ideation_prev_w1)) +
  geom_col(fill = "#d73027", alpha = 0.8) +
  coord_flip() +
  labs(title = "Wave 1 Ideation Prevalence by Community",
       x = "Community ID", y = "Proportion") +
  theme_minimal(base_size = 10)

ggsave(file.path(FIGURES_DIR, "ideation_prevalence_by_community.png"),
       p_ideation, width = 8, height = 12, dpi = 300)

message("=== Phase 2: Descriptives complete. ===")
message("Outputs: results/", output_ver, "/")
message("Figures: results/", figures_ver, "/")
