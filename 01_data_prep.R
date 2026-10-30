source("utils/project_setup.R")

# =============================================================================
# 01_data_prep.R
# Purpose:  Load raw Add Health data, construct all composite variables,
#           build per-community DIRECTED network objects (A -> B means A
#           nominated B; no symmetrization) with nodal covariates, and save
#           processed RDS files.
# Inputs:   $RAW_DATA_ROOT  (raw .xpt files)
# Outputs:  data/{output_ver}/community_{scid}_network_directed_w1.rds
#           data/{output_ver}/community_{scid}_network_directed_w2.rds
#           data/{output_ver}/community_{scid}_nodal_covariates.rds
#           data/{output_ver}/all_communities_descriptives.csv
#           data/{output_ver}/data_prep_log.txt
# Date:     2026-03-23
# =============================================================================

# =============================================================================
# CONFIG
# =============================================================================
output_ver <- "processed"    # Subdirectory under data/ for all processed outputs

# =============================================================================

set.seed(20240101)
# .Renviron is read by project_setup.R when present.

library(haven)
library(tidyverse)
library(network)
library(here)

source(here::here("utils", "composite_helpers.R"))
source(here::here("utils", "network_helpers.R"))

OUTPUT_DIR <- here::here("data", output_ver)
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

log_con <- file(file.path(OUTPUT_DIR, "data_prep_log.txt"), open = "w")
log_msg <- function(...) {
  msg <- paste0(format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "), ...)
  message(msg)
  writeLines(msg, log_con)
}

log_msg("=== Phase 1: Data Preparation ===")
log_msg("output_ver: ", output_ver)

# =============================================================================
# 1a. Load raw files
# =============================================================================
raw_root <- Sys.getenv("RAW_DATA_ROOT")

if (raw_root == "") stop("RAW_DATA_ROOT environment variable is not set.")

log_msg("Loading raw data from: ", raw_root)

inhome   <- haven::read_xpt(file.path(raw_root, "Standard/Survey/W1/allwave1.xpt")) %>%
              haven::zap_labels()
inhome2  <- haven::read_xpt(file.path(raw_root, "Standard/Survey/W2/wave2.xpt")) %>%
              haven::zap_labels()
inschool <- haven::read_xpt(file.path(raw_root, "Standard/InSchool/Inschool.xpt")) %>%
              haven::zap_labels()
friend1  <- haven::read_xpt(file.path(raw_root, "Standard/Friendship/hfriend1.xpt")) %>%
              haven::zap_labels()
friend2  <- haven::read_xpt(file.path(raw_root, "Standard/Friendship/hfriend2.xpt")) %>%
              haven::zap_labels()
schinfo <- haven::read_xpt(file.path(raw_root, "Standard/InSchool/Schinfo.xpt")) %>%
  haven::zap_labels()

log_msg("Wave 1 N: ", nrow(inhome), "  Wave 2 N: ", nrow(inhome2),
        "  In-school N: ", nrow(inschool))

# =============================================================================
# 1b. Build master respondent table
# =============================================================================

# Get saturated schools
sat_schools <- schinfo %>%
  select(SCID, SAT_SCHL) %>%
  filter(SAT_SCHL == 1)

master <- inhome %>%
  select(AID, COMMID, everything())

log_msg("N distinct communities (before filtering): ", n_distinct(master$COMMID))

# Filter to only respondents in saturated schools, assign community id
master <- master %>%
  filter(SCID %in% sat_schools$SCID) %>%
  mutate(
    community_id = COMMID,
    gender = BIO_SEX,
    SCID = as.numeric(SCID),
    SSCID = as.numeric(SSCID),
  )

log_msg("N distinct communities (after filtering): ", n_distinct(master$community_id))

club_cols <- paste0("S44A", 1:33)
inschool_slim <- inschool %>%
  select(AID, S1, S44, all_of(club_cols)) %>%
  rename(age = S1, no_club_binary_w1 = S44) %>%
  mutate(age = as.numeric(age))

master <- master %>%
  select(-any_of(c("age", "no_club_binary_w1", club_cols))) %>%
  left_join(inschool_slim, by = "AID") %>%
  left_join(inhome2 %>% select(AID, starts_with("H2")), by = "AID") %>%
  mutate(in_wave2 = as.integer(!is.na(H2SU1))) %>%
  mutate(n_clubs = rowSums(across(all_of(club_cols))))

# In-school club items S44A1-S44A33 (all 0/1), kept as-is in the covariate
# file. The shared-club edgecov is built from them at fit time
# (build_shared_club_edgecov() in utils/model_pipeline.R).
club_items <- intersect(club_cols, names(inschool))
if (length(club_items) < 33)
  log_msg("WARNING: club items missing from in-school data: ",
          paste(setdiff(club_cols, club_items), collapse = ", "))

# Wave 1 grade (in-home H1GI20), used for nodematch("grade_w1").
master <- master %>%
  mutate(grade_w1 = rna(H1GI20, na_vals = c(96, 97, 98, 99)))

log_msg("Wave 2 participation rate: ",
        round(mean(master$in_wave2, na.rm = TRUE) * 100, 1), "%")

# =============================================================================
# 1c. Construct composite variables
# =============================================================================
log_msg("Constructing composite variables...")

master <- master %>%
  mutate(cesd_w1 = build_cesd(., "H1FS"), cesd_w2 = build_cesd(., "H2FS"),
         social_support_w1 = build_social_support(., "H1PR"),
         social_support_w2 = build_social_support(., "H2PR"),
         hopelessness_w1 = build_hopelessness(., "H1FS"),
         hopelessness_w2 = build_hopelessness(., "H2FS"),
         loneliness_w1 = build_loneliness(., "H1FS"),
         loneliness_w2 = build_loneliness(., "H2FS"))

w1_ideation <- build_ideation_vars(master, wave = 1) %>%
  select(AID, ideation_binary_w1 = ideation_binary,
         attempt_count_w1 = attempt_count, attempt_binary_w1 = attempt_binary)
w2_ideation <- build_ideation_vars(master, wave = 2) %>%
  select(AID, ideation_binary_w2 = ideation_binary,
         attempt_count_w2 = attempt_count, attempt_binary_w2 = attempt_binary)

master <- master %>%
  left_join(w1_ideation, by = "AID") %>%
  left_join(w2_ideation, by = "AID")

w1_exposure <- build_exposure_vars(master, wave = 1) %>%
  select(AID, peer_attempt_exposure_w1 = peer_attempt_exposure,
         peer_death_exposure_w1 = peer_death_exposure,
         family_attempt_exposure_w1 = family_attempt_exposure,
         family_death_exposure_w1 = family_death_exposure,
         any_exposure_w1 = any_exposure)
w2_exposure <- build_exposure_vars(master, wave = 2) %>%
  select(AID, peer_attempt_exposure_w2 = peer_attempt_exposure,
         peer_death_exposure_w2 = peer_death_exposure,
         family_attempt_exposure_w2 = family_attempt_exposure,
         family_death_exposure_w2 = family_death_exposure,
         any_exposure_w2 = any_exposure)

master <- master %>%
  left_join(w1_exposure, by = "AID") %>%
  left_join(w2_exposure, by = "AID") %>%
  mutate(
    ideation_onset     = as.integer(ideation_binary_w1 == 0 & ideation_binary_w2 == 1),
    ideation_remission = as.integer(ideation_binary_w1 == 1 & ideation_binary_w2 == 0)
  )

master <- build_substance_use_w1(master)
master <- build_substance_use_w2(master)
master <- build_ses(master, wave = 1)
master_ses2 <- build_ses(master, wave = 2) %>%
  select(AID, par_edu_w2 = par_edu, welfare_w2 = welfare)
master <- master %>%
  left_join(master_ses2, by = "AID") %>%
  rename(par_edu_w1 = par_edu, welfare_w1 = welfare)
master <- build_race_ethnicity(master)
master <- build_treatment_w2(master)

log_msg("Composite construction complete.")

# =============================================================================
# 1e. Build edgelists
# =============================================================================
el_w1_raw <- to_edgelist(friend1)
el_w2_raw <- to_edgelist(friend2)
log_msg("Raw edgelist sizes — W1: ", nrow(el_w1_raw), "  W2: ", nrow(el_w2_raw))

community_map <- setNames(master$community_id, as.character(master$AID))
el_w1_filtered <- filter_edgelist(el_w1_raw, master$AID, community_map)
el_w2_filtered <- filter_edgelist(el_w2_raw, master$AID, community_map)

log_msg("W1 phantom nominees dropped: ",     el_w1_filtered$n_phantom_dropped)
log_msg("W1 cross-community dropped: ",      el_w1_filtered$n_cross_community_dropped)
log_msg("W2 phantom nominees dropped: ",     el_w2_filtered$n_phantom_dropped)
log_msg("W2 cross-community dropped: ",      el_w2_filtered$n_cross_community_dropped)

el_w1 <- el_w1_filtered$edgelist
el_w2 <- el_w2_filtered$edgelist

# =============================================================================
# 1f – 1i. Build and save per-community directed networks
# =============================================================================
community_ids <- sort(unique(master$community_id))
community_ids <- community_ids[!is.na(community_ids)]
log_msg("Processing ", length(community_ids), " communities...")

community_summary <- tibble()

for (cid in community_ids) {
  nodes_df <- master %>% filter(community_id == cid)
  nodes    <- nodes_df$AID
  n_nodes  <- length(nodes)

  el_c_w1 <- el_w1 %>%
    mutate(nominator = as.numeric(nominator), nominee = as.numeric(nominee)) %>%
    filter(nominator %in% nodes, nominee %in% nodes)
  el_c_w2 <- el_w2 %>%
    mutate(nominator = as.numeric(nominator), nominee = as.numeric(nominee)) %>%
    filter(nominator %in% nodes, nominee %in% nodes)

  log_msg(sprintf("  Community %s: W1 nominations=%d, W2 nominations=%d",
                  cid, nrow(el_c_w1), nrow(el_c_w2)))

  recip_w1 <- compute_reciprocity(el_c_w1)
  recip_w2 <- compute_reciprocity(el_c_w2)

  # Directed networks: an arc A -> B means A nominated B.
  net_w1 <- make_directed_network(el_c_w1, nodes)
  net_w2 <- make_directed_network(el_c_w2, nodes)

  n_arcs_w1 <- network::network.edgecount(net_w1)
  n_arcs_w2 <- network::network.edgecount(net_w2)

  arc_set <- function(el_c) unique(paste(el_c$nominator, el_c$nominee, sep = "_"))
  a_set_w1 <- arc_set(el_c_w1)
  a_set_w2 <- arc_set(el_c_w2)

  n_dissolved   <- sum(!a_set_w1 %in% a_set_w2)
  n_new         <- sum(!a_set_w2 %in% a_set_w1)
  turnover_diss <- ifelse(length(a_set_w1) > 0, n_dissolved / length(a_set_w1), NA_real_)
  turnover_form <- ifelse(length(a_set_w2) > 0, n_new      / length(a_set_w2), NA_real_)

  covar_cols <- intersect(c(
    "AID", "community_id", "age", "gender", "race_ethnicity", "hispanic",
    "cesd_w1", "cesd_w2", "social_support_w1", "social_support_w2",
    "hopelessness_w1", "hopelessness_w2", "loneliness_w1", "loneliness_w2",
    "ideation_binary_w1", "ideation_binary_w2",
    "attempt_binary_w1", "attempt_binary_w2",
    "attempt_count_w1", "attempt_count_w2",
    "any_exposure_w1", "any_exposure_w2",
    "peer_death_exposure_w1", "peer_death_exposure_w2",
    "peer_attempt_exposure_w1", "peer_attempt_exposure_w2",
    "family_attempt_exposure_w1", "family_attempt_exposure_w2",
    "family_death_exposure_w1", "family_death_exposure_w2",
    "substance_use_w1", "substance_use_w2",
    "par_edu_w1", "par_edu_w2", "welfare_w1", "welfare_w2",
    "any_treatment", "ideation_onset", "ideation_remission",
    "in_wave2",
    "grade_w1", "n_clubs", "no_club_binary_w1", club_cols
  ), names(nodes_df))
  covars <- nodes_df %>% select(all_of(covar_cols))

  net_w1 <- attach_covariates(net_w1, covars %>%
    select(AID, ends_with("_w1"), gender, age, race_ethnicity, hispanic,
           community_id))
  net_w2 <- attach_covariates(net_w2, covars %>%
    select(AID, ends_with("_w1"), ends_with("_w2"), gender, age,
           race_ethnicity, hispanic, in_wave2, community_id))
  
  cesd_w1 <- network::get.vertex.attribute(net_w1, "cesd_w1")
  network::set.vertex.attribute(net_w1, "cesd_w1_2", cesd_w1*cesd_w1)
  network::set.vertex.attribute(net_w2, "cesd_w1_2", cesd_w1*cesd_w1)
  
  cesd_q3 <- quantile(cesd_w1, probs=0.75, na.rm=TRUE)
  network::set.vertex.attribute(net_w1, "cesd_q3", cesd_w1 > cesd_q3)

  small_community <- n_nodes < 50
  attrition_rate  <- mean(!nodes_df$in_wave2, na.rm = TRUE)
  high_attrition  <- attrition_rate > 0.30
  if (nrow(el_c_w1) != length(a_set_w1) || nrow(el_c_w2) != length(a_set_w2))
    log_msg("  Community ", cid, ": duplicate nominations dropped (W1 ",
            nrow(el_c_w1) - length(a_set_w1), ", W2 ", nrow(el_c_w2) - length(a_set_w2), ")")

  log_msg(sprintf("Community %s: n=%d, W1 arcs=%d, W2 arcs=%d, attrition=%.1f%%",
                  cid, n_nodes, n_arcs_w1, n_arcs_w2, attrition_rate * 100))

  saveRDS(net_w1, file.path(OUTPUT_DIR, paste0("community_", cid, "_network_directed_w1.rds")))
  saveRDS(net_w2, file.path(OUTPUT_DIR, paste0("community_", cid, "_network_directed_w2.rds")))
  saveRDS(covars, file.path(OUTPUT_DIR, paste0("community_", cid, "_nodal_covariates.rds")))

  community_summary <- bind_rows(community_summary, tibble(
    community_id         = cid,
    n_nodes              = n_nodes,
    n_arcs_w1            = n_arcs_w1,
    n_arcs_w2            = n_arcs_w2,
    density_w1           = n_arcs_w1 / (n_nodes * (n_nodes - 1)),
    density_w2           = n_arcs_w2 / (n_nodes * (n_nodes - 1)),
    reciprocity_w1       = recip_w1,
    reciprocity_w2       = recip_w2,
    turnover_dissolution = turnover_diss,
    turnover_formation   = turnover_form,
    attrition_rate       = attrition_rate,
    small_community      = small_community,
    high_attrition       = high_attrition,
    ideation_prev_w1     = mean(covars$ideation_binary_w1, na.rm = TRUE),
    ideation_prev_w2     = mean(covars$ideation_binary_w2, na.rm = TRUE)
  ))
}

write_csv(community_summary, file.path(OUTPUT_DIR, "all_communities_descriptives.csv"))
log_msg("Community summary saved.")
log_msg("=== Phase 1 complete. ===")
log_msg("Small communities (n < 50): ",  sum(community_summary$small_community, na.rm = TRUE))
log_msg("High-attrition communities: ",  sum(community_summary$high_attrition,  na.rm = TRUE))
close(log_con)
