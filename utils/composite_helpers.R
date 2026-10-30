# =============================================================================
# composite_helpers.R
# Purpose:  Functions for constructing all composite and recoded survey
#           variables used in the suicide risk network analysis.
# Inputs:   Called by 01_data_prep.R
# Outputs:  Functions loaded into calling environment
# Date:     2026-03-23
# =============================================================================

library(tidyverse)

# -----------------------------------------------------------------------------
# rna()
# Recode out-of-range / refusal codes to NA and coerce to numeric.
# Default NA codes cover Add Health conventions (6,7,8,96,97,98).
# -----------------------------------------------------------------------------
rna <- function(x, na_vals = c(6, 7, 8, 96, 97, 98)) {
  x <- as.numeric(x)
  ifelse(x %in% na_vals, NA_real_, x)
}


# -----------------------------------------------------------------------------
# build_cesd()
# Construct the 19-item CESD-equivalent depression score (0–57).
# Items 4, 8, 11, 15 are positive affect — reverse-coded (3 - x) before summing.
# Returns NA if any item is missing.
# prefix: "H1FS" or "H2FS"
# df:     data frame containing those columns
# -----------------------------------------------------------------------------
build_cesd <- function(df, prefix) {
  cols <- paste0(prefix, 1:19)
  missing_cols <- cols[!cols %in% names(df)]
  if (length(missing_cols) > 0) {
    warning("Missing CESD columns: ", paste(missing_cols, collapse = ", "))
    return(rep(NA_real_, nrow(df)))
  }
  m <- sapply(cols, function(v) rna(df[[v]], na_vals = c(6, 7, 8, 9)))
  m[, c(4, 8, 11, 15)] <- 3 - m[, c(4, 8, 11, 15)]
  rowSums(m, na.rm = FALSE)
}


# -----------------------------------------------------------------------------
# build_social_support()
# 8-item composite (H1PR1–H1PR8 or H2PR1–H2PR8).
# Item 6 ("want to leave home") is reverse-coded: 6 - x.
# Values 6, 96, 98 = NA. Higher score = more support.
# Returns row means (NA if any item missing).
# prefix: "H1PR" or "H2PR"
# -----------------------------------------------------------------------------
build_social_support <- function(df, prefix) {
  cols <- paste0(prefix, 1:8)
  missing_cols <- cols[!cols %in% names(df)]
  if (length(missing_cols) > 0) {
    warning("Missing social support columns: ", paste(missing_cols, collapse = ", "))
    return(rep(NA_real_, nrow(df)))
  }
  d <- df %>%
    mutate(across(all_of(cols), ~ifelse(as.numeric(.) %in% c(6, 96, 98), NA, as.numeric(.))))
  item6 <- paste0(prefix, "6")
  d[[paste0(item6, "_r")]] <- 6 - d[[item6]]
  items <- c(paste0(prefix, c("1","2","3","4","5")),
             paste0(item6, "_r"),
             paste0(prefix, c("7","8")))
  rowMeans(as.matrix(d[, items]), na.rm = FALSE)
}


# -----------------------------------------------------------------------------
# build_ideation_vars()
# Recode ideation (H1SU1/H2SU1) and attempt (H1SU2/H2SU2) variables.
# Returns a data frame with ideation_binary and attempt_binary columns.
# wave: 1 or 2
# df: data frame containing raw columns
# -----------------------------------------------------------------------------
build_ideation_vars <- function(df, wave) {
  su1 <- paste0("H", wave, "SU1")
  su2 <- paste0("H", wave, "SU2")

  out <- df %>%
    mutate(
      ideation_binary  = rna(.data[[su1]]),
      attempt_count    = rna(.data[[su2]]),
      attempt_binary   = as.integer(!is.na(attempt_count) & attempt_count > 0)
    )
  out
}


# -----------------------------------------------------------------------------
# build_exposure_vars()
# Recode peer/family suicide exposure variables and derive any_exposure flag.
# wave: 1 or 2
# -----------------------------------------------------------------------------
build_exposure_vars <- function(df, wave) {
  w <- wave
  peer_att  <- paste0("H", w, "SU4")
  peer_dth  <- paste0("H", w, "SU5")
  fam_att   <- paste0("H", w, "SU6")
  fam_dth   <- paste0("H", w, "SU7")

  out <- df %>%
    mutate(
      peer_attempt_exposure  = rna(.data[[peer_att]]),
      peer_death_exposure    = rna(.data[[peer_dth]]),
      family_attempt_exposure = rna(.data[[fam_att]]),
      family_death_exposure  = rna(.data[[fam_dth]]),
      any_exposure = as.integer(
        (!is.na(peer_attempt_exposure)  & peer_attempt_exposure  == 1) |
        (!is.na(peer_death_exposure)    & peer_death_exposure    == 1) |
        (!is.na(family_attempt_exposure)& family_attempt_exposure == 1) |
        (!is.na(family_death_exposure)  & family_death_exposure  == 1)
      )
    )
  out
}


# -----------------------------------------------------------------------------
# build_hopelessness()
# Reverse-code hopefulness item: hopelessness = 3 - rna(H_FS8).
# Higher value = more hopeless = higher risk.
# prefix: "H1FS" or "H2FS"
# -----------------------------------------------------------------------------
build_hopelessness <- function(df, prefix) {
  col <- paste0(prefix, "8")
  3 - rna(df[[col]])
}


# -----------------------------------------------------------------------------
# build_loneliness()
# Recode loneliness item (H1FS13 / H2FS13). Higher = lonelier.
# prefix: "H1FS" or "H2FS"
# -----------------------------------------------------------------------------
build_loneliness <- function(df, prefix) {
  col <- paste0(prefix, "13")
  rna(df[[col]])
}


# -----------------------------------------------------------------------------
# build_substance_use_w1()
# Binary composite: any substance use at wave 1.
# -----------------------------------------------------------------------------
build_substance_use_w1 <- function(df) {
  df %>%
    mutate(
      smoked_ever    = ifelse(as.numeric(H1TO1)  == 1, 1L,
                       ifelse(as.numeric(H1TO1)  == 0, 0L, NA_integer_)),
      alcohol_ever   = ifelse(as.numeric(H1TO15) %in% 1:6, 1L,
                       ifelse(as.numeric(H1TO15) == 7, 0L, NA_integer_)),
      marijuana_ever = ifelse(as.numeric(H1TO31) > 0,
                         ifelse(as.numeric(H1TO31) >= 996, NA_integer_, 1L),
                         ifelse(as.numeric(H1TO31) == 0, 0L, NA_integer_)),
      cocaine_ever   = ifelse(as.numeric(H1TO35) > 0,
                         ifelse(as.numeric(H1TO35) >= 996, NA_integer_, 1L),
                         ifelse(as.numeric(H1TO35) == 0, 0L, NA_integer_)),
      other_drug     = ifelse(as.numeric(H1TO43) == 1, 1L,
                       ifelse(as.numeric(H1TO43) == 0, 0L, NA_integer_)),
      substance_use_w1 = as.integer(
        (!is.na(smoked_ever)    & smoked_ever    == 1) |
        (!is.na(alcohol_ever)   & alcohol_ever   == 1) |
        (!is.na(marijuana_ever) & marijuana_ever == 1) |
        (!is.na(cocaine_ever)   & cocaine_ever   == 1) |
        (!is.na(other_drug)     & other_drug     == 1)
      )
    ) %>%
    select(-smoked_ever, -alcohol_ever, -marijuana_ever, -cocaine_ever, -other_drug)
}


# -----------------------------------------------------------------------------
# build_substance_use_w2()
# Binary composite: any substance use at wave 2.
# -----------------------------------------------------------------------------
build_substance_use_w2 <- function(df) {
  df %>%
    mutate(
      smoked_ever    = ifelse(as.numeric(H2TO1)  == 1, 1L,
                       ifelse(as.numeric(H2TO1)  == 0, 0L, NA_integer_)),
      alcohol_ever   = ifelse(as.numeric(H2TO19) %in% 1:6, 1L,
                       ifelse(as.numeric(H2TO19) == 7, 0L, NA_integer_)),
      marijuana_ever = ifelse(as.numeric(H2TO44) == 1, 1L,
                       ifelse(as.numeric(H2TO44) == 0, 0L, NA_integer_)),
      cocaine_ever   = ifelse(as.numeric(H2TO50) == 1, 1L,
                       ifelse(as.numeric(H2TO50) == 0, 0L, NA_integer_)),
      other_drug     = ifelse(as.numeric(H2TO61) == 1, 1L,
                       ifelse(as.numeric(H2TO61) == 0, 0L, NA_integer_)),
      substance_use_w2 = as.integer(
        (!is.na(smoked_ever)    & smoked_ever    == 1) |
        (!is.na(alcohol_ever)   & alcohol_ever   == 1) |
        (!is.na(marijuana_ever) & marijuana_ever == 1) |
        (!is.na(cocaine_ever)   & cocaine_ever   == 1) |
        (!is.na(other_drug)     & other_drug     == 1)
      )
    ) %>%
    select(-smoked_ever, -alcohol_ever, -marijuana_ever, -cocaine_ever, -other_drug)
}


# -----------------------------------------------------------------------------
# build_ses()
# Parental education composite and welfare flag.
# wave: 1 (uses H1RM1, H1RF1, H1RM9, H1RF9) or 2 (H2RM1, H2RF1, H2RM9, H2RF9)
# -----------------------------------------------------------------------------
build_ses <- function(df, wave) {
  w <- wave
  rm1 <- paste0("H", w, "RM1")
  rf1 <- paste0("H", w, "RF1")
  rm9 <- paste0("H", w, "RM9")
  rf9 <- paste0("H", w, "RF9")

  df %>%
    mutate(
      mom_edu = ifelse(as.numeric(.data[[rm1]]) %in% c(97, 96, 98, 10, 11, 12),
                       NA_real_, as.numeric(.data[[rm1]])),
      dad_edu = ifelse(as.numeric(.data[[rf1]]) %in% c(97, 96, 98, 10, 11, 12),
                       NA_real_, as.numeric(.data[[rf1]])),
      par_edu = pmax(mom_edu, dad_edu, na.rm = TRUE),
      welfare = as.integer(as.numeric(.data[[rm9]]) == 1 |
                           as.numeric(.data[[rf9]]) == 1)
    ) %>%
    select(-mom_edu, -dad_edu)
}


# -----------------------------------------------------------------------------
# build_race_ethnicity()
# Construct combined race/ethnicity factor with Hispanic priority.
# Source cols: H1GI9 (race), H1GI4 (Hispanic), both from wave 1 only.
# -----------------------------------------------------------------------------
build_race_ethnicity <- function(df) {
  df %>%
    mutate(
      hispanic     = ifelse(as.numeric(H1GI4) %in% c(6,7,8,9), NA, as.numeric(H1GI4)),
      race_raw     = ifelse(as.numeric(H1GI9) %in% c(6,7,8,9), NA, as.numeric(H1GI9)),
      race_ethnicity = case_when(
        hispanic == 1                           ~ "Hispanic",
        race_raw == 1                           ~ "White",
        race_raw == 2                           ~ "Black",
        race_raw == 3                           ~ "American Indian",
        race_raw == 4                           ~ "Asian/PI",
        race_raw == 5                           ~ "Other",
        TRUE                                    ~ NA_character_
      )
    ) %>%
    select(-race_raw)
}


# -----------------------------------------------------------------------------
# build_treatment_w2()
# Recode wave 2 mental health and SUD treatment variables.
# -----------------------------------------------------------------------------
build_treatment_w2 <- function(df) {
  df %>%
    mutate(
      mh_treatment  = ifelse(as.numeric(H2HS5) %in% c(6, 8), NA, as.numeric(H2HS5)),
      sud_treatment = ifelse(as.numeric(H2HS7) %in% c(6, 8), NA, as.numeric(H2HS7)),
      any_treatment = as.integer(
        (!is.na(mh_treatment)  & mh_treatment  == 1) |
        (!is.na(sud_treatment) & sud_treatment == 1)
      )
    )
}
