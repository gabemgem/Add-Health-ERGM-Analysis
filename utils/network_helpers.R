# =============================================================================
# network_helpers.R
# Purpose:  Reusable functions for edgelist construction, directed network
#           creation, and nodal covariate attachment.
# Inputs:   Called by 01_data_prep.R and downstream scripts
# Outputs:  Functions loaded into calling environment
# Date:     2026-03-23
# =============================================================================

library(network)
library(tidyverse)

# -----------------------------------------------------------------------------
# to_edgelist()
# Convert wide friendship nomination file to long (nominator, nominee) edgelist.
# Drops rows where nominee is NA or 0 (no nomination).
# -----------------------------------------------------------------------------
to_edgelist <- function(friend_df) {
  nom_cols <- c(paste0("MF_AID", 1:5), paste0("FF_AID", 1:5))
  nom_cols <- nom_cols[nom_cols %in% names(friend_df)]
  friend_df %>%
    select(AID, all_of(nom_cols)) %>%
    # Coerce all nomination columns to numeric before pivoting so that type
    # differences between the AID column and nomination columns don't cause
    # silent join failures downstream.
    mutate(across(all_of(nom_cols), as.numeric),
           AID = as.numeric(AID)) %>%
    pivot_longer(-AID, names_to = "slot", values_to = "nominee") %>%
    filter(!is.na(nominee), nominee > 0) %>%
    select(nominator = AID, nominee)
}


# -----------------------------------------------------------------------------
# filter_edgelist()
# Restrict an edgelist to:
#   1. Nominees present in `valid_aids` (drops phantoms)
#   2. Both nominator and nominee share the same community_id
# Returns list: $edgelist, $n_phantom_dropped, $n_cross_community_dropped
# -----------------------------------------------------------------------------
filter_edgelist <- function(edgelist, valid_aids, community_map) {
  # community_map: named vector community_id, names = AID (as character)
  aid_char <- as.character(valid_aids)

  # Step 1 — drop phantom nominees
  n_before <- nrow(edgelist)
  el <- edgelist %>%
    filter(as.character(nominee) %in% aid_char)
  n_phantom <- n_before - nrow(el)

  # Step 2 — drop cross-community nominations
  el <- el %>%
    mutate(
      comm_nom  = community_map[as.character(nominator)],
      comm_nom2 = community_map[as.character(nominee)]
    ) %>%
    filter(!is.na(comm_nom), !is.na(comm_nom2), comm_nom == comm_nom2) %>%
    select(nominator, nominee)

  n_cross <- (n_before - n_phantom) - nrow(el)

  list(
    edgelist               = el,
    n_phantom_dropped      = n_phantom,
    n_cross_community_dropped = n_cross
  )
}


# -----------------------------------------------------------------------------
# attach_covariates()
# Attach a data frame of nodal covariates to a network object.
# covar_df must have an AID column; rows matched by AID to vertex names.
# All other columns are set as vertex attributes.
# -----------------------------------------------------------------------------
attach_covariates <- function(net, covar_df) {
  # Keep both sides as character so match() doesn't fail on type differences
  # (match() does not coerce types — numeric vs character returns all NA).
  vnames        <- network::network.vertex.names(net)
  covar_df$AID  <- as.character(covar_df$AID)

  attr_cols <- setdiff(names(covar_df), "AID")
  for (col in attr_cols) {
    vals <- covar_df[[col]][match(vnames, covar_df$AID)]
    network::set.vertex.attribute(net, col, vals)
  }
  net
}


# -----------------------------------------------------------------------------
# make_directed_network()
# Build a DIRECTED network::network object from a raw nomination edgelist.
# Preserves nominator → nominee arc direction; no symmetrization applied.
# Deduplicates arcs defensively (duplicate nominations shouldn't occur in Add
# Health but can arise from data entry errors in the raw friendship file).
# nodes: numeric vector of AIDs that are vertices (includes isolates).
# -----------------------------------------------------------------------------
make_directed_network <- function(edgelist, nodes) {
  nodes <- as.numeric(nodes)
  edgelist <- edgelist %>%
    mutate(nominator = as.numeric(nominator),
           nominee   = as.numeric(nominee)) %>%
    distinct(nominator, nominee)

  n_arcs <- nrow(edgelist)
  message("  make_directed_network: ", n_arcs, " directed arcs")

  net <- network::network.initialize(length(nodes), directed = TRUE)
  network::network.vertex.names(net) <- as.character(nodes)

  if (n_arcs > 0) {
    el_mat     <- as.matrix(edgelist[, c("nominator", "nominee")])
    aid_to_idx <- setNames(seq_along(nodes), as.character(nodes))
    el_idx     <- matrix(
      c(aid_to_idx[as.character(el_mat[, 1])],
        aid_to_idx[as.character(el_mat[, 2])]),
      ncol = 2
    )
    n_unmatched <- sum(is.na(el_idx[, 1]) | is.na(el_idx[, 2]))
    if (n_unmatched > 0) {
      warning("  make_directed_network: ", n_unmatched,
              " arc(s) dropped — AID not found in node list (type mismatch?)")
    }
    el_idx <- el_idx[!is.na(el_idx[, 1]) & !is.na(el_idx[, 2]), , drop = FALSE]
    if (nrow(el_idx) > 0) {
      network::add.edges(net, el_idx[, 1], el_idx[, 2])
    }
  }

  net
}


# -----------------------------------------------------------------------------
# compute_reciprocity()
# Compute reciprocity rate from the raw (directed) nomination edgelist.
# Returns proportion of directed ties that are reciprocated.
# -----------------------------------------------------------------------------
compute_reciprocity <- function(edgelist) {
  edgelist <- edgelist %>% distinct(nominator, nominee)   # duplicate nominations count once
  if (nrow(edgelist) == 0) return(NA_real_)
  recip <- edgelist %>%
    inner_join(edgelist,
               by = c("nominator" = "nominee", "nominee" = "nominator"))
  nrow(recip) / nrow(edgelist)
}
