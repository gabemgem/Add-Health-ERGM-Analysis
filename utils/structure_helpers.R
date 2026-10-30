# =============================================================================
# structure_helpers.R
# Purpose:  Directed network structure descriptives: dyad census, triad
#           census with its U|MAN expectation, and in/out-degree
#           distributions. All outputs are community-level counts.
# Used by:  02b_network_structure.R (computation), results_report.Rmd (labels)
# Date:     2026-10-19
# =============================================================================

# The 16 directed triad types (Holland & Leinhardt MAN labels), in the order
# returned by igraph::triad_census() and sna::triad.census().
triad_types <- tibble::tribble(
  ~type,  ~class,                ~description,
  "003",  "empty / single dyad", "empty",
  "012",  "empty / single dyad", "one asymmetric tie",
  "102",  "empty / single dyad", "one mutual tie",
  "021D", "open",                "out-star: one person names both others",
  "021U", "open",                "in-star: both name the same person",
  "021C", "open",                "two-path: A -> B -> C",
  "111D", "open",                "one mutual + one asymmetric tie",
  "111U", "open",                "one mutual + one asymmetric tie",
  "201",  "open",                "two mutual ties",
  "030T", "closed, transitive",  "transitive triple",
  "030C", "closed, cyclic",      "cyclic triple: A -> B -> C -> A",
  "120D", "closed, transitive",  "mutual pair + both named by third",
  "120U", "closed, transitive",  "mutual pair + third names both",
  "120C", "closed, cyclic",      "mutual pair + two-path through third",
  "210",  "closed, transitive",  "two mutual ties + one asymmetric tie",
  "300",  "closed, transitive",  "complete: all ties mutual"
)

# Triad census of a directed network given as an n x n 0/1 matrix.
# igraph's algorithm is linear in the number of arcs (fast for large
# communities); sna is used if igraph is not installed. Both return the same
# counts in the same order.
triad_census_matrix <- function(m) {
  if (requireNamespace("igraph", quietly = TRUE)) {
    g <- igraph::graph_from_adjacency_matrix(m, mode = "directed")
    as.numeric(suppressWarnings(igraph::triad_census(g)))
  } else {
    as.numeric(sna::triad.census(m, mode = "digraph"))
  }
}

# Observed triad census and its expectation under U|MAN: random directed
# networks with the same numbers of mutual, asymmetric and null dyads.
# Departures from expected therefore reflect structure beyond density and
# reciprocity (closure, popularity, hierarchy).
triad_census_uman <- function(m, n_sim = 100, seed = 20240104) {
  n <- nrow(m)
  obs <- triad_census_matrix(m)
  dc <- sna::dyad.census(m)
  set.seed(seed)
  sims <- vapply(seq_len(n_sim), function(i) {
    g <- sna::rguman(1, n, mut = dc[1, "Mut"], asym = dc[1, "Asym"], null = dc[1, "Null"], method = "exact")
    triad_census_matrix(g)
  }, numeric(16))
  tibble::tibble(type = triad_types$type, observed = obs,
                 expected = rowMeans(sims), expected_sd = apply(sims, 1, stats::sd))
}

# Adjacency matrix of a wave's network, restricted to people observed in
# that wave (`in_wave2` for wave 2) so attrition does not show up as isolates.
wave_matrix <- function(net, wave) {
  m <- as.matrix(net)
  diag(m) <- 0
  if (wave == 2 && "in_wave2" %in% network::list.vertex.attributes(net)) {
    keep <- network::get.vertex.attribute(net, "in_wave2") %in% 1
    m <- m[keep, keep, drop = FALSE]
  }
  m
}

# All structure descriptives for one community and wave.
network_structure <- function(net, community_id, wave, n_sim = 100) {
  m <- wave_matrix(net, wave)
  n <- nrow(m)
  ids <- tibble::tibble(community_id = as.character(community_id), wave = wave, n_nodes = n)
  dc <- sna::dyad.census(m)
  dyads <- dplyr::bind_cols(ids, tibble::tibble(mutual = dc[1, "Mut"], asymmetric = dc[1, "Asym"],
                                                null = dc[1, "Null"], n_arcs = sum(m)))
  indeg <- colSums(m); outdeg <- rowSums(m)
  degrees <- dplyr::bind_rows(
    tibble::tibble(direction = "in", degree = indeg),
    tibble::tibble(direction = "out", degree = outdeg)) |>
    dplyr::count(direction, degree, name = "n_people")
  triads <- if (n >= 3) triad_census_uman(m, n_sim) else NULL
  list(dyads = dyads,
       degrees = dplyr::bind_cols(ids[rep(1, nrow(degrees)), ], degrees),
       triads = if (is.null(triads)) NULL else dplyr::bind_cols(ids[rep(1, 16), ], triads))
}

# In- and out-degree histograms (people summed over the communities in
# `degrees`; pass one community's rows for a per-community plot), one panel
# per wave and direction on a shared x-axis from 0 to `max_degree`. Out-degree
# is capped at 10 nominations; any degree above `max_degree` is counted in the
# last bar, labelled "max_degree+", so no one is dropped. A plain histogram
# with no other figures beside it is exempt from the small-cell rules.
degree_plot <- function(degrees, max_degree = 20, y_label = "People", base_size = 11) {
  d <- degrees |>
    dplyr::mutate(degree = pmin(degree, max_degree)) |>
    dplyr::group_by(wave, direction, degree) |>
    dplyr::summarise(n_people = sum(n_people), .groups = "drop")
  breaks <- seq(0, max_degree, by = 5)
  ggplot2::ggplot(d, ggplot2::aes(degree, n_people)) +
    ggplot2::geom_col(fill = "#2a6db0") +
    ggplot2::facet_grid(paste("Wave", wave) ~ paste0(direction, "-degree")) +
    ggplot2::scale_x_continuous(breaks = breaks, labels = ifelse(breaks == max_degree, paste0(max_degree, "+"), breaks),
                                limits = c(-0.5, max_degree + 0.5), expand = c(0, 0)) +
    ggplot2::labs(x = "Degree (out-degree is capped at 10 nominations)", y = y_label) +
    ggplot2::theme_minimal(base_size = base_size)
}

