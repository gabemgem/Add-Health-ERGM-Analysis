# =============================================================================
# plot_helpers.R
# Purpose:  Consistent network visualization functions for the suicide risk
#           network analysis project.
# Inputs:   Called by 02_descriptives.R and downstream scripts
# Outputs:  Functions loaded into calling environment; plots saved to the
#           figures_dir passed by each caller.
# Date:     2026-03-23
# =============================================================================

library(network)
library(ggplot2)
library(ggraph)
library(igraph)
library(tidyverse)
library(here)


# -----------------------------------------------------------------------------
# net_to_igraph()
# Convert a network::network object to igraph for ggraph plotting.
# Preserves vertex attributes and directed/undirected status.
# -----------------------------------------------------------------------------
net_to_igraph <- function(net) {
  n       <- network::network.size(net)
  vnames  <- network::network.vertex.names(net)
  el      <- network::as.edgelist(net)   # numeric vertex indices
  is_dir  <- network::is.directed(net)

  ig <- igraph::make_empty_graph(n = n, directed = is_dir)
  igraph::V(ig)$name <- vnames
  if (nrow(el) > 0) {
    ig <- igraph::add_edges(ig, as.vector(t(el)))
  }

  vattrnames <- setdiff(network::list.vertex.attributes(net), c("na", "vertex.names"))
  for (attr in vattrnames) {
    vals <- network::get.vertex.attribute(net, attr)
    ig <- igraph::set_vertex_attr(ig, attr, value = vals)
  }

  ig
}


# -----------------------------------------------------------------------------
# plot_network_ideation()
# Network visualization with nodes colored by ideation_binary.
# Saves PNG to figures_dir/{label}.png.
# figures_dir: full path to output directory (default: results/figures/).
# Returns ggraph object invisibly.
# -----------------------------------------------------------------------------
network_ideation <- function(ig, wave) {
  # Never substitute another wave's status for a missing wave-specific value.
  attributes <- intersect(c(paste0("ideation_binary_w", wave), "ideation_binary"),
                          igraph::vertex_attr_names(ig))
  if (!length(attributes)) return(rep(NA_real_, igraph::vcount(ig)))
  igraph::vertex_attr(ig, attributes[1])
}

plot_network_ideation <- function(net, label, wave_label = "",
                                   figures_dir = NULL, wave = 1L) {
  if (is.null(figures_dir)) figures_dir <- here::here("results", "figures")
  dir.create(figures_dir, showWarnings = FALSE, recursive = TRUE)

  ig <- net_to_igraph(net)

  ideation <- network_ideation(ig, wave)
  ideation_fct <- factor(
    ifelse(is.na(ideation), "Unknown",
           ifelse(ideation == 1, "Ideation", "No Ideation")),
    levels = c("No Ideation", "Ideation", "Unknown")
  )

  edge_geom <- if (network::is.directed(net)) {
    geom_edge_link(alpha = 0.2, color = "gray60",
                   arrow = arrow(length = unit(2, "mm"), type = "open"),
                   end_cap = circle(2, "mm"))
  } else {
    geom_edge_link(alpha = 0.2, color = "gray60")
  }

  set.seed(42)
  p <- ggraph(ig, layout = "fr") +
    edge_geom +
    geom_node_point(aes(color = ideation_fct), size = 2.5, alpha = 0.85) +
    scale_color_manual(
      values = c("No Ideation" = "#4575b4",
                 "Ideation"    = "#d73027",
                 "Unknown"     = "#999999"),
      name = "Suicide Ideation"
    ) +
    labs(
      title    = paste0("Community Network",
                        if (nchar(wave_label) > 0) paste0(" — ", wave_label) else ""),
      subtitle = paste0(igraph::vcount(ig), " nodes, ",
                        igraph::ecount(ig), if (network::is.directed(net)) " arcs" else " edges")
    ) +
    theme_void(base_size = 12) +
    theme(legend.position = "bottom",
          plot.title = element_text(face = "bold"))

  out_path <- file.path(figures_dir, paste0(label, ".png"))
  ggsave(out_path, p, width = 8, height = 7, dpi = 300)
  message("Plot saved: ", out_path)

  invisible(p)
}


# -----------------------------------------------------------------------------
# plot_network_sidebyside()
# Side-by-side network plots for wave 1 and wave 2 with consistent layout.
# figures_dir: full path to output directory (default: results/figures/).
# -----------------------------------------------------------------------------
plot_network_sidebyside <- function(net_w1, net_w2, label,
                                     figures_dir = NULL) {
  if (is.null(figures_dir)) figures_dir <- here::here("results", "figures")
  dir.create(figures_dir, showWarnings = FALSE, recursive = TRUE)

  ig1 <- net_to_igraph(net_w1)
  ig2 <- net_to_igraph(net_w2)

  set.seed(42)
  layout1  <- igraph::layout_with_fr(ig1)
  v1_names <- igraph::V(ig1)$name
  v2_names <- igraph::V(ig2)$name

  ideation1 <- network_ideation(ig1, 1L)
  ideation2 <- network_ideation(ig2, 2L)

  color_vals <- c("No Ideation" = "#4575b4",
                  "Ideation"    = "#d73027",
                  "Unknown"     = "#999999")

  is_dir <- network::is.directed(net_w1)
  edge_geom <- if (is_dir) {
    geom_edge_link(alpha = 0.2, color = "gray60",
                   arrow = arrow(length = unit(2, "mm"), type = "open"),
                   end_cap = circle(2, "mm"))
  } else {
    geom_edge_link(alpha = 0.2, color = "gray60")
  }

  make_plot <- function(ig, layout_mat, ideation, wave_label) {
    fct <- factor(
      ifelse(is.na(ideation), "Unknown",
             ifelse(ideation == 1, "Ideation", "No Ideation")),
      levels = c("No Ideation", "Ideation", "Unknown")
    )
    ggraph(ig, layout = layout_mat) +
      edge_geom +
      geom_node_point(aes(color = fct), size = 2, alpha = 0.85) +
      scale_color_manual(values = color_vals, name = "Ideation") +
      labs(title = wave_label) +
      theme_void(base_size = 11) +
      theme(legend.position = "bottom",
            plot.title = element_text(face = "bold", hjust = 0.5))
  }

  p1 <- make_plot(ig1, layout1, ideation1, "Wave 1")
  p2 <- make_plot(ig2, layout1[match(v2_names, v1_names), , drop = FALSE],
                  ideation2, "Wave 2")

  combined <- tryCatch({
    library(patchwork)
    p1 + p2 + patchwork::plot_layout(guides = "collect")
  }, error = function(e) p1)

  out_path <- file.path(figures_dir, paste0(label, "_sidebyside.png"))
  ggsave(out_path, combined, width = 14, height = 7, dpi = 300)
  message("Side-by-side plot saved: ", out_path)

  invisible(combined)
}


# -----------------------------------------------------------------------------
# forest_plot()
# Standard forest plot for meta-analysis results from metafor.
# figures_dir: full path to output directory (default: results/figures/).
# -----------------------------------------------------------------------------
forest_plot <- function(rma_fit, label, param_label = "",
                         figures_dir = NULL) {
  if (is.null(figures_dir)) figures_dir <- here::here("results", "figures")
  dir.create(figures_dir, showWarnings = FALSE, recursive = TRUE)

  out_path <- file.path(figures_dir, paste0(label, "_forest.png"))
  png(out_path, width = 1400, height = max(600, 25 * rma_fit$k + 200), res = 150)
  metafor::forest(
    rma_fit,
    main    = paste0("Forest Plot: ", param_label),
    xlab    = "Log-Odds (STERGM coefficient)",
    cex     = 0.75,
    header  = TRUE,
    refline = 0
  )
  dev.off()
  message("Forest plot saved: ", out_path)
  invisible(out_path)
}


# -----------------------------------------------------------------------------
# funnel_plot()
# Standard funnel plot for small-study bias assessment.
# figures_dir: full path to output directory (default: results/figures/).
# -----------------------------------------------------------------------------
funnel_plot <- function(rma_fit, label, param_label = "",
                         figures_dir = NULL) {
  if (is.null(figures_dir)) figures_dir <- here::here("results", "figures")
  dir.create(figures_dir, showWarnings = FALSE, recursive = TRUE)

  out_path <- file.path(figures_dir, paste0(label, "_funnel.png"))
  png(out_path, width = 900, height = 700, res = 150)
  metafor::funnel(
    rma_fit,
    main = paste0("Funnel Plot: ", param_label),
    xlab = "Log-Odds"
  )
  dev.off()
  message("Funnel plot saved: ", out_path)
  invisible(out_path)
}
