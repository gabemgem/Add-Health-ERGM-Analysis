# =============================================================================
# report_helpers.R
# Purpose:  Display helpers for results_report.Rmd: locating result CSVs,
#           side-by-side model tables whose columns are shown/hidden by the
#           report's model checkboxes, cell formats, and plots.
#           Reads only CSV/text results; no data access.
# Date:     2026-10-19
# =============================================================================

source("utils/coefficient_labels.R", local = TRUE)

# ---- Locating result files ------------------------------------------------------
# Results may be in a fresh pipeline bundle or the archived reference folder.
result_index <- function(root) {
  if (!dir.exists(root)) stop("Results directory does not exist: ", root)
  f <- list.files(root, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
  if (anyDuplicated(basename(f))) stop("Duplicate result filenames under ", root,
                                      "; select a single result bundle.")
  f[order(nchar(f))]
}

# Empty files (a table with no columns is written as 0 bytes) count as missing.
read_result <- function(index, name) {
  hit <- index[basename(index) == name]
  if (length(hit) == 0 || file.size(hit[1]) == 0) return(NULL)
  bytes <- readBin(hit[1], "raw", n = file.size(hit[1]))
  encoding <- if (validUTF8(rawToChar(bytes))) "UTF-8" else "Windows-1252"
  d <- tryCatch(readr::read_csv(hit[1], show_col_types = FALSE, guess_max = 100000,
                               locale = readr::locale(encoding = encoding)), error = function(e) NULL)
  if (!is.null(d) && ncol(d) == 0) return(NULL)
  # The manuscript's archived descriptive export has presentation headings.
  # Normalize those headings at read time, leaving its saved values untouched.
  if (!is.null(d) && name == "all_communities_descriptives_network_directed_full.csv") {
    aliases <- c("Community ID" = "community_id", "Wave" = "wave", "N Nodes" = "n_nodes",
      "N Edges" = "n_edges_arcs", "Density" = "density", "Avg. Degree" = "avg_degree",
      "Clustering" = "clustering", "Reciprocity" = "reciprocity", "N Isolates" = "n_isolates",
      "N Components" = "n_components", "Largest Component" = "largest_component",
      "Female %" = "female_share", "Age Avg." = "age_mean", "Age SD" = "age_sd",
      "rase_asian_pi" = "race_asian_pi", "Race Heterogeneity" = "race_heterogeneity",
      "Welfare receivers W1" = "welfare_share_w1", "Ideation W1" = "ideation_prev_w1",
      "Ideation W2" = "ideation_prev_w2", "Suicide Attempts W1" = "attempt_prev_w1",
      "Suicide Attempts W2" = "attempt_prev_w2", "Any Exposure W1" = "any_exposure_prev_w1",
      "Any Exposure W2" = "any_exposure_prev_w2", "CESD W1 Avg." = "cesd_w1_mean",
      "CESD W1 SD" = "cesd_w1_sd", "CESD W2 Avg." = "cesd_w2_mean", "CESD W2 SD" = "cesd_w2_sd",
      "Hopelessness W1 Avg." = "hopelessness_w1_mean", "Hopelessness W1 SD" = "hopelessness_w1_sd",
      "Hopelessness W2 Avg." = "hopelessness_w2_mean", "Hopelessness W2 SD" = "hopelessness_w2_sd",
      "Loneliness W1 Avg." = "loneliness_w1_mean", "Loneliness W1 SD" = "loneliness_w1_sd",
      "Loneliness W2 Avg." = "loneliness_w2_mean", "Loneliness W2 SD" = "loneliness_w2_sd",
      "Social Support W1 Avg." = "social_support_w1_mean", "Social Support W1 SD" = "social_support_w1_sd",
      "Social Support W2 Avg." = "social_support_w2_mean", "Social Support W2 SD" = "social_support_w2_sd",
      "Substance Use W1 Avg." = "substance_use_prev_w1", "Substance Use W1 SD" = "substance_use_prev_w1_sd",
      "Substance Use W2 Avg." = "substance_use_prev_w2", "Substance Use W2 SD" = "substance_use_prev_w2_sd",
      "Wave 2 Attrition" = "wave2_attrition")
    match_names <- match(names(d), names(aliases))
    names(d)[!is.na(match_names)] <- unname(aliases[match_names[!is.na(match_names)]])
  }
  if (!is.null(d) && "community_id" %in% names(d)) d$community_id <- as.character(d$community_id)
  d
}

# ---- Formatting --------------------------------------------------------------------
# Clean ERGM/STERGM term names into plain-English labels for display.
clean_term <- function(x, max_width = 60) {
  coefficient_label(x, max_width)
}

# Fixed notation unless wider than 5 characters, then scientific; integers
# print without decimals; non-finite values print as an em dash.
fmt_num <- function(x, digits = 3) {
  x <- suppressWarnings(as.numeric(x))
  is_int <- !is.na(x) & x == round(x)
  fixed <- ifelse(is_int, formatC(x, format = "d", big.mark = ""), formatC(x, format = "f", digits = digits))
  width <- nchar(sub("^-", "", fixed))
  out <- ifelse(width > 5, formatC(x, format = "e", digits = 2), fixed)
  ifelse(is.finite(x), out, "—")
}

fmt_p <- function(p) ifelse(is.na(p), "—", formatC(p, digits = 2, format = "g"))

# Figures blanked on the analysis machine under the small-cell rules
# (utils/disclosure.R) arrive as this symbol; keep it, format the rest.
SUPPRESSED_SYMBOL <- "§"
is_suppressed <- function(x) !is.na(x) & as.character(x) == SUPPRESSED_SYMBOL
as_num <- function(x) suppressWarnings(as.numeric(x))   # suppressed -> NA
fmt_disc <- function(x, digits = 3) ifelse(is_suppressed(x), SUPPRESSED_SYMBOL, fmt_num(as_num(x), digits))
pct_disc <- function(x, accuracy = 0.1) {
  ifelse(is_suppressed(x), SUPPRESSED_SYMBOL, ifelse(is.na(as_num(x)), "—", scales::percent(as_num(x), accuracy)))
}

# Stars: + p<.1, * p<.05, ** p<.01, *** p<.001.
p_stars <- function(p) {
  dplyr::case_when(is.na(p) ~ "", p < 0.001 ~ "***", p < 0.01 ~ "**", p < 0.05 ~ "*", p < 0.1 ~ "+", TRUE ~ "")
}

kable_styled <- function(df, caption = NULL, escape = TRUE, full_width = FALSE, font_size = 13) {
  knitr::kable(df, format = "html", caption = caption, escape = escape,
               align = c("l", rep("r", max(0, ncol(df) - 1)))) |>
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                              full_width = full_width, font_size = font_size)
}

# ---- Model toggles and side-by-side tables -----------------------------------------
# Every model column cell carries data-model="<group>:<model_id>"; a checkbox
# with the same data-model value shows/hides them all (see toggle_assets()).
toggle_key <- function(group, id) paste0(group, ":", id)

toggle_assets <- function() {
  htmltools::HTML('
<style>
.model-toggles { position: sticky; top: 0; z-index: 1000; background: #f7f7f9; border: 1px solid #ddd;
  border-radius: 4px; padding: 6px 12px; margin: 10px 0 16px; font-size: 13px; }
.model-toggle-label { margin: 0 12px 0 4px; font-weight: normal; cursor: pointer; white-space: nowrap; }
.model-toggles button { margin-left: 4px; font-size: 11px; padding: 0 6px; }
table.model-table { border-collapse: collapse; margin: 8px 0 22px; font-size: 13px; }
table.model-table caption { caption-side: top; font-weight: bold; color: #333; padding-bottom: 6px; }
table.model-table th, table.model-table td { padding: 4px 10px; text-align: center; vertical-align: top;
  border-bottom: 1px solid #e5e5e5; }
table.model-table th { border-bottom: 2px solid #555; background: #fafafa; }
table.model-table td.term { text-align: left; white-space: nowrap; }
table.model-table tr.footer-row td { background: #fafafa; font-size: 12px; color: #444; }
table.model-table tr.first-footer td { border-top: 2px solid #555; }
.se { color: #666; } .pi { color: #2a6db0; font-size: 11px; } .kk { color: #999; font-size: 11px; }
.nc { color: #b33; font-weight: bold; }
.verdict { display: inline-block; padding: 1px 8px; border-radius: 10px; font-weight: bold; font-size: 12px; }
.verdict-ok { background: #d4edda; color: #155724; }
.verdict-caution { background: #fff3cd; color: #856404; }
.verdict-poor { background: #f8d7da; color: #721c24; }
.reasons { font-size: 11px; color: #555; }
.note { background: #f4f8fb; border-left: 4px solid #2a6db0; padding: 8px 14px; margin: 12px 0; }
</style>
<script>
document.addEventListener("change", function (e) {
  if (!e.target.classList.contains("model-toggle")) return;
  var key = e.target.getAttribute("data-model");
  document.querySelectorAll("[data-model=\\"" + key + "\\"]").forEach(function (el) {
    if (el.classList.contains("model-toggle")) { el.checked = e.target.checked; return; }
    el.style.display = e.target.checked ? "" : "none";
  });
});
document.addEventListener("click", function (e) {
  var all = e.target.classList.contains("toggle-all"), none = e.target.classList.contains("toggle-none");
  if (!all && !none) return;
  var group = e.target.getAttribute("data-group");
  document.querySelectorAll("input.model-toggle[data-group=\\"" + group + "\\"]").forEach(function (box) {
    if (box.checked !== all) { box.checked = all; box.dispatchEvent(new Event("change", { bubbles: true })); }
  });
});
</script>')
}

toggle_panel <- function(group, ids, labels, title = "Show models:") {
  boxes <- paste0(sprintf(
    '<label class="model-toggle-label"><input type="checkbox" class="model-toggle" data-group="%s" data-model="%s" checked> %s</label>',
    group, toggle_key(group, ids), htmltools::htmlEscape(labels)), collapse = "")
  htmltools::HTML(sprintf(
    '<div class="model-toggles"><strong>%s</strong> %s<button type="button" class="toggle-all" data-group="%s">All</button><button type="button" class="toggle-none" data-group="%s">None</button></div>',
    title, boxes, group, group))
}

# long: model_id, term, cell (HTML). footer: model_id, row, value (text).
# Columns follow `ids` order; rows follow `terms` (default: first appearance).
column_table <- function(long, group, ids, labels, footer = NULL, caption = NULL, terms = NULL,
                         row_header = "Term") {
  keep <- ids %in% unique(c(long$model_id, footer$model_id))
  ids <- ids[keep]; labels <- labels[keep]
  if (length(ids) == 0) return("<p><em>No results available.</em></p>")
  long <- long[order(match(long$model_id, ids)), ]
  terms <- unique(c(intersect(terms, long$term), long$term))
  th <- paste0(sprintf('<th data-model="%s">%s</th>', toggle_key(group, ids), htmltools::htmlEscape(labels)), collapse = "")
  rows <- vapply(terms, function(tm) {
    cells <- vapply(ids, function(id) {
      v <- long$cell[long$model_id == id & long$term == tm]
      sprintf('<td data-model="%s">%s</td>', toggle_key(group, id), if (length(v)) v[1] else "—")
    }, character(1))
    sprintf('<tr><td class="term">%s</td>%s</tr>', htmltools::htmlEscape(clean_term(tm)), paste(cells, collapse = ""))
  }, character(1))
  frows <- character(0)
  if (!is.null(footer) && nrow(footer) > 0) {
    fr <- unique(footer$row)
    frows <- vapply(seq_along(fr), function(i) {
      cells <- vapply(ids, function(id) {
        v <- footer$value[footer$model_id == id & footer$row == fr[i]]
        sprintf('<td data-model="%s">%s</td>', toggle_key(group, id), if (length(v)) htmltools::htmlEscape(v[1]) else "")
      }, character(1))
      sprintf('<tr class="footer-row%s"><td class="term">%s</td>%s</tr>', if (i == 1) " first-footer" else "",
              htmltools::htmlEscape(fr[i]), paste(cells, collapse = ""))
    }, character(1))
  }
  sprintf('<table class="model-table">%s<thead><tr><th>%s</th>%s</tr></thead><tbody>%s%s</tbody></table>',
          if (is.null(caption)) "" else sprintf("<caption>%s</caption>", htmltools::htmlEscape(caption)),
          row_header, th, paste(rows, collapse = ""), paste(frows, collapse = ""))
}

# Pooled cell: estimate + stars, (SE), [95% prediction interval], k.
pooled_cells <- function(meta) {
  meta |> dplyr::mutate(cell = ifelse(
    is.na(estimate),
    paste0("<span class='kk' title='fewer than 3 usable communities'>k = ", k, ", too few to pool</span>"),
    paste0(fmt_num(estimate), "<sup>", p_stars(p.value), "</sup>",
           "<br><span class='se'>(", fmt_num(std.error), ")</span>",
           "<br><span class='pi' title='95% prediction interval: range expected for a new community'>[",
           fmt_num(pi.lb, 2), ", ", fmt_num(pi.ub, 2), "]</span>",
           "<br><span class='kk' title='communities pooled'>k = ", k, "</span>")))
}

# Per-community cell: estimate + stars (and SE below if `se`); dagger = SE not
# estimable; n/c = the whole fit (or STERGM sub-model) is unusable.
community_cells <- function(coefs, se = FALSE) {
  coefs |> dplyr::mutate(cell = dplyr::case_when(
    problem & !is.na(problem_reason) & !grepl("^(no standard error|infinite estimate)$", problem_reason) ~
      paste0("<span class='nc' title='", htmltools::htmlEscape(problem_reason), "'>n/c</span>"),
    !is.finite(std.error) ~ paste0(fmt_num(estimate), "†"),
    TRUE ~ paste0(fmt_num(estimate), p_stars(p.value),
                  if (se) paste0("<br><span class='se'>(", fmt_num(std.error), ")</span>") else "")))
}

trust_cells <- function(trust) {
  trust |> dplyr::mutate(cell = paste0(
    "<span class='verdict verdict-", verdict, "' title='", htmltools::htmlEscape(reasons), "'>", verdict, "</span>",
    ifelse(nzchar(reasons), paste0("<br><span class='reasons'>", htmltools::htmlEscape(reasons), "</span>"), "")))
}

# ---- Per-community view ----------------------------------------------------------------
# One block per community (descriptives, ERGM table, STERGM tables, fit notes,
# GOF plots); a dropdown shows one block at a time, or all of them.

community_view_assets <- function() {
  htmltools::HTML('
<style>
.community-nav { position: sticky; top: 0; z-index: 1001; background: #fff; border: 1px solid #ddd; border-radius: 4px;
  padding: 8px 12px; margin: 10px 0 12px; font-size: 14px; }
.community-nav select { min-width: 280px; margin: 0 6px; }
.community-nav button { font-size: 12px; padding: 1px 8px; margin-left: 2px; }
.community-block { border-top: 3px solid #2a6db0; padding-top: 4px; margin-bottom: 36px; }
.community-block h3 { margin-top: 8px; }
.community-block h4 { margin-top: 22px; font-size: 16px; }
.community-flags { color: #8a4b08; font-size: 13px; font-weight: normal; }
.community-nav .model-toggles { position: static; border: none; background: none; padding: 2px 0 0; margin: 4px 0 0; }
.community-events { font-size: 13px; color: #444; margin: -4px 0 10px; }
table.notes-table { border-collapse: collapse; font-size: 12px; margin: 6px 0 16px; width: 100%; }
table.notes-table th, table.notes-table td { border-bottom: 1px solid #e5e5e5; padding: 3px 8px; text-align: left; vertical-align: top; }
table.notes-table th { border-bottom: 2px solid #555; background: #fafafa; }
</style>
<script>
function showCommunity(v) {
  document.querySelectorAll(".community-block").forEach(function (el) {
    el.style.display = (v === "__all__" || el.getAttribute("data-community") === v) ? "" : "none";
  });
}
document.addEventListener("change", function (e) {
  if (e.target.id === "community-select") showCommunity(e.target.value);
});
document.addEventListener("click", function (e) {
  var step = e.target.getAttribute("data-step");
  if (!step) return;
  var sel = document.getElementById("community-select");
  var opts = Array.prototype.filter.call(sel.options, function (o) { return o.value !== "__all__"; });
  var i = opts.findIndex(function (o) { return o.value === sel.value; });
  i = Math.max(0, Math.min(opts.length - 1, (i < 0 ? 0 : i + Number(step))));
  sel.value = opts[i].value;
  showCommunity(sel.value);
});
</script>')
}

# Dropdown: featured communities first, then the rest; plus "All communities".
# `extra` is HTML placed inside the sticky bar (e.g. model toggles).
community_selector <- function(cids, labels, featured = character(0), extra = "") {
  opt <- function(ids) paste0(sprintf('<option value="%s">%s</option>', htmltools::htmlEscape(ids),
                                      htmltools::htmlEscape(labels[ids])), collapse = "")
  feat <- intersect(featured, cids)
  rest <- setdiff(cids, feat)
  body <- paste0(if (length(feat)) paste0('<optgroup label="Featured">', opt(feat), '</optgroup>'),
                 '<optgroup label="', if (length(feat)) "Other communities" else "Communities", '">', opt(rest), '</optgroup>',
                 '<option value="__all__">All communities (long)</option>')
  sprintf('<div class="community-nav"><strong>Community:</strong><select id="community-select">%s</select><button type="button" data-step="-1">&larr; Previous</button><button type="button" data-step="1">Next &rarr;</button>%s</div>',
          body, extra)
}

# Fit notes for one community and one result set (E or S): one row per model
# (and sub-model) with status, method, why it failed, dropped terms, imputed
# values, warnings, collinear pairs and GOF statistics that fit poorly.
community_notes <- function(R, cid, group, ids, labels, corr_threshold, gof_p) {
  if (is.null(R$info)) return(tibble::tibble())
  info <- dplyr::filter(R$info, community_id == cid, model_id %in% ids)
  if (nrow(info) == 0) return(tibble::tibble())
  term_part <- function(t) dplyr::case_when(grepl("^Form\\(", t) ~ "formation",
                                            grepl("^(Persist|Diss)\\(", t) ~ "dissolution", TRUE ~ "ergm")
  corr <- if (is.null(R$corr)) tibble::tibble(model_id = character(), part = character(), collinear = character()) else R$corr |>
    dplyr::filter(community_id == cid, abs(r) >= corr_threshold) |>
    dplyr::mutate(part = term_part(term1),
                  pair = sprintf("%s & %s (r = %s)", clean_term(term1, 35), clean_term(term2, 35), fmt_num(r, 2))) |>
    dplyr::group_by(model_id, part) |> dplyr::summarise(collinear = paste(pair, collapse = "; "), .groups = "drop")
  gof <- if (is.null(R$gof)) tibble::tibble(model_id = character(), gof_poor = character()) else R$gof |>
    dplyr::filter(community_id == cid, p.value < gof_p) |>
    dplyr::group_by(model_id) |> dplyr::summarise(gof_poor = paste(unique(statistic), collapse = ", "), .groups = "drop")
  out <- info |>
    dplyr::left_join(corr, by = c("model_id", "part")) |>
    dplyr::left_join(gof, by = "model_id") |>
    dplyr::mutate(order = match(model_id, ids)) |> dplyr::arrange(order, part)
  short <- function(x, n = 220) ifelse(is.na(x) | !nzchar(x), "", ifelse(nchar(x) > n, paste0(substr(x, 1, n), "…"), x))
  rows <- vapply(seq_len(nrow(out)), function(i) {
    o <- out[i, ]
    notes <- c(
      if (!is.na(o$problem) && nzchar(o$problem)) paste0("<b>Problem:</b> ", htmltools::htmlEscape(o$problem)),
      if (!is.na(o$error) && nzchar(o$error)) paste0("<b>Error:</b> ", htmltools::htmlEscape(short(o$error))),
      if (!is.na(o$terms_dropped) && nzchar(o$terms_dropped)) paste0("<b>Dropped:</b> ", htmltools::htmlEscape(o$terms_dropped)),
      if ("imputed" %in% names(o) && !is.na(o$imputed) && nzchar(o$imputed)) paste0("<b>Imputed:</b> ", htmltools::htmlEscape(o$imputed)),
      if (!is.na(o$collinear)) paste0("<b>Collinear (|r| &ge; ", corr_threshold, "):</b> ", htmltools::htmlEscape(o$collinear)),
      if (!is.na(o$gof_poor)) paste0("<b>Poor GOF (p &lt; ", gof_p, "):</b> ", htmltools::htmlEscape(o$gof_poor)),
      if (!is.na(o$warnings) && nzchar(o$warnings))
        sprintf("<b>Warnings:</b> <span title='%s'>%s</span>", htmltools::htmlEscape(o$warnings), htmltools::htmlEscape(short(o$warnings, 160)))
    )
    sprintf('<tr data-model="%s"><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>',
            toggle_key(group, o$model_id), htmltools::htmlEscape(labels[[o$model_id]]),
            if (o$part == "ergm") "" else htmltools::htmlEscape(o$part),
            htmltools::htmlEscape(paste0(o$status, ifelse(is.na(o$method) | o$status == "too small", "",
                                                          paste0(" (", ifelse(o$method == "MCMLE", "MCMLE", "SA"), ")")))),
            if (length(notes)) paste(notes, collapse = "<br>") else "")
  }, character(1))
  tibble::tibble(html = rows)
}

# GOF images for one community: one collapsible block per model.
community_gof_images <- function(img_idx, type, cid, model_ids, labels) {
  out <- character(0)
  for (id in model_ids) {
    f <- img_idx[basename(img_idx) == sprintf("%s_models_%s_community_%s_gof.png", type, id, cid)]
    if (length(f) == 0) next
    out <- c(out, sprintf('<details><summary>GOF: %s</summary><img src="%s" style="width:100%%;max-width:1100px;"></details>',
                          htmltools::htmlEscape(labels[[id]]), knitr::image_uri(f[1])))
  }
  paste(out, collapse = "")
}

# ---- Plots ---------------------------------------------------------------------------
# Per-community estimates (95% CI) with the pooled estimate, its CI (dark band)
# and prediction interval (light band), one panel per term.
forest_plot <- function(coefs, meta) {
  d <- coefs |>
    dplyr::filter(!problem, is.finite(std.error)) |>
    dplyr::group_by(term) |>
    dplyr::mutate(rank = rank(estimate, ties.method = "first"), term_lab = clean_term(term, 40)) |>
    dplyr::ungroup()
  m <- dplyr::filter(meta, !is.na(estimate)) |> dplyr::mutate(term_lab = clean_term(term, 40))
  ggplot2::ggplot(d, ggplot2::aes(x = estimate, y = rank)) +
    ggplot2::geom_rect(data = m, ggplot2::aes(xmin = pi.lb, xmax = pi.ub, ymin = -Inf, ymax = Inf),
                       inherit.aes = FALSE, fill = "#2a6db0", alpha = 0.12) +
    ggplot2::geom_rect(data = m, ggplot2::aes(xmin = ci.lb, xmax = ci.ub, ymin = -Inf, ymax = Inf),
                       inherit.aes = FALSE, fill = "#2a6db0", alpha = 0.3) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40") +
    ggplot2::geom_vline(data = m, ggplot2::aes(xintercept = estimate), colour = "#2a6db0") +
    ggplot2::geom_linerange(ggplot2::aes(xmin = estimate - 1.96 * std.error, xmax = estimate + 1.96 * std.error),
                            colour = "grey55", linewidth = 0.3) +
    ggplot2::geom_point(ggplot2::aes(shape = small_community %in% TRUE), size = 1.1) +
    ggplot2::scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 1), labels = c("n >= 50", "n < 50"), name = NULL) +
    ggplot2::facet_wrap(~ term_lab, scales = "free") +
    ggplot2::labs(x = "Estimate (log-odds), per-community 95% CI", y = "Communities (sorted)") +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(axis.text.y = ggplot2::element_blank(), panel.grid.major.y = ggplot2::element_blank(),
                   legend.position = "bottom")
}

# Caterpillar plot for a paper: one panel per term; within a panel each row is
# a community (sorted by its estimate) with its 95% CI. Estimates whose CI
# excludes 0 (p < .05) are drawn dark and filled; the rest light and open, so
# significance survives grayscale printing. Featured communities get an orange
# ring and a label. Only usable fits are shown (no failed/degenerate fits, SE
# estimable). `featured` is a named vector: names = community ids, values =
# labels to print (use pseudonyms such as "A", "B", "C", not school ids).
# `order`: "estimate" sorts each panel's rows by that term's estimate;
# "community" gives every panel the same rows, sorted by community id, so a
# community sits on the same row in every panel (a term missing for a
# community leaves its row empty). `row_labels = TRUE` prints community ids on
# the y-axis (community order only; ids are school ids, so not for the paper).
# Returns the plot with attributes n_shown / n_excluded for the caption.
coef_caterpillar <- function(coefs, model, part_name, terms = NULL, featured = NULL, ncol = 3,
                             pooled = NULL, order = c("estimate", "community"), row_labels = FALSE,
                             community_ids = NULL) {
  order <- match.arg(order)
  ink <- "#0b0b0b"; ink2 <- "#52514e"; sig_col <- "#2a78d6"; ns_col <- "#b9b8b0"; feat_col <- "#eb6834"
  all <- dplyr::filter(coefs, model_id == model, part == part_name)
  if (!is.null(terms)) all <- dplyr::filter(all, term %in% terms)
  d <- all |> dplyr::filter(!problem, is.finite(std.error), is.finite(estimate))
  if (nrow(d) == 0) return(NULL)
  term_order <- if (is.null(terms)) unique(d$term) else intersect(terms, d$term)
  d <- d |>
    dplyr::mutate(lo = estimate - 1.96 * std.error, hi = estimate + 1.96 * std.error,
                  sig = p.value < 0.05,
                  feat = community_id %in% names(featured),
                  feat_lab = ifelse(feat, unname(featured[community_id]), NA_character_)) |>
    dplyr::group_by(term) |>
    dplyr::mutate(rank = rank(estimate, ties.method = "first"),
                  strip = sprintf("%s\n%d of %d significant\n%d positive, %d negative",
                                  paste(strwrap(clean_term(term[1], 80), width = 34), collapse = "\n"),
                                  sum(sig), dplyr::n(), sum(sig & estimate > 0), sum(sig & estimate < 0))) |>
    dplyr::ungroup()
  d$strip <- factor(d$strip, levels = unique(d$strip[order(match(d$term, term_order))]))
  ids <- if (is.null(community_ids)) unique(d$community_id) else community_ids
  ids <- ids[order(suppressWarnings(as.numeric(ids)), ids)]
  if (order == "community") d$rank <- length(ids) + 1 - match(d$community_id, ids)   # first id at the top
  p <- ggplot2::ggplot(d, ggplot2::aes(x = estimate, y = rank)) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = ink2, linewidth = 0.3)
  if (!is.null(pooled)) {
    pm <- dplyr::filter(pooled, model_id == model, part == part_name, term %in% d$term, !is.na(estimate)) |>
      dplyr::left_join(dplyr::distinct(d, term, strip), by = "term")
    p <- p + ggplot2::geom_vline(data = pm, ggplot2::aes(xintercept = estimate), colour = ink2, linewidth = 0.4)
  }
  p <- p +
    ggplot2::geom_linerange(data = dplyr::filter(d, !sig), ggplot2::aes(xmin = lo, xmax = hi), colour = ns_col, linewidth = 0.35) +
    ggplot2::geom_linerange(data = dplyr::filter(d, sig), ggplot2::aes(xmin = lo, xmax = hi), colour = sig_col, linewidth = 0.6) +
    ggplot2::geom_point(data = dplyr::filter(d, !sig), shape = 21, size = 1.3, stroke = 0.4, colour = "#898781", fill = "white") +
    ggplot2::geom_point(data = dplyr::filter(d, sig), shape = 21, size = 1.5, stroke = 0.4, colour = sig_col, fill = sig_col) +
    ggplot2::geom_point(data = dplyr::filter(d, feat), shape = 21, size = 3, stroke = 0.8, colour = feat_col, fill = NA) +
    ggplot2::geom_text(data = dplyr::filter(d, feat), ggplot2::aes(x = hi, label = feat_lab), hjust = -0.25, size = 2.6,
                       colour = ink, fontface = "bold") +
    ggplot2::facet_wrap(~ strip, scales = if (order == "community") "free_x" else "free", ncol = ncol) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.05, 0.12))) +
    ggplot2::labs(x = "Estimate (log-odds) with 95% CI",
                  y = if (order == "community") "Communities, sorted by ID" else "Communities, sorted by estimate") +
    ggplot2::theme_minimal(base_size = 9) +
    ggplot2::theme(axis.text.y = ggplot2::element_blank(), panel.grid.major.y = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(), panel.grid.major.x = ggplot2::element_line(colour = "#e1e0d9", linewidth = 0.3),
                   strip.text = ggplot2::element_text(hjust = 0, colour = ink, size = 8.5, lineheight = 1.05),
                   axis.title = ggplot2::element_text(colour = ink2), axis.text.x = ggplot2::element_text(colour = ink2),
                   panel.spacing.x = ggplot2::unit(10, "pt"))
  if (order == "community") {
    # Same rows in every panel: a faint guide per row helps follow a community across.
    p <- p +
      ggplot2::scale_y_continuous(breaks = seq_along(ids), labels = rev(ids), limits = c(0.5, length(ids) + 0.5),
                                  expand = c(0, 0)) +
      ggplot2::theme(panel.grid.major.y = ggplot2::element_line(colour = "#f0efec", linewidth = 0.25),
                     axis.text.y = if (row_labels) ggplot2::element_text(colour = ink2, size = 6) else ggplot2::element_blank())
  }
  attr(p, "n_shown") <- dplyr::n_distinct(d$community_id)
  attr(p, "n_excluded") <- dplyr::n_distinct(all$community_id) - dplyr::n_distinct(d$community_id)
  attr(p, "n_terms") <- length(unique(d$term))
  p
}

# Save caterpillar plots in parts of at most six panels, preserving term order.
# Each part is a vector PDF and 300-dpi PNG named <file_stem>_part1, etc.
# Height is calculated separately for each part unless explicitly supplied.
# Returns pages (each with plot/files/counts), all files, and overall counts;
# plot is the first page for compatibility. NULL means no usable estimates.
save_caterpillar <- function(coefs, model, part_name, file_stem, terms = NULL, featured = NULL,
                             ncol = 3, width = 7, height = NULL, pooled = NULL, info = NULL,
                             order = "estimate", row_labels = FALSE) {
  all <- dplyr::filter(coefs, model_id == model, part == part_name)
  if (!is.null(terms)) all <- dplyr::filter(all, term %in% terms)
  usable <- dplyr::filter(all, !problem, is.finite(std.error), is.finite(estimate))
  if (!nrow(usable)) return(NULL)
  term_order <- if (is.null(terms)) unique(usable$term) else intersect(terms, usable$term)
  term_parts <- split(term_order, ceiling(seq_along(term_order) / 6L))
  # Preserve the same community rows across parts in the community-order set.
  ids <- unique(usable$community_id)
  dir.create(dirname(file_stem), showWarnings = FALSE, recursive = TRUE)
  pages <- lapply(seq_along(term_parts), function(j) {
    p <- coef_caterpillar(coefs, model, part_name, term_parts[[j]], featured, ncol,
                          pooled, order, row_labels, community_ids = ids)
    page_height <- if (is.null(height)) 0.6 + 1.9 * ceiling(attr(p, "n_terms") / ncol) else height
    files <- paste0(file_stem, "_part", j, c(".pdf", ".png"))
    ggplot2::ggsave(files[1], p, width = width, height = page_height)
    ggplot2::ggsave(files[2], p, width = width, height = page_height, dpi = 300, bg = "white")
    list(plot = p, files = files, terms = term_parts[[j]], n_shown = attr(p, "n_shown"))
  })
  i <- if (is.null(info)) NULL else dplyr::filter(info, model_id == model, part == part_name)
  list(plot = pages[[1]]$plot, pages = pages,
       files = unlist(lapply(pages, `[[`, "files"), use.names = FALSE),
       n_shown = dplyr::n_distinct(usable$community_id),
       n_fit = if (is.null(i)) NA_integer_ else sum(!i$status %in% c("too small", "too few events")),
       n_skipped = if (is.null(i)) NA_integer_ else sum(i$status %in% c("too small", "too few events")))
}

# Featured-community labels: a named vector (names = community ids, values =
# labels). Accepts ids only (labelled "A", "B", ... in order) or a named vector.
featured_labels <- function(featured) {
  if (length(featured) == 0) return(NULL)
  if (is.null(names(featured))) return(setNames(LETTERS[seq_along(featured)], as.character(featured)))
  setNames(as.character(featured), names(featured))
}

comparison_plot <- function(meta, integrated, labels) {
  d <- dplyr::bind_rows(
    meta |> dplyr::transmute(model_id, term, estimate, lo = ci.lb, hi = ci.ub, method = "Two-step (pooled)"),
    integrated |> dplyr::transmute(model_id, term, estimate, lo = estimate - 1.96 * std.error,
                                   hi = estimate + 1.96 * std.error, method = "Integrated")) |>
    dplyr::semi_join(integrated, by = c("model_id", "term")) |>
    dplyr::semi_join(meta, by = c("model_id", "term")) |>
    dplyr::mutate(model = labels[model_id], term = clean_term(term, 40))
  ggplot2::ggplot(d, ggplot2::aes(x = estimate, y = term, colour = method)) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
    ggplot2::geom_pointrange(ggplot2::aes(xmin = lo, xmax = hi), position = ggplot2::position_dodge(width = 0.5), size = 0.3) +
    ggplot2::facet_wrap(~ model, scales = "free_x") +
    ggplot2::scale_colour_manual(values = c("Two-step (pooled)" = "#2a6db0", "Integrated" = "#dd8452")) +
    ggplot2::labs(x = "Estimate (log-odds) with 95% CI", y = NULL, colour = NULL) +
    ggplot2::theme_minimal(base_size = 10) + ggplot2::theme(legend.position = "bottom")
}
