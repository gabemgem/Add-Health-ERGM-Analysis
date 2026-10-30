# =============================================================================
# disclosure.R
# Purpose:  Small-cell rules for every aggregate figure exported from the
#           analysis machine. A community's figures for one variable form a
#           row of a cross-tabulation (community x categories of the variable)
#           or a quantity (mean, SD). The rules:
#             1. No row or column may have all of its cases in a single cell.
#             2. No row or column total may be fewer than MIN_CELL (10).
#             3. No cell may have fewer than MIN_CELL cases (1 to 9). Empty
#                (0) cells are allowed, except as rule 1 forbids; set
#                ZERO_CELLS_ALLOWED <- FALSE to treat 0 as fewer than 10 too.
#             4. No quantity (mean, SD, index) may be based on fewer than
#                MIN_CELL cases.
#           A row that breaks a rule is blanked as a whole: every column
#           derived from it is replaced by SUPPRESSED.
# Used by:  02_descriptives.R
# Date:     2026-10-21
# =============================================================================

MIN_CELL <- 10
ZERO_CELLS_ALLOWED <- TRUE
SUPPRESSED <- "§"   # section sign; explain it in a note ("too few cases to report")

# ---- Building blocks -------------------------------------------------------------
# Each returns a "group": the columns derived from one row of one table, the
# category counts behind them (for the column checks), and whether the row
# passes. Values are always computed; suppression is applied when writing.

# Rows pass rules 1-3: total >= MIN_CELL, cases in at least two cells, and no
# cell with 1 to MIN_CELL - 1 cases (or any cell below MIN_CELL if zero cells
# are not allowed).
row_passes <- function(counts) {
  n <- sum(counts)
  small <- if (ZERO_CELLS_ALLOWED) counts > 0 & counts < MIN_CELL else counts < MIN_CELL
  n >= MIN_CELL && sum(counts > 0) >= 2 && !any(small)
}

# Binary variable (TRUE/1 = "yes"): share of yes among non-missing values.
disc_binary <- function(col, x) {
  x <- as.logical(x)
  counts <- c(yes = sum(x, na.rm = TRUE), no = sum(!x, na.rm = TRUE))
  list(values = setNames(if (sum(counts)) counts[["yes"]] / sum(counts) else NA_real_, col),
       counts = counts, ok = row_passes(counts))
}

# Categorical variable: share in each level among non-missing values.
# `levels` is named: names = output columns, values = category labels.
# `extra` adds quantities computed from the shares (e.g. a heterogeneity
# index); they are suppressed with the row.
disc_categorical <- function(levels, x, extra = list()) {
  x <- x[!is.na(x)]
  counts <- vapply(levels, function(l) sum(x == l), numeric(1))
  names(counts) <- levels
  p <- if (length(x)) counts / length(x) else rep(NA_real_, length(levels))
  vals <- c(setNames(as.numeric(p), names(levels)),
            vapply(extra, function(f) if (length(x)) f(as.numeric(p)) else NA_real_, numeric(1)))
  list(values = vals, counts = counts, ok = row_passes(counts))
}

# Quantities (mean, SD, ...) of a numeric variable: rule 4 only.
# `fns` is named: names = output columns.
disc_quantity <- function(fns, x) {
  x <- x[!is.na(x)]
  vals <- vapply(fns, function(f) if (length(x)) f(x) else NA_real_, numeric(1))
  list(values = vals, counts = c(n = length(x)), ok = length(x) >= MIN_CELL, quantity = TRUE)
}

# ---- Applying the rules -------------------------------------------------------------
# `groups`: named list of groups for one community (or one community and
# wave: pass `wave`, and the groups then apply only to that wave's row).
# Returns a one-row tibble of values (numeric) plus the long table of counts
# and pass/fail per group.
disc_collect <- function(community_id, groups, wave = NA) {
  vals <- tibble::as_tibble(as.list(unlist(unname(lapply(groups, `[[`, "values")))))
  long <- dplyr::bind_rows(lapply(names(groups), function(g) tibble::tibble(
    community_id = as.character(community_id), wave = wave, group = g, columns = list(names(groups[[g]]$values)),
    category = names(groups[[g]]$counts), count = unname(groups[[g]]$counts),
    quantity = isTRUE(groups[[g]]$quantity), ok = groups[[g]]$ok)))
  list(values = dplyr::mutate(vals, community_id = community_id, .before = 1), cells = long)
}

# Column checks across communities, applied after the row checks. For each
# table (group), only communities whose row is shown count. A category whose
# shown cases are all in one community, or total 1 to MIN_CELL - 1, breaks
# rules 1-2; following the row rule, the community rows holding its cases are
# blanked. Repeats until nothing changes. Quantities have no columns to check.
disc_column_checks <- function(cells) {
  repeat {
    shown <- dplyr::filter(cells, ok, !quantity)
    bad <- shown |>
      dplyr::group_by(group, category) |>
      dplyr::summarise(total = sum(count), rows = sum(count > 0), .groups = "drop") |>
      dplyr::filter(total > 0, total < MIN_CELL | rows == 1)
    if (nrow(bad) == 0) break
    hit <- dplyr::semi_join(shown, bad, by = c("group", "category")) |> dplyr::filter(count > 0) |>
      dplyr::distinct(community_id, group)
    cells <- cells |>
      dplyr::left_join(dplyr::mutate(hit, blank = TRUE), by = c("community_id", "group")) |>
      dplyr::mutate(ok = ok & is.na(blank)) |> dplyr::select(-blank)
  }
  cells
}

# Replace suppressed values with SUPPRESSED. `values` has one row per
# community (or community x wave; all rows of a community share its groups);
# `cells` comes from disc_column_checks(). Every value column becomes
# character so the symbol can sit beside numbers in a CSV.
disc_apply <- function(values, cells) {
  blanked <- cells |> dplyr::filter(!ok) |> dplyr::distinct(community_id, wave, group, columns)
  out <- values |> dplyr::mutate(dplyr::across(where(is.numeric) & !dplyr::any_of(c("community_id", "wave")), ~ format_value(.x)))
  for (i in seq_len(nrow(blanked))) {
    rows <- as.character(out$community_id) == blanked$community_id[i]
    if (!is.na(blanked$wave[i]) && "wave" %in% names(out)) rows <- rows & out$wave == blanked$wave[i]
    for (col in intersect(blanked$columns[[i]], names(out))) out[[col]][rows] <- SUPPRESSED
  }
  out
}

# Columns are checked per table, so wave-specific groups need distinct names
# (e.g. "isolates_w1", "isolates_w2"); disc_collect() takes care of rows.

# Full precision text for numbers; NA stays empty.
format_value <- function(x) ifelse(is.na(x), NA_character_, format(x, digits = 15, trim = TRUE, scientific = FALSE))

# A count of people (k of n) for free-text logs: shown only when both k and
# n - k are at least MIN_CELL, otherwise SUPPRESSED.
disc_count_text <- function(k, n) {
  if (k >= MIN_CELL && (n - k) >= MIN_CELL) as.character(k) else SUPPRESSED
}
