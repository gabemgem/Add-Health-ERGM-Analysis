# Paper tables from aggregate coefficient CSVs; no models are fitted.
# Requires the CSV readers in model_pipeline.R and result_index/read_result
# in report_helpers.R. RTF opens as an editable table in Microsoft Word.

paper_term_label <- function(term) {
  coefficient_label(term)
}

paper_number <- function(x, digits = 2) {
  # Avoid displaying negative zero after rounding.
  x[is.finite(x) & abs(x) < 0.5 * 10^(-digits)] <- 0
  formatC(x, format = "f", digits = digits)
}

paper_cell <- function(row, fit, digits = 2) {
  if (!is.null(fit) && nrow(fit) &&
      (is.na(fit$status[1]) || fit$status[1] != "fit")) return("n/e")
  if (!nrow(row)) return("n/e")
  r <- row[1, ]
  if (is.na(r$problem) || r$problem || !is.finite(r$estimate) ||
      !is.finite(r$std.error) || r$std.error <= 0) return("n/e")
  stars <- if (!is.finite(r$p.value)) "" else if (r$p.value < .001) "***" else
    if (r$p.value < .01) "**" else if (r$p.value < .05) "*" else if (r$p.value < .1) "+" else ""
  ci <- r$estimate + c(-1, 1) * stats::qnorm(.975) * r$std.error
  paste0(paper_number(r$estimate, digits), stars, "\n[",
         paper_number(ci[1], digits), ", ", paper_number(ci[2], digits), "]")
}

paper_spec_terms <- function(spec, part) {
  # Standard terms used by this project's configs. Other ERGM terms may
  # expand into multiple statistics; their names come from coefficient CSVs.
  keys <- unlist(lapply(split_terms(spec), function(term) {
    call <- str2lang(term)
    if (is.symbol(call)) return(as.character(call))
    fn <- as.character(call[[1]])
    if (fn %in% c("nodeicov", "nodeocov", "nodematch", "edgecov"))
      return(paste(fn, as.character(call[[2]]), sep = "."))
    if (fn %in% c("gwidegree", "gwodegree"))
      return(paste(if (fn == "gwidegree") "gwideg" else "gwodeg", "fixed", call[[2]], sep = "."))
    if (fn %in% c("gwesp", "gwdsp")) {
      args <- as.list(call)
      type <- if ("type" %in% names(args)) args$type else "OTP"
      return(paste(fn, type, "fixed", call[[2]], sep = "."))
    }
    character(0)
  }), use.names = FALSE)
  if (part == "ergm") keys else paste0(if (part == "formation") "Form(1)~" else "Persist(1)~", keys)
}

build_paper_table <- function(coefs, info, defs, cid, part, digits = 2) {
  # The term union spans all communities so dropped terms remain visible.
  cc <- coefs[coefs$model_id %in% defs$model_id & coefs$part == part, ]
  cc <- cc[order(match(cc$model_id, defs$model_id)), ]
  if (anyDuplicated(cc[c("model_id", "community_id", "part", "term")]))
    stop("Duplicate coefficient rows in paper table inputs.")
  field <- switch(part, ergm = "terms", formation = "formation_terms", dissolution = "dissolution_terms")
  expected <- setNames(lapply(seq_len(nrow(defs)), function(i)
    unique(c(paper_spec_terms(defs[[field]][i], part), cc$term[cc$model_id == defs$model_id[i]]))), defs$model_id)
  terms <- unique(unlist(expected, use.names = FALSE))
  if (!length(terms)) stop("No coefficient rows for selected models: ", part)
  ci <- if (is.null(info)) NULL else info[info$community_id == cid & info$part == part, ]
  table <- data.frame(Term = paper_term_label(terms), check.names = FALSE)
  for (i in seq_len(nrow(defs))) {
    id <- defs$model_id[i]
    fit <- if (is.null(ci)) NULL else ci[ci$model_id == id, ]
    model_terms <- expected[[id]]
    table[[defs$paper_name[i]]] <- vapply(terms, function(tm) {
      if (!tm %in% model_terms) return("\u2014")
      row <- cc[cc$model_id == id & cc$community_id == cid & cc$term == tm, ]
      paper_cell(row, fit, digits)
    }, character(1))
  }
  # Status refers to the whole fit; term-level failures may exist in a fit
  # whose saved status is "fit". Report those explicitly as well.
  status <- vapply(defs$model_id, function(id) {
    fit <- if (is.null(ci)) NULL else ci[ci$model_id == id, ]
    rows <- cc[cc$model_id == id & cc$community_id == cid, ]
    if (!is.null(fit) && nrow(fit) && !is.na(fit$status[1]) && fit$status[1] != "fit")
      return(as.character(fit$status[1]))
    if (!nrow(rows)) return("No results")
    if (any(is.na(rows$problem) | rows$problem | !is.finite(rows$std.error) |
            rows$std.error <= 0 | !is.finite(rows$estimate))) return("Some terms not estimable")
    if (any(table[[defs$paper_name[match(id, defs$model_id)]]] == "n/e"))
      return("Some terms unavailable")
    "Usable"
  }, character(1))
  footer <- data.frame(Term = "Estimation status", check.names = FALSE)
  for (i in seq_len(nrow(defs))) footer[[defs$paper_name[i]]] <- status[i]
  list(data = rbind(table, footer), n_terms = length(terms))
}

paper_table_notes <- function(defs, part) {
  c("Entries are log-odds coefficients with 95% Wald confidence intervals in brackets. Intervals use estimate +/- 1.959964 times its standard error.",
    "+ p < .10; * p < .05; ** p < .01; *** p < .001 (two-sided, unadjusted).",
    "An em dash indicates a term not included in the model. n/e indicates an unavailable or unusable estimate; no confidence interval or stars are shown for these estimates. Dropped terms and estimation problems should be checked against the report diagnostics.",
    if (part == "dissolution") "Persistence coefficients: positive values indicate greater persistence of existing ties, not greater dissolution.",
    paste(paste0(defs$paper_name, ": ", defs$desc), collapse = "; "))
}

paper_html <- function(table, title, notes, n_terms) {
  esc <- function(x) as.character(htmltools::htmlEscape(x))
  rows <- vapply(seq_len(nrow(table)), function(i) {
    values <- vapply(table[i, ], function(x) gsub("\n", "<br>", esc(x), fixed = TRUE), character(1))
    paste0('<tr', if (i == n_terms + 1L) ' class="footer"' else '', '>',
           '<th scope="row">', values[1], '</th>',
           paste0('<td>', values[-1], '</td>', collapse = ''), '</tr>')
  }, character(1))
  paste0('<!DOCTYPE html><html lang="en"><head><meta charset="utf-8"><title>', esc(title),
    '</title><style>body{font-family:"Times New Roman",serif;color:#000;margin:32px;max-width:1000px}',
    'table{border-collapse:collapse;width:100%;font-size:11pt;table-layout:fixed}',
    'caption{text-align:left;font-weight:bold;font-size:12pt;margin-bottom:12px}',
    'th,td{padding:7px 9px;vertical-align:middle;border:1px solid #d9d9d9}',
    'thead th{background:#eee}th:first-child{width:32%;text-align:left}',
    'tbody th{font-weight:normal}td{text-align:center}.footer{font-size:10pt;background:#f5f5f5}',
    '.notes{font-size:10pt;line-height:1.25}thead{display:table-header-group}',
    'tr{break-inside:avoid}@media print{body{margin:0}table{font-size:10pt}}</style></head><body>',
    '<table><caption>', esc(title), '</caption><thead><tr>',
    paste0('<th scope="col">', esc(names(table)), '</th>', collapse = ''),
    '</tr></thead><tbody>', paste(rows, collapse = ''), '</tbody></table>',
    '<div class="notes">', paste0('<p>', esc(notes), '</p>', collapse = ''), '</div></body></html>')
}

# ASCII RTF with explicit Unicode escapes; handles braces and backslashes in
# configurable titles/labels, plus supplementary characters via UTF-16 pairs.
paper_rtf_escape <- function(x) {
  codes <- utf8ToInt(enc2utf8(x))
  paste0(vapply(codes, function(k) {
    if (k == 10L) return("\\line ")
    if (k %in% c(92L, 123L, 125L)) return(paste0("\\", intToUtf8(k)))
    if (k >= 32L && k <= 126L) return(intToUtf8(k))
    units <- if (k <= 65535L) k else {
      v <- k - 65536L; c(55296L + v %/% 1024L, 56320L + v %% 1024L)
    }
    paste0("\\u", ifelse(units > 32767L, units - 65536L, units), "?", collapse = "")
  }, character(1)), collapse = "")
}

paper_rtf <- function(table, title, notes, n_terms) {
  n <- ncol(table)
  # 6.5-inch portrait text area; switch to landscape for longer sequences.
  landscape <- n > 4L
  width <- if (landscape) 12960L else 9360L
  ends <- c(3300L, round(seq(3300L, width, length.out = n)[-1]))
  row <- function(values, header = FALSE, footer = FALSE) {
    borders <- "\\clbrdrt\\brdrs\\brdrw5\\brdrcf1\\clbrdrb\\brdrs\\brdrw5\\brdrcf1\\clbrdrl\\brdrs\\brdrw5\\brdrcf1\\clbrdrr\\brdrs\\brdrw5\\brdrcf1"
    paste0("\\trowd\\trgaph100\\trleft0\\trkeep", if (header) "\\trhdr" else "",
      paste0(borders, "\\clvertalc", if (header) "\\clcbpat2" else "", "\\cellx", ends, collapse = ""),
      paste0(vapply(seq_along(values), function(j) paste0("\\pard\\intbl",
        if (j == 1L) "\\ql" else "\\qc", "\\sa80\\sb80\\fs", if (footer) "18 " else "20 ",
        if (header) "\\b " else "\\b0 ", paper_rtf_escape(values[j]), "\\cell "), character(1)), collapse = ""),
      "\\row\n")
  }
  paste0("{\\rtf1\\ansi\\deff0\\uc1{\\fonttbl{\\f0 Times New Roman;}}",
    "{\\colortbl;\\red217\\green217\\blue217;\\red238\\green238\\blue238;}",
    if (landscape) "\\landscape\\paperw15840\\paperh12240" else "\\paperw12240\\paperh15840",
    "\\margl1440\\margr1440\\margt1080\\margb1080\\f0\n",
    "\\pard\\keepn\\b\\fs24 ", paper_rtf_escape(title), "\\b0\\par\n",
    row(names(table), header = TRUE),
    paste0(vapply(seq_len(nrow(table)), function(i)
      row(as.character(unlist(table[i, ], use.names = FALSE)), footer = i > n_terms), character(1)), collapse = ""),
    paste0(vapply(notes, function(note) paste0("\\pard\\ql\\fs18\\sa100 ",
      paper_rtf_escape(note), "\\par\n"), character(1)), collapse = ""), "}")
}

export_paper_tables <- function(results_root, out_dir = "tables_paper",
                                communities = c("406" = "A", "408" = "B", "428" = "C", "477" = "D"),
                                config_dir = "config", digits = 2) {
  if (!length(communities) || anyNA(communities) || any(!nzchar(communities)))
    stop("Provide nonempty community IDs or a named ID-to-label vector.")
  if (is.null(names(communities))) names(communities) <- communities
  if (any(!nzchar(names(communities))) || anyDuplicated(names(communities)))
    stop("Community IDs must be nonempty and unique.")
  if (length(digits) != 1L || !is.finite(digits) || digits < 0 || digits > 6 || digits != as.integer(digits))
    stop("digits must be an integer from 0 to 6.")
  idx <- result_index(results_root)
  manifest <- list()
  for (type in c("ergm", "stergm")) {
    defs <- if (type == "ergm") read_ergm_models(file.path(config_dir, "ergm_model_desc.csv")) else
      read_stergm_models(file.path(config_dir, "stergm_model_desc.csv"))
    # Paper inclusion is independent of show_in_report.
    defs <- defs[nzchar(defs$paper_name), ]
    if (!nrow(defs)) { message("No ", toupper(type), " paper_name entries; paper tables skipped."); next }
    if (anyDuplicated(defs$paper_name)) stop("Duplicate paper_name entries in ", type, " configuration.")
    coefs <- read_result(idx, paste0(type, "_coefs.csv"))
    info <- read_result(idx, paste0(type, "_fit_info.csv"))
    if (is.null(coefs)) stop("Missing ", type, " coefficient CSV under ", results_root)
    run <- read_result(idx, paste0(type, "_models_run.csv"))
    fields <- if (type == "ergm") "terms" else c("formation_terms", "dissolution_terms")
    if (!is.null(run)) for (id in intersect(defs$model_id, run$model_id)) {
      a <- defs[defs$model_id == id, ]; b <- run[run$model_id == id, ][1, ]
      for (field in fields) if (!identical(trimws(as.character(a[[field]])), trimws(as.character(b[[field]]))))
        stop("Saved results differ from the configured ", field, " for ", id, "; refit before exporting paper tables.")
    }
    parts <- if (type == "ergm") "ergm" else c("formation", "dissolution")
    for (part in parts) {
      pd <- if (type == "ergm") defs else defs[defs$part %in% c(part, "both"), ]
      if (!nrow(pd)) next
      for (cid in names(communities)) {
        title <- paste0("Community ", communities[[cid]], ": ", switch(part,
          ergm = "Wave I friendship ERGMs", formation = "Friendship formation STERGMs",
          dissolution = "Friendship persistence STERGMs"))
        tab <- build_paper_table(coefs, info, pd, cid, part, digits)
        notes <- paper_table_notes(pd, part)
        dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
        stem <- paste0(type, "_", part, "_community_", gsub("[^A-Za-z0-9_-]", "_", cid))
        for (format in c("html", "rtf", "csv")) {
          file <- file.path(out_dir, paste0(stem, ".", format))
          if (format == "csv") readr::write_csv(tab$data, file, na = "") else
            writeLines(if (format == "html") paper_html(tab$data, title, notes, tab$n_terms) else
              paper_rtf(tab$data, title, notes, tab$n_terms), file, useBytes = TRUE)
          manifest[[length(manifest) + 1L]] <- data.frame(type = type, part = part,
            community_id = cid, community_label = communities[[cid]],
            models = paste(pd$model_id, collapse = "; "), format = format, file = file)
        }
      }
    }
  }
  if (!length(manifest)) return(tibble::tibble())
  manifest <- dplyr::bind_rows(manifest)
  readr::write_csv(manifest, file.path(out_dir, "paper_tables_manifest.csv"))
  message("Wrote ", nrow(manifest), " paper table files to ", out_dir)
  invisible(manifest)
}
