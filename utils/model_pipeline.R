# =============================================================================
# model_pipeline.R
# Purpose:  CSV-driven ERGM / STERGM pipeline.
#             - read model definitions (config/ergm_model_desc.csv, config/stergm_model_desc.csv)
#             - fit every model to every community, one background R process
#               per community fit (callr) so a hung fit can be killed after
#               a hard time limit and several communities can run in parallel
#             - cache per-community results; re-running only fits communities
#               whose definition, data or settings changed
#             - write everything the results report needs as CSV/text
#           Fits run in a separate process, so this file must stay
#           self-contained: the worker sources only this file.
# Used by:  03_run_ergm_models.R, 04_run_stergm_models.R
# Date:     2026-10-19
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(readr)
  library(network)
})

# =============================================================================
# Model definitions
# =============================================================================

flag_value <- function(x, default) {
  x <- toupper(trimws(as.character(x)))
  ifelse(is.na(x) | x == "", default, x %in% c("TRUE", "T", "YES", "Y", "1"))
}

blank <- function(x) is.na(x) | !nzchar(trimws(x))

# Model definition files live in config/ next to the scripts:
#   config/ergm_model_desc.csv   (read_ergm_models)
#   config/stergm_model_desc.csv (read_stergm_models)
# Row order sets the column order in the report. The id column may be named
# model_id or model.
read_model_csv <- function(path) {
  if (!file.exists(path)) stop("Model definition file not found: ", path, call. = FALSE)
  d <- readr::read_csv(path, col_types = readr::cols(.default = "c"), na = c("", "NA"), comment = "#")
  if (!"model_id" %in% names(d) && "model" %in% names(d)) d <- dplyr::rename(d, model_id = model)
  check_model_ids(d, path)
  d$model_id <- trimws(d$model_id)
  d
}

# ERGM definitions: model_id, label, desc, terms, min_network_size,
# moderators, run_integrated, integrated_edges_lm, show_in_report.
read_ergm_models <- function(path) {
  d <- read_model_csv(path)
  if (!"terms" %in% names(d)) stop(path, " needs a 'terms' column.", call. = FALSE)
  for (col in c("label", "paper_name", "desc", "min_network_size", "moderators", "run_integrated",
                "integrated_edges_lm", "show_in_report"))
    if (!col %in% names(d)) d[[col]] <- NA_character_
  d |> mutate(
    label = coalesce(label, model_id), paper_name = trimws(coalesce(paper_name, "")), desc = coalesce(desc, ""),
    min_network_size = as.integer(min_network_size),
    moderators = coalesce(moderators, ""),
    run_integrated = flag_value(run_integrated, FALSE),
    integrated_edges_lm = coalesce(integrated_edges_lm, "~ log(n)"),
    show_in_report = flag_value(show_in_report, TRUE)
  )
}

# STERGM definitions: model_id, label, desc, formation_terms,
# dissolution_terms, min_network_size, moderators, show_in_report.
# A blank formation_terms or dissolution_terms means that sub-model is not
# part of this model: it is fit as edges-only (CMLE fits the two sub-models
# independently, so this does not affect the other one) and not reported.
read_stergm_models <- function(path) {
  d <- read_model_csv(path)
  for (col in c("formation_terms", "dissolution_terms"))
    if (!col %in% names(d)) stop(path, " needs a '", col, "' column.", call. = FALSE)
  for (col in c("label", "paper_name", "desc", "min_network_size", "moderators", "show_in_report"))
    if (!col %in% names(d)) d[[col]] <- NA_character_
  d |> mutate(
    label = coalesce(label, model_id), paper_name = trimws(coalesce(paper_name, "")), desc = coalesce(desc, ""),
    min_network_size = as.integer(min_network_size),
    moderators = coalesce(moderators, ""),
    show_in_report = flag_value(show_in_report, TRUE),
    part = case_when(!blank(formation_terms) & !blank(dissolution_terms) ~ "both",
                     !blank(formation_terms) ~ "formation",
                     !blank(dissolution_terms) ~ "dissolution",
                     TRUE ~ NA_character_)
  )
}

check_model_ids <- function(d, path) {
  if (!"model_id" %in% names(d)) stop(path, " needs a 'model_id' (or 'model') column.", call. = FALSE)
  d$model_id <- trimws(d$model_id)
  bad <- d$model_id[!grepl("^[A-Za-z0-9_.-]+$", d$model_id)]
  if (length(bad)) stop("Invalid model_id(s) in ", path, ": ", paste(bad, collapse = ", "), call. = FALSE)
  dup <- d$model_id[duplicated(d$model_id)]
  if (length(dup)) stop("Duplicate model_id(s) in ", path, ": ", paste(dup, collapse = ", "), call. = FALSE)
  invisible(TRUE)
}

# =============================================================================
# Term handling
# =============================================================================

# "edges + mutual + nodematch('gender')" -> c("edges", "mutual", "nodematch(\"gender\")")
split_terms <- function(terms) {
  if (blank(terms)) return(character(0))
  f <- stats::as.formula(paste("~", terms))
  attr(stats::terms(f, keep.order = TRUE), "term.labels")
}

vertex_attr_terms <- c("nodecov", "nodeicov", "nodeocov", "nodematch", "nodemix", "nodefactor",
                       "nodeifactor", "nodeofactor", "absdiff", "absdiffcat", "diff", "smallerthan",
                       "greaterthan", "sociality", "receiver", "sender")
network_attr_terms <- c("edgecov", "dyadcov")

# Attribute referenced by one term label, or NULL.
term_attribute <- function(label) {
  call <- tryCatch(str2lang(label), error = function(e) NULL)
  if (!is.call(call)) return(NULL)
  fn <- as.character(call[[1]])
  if (!fn %in% c(vertex_attr_terms, network_attr_terms)) return(NULL)
  args <- as.list(call)[-1]
  if (length(args) == 0) return(NULL)
  nm <- names(args); if (is.null(nm)) nm <- rep("", length(args))
  a <- if ("attr" %in% nm) args[["attr"]] else if ("x" %in% nm) args[["x"]] else args[[which(nm == "")[1]]]
  if (!is.character(a) || length(a) != 1) return(NULL)
  list(fn = fn, attr = a, kind = if (fn %in% network_attr_terms) "network" else "vertex")
}

# Counts of people written to exported logs (e.g. how many values were
# imputed) follow the small-cell rules in utils/disclosure.R: shown only when
# both k and n - k are at least 10, otherwise the suppression symbol. Kept
# here as well because worker processes source only this file.
disclosed_count <- function(k, n, min_cell = 10) {
  if (k >= min_cell && (n - k) >= min_cell) as.character(k) else "§"
}

# Impute missing values of the vertex attributes a model uses (ergm cannot
# handle NA). Rules follow impute_network_covariates() in model_helpers.R:
# 0/1 attributes -> 0, other numeric -> community mean, other -> mode.
impute_vertex_attrs <- function(net, attrs) {
  log <- character(0)
  for (a in attrs) {
    if (!a %in% network::list.vertex.attributes(net)) next
    v <- network::get.vertex.attribute(net, a)
    v <- if (is.numeric(v) || is.logical(v)) as.numeric(v) else as.character(v)
    miss <- is.na(v)
    if (any(miss) && !all(miss)) {
      if (is.numeric(v) && all(v[!miss] %in% c(0, 1))) {
        v[miss] <- 0; how <- "zero"
      } else if (is.numeric(v)) {
        v[miss] <- mean(v, na.rm = TRUE); how <- "mean"
      } else {
        v[miss] <- names(sort(table(v), decreasing = TRUE))[1]; how <- "mode"
      }
      log <- c(log, sprintf("%s: %s (%s)", a, disclosed_count(sum(miss), length(v)), how))
    }
    network::set.vertex.attribute(net, a, v)
  }
  list(net = net, log = paste(log, collapse = "; "))
}

# =============================================================================
# Shared-club dyadic covariate
# =============================================================================

# In-school clubs, organizations and teams (S44A1-S44A33), kept in the nodal
# covariates file by 01_data_prep.R.
club_cols <- paste0("S44A", 1:33)

# Network attribute name for each element of build_shared_club_edgecov(), so
# model formulas can say edgecov('shared_club'), edgecov('shared_club_count')
# or edgecov('shared_club_jaccard'). edgecov('shared_club') is the analysis
# machine's edgecov(sc$any, "shared_club").
shared_club_attrs <- c(any = "shared_club", count = "shared_club_count", jaccard = "shared_club_jaccard")

# Shared-club matrices for `net`, rows/columns in vertex order:
#   any     - 1 if i and j share at least one club, else 0
#   count   - number of clubs i and j share
#   jaccard - shared clubs / clubs either belongs to (0 if neither is in a club)
# Missing club items count as "not a member". Prints how many vertices have no
# club data at all; that line is also returned as `missing_msg`.
# `source` names the covariates in error messages (e.g. the file path).
build_shared_club_edgecov <- function(net, covars, cols = club_cols, source = "the nodal covariates") {
  aids <- network::get.vertex.attribute(net, "vertex.names")
  wanted <- cols
  cols <- intersect(cols, names(covars))
  if (length(cols) == 0) {
    similar <- grep("S44|club", names(covars), ignore.case = TRUE, value = TRUE)
    stop("No club columns (", wanted[1], " ... ", wanted[length(wanted)], ") in ", source, ". ",
         "It has ", ncol(covars), " columns; ",
         if (length(similar)) paste0("club-like columns: ", paste(head(similar, 10), collapse = ", "), ". ")
         else "none look like club items. ",
         "Re-run 01_data_prep.R, which keeps S44A1-S44A33 in the covariate files, and check that ",
         "this file was written by it (see its modification time).", call. = FALSE)
  }
  df <- covars[match(as.character(aids), as.character(covars$AID)), cols, drop = FALSE]
  n_missing <- sum(rowSums(!is.na(df)) == 0)
  missing_msg <- sprintf("shared_club: %s vertices missing club data", disclosed_count(n_missing, length(aids)))
  message(missing_msg)
  P <- as.matrix(df)
  P[is.na(P)] <- 0
  count <- tcrossprod(P)
  n_clubs <- diag(count)
  union <- outer(n_clubs, n_clubs, "+") - count
  jaccard <- ifelse(union > 0, count / union, 0)
  out <- list(any = (count > 0) * 1, count = count, jaccard = jaccard)
  out <- lapply(out, function(m) { diag(m) <- 0; dimnames(m) <- NULL; m })
  c(out, missing_msg = missing_msg)
}

# If `terms` use a shared-club edgecov, build it from the community's nodal
# covariates and set it on each network in `nets` (same vertex set).
# Returns list(nets, log).
attach_shared_club_terms <- function(nets, terms, covars_path) {
  attrs <- unlist(lapply(terms, function(t) { a <- term_attribute(t); if (!is.null(a) && a$kind == "network") a$attr }))
  if (!any(attrs %in% shared_club_attrs)) return(list(nets = nets, log = ""))
  if (!file.exists(covars_path)) stop("Nodal covariates file not found: ", covars_path, call. = FALSE)
  sc <- build_shared_club_edgecov(nets[[1]], readRDS(covars_path),
                                  source = sprintf("%s (modified %s)", covars_path,
                                                   format(file.mtime(covars_path), "%Y-%m-%d %H:%M")))
  nets <- lapply(nets, function(net) {
    for (k in names(shared_club_attrs)) network::set.network.attribute(net, shared_club_attrs[[k]], sc[[k]])
    net
  })
  list(nets = nets, log = sc$missing_msg)
}

# TRUE if any of the term strings (formula right-hand sides) uses a shared-club edgecov.
uses_shared_club <- function(terms) {
  attrs <- unlist(lapply(unlist(lapply(terms, split_terms)), function(t) term_attribute(t)$attr))
  any(attrs %in% shared_club_attrs)
}

covars_file <- function(data_dir, cid) file.path(data_dir, paste0("community_", cid, "_nodal_covariates.rds"))

# Why a vertex attribute cannot be used in this community, or NA if it can.
# Mirrors attr_variation_check() in model_helpers.R.
attr_problem <- function(net, a, min_minority = 5) {
  if (!a %in% network::list.vertex.attributes(net)) return("not on network")
  v <- network::get.vertex.attribute(net, a)
  v <- v[!is.na(v)]
  if (length(unique(v)) < 2) return("no variation")
  tbl <- table(v)
  if (length(tbl) == 2 && min(tbl) < min_minority) return(sprintf("minority n = %d < %d", min(tbl), min_minority))
  NA_character_
}

# Prepare one network for a set of terms: impute, check attributes, and (if
# drop_invalid) remove terms whose attribute cannot be used here.
# Returns list(net, terms, dropped, imputed).
prepare_terms <- function(net, terms, settings) {
  attrs <- lapply(terms, term_attribute)
  v_attrs <- unique(unlist(lapply(attrs, function(x) if (!is.null(x) && x$kind == "vertex") x$attr)))
  imp <- impute_vertex_attrs(net, v_attrs)
  net <- imp$net
  keep <- rep(TRUE, length(terms))
  dropped <- character(0)
  for (i in seq_along(terms)) {
    a <- attrs[[i]]
    if (is.null(a)) next
    problem <- if (a$kind == "network") {
      if (is.null(network::get.network.attribute(net, a$attr))) "network attribute missing" else NA_character_
    } else attr_problem(net, a$attr, settings$min_minority)
    if (!is.na(problem)) {
      if (!isTRUE(settings$drop_invalid_terms) && a$kind == "vertex" && problem != "not on network") next
      keep[i] <- FALSE
      dropped <- c(dropped, sprintf("%s [%s]", terms[i], problem))
    }
  }
  list(net = net, terms = terms[keep], dropped = paste(dropped, collapse = "; "), imputed = imp$log)
}

# Copy vertex attributes (matched by vertex name) and missing network
# attributes from wave 1 to wave 2, so both STERGM sub-models see the same
# wave 1 covariates.
copy_w1_attributes <- function(w1, w2, v_attrs, n_attrs) {
  id1 <- network::get.vertex.attribute(w1, "vertex.names")
  id2 <- network::get.vertex.attribute(w2, "vertex.names")
  for (a in intersect(v_attrs, network::list.vertex.attributes(w1)))
    network::set.vertex.attribute(w2, a, network::get.vertex.attribute(w1, a)[match(id2, id1)])
  for (a in n_attrs) {
    if (is.null(network::get.network.attribute(w2, a)) && !is.null(network::get.network.attribute(w1, a)) &&
        identical(id1, id2))
      network::set.network.attribute(w2, a, network::get.network.attribute(w1, a))
  }
  w2
}

# =============================================================================
# Fitting (runs inside the worker process)
# =============================================================================

ergm_control <- function(settings, method = c("MCMLE", "Stochastic-Approximation")) {
  method <- match.arg(method)
  args <- list(seed = settings$seed, main.method = method)
  if (method == "MCMLE") args$MCMLE.maxit <- settings$mcmle_maxit
  # MPLE.check runs a linear-programming check that can hang indefinitely on
  # small networks; only some ergm versions have the option.
  if ("MPLE.check" %in% names(formals(ergm::control.ergm))) args$MPLE.check <- FALSE
  do.call(ergm::control.ergm, args)
}

# Fit-level problems that make every coefficient unreliable.
fit_problem <- function(fit, warnings) {
  reasons <- c(
    if (isTRUE(fit$failure)) "estimation failed",
    if (any(grepl("did not converge|not converged", warnings, ignore.case = TRUE))) "did not converge",
    if (any(grepl("did not mix|degenera", warnings, ignore.case = TRUE))) "MCMC did not mix"
  )
  if (length(reasons)) paste(reasons, collapse = "; ") else NA_character_
}

capture_fit <- function(expr) {
  warns <- character(0)
  fit <- withCallingHandlers(
    tryCatch(expr, error = function(e) structure(conditionMessage(e), class = "fit_error")),
    warning = function(w) { warns <<- c(warns, conditionMessage(w)); invokeRestart("muffleWarning") }
  )
  list(fit = if (inherits(fit, "fit_error")) NULL else fit,
       error = if (inherits(fit, "fit_error")) as.character(fit) else NA_character_,
       warnings = unique(warns[!grepl("Rglpk", warns)]))
}

# Fit with settings$method; if settings$fallback and the fit fails or does not
# converge, retry with the other method. `fit_fun(control)` performs one fit.
fit_with_fallback <- function(fit_fun, settings) {
  primary <- settings$method
  r <- capture_fit(fit_fun(ergm_control(settings, primary)))
  r$method <- primary
  bad <- is.null(r$fit) || !is.na(fit_problem(r$fit, r$warnings))
  if (bad && isTRUE(settings$fallback)) {
    other <- setdiff(c("Stochastic-Approximation", "MCMLE"), primary)
    message("Retrying with ", other)
    r2 <- capture_fit(fit_fun(ergm_control(settings, other)))
    r2$method <- other
    if (!is.null(r2$fit) && (is.null(r$fit) || is.na(fit_problem(r2$fit, r2$warnings)))) return(r2)
  }
  r
}

# Coefficient table. Term-level problems: non-finite, |estimate| or SE above
# the thresholds. Following the project's degeneracy rule, a fit (ERGM) or
# sub-model (STERGM) with any extreme term is flagged as a whole.
tidy_fit <- function(fit, settings) {
  est <- stats::coef(fit)
  v <- tryCatch(stats::vcov(fit), error = function(e) NULL)
  se <- if (is.null(v)) rep(NA_real_, length(est)) else sqrt(abs(diag(v)))[names(est)]
  z <- est / se
  tibble(term = names(est), estimate = unname(est), std.error = unname(se),
         statistic = unname(z), p.value = unname(2 * stats::pnorm(-abs(z))),
         conf.low = unname(est - 1.96 * se), conf.high = unname(est + 1.96 * se))
}

flag_problems <- function(coefs, fit_level_problem, settings) {
  coefs |>
    mutate(extreme = abs(estimate) > settings$max_abs_coef |
             (is.finite(std.error) & std.error > settings$max_se)) |>
    group_by(part) |>
    mutate(degenerate_flag = any(extreme, na.rm = TRUE)) |>
    ungroup() |>
    mutate(problem_reason = case_when(
      !is.na(fit_level_problem) ~ fit_level_problem,
      degenerate_flag ~ sprintf("degenerate (|coef| > %s or SE > %s)", settings$max_abs_coef, settings$max_se),
      !is.finite(estimate) ~ "infinite estimate",
      !is.finite(std.error) ~ "no standard error",
      TRUE ~ NA_character_),
      problem = !is.na(problem_reason)) |>
    select(-extreme)
}

# Upper-triangle coefficient correlations (cov2cor of vcov), long format.
coef_correlations <- function(fit) {
  v <- tryCatch(stats::vcov(fit), error = function(e) NULL)
  if (is.null(v) || nrow(v) < 2 || any(!is.finite(diag(v))) || any(diag(v) <= 0)) return(tibble())
  r <- suppressWarnings(stats::cov2cor(v))
  idx <- which(upper.tri(r), arr.ind = TRUE)
  tibble(term1 = rownames(r)[idx[, 1]], term2 = colnames(r)[idx[, 2]], r = r[idx])
}

# Goodness of fit as a tidy table: one row per statistic category with the
# observed value, simulated min/mean/max and MC p-value.
gof_table <- function(g) {
  parts <- grep("^summary\\.", names(g), value = TRUE)
  dplyr::bind_rows(lapply(parts, function(p) {
    m <- g[[p]]
    if (is.null(m) || NROW(m) == 0) return(NULL)
    pcol <- grep("p-?value", colnames(m), value = TRUE)[1]
    tibble(statistic = sub("^summary\\.", "", p), category = rownames(m),
           obs = m[, "obs"], min = m[, "min"], mean = m[, "mean"], max = m[, "max"],
           p.value = if (is.na(pcol)) NA_real_ else m[, pcol])
  }))
}

# Files for one community's result within a model's output directory.
community_files <- function(model_dir, cid) {
  stem <- file.path(model_dir, paste0("community_", cid))
  # GOF images are named <type-folder>_<model>_community_<cid>_gof.png so the
  # names stay unique when the report bundle is flattened into one folder.
  png_name <- sprintf("%s_%s_community_%s_gof.png", basename(dirname(model_dir)), basename(model_dir), cid)
  list(info = paste0(stem, "_info.csv"), coefs = paste0(stem, "_coefs.csv"),
       gof = paste0(stem, "_gof.csv"), corr = paste0(stem, "_corr.csv"),
       gof_png = file.path(model_dir, "figures", png_name),
       model = paste0(stem, "_model.rds"), log = file.path(model_dir, "logs", paste0("community_", cid, ".log")))
}

# Fit one model to one community and write its result files. Runs in the
# worker process. The info file is written right after the fit, before GOF,
# so a time-out during GOF still keeps the estimates.
run_community_job <- function(job) {
  s <- job$settings
  set.seed(s$seed)
  files <- community_files(job$model_dir, job$community_id)
  # Never mix a failed refit with coefficients or diagnostics from an older spec.
  stale <- unlist(files[c("coefs", "corr", "gof", "gof_png", "model")], use.names = FALSE)
  for (path in stale[file.exists(stale)]) file.remove(path)
  start <- Sys.time()
  info <- tibble(model_id = job$model_id, community_id = job$community_id, type = job$type,
                 spec_hash = job$spec_hash, status = NA_character_, method = NA_character_,
                 error = NA_character_, problem = NA_character_, warnings = "",
                 n_nodes = NA_integer_, n_arcs_w1 = NA_integer_, n_arcs_w2 = NA_integer_,
                 n_formed = NA_integer_, n_dissolved = NA_integer_,
                 terms_dropped = "", imputed = "", aic = NA_real_, bic = NA_real_,
                 seconds = NA_real_, gof = "not run")
  write_info <- function() {
    info$seconds <<- round(as.numeric(difftime(Sys.time(), start, units = "secs")), 1)
    readr::write_csv(info, files$info)
  }

  w1 <- readRDS(job$w1_path)
  info$n_nodes <- network::network.size(w1)
  info$n_arcs_w1 <- network::network.edgecount(w1)
  if (info$n_nodes < job$min_size) {
    info$status <- "too small"; write_info(); return(invisible(info))
  }

  if (job$type == "ergm") {
    sc <- attach_shared_club_terms(list(w1), split_terms(job$terms), job$covars_path)
    w1 <- sc$nets[[1]]
    prep <- prepare_terms(w1, split_terms(job$terms), s)
    info$terms_dropped <- prep$dropped
    info$imputed <- paste(c(prep$imputed, sc$log)[nzchar(c(prep$imputed, sc$log))], collapse = "; ")
    if (length(prep$terms) == 0) { info$status <- "no usable terms"; write_info(); return(invisible(info)) }
    net <- prep$net
    rhs <- paste(prep$terms, collapse = " + ")
    message("Community ", job$community_id, " (n=", info$n_nodes, "): net ~ ", rhs)
    r <- fit_with_fallback(function(control) {
      f <- stats::as.formula(paste("net ~", rhs))
      ergm::ergm(f, control = control, eval.loglik = isTRUE(s$eval_loglik))
    }, s)
    parts <- tibble(raw = "", part = "ergm")
  } else {
    suppressPackageStartupMessages(library(tergm))
    w2 <- readRDS(job$w2_path)
    info$n_arcs_w2 <- network::network.edgecount(w2)
    ev <- formation_dissolution_counts(w1, w2)
    info$n_formed <- ev$formed; info$n_dissolved <- ev$dissolved
    need_form <- !blank(job$formation_terms); need_diss <- !blank(job$dissolution_terms)
    if ((need_form && ev$formed < s$min_events) || (need_diss && ev$dissolved < s$min_events)) {
      info$status <- "too few events"
      info$error <- sprintf("formed = %s, dissolved = %s (minimum %s)", ev$formed, ev$dissolved, s$min_events)
      write_info(); return(invisible(info))
    }
    # Shared-club edgecov (built from the nodal covariates) goes on both waves.
    # Then impute/check on wave 1 and copy the (imputed) wave 1 covariates and
    # any other edgecov network attributes onto wave 2.
    all_terms <- c(split_terms(job$formation_terms), split_terms(job$dissolution_terms))
    sc <- attach_shared_club_terms(list(w1, w2), all_terms, job$covars_path)
    w1 <- sc$nets[[1]]; w2 <- sc$nets[[2]]
    a <- Filter(Negate(is.null), lapply(all_terms, term_attribute))
    pf <- prepare_terms(w1, split_terms(job$formation_terms), s); w1 <- pf$net
    pd <- prepare_terms(w1, split_terms(job$dissolution_terms), s); w1 <- pd$net
    w2 <- copy_w1_attributes(w1, w2,
                             unique(unlist(lapply(a, function(x) if (x$kind == "vertex") x$attr))),
                             unique(unlist(lapply(a, function(x) if (x$kind == "network") x$attr))))
    info$terms_dropped <- paste(c(if (nzchar(pf$dropped)) paste("formation:", pf$dropped),
                                  if (nzchar(pd$dropped)) paste("dissolution:", pd$dropped)), collapse = " | ")
    imp <- c(pf$imputed, pd$imputed, sc$log)
    info$imputed <- paste(unique(imp[nzchar(imp)]), collapse = "; ")
    form_rhs <- if (length(pf$terms)) paste(pf$terms, collapse = " + ") else "edges"
    diss_rhs <- if (length(pd$terms)) paste(pd$terms, collapse = " + ") else "edges"
    message("Community ", job$community_id, " (n=", info$n_nodes, ")\n  formation:   ~ ", form_rhs,
            "\n  dissolution: ~ ", diss_rhs)
    r <- fit_with_fallback(function(control) {
      tergm::stergm(list(w1, w2),
                    formation = stats::as.formula(paste("~", form_rhs)),
                    dissolution = stats::as.formula(paste("~", diss_rhs)),
                    estimate = "CMLE", times = c(1, 2),
                    control = tergm::control.stergm(CMLE.ergm = control))
    }, s)
    parts <- tibble(raw = c("^Form\\(", "^(Persist|Diss)\\("), part = c("formation", "dissolution"))[
      c(need_form, need_diss), ]
  }

  info$method <- if (is.null(r$fit)) NA_character_ else r$method
  info$error <- r$error
  info$warnings <- paste(r$warnings, collapse = " | ")
  if (is.null(r$fit)) { info$status <- "failed"; write_info(); return(invisible(info)) }

  fit <- r$fit
  info$problem <- fit_problem(fit, r$warnings)
  if (isTRUE(s$eval_loglik) && job$type == "ergm") {
    info$aic <- tryCatch(stats::AIC(fit), error = function(e) NA_real_)
    info$bic <- tryCatch(stats::BIC(fit), error = function(e) NA_real_)
  }
  coefs <- tidy_fit(fit, s)
  coefs <- dplyr::bind_rows(lapply(seq_len(nrow(parts)), function(i) {
    rows <- if (parts$raw[i] == "") rep(TRUE, nrow(coefs)) else grepl(parts$raw[i], coefs$term, perl = TRUE)
    mutate(coefs[rows, ], part = parts$part[i])
  }))
  coefs <- flag_problems(coefs, info$problem, s) |>
    mutate(model_id = job$model_id, community_id = job$community_id, .before = 1)
  info$status <- if (!is.na(info$problem)) "problem fit" else if (any(coefs$degenerate_flag)) "degenerate" else "fit"
  readr::write_csv(coefs, files$coefs)
  readr::write_csv(mutate(coef_correlations(fit), model_id = job$model_id, community_id = job$community_id, .before = 1),
                   files$corr)
  write_info()
  if (isTRUE(s$save_models)) saveRDS(fit, files$model)

  if (isTRUE(s$run_gof)) {
    g <- tryCatch(ergm::gof(fit, control = ergm::control.gof.ergm(nsim = s$n_gof_sim)),
                  error = function(e) { message("GOF failed: ", conditionMessage(e)); NULL })
    tbl <- if (is.null(g)) NULL else gof_table(g)
    if (!is.null(tbl) && nrow(tbl) > 0) {
      readr::write_csv(mutate(tbl, model_id = job$model_id, community_id = job$community_id, .before = 1), files$gof)
      info$gof <- "done"
      if (isTRUE(s$gof_plots)) {
        # Boxes = simulated networks from the fitted model; line = observed network.
        # plot() on a gof object draws one page per statistic, so lay the
        # panels out on one page first.
        dir.create(dirname(files$gof_png), recursive = TRUE, showWarnings = FALSE)
        k <- max(1, length(grep("^pobs\\.", names(g))))
        grDevices::png(files$gof_png, width = 1500, height = 450 * ceiling(k / 3), res = 110)
        graphics::par(mfrow = c(ceiling(k / 3), min(k, 3)), oma = c(0, 0, 2, 0))
        tryCatch({
          plot(g)
          graphics::title(sprintf("%s, community %s", job$model_id, job$community_id), outer = TRUE)
        }, error = function(e) message("GOF plot failed: ", conditionMessage(e)))
        grDevices::dev.off()
      }
    } else info$gof <- "failed"
    write_info()
  }
  invisible(info)
}

# Formation / dissolution event counts on the shared node set (directed).
formation_dissolution_counts <- function(w1, w2) {
  id1 <- network::get.vertex.attribute(w1, "vertex.names")
  id2 <- network::get.vertex.attribute(w2, "vertex.names")
  shared <- intersect(id1, id2)
  m1 <- as.matrix(w1)[match(shared, id1), match(shared, id1)]
  m2 <- as.matrix(w2)[match(shared, id2), match(shared, id2)]
  div <- if (network::is.directed(w1)) 1 else 2
  list(formed = as.integer(sum(m2 == 1 & m1 == 0) / div),
       dissolved = as.integer(sum(m2 == 0 & m1 == 1) / div))
}

# Run `expr`; if an error escapes it (a crash, not a failed fit, which
# capture_fit() already records), print the error and the call stack to the
# worker's log before re-raising it.
with_traceback <- function(expr) {
  withCallingHandlers(expr, error = function(e) {
    calls <- vapply(sys.calls(), function(cl) paste(deparse(cl, nlines = 1), collapse = ""), "")
    message("CRASH: ", conditionMessage(e), "\nCall stack (innermost last):\n",
            paste0("  ", seq_along(calls), ": ", substr(calls, 1, 200), collapse = "\n"))
  })
}

# Entry point for the worker process: loads packages and this file, then fits.
model_job_entry <- function(job) {
  source(file.path(job$utils_dir, "project_setup.R"), encoding = "UTF-8")
  suppressPackageStartupMessages({ library(network); library(ergm) })
  source(file.path(job$utils_dir, "model_pipeline.R"), encoding = "UTF-8")
  with_traceback(run_community_job(job))
  invisible(TRUE)
}

# =============================================================================
# Job scheduling (main process)
# =============================================================================

# Run jobs with up to n_workers background R processes, killing any that run
# longer than time_limit seconds. Without callr, runs them in this process
# with setTimeLimit() (which cannot interrupt ergm's compiled code).
# Returns a tibble: community_id, outcome ("finished", "timed out", "crashed"), message.
run_jobs <- function(jobs, n_workers, time_limit) {
  if (length(jobs) == 0) return(tibble(community_id = character(), outcome = character(), message = character()))
  for (j in jobs) dir.create(dirname(community_files(j$model_dir, j$community_id)$log), recursive = TRUE, showWarnings = FALSE)

  if (!requireNamespace("callr", quietly = TRUE)) {
    message("callr not installed: fitting in this R session; time limits may not stop a hung fit.")
    return(dplyr::bind_rows(lapply(jobs, function(j) {
      setTimeLimit(elapsed = time_limit, transient = TRUE)
      on.exit(setTimeLimit(elapsed = Inf), add = TRUE)
      res <- tryCatch({ run_community_job(j); "finished" },
                      error = function(e) if (grepl("time limit", conditionMessage(e))) "timed out" else conditionMessage(e))
      tibble(community_id = as.character(j$community_id),
             outcome = if (res %in% c("finished", "timed out")) res else "crashed",
             message = if (res %in% c("finished", "timed out")) NA_character_ else res)
    })))
  }

  queue <- jobs
  running <- list()
  results <- list()
  done <- 0
  while (length(queue) > 0 || length(running) > 0) {
    while (length(running) < n_workers && length(queue) > 0) {
      j <- queue[[1]]; queue <- queue[-1]
      log <- community_files(j$model_dir, j$community_id)$log
      p <- callr::r_bg(model_job_entry, args = list(job = j), libpath = .libPaths(),
                       stdout = log, stderr = "2>&1", supervise = TRUE)
      running[[length(running) + 1]] <- list(job = j, proc = p, start = Sys.time())
    }
    Sys.sleep(1)
    still <- list()
    for (r in running) {
      elapsed <- as.numeric(difftime(Sys.time(), r$start, units = "secs"))
      cid <- as.character(r$job$community_id)
      if (!r$proc$is_alive()) {
        err <- tryCatch({ r$proc$get_result(); NA_character_ }, error = function(e) {
          msg <- conditionMessage(e)
          if (!is.null(e$parent)) msg <- conditionMessage(e$parent)
          msg
        })
        results[[cid]] <- tibble(community_id = cid, outcome = if (is.na(err)) "finished" else "crashed", message = err)
      } else if (elapsed > time_limit) {
        r$proc$kill()
        results[[cid]] <- tibble(community_id = cid, outcome = "timed out", message = sprintf("killed after %d s", time_limit))
      } else {
        still[[length(still) + 1]] <- r
        next
      }
      done <- done + 1
      message(sprintf("  [%s] community %s: %s (%d/%d, %.0f s)", r$job$model_id, cid,
                      results[[cid]]$outcome, done, length(jobs), elapsed))
    }
    running <- still
  }
  dplyr::bind_rows(results)
}

# =============================================================================
# Running a model definition over all communities (main process)
# =============================================================================

# Fingerprint of everything that affects one community's result.
spec_hash <- function(def_fields, data_files, settings) {
  if (any(!file.exists(data_files))) stop("Missing model input: ", paste(data_files[!file.exists(data_files)], collapse = ", "))
  code <- sort(list.files(here::here("utils"), pattern = "\\.R$", full.names = TRUE))
  packages <- c("ergm", "tergm", "network", "sna", "rlang")
  rlang::hash(list(def = def_fields,
                   data = unname(tools::md5sum(data_files)),
                   code = unname(tools::md5sum(code)),
                   R = as.character(getRversion()),
                   packages = vapply(packages, function(p) as.character(utils::packageVersion(p)), ""),
                   settings = settings))
}

# A saved result can be reused if it was made with the same fingerprint and
# did not crash. Crashes (the worker R process died or hit an error outside
# the fit) are usually environmental, so they are retried on the next run;
# failed and timed-out fits are kept.
result_up_to_date <- function(old, h, files = NULL, settings = NULL) {
  current <- !is.null(old) && identical(as.character(old$spec_hash[1]), h) &&
    !identical(as.character(old$status[1]), "crashed")
  if (!current || is.null(files)) return(current)
  if (old$status[1] %in% c("fit", "degenerate", "problem fit")) {
    required <- c(files$coefs, files$corr)
    if (isTRUE(settings$save_models)) required <- c(required, files$model)
    if (identical(old$gof[1], "done")) {
      required <- c(required, files$gof)
      if (isTRUE(settings$gof_plots)) required <- c(required, files$gof_png)
    }
    if (any(!file.exists(required))) return(FALSE)
    if (file.size(files$coefs) == 0) return(FALSE)
  }
  TRUE
}

read_csv_quiet <- function(path) {
  if (!file.exists(path)) return(NULL)
  d <- readr::read_csv(path, show_col_types = FALSE, guess_max = 10000)
  if ("community_id" %in% names(d)) d$community_id <- as.character(d$community_id)
  d
}

# Settings shared by the run scripts; each script can override entries.
default_model_settings <- function() {
  list(
    seed = 20240102,
    # Estimation method for every ERGM and STERGM (CMLE) fit, including the
    # integrated model: "Stochastic-Approximation" (project default) or "MCMLE".
    method = "Stochastic-Approximation",
    # Optional second attempt with the other method if the first fails or
    # does not converge. FALSE = report the failure instead.
    fallback = FALSE,
    mcmle_maxit = 60,
    eval_loglik = TRUE,          # AIC/BIC for ERGMs (bridge sampling; slower)
    max_abs_coef = 10,           # project degeneracy rule: |coef| > 10 ...
    max_se = 10,                 # ... or SE > 10 flags the whole fit / sub-model
    min_minority = 5,            # binary attributes need >= 5 in the smaller group
    drop_invalid_terms = TRUE,   # drop terms whose attribute has no usable variation in a community
    min_events = 10,             # STERGM: minimum formation / dissolution events
    run_gof = TRUE,
    n_gof_sim = 100,
    gof_plots = TRUE,            # also save each community's GOF plot as a PNG (bundled with the report)
    save_models = TRUE,          # keep fitted model .rds files on the analysis machine
    default_min_size = list(ergm = 15, stergm = 20, integrated = 15)
  )
}

# ctx: list(type, out_root, utils_dir, data_dir, communities (tibble with
# community_id), settings, n_workers, time_limit, force_rerun)
run_model_definition <- function(def, ctx) {
  type <- ctx$type
  model_dir <- file.path(ctx$out_root, def$model_id)
  dir.create(file.path(model_dir, "logs"), recursive = TRUE, showWarnings = FALSE)
  min_size <- if (is.na(def$min_network_size)) ctx$settings$default_min_size[[type]] else def$min_network_size
  def_fields <- if (type == "ergm") list(def$terms, min_size) else
    list(def$formation_terms, def$dissolution_terms, min_size)
  club <- uses_shared_club(if (type == "ergm") def$terms else c(def$formation_terms, def$dissolution_terms))

  jobs <- list()
  for (cid in ctx$communities$community_id) {
    w1 <- file.path(ctx$data_dir, paste0("community_", cid, "_network_directed_w1.rds"))
    w2 <- file.path(ctx$data_dir, paste0("community_", cid, "_network_directed_w2.rds"))
    cv <- covars_file(ctx$data_dir, cid)
    if (!file.exists(w1) || (type == "stergm" && !file.exists(w2)))
      stop("Missing prepared network for community ", cid, "; rerun 01_data_prep.R.")
    h <- spec_hash(def_fields, c(if (type == "ergm") w1 else c(w1, w2), if (club) cv), ctx$settings)
    files <- community_files(model_dir, cid)
    old <- read_csv_quiet(files$info)
    if (!isTRUE(ctx$force_rerun) && result_up_to_date(old, h, files, ctx$settings)) next
    jobs[[length(jobs) + 1]] <- list(
      type = type, model_id = def$model_id, community_id = cid, spec_hash = h,
      w1_path = w1, w2_path = w2, covars_path = cv, min_size = min_size, model_dir = model_dir,
      utils_dir = ctx$utils_dir, settings = ctx$settings,
      terms = def$terms, formation_terms = def$formation_terms, dissolution_terms = def$dissolution_terms)
  }
  message(sprintf("[%s] %d communities to fit, %d up to date", def$model_id, length(jobs),
                  nrow(ctx$communities) - length(jobs)))
  outcomes <- run_jobs(jobs, ctx$n_workers, ctx$time_limit)

  # Record jobs that ended without writing an info file (time-out or crash).
  for (i in seq_len(nrow(outcomes))) {
    f <- community_files(model_dir, outcomes$community_id[i])
    j <- Filter(function(x) as.character(x$community_id) == outcomes$community_id[i], jobs)[[1]]
    info <- read_csv_quiet(f$info)
    if (is.null(info) || !identical(as.character(info$spec_hash[1]), j$spec_hash)) {
      readr::write_csv(tibble(model_id = def$model_id, community_id = outcomes$community_id[i], type = type,
                              spec_hash = j$spec_hash, status = outcomes$outcome[i],
                              error = outcomes$message[i], gof = "not run"), f$info)
      for (stale in c(f$coefs, f$corr, f$gof)) if (file.exists(stale)) file.remove(stale)
    } else if (outcomes$outcome[i] == "timed out" && info$gof[1] == "not run" && isTRUE(ctx$settings$run_gof)) {
      info$gof <- "timed out"
      readr::write_csv(info, f$info)
    }
  }
  collect_model_results(def$model_id, model_dir, ctx$communities$community_id)
}

# Combine a model's per-community files.
collect_model_results <- function(model_id, model_dir, community_ids) {
  read_all <- function(kind) {
    dplyr::bind_rows(lapply(community_ids, function(cid) read_csv_quiet(community_files(model_dir, cid)[[kind]])))
  }
  list(info = read_all("info"), coefs = read_all("coefs"), gof = read_all("gof"), corr = read_all("corr"))
}

# Community-level covariates used for pooling, moderators and selection
# checks: size, directed wave 1 density and reciprocity, ideation prevalence,
# flags. Aggregate statistics only.
community_covariates <- function(data_dir, communities) {
  dplyr::bind_rows(lapply(communities$community_id, function(cid) {
    w1 <- file.path(data_dir, paste0("community_", cid, "_network_directed_w1.rds"))
    if (!file.exists(w1)) return(NULL)
    net <- readRDS(w1)
    n <- network::network.size(net)
    e <- network::network.edgecount(net)
    tibble(community_id = as.character(cid), n_nodes = n, n_arcs_w1 = e,
           density_w1 = if (n > 1) e / (n * (n - 1)) else NA_real_,
           reciprocity_w1 = tryCatch(sna::grecip(net, measure = "edgewise"), error = function(err) NA_real_))
  })) |>
    left_join(communities |>
                mutate(community_id = as.character(community_id)) |>
                select(community_id, any_of(c("ideation_prev_w1", "attrition_rate", "small_community", "high_attrition"))),
              by = "community_id") |>
    mutate(small_community = coalesce(as.logical(small_community), n_nodes < 50),
           high_attrition = coalesce(as.logical(high_attrition), FALSE))
}

# =============================================================================
# Combined outputs (main process)
# =============================================================================

# Part-level fit status: one row per model x community x part. For STERGMs a
# sub-model can be degenerate while the other part is fine.
part_level_info <- function(info, coefs, defs, type) {
  parts <- if (type == "ergm") tibble(model_id = defs$model_id, part = "ergm") else
    dplyr::bind_rows(
      tibble(model_id = defs$model_id[defs$part %in% c("formation", "both")], part = "formation"),
      tibble(model_id = defs$model_id[defs$part %in% c("dissolution", "both")], part = "dissolution"))
  degen <- if (nrow(coefs)) coefs |> group_by(model_id, community_id, part) |>
    summarise(degenerate = any(degenerate_flag), .groups = "drop") else
    tibble(model_id = character(), community_id = character(), part = character(), degenerate = logical())
  info |>
    inner_join(parts, by = "model_id", relationship = "many-to-many") |>
    left_join(degen, by = c("model_id", "community_id", "part")) |>
    mutate(status = ifelse(status %in% c("fit", "degenerate"),
                           ifelse(degenerate %in% TRUE, "degenerate", "fit"), status)) |>
    select(-degenerate)
}

# Fit every definition, then write the combined CSVs the report reads.
run_all_models <- function(defs, ctx, exclude_small = TRUE) {
  res <- lapply(seq_len(nrow(defs)), function(i) run_model_definition(defs[i, ], ctx))
  write_model_outputs(ctx$type, defs,
                      info = dplyr::bind_rows(lapply(res, `[[`, "info")),
                      coefs = dplyr::bind_rows(lapply(res, `[[`, "coefs")),
                      gof = dplyr::bind_rows(lapply(res, `[[`, "gof")),
                      corr = dplyr::bind_rows(lapply(res, `[[`, "corr")),
                      ctx = ctx, exclude_small = exclude_small)
}

# Files are prefixed with the model type so names stay unique when results
# are flattened into one download bundle. Pooled tables use meta_trust.R.
write_model_outputs <- function(type, defs, info, coefs, gof, corr, ctx, exclude_small) {
  out <- ctx$out_root
  pinfo <- part_level_info(info, coefs, defs, type)
  w <- function(x, name) readr::write_csv(x, file.path(out, paste0(type, "_", name, ".csv")))
  w(mutate(defs, run_at = format(Sys.time(), "%Y-%m-%d %H:%M")), "models_run")
  w(pinfo, "fit_info")
  w(coefs, "coefs")
  w(gof, "gof")
  w(corr, "coef_corr")
  readr::write_csv(ctx$covariates, file.path(dirname(out), "community_covariates.csv"))
  if (nrow(coefs) > 0) {
    meta <- meta_analyze(coefs, ctx$covariates, exclude_small)
    w(meta, "meta")
    w(meta_regression(coefs, ctx$covariates, setNames(defs$moderators, defs$model_id), exclude_small), "meta_regression")
    w(selection_check(pinfo, ctx$covariates, exclude_small), "selection")
    message(sprintf("%s: %d models, %d community fits, %d pooled terms", type, nrow(defs), nrow(info), nrow(meta)))
  }
  invisible(list(info = pinfo, coefs = coefs, gof = gof, corr = corr))
}

