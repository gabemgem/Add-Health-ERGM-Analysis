# =============================================================================
# meta_trust.R
# Purpose:  Two-step multi-network analysis: pool per-community ERGM/STERGM
#           estimates with random-effects meta-analysis (Snijders & Baerveldt
#           2003; An 2015; Tolochko & Boomgaarden 2024), plus the checks used
#           to judge whether each pooled estimate can be trusted.
#
#           Used in two places with identical logic:
#             - 03_run_ergm_models.R / 04_run_stergm_models.R (analysis
#               machine) write the pooled tables as CSVs;
#             - results_report.Rmd (local) re-pools from the downloaded
#               per-community coefficient CSVs, so pooling choices (e.g.
#               excluding small communities) can be changed without rerunning
#               models.
#           Only needs dplyr, tibble and metafor. No data access, no here().
# Date:     2026-10-19
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(metafor)
})

# rma with REML, falling back to more robust settings if it fails to converge.
safe_rma <- function(...) {
  fit <- tryCatch(metafor::rma(..., method = "REML"), error = function(e) NULL)
  if (is.null(fit)) fit <- tryCatch(metafor::rma(..., method = "REML", control = list(stepadj = 0.5, maxiter = 1000)),
                                    error = function(e) NULL)
  if (is.null(fit)) fit <- tryCatch(metafor::rma(..., method = "DL"), error = function(e) NULL)
  fit
}

# Shift in the pooled estimate, in units of the primary pooled SE, when the
# analysis set is changed to `alt` (NA if the set is unchanged or too small).
pooled_shift <- function(alt, primary_fit, primary_k) {
  if (is.null(primary_fit) || nrow(alt) < 3 || nrow(alt) == primary_k) return(NA_real_)
  f <- safe_rma(yi = alt$estimate, sei = alt$std.error)
  if (is.null(f)) NA_real_ else abs(as.numeric(f$b) - as.numeric(primary_fit$b)) / primary_fit$se
}

# Pool one term. `d_primary` = estimates in the primary analysis set;
# `d_all` = every usable estimate (incl. small communities), used for the
# sensitivity checks. Both have community_id, estimate, std.error, n_nodes,
# small_community, high_attrition.
pool_term <- function(d_primary, d_all, strict_se = 5) {
  k <- nrow(d_primary)
  # Keep a stable schema when too few communities contribute or pooling fails.
  # The report still needs these columns to show unavailable estimates safely.
  base <- tibble(k = k, estimate = NA_real_, std.error = NA_real_, p.value = NA_real_,
    ci.lb = NA_real_, ci.ub = NA_real_, pi.lb = NA_real_, pi.ub = NA_real_,
    tau2 = NA_real_, I2 = NA_real_, Q = NA_real_, Q_p = NA_real_,
    T2 = NA_real_, T2_p = NA_real_, aic_fe = NA_real_, aic_re = NA_real_,
    loo_max_shift_se = NA_real_, loo_most_influential = NA_character_,
    strict_k = sum(d_primary$std.error <= strict_se), strict_shift_se = NA_real_,
    small_shift_se = NA_real_, attrition_shift_se = NA_real_, size_slope_p = NA_real_)
  if (k < 3) return(base)

  re <- safe_rma(yi = d_primary$estimate, sei = d_primary$std.error)
  if (is.null(re)) return(base)
  pred <- stats::predict(re)

  # Snijders & Baerveldt (2003) T^2: is the effect zero in every community?
  t2 <- sum((d_primary$estimate / d_primary$std.error)^2)

  # Fixed vs random effects (An 2015): AIC under ML.
  aic_fe <- tryCatch(stats::AIC(metafor::rma(yi = d_primary$estimate, sei = d_primary$std.error, method = "FE")),
                     error = function(e) NA_real_)
  aic_re <- tryCatch(stats::AIC(metafor::rma(yi = d_primary$estimate, sei = d_primary$std.error, method = "ML")),
                     error = function(e) NA_real_)

  # Influence: largest leave-one-out change, in pooled SEs.
  loo <- tryCatch(metafor::leave1out(re), error = function(e) NULL)
  loo_shift <- if (is.null(loo)) NA_real_ else max(abs(loo$estimate - as.numeric(re$b))) / re$se
  loo_comm <- if (is.null(loo)) NA else d_primary$community_id[which.max(abs(loo$estimate - as.numeric(re$b)))]

  # Does the effect change with community size?
  size_fit <- if (length(unique(d_primary$n_nodes)) > 2) {
    safe_rma(yi = d_primary$estimate, sei = d_primary$std.error, mods = log(d_primary$n_nodes))
  } else NULL

  tibble(
    k = k,
    estimate = as.numeric(re$b), std.error = re$se, p.value = re$pval,
    ci.lb = re$ci.lb, ci.ub = re$ci.ub,
    pi.lb = if (!is.null(pred$pi.lb)) pred$pi.lb else NA_real_,
    pi.ub = if (!is.null(pred$pi.ub)) pred$pi.ub else NA_real_,
    tau2 = re$tau2, I2 = re$I2, Q = re$QE, Q_p = re$QEp,
    T2 = t2, T2_p = stats::pchisq(t2, df = k, lower.tail = FALSE),
    aic_fe = aic_fe, aic_re = aic_re,
    loo_max_shift_se = loo_shift, loo_most_influential = as.character(loo_comm),
    strict_k = sum(d_primary$std.error <= strict_se),
    strict_shift_se = pooled_shift(d_primary[d_primary$std.error <= strict_se, ], re, k),
    small_shift_se = pooled_shift(d_all, re, k),
    attrition_shift_se = pooled_shift(d_primary[!d_primary$high_attrition %in% TRUE, ], re, k),
    size_slope_p = if (is.null(size_fit)) NA_real_ else size_fit$pval[2]
  )
}

# Usable per-community estimates joined to community covariates.
# coefs: model_id, part, community_id, term, estimate, std.error, problem.
usable_estimates <- function(coefs, covariates) {
  coefs |>
    filter(!problem, is.finite(estimate), is.finite(std.error), std.error > 0) |>
    select(-any_of(c("small_community", "high_attrition", "n_nodes"))) |>
    left_join(covariates, by = "community_id")
}

# Pool every model x part x term. Primary set excludes small communities when
# exclude_small = TRUE (project convention: n < 50 excluded from the primary
# meta-analysis, included as a sensitivity check).
meta_analyze <- function(coefs, covariates, exclude_small = TRUE) {
  u <- usable_estimates(coefs, covariates)
  if (nrow(u) == 0) return(tibble())
  u |>
    group_by(model_id, part, term) |>
    group_modify(function(d, key) {
      primary <- if (exclude_small) d[!d$small_community %in% TRUE, ] else d
      pool_term(primary, d)
    }) |>
    ungroup()
}

# Meta-regression of each term on community covariates (An 2015).
# moderators: named character vector model_id -> "log(n_nodes) + ideation_prev_w1".
meta_regression <- function(coefs, covariates, moderators, exclude_small = TRUE) {
  moderators <- moderators[!is.na(moderators) & nzchar(moderators)]
  if (length(moderators) == 0) return(tibble())
  u <- usable_estimates(coefs, covariates) |> filter(model_id %in% names(moderators))
  if (exclude_small) u <- filter(u, !small_community %in% TRUE)
  u |>
    group_by(model_id, part, term) |>
    group_modify(function(d, key) {
      if (nrow(d) < 5) return(tibble())
      fit <- tryCatch(metafor::rma(yi = estimate, sei = std.error, data = d, method = "REML",
                                   mods = stats::as.formula(paste("~", moderators[[key$model_id]]))),
                      error = function(e) NULL)
      if (is.null(fit)) return(tibble())
      tibble(coefficient = rownames(fit$beta), estimate = as.numeric(fit$beta),
             std.error = fit$se, p.value = fit$pval, k = fit$k,
             R2 = if (!is.null(fit$R2)) fit$R2 else NA_real_, QM_p = fit$QMp)
    }) |>
    ungroup()
}

# Do the communities that contribute to pooling look like the rest?
# fit_info: model_id, part, community_id, status ("fit" = usable).
selection_check <- function(fit_info, covariates, exclude_small = TRUE) {
  d <- fit_info |>
    select(model_id, part, community_id, status) |>
    left_join(covariates, by = "community_id") |>
    mutate(used = status == "fit" & !(exclude_small & small_community %in% TRUE))
  test_p <- function(x, used) {
    if (sum(used & !is.na(x)) < 2 || sum(!used & !is.na(x)) < 2) return(NA_real_)
    suppressWarnings(stats::wilcox.test(x[used], x[!used])$p.value)
  }
  d |>
    group_by(model_id, part) |>
    summarise(
      n_used = sum(used), n_total = n(),
      median_n_used = stats::median(n_nodes[used], na.rm = TRUE),
      median_n_other = stats::median(n_nodes[!used], na.rm = TRUE),
      size_p = test_p(n_nodes, used),
      density_p = test_p(density_w1, used),
      ideation_p = test_p(ideation_prev_w1, used),
      .groups = "drop"
    )
}

# ---- Trust assessment ---------------------------------------------------------
# One verdict per pooled term. Each check is "ok", "caution" or "poor"; the
# verdict is the worst. Thresholds are rules of thumb, documented in the report.
trust_checks <- tibble::tribble(
  ~check,               ~reason,                                ~rule,
  "chk_coverage",       "few communities pooled",               "share of ALL communities pooled: caution < 75%, poor < 50% or k < 10",
  "chk_heterogeneity",  "large between-community variation",    "I^2 > 75%",
  "chk_influence",      "driven by one community",              "leave-one-out shift > 1 SE",
  "chk_imprecise",      "sensitive to imprecise estimates",     "shift > 1 SE when estimates with SE > 5 are dropped",
  "chk_small",          "depends on small communities",         "shift > 1 SE when small communities (n < 50) are added",
  "chk_attrition",      "depends on high-attrition communities", "shift > 1 SE when high-attrition communities are dropped",
  "chk_size",           "depends on community size",            "meta-regression on log(n): p < .05",
  "chk_integrated",     "disagrees with integrated model",      "|two-step - integrated| / combined SE > 2"
)

assess_trust <- function(meta, n_communities_total, integrated = NULL) {
  if (nrow(meta) == 0) return(meta)
  shift_chk <- function(x) ifelse(!is.na(x) & x > 1, "caution", "ok")
  t <- meta |>
    mutate(
      coverage = k / n_communities_total,
      chk_coverage = case_when(k < 10 | coverage < 0.5 ~ "poor", coverage < 0.75 ~ "caution", TRUE ~ "ok"),
      chk_heterogeneity = ifelse(is.na(I2) | I2 > 75, "caution", "ok"),
      chk_influence = shift_chk(loo_max_shift_se),
      chk_imprecise = shift_chk(strict_shift_se),
      chk_small = shift_chk(small_shift_se),
      chk_attrition = shift_chk(attrition_shift_se),
      chk_size = ifelse(!is.na(size_slope_p) & size_slope_p < 0.05, "caution", "ok")
    )
  if (!is.null(integrated) && nrow(integrated) > 0) {
    t <- t |>
      left_join(integrated |> select(model_id, term, int_estimate = estimate, int_std.error = std.error),
                by = c("model_id", "term")) |>
      mutate(integrated_z = (estimate - int_estimate) / sqrt(std.error^2 + int_std.error^2),
             chk_integrated = ifelse(!is.na(integrated_z) & abs(integrated_z) > 2, "caution", "ok"))
  } else {
    t <- mutate(t, int_estimate = NA_real_, integrated_z = NA_real_, chk_integrated = "ok")
  }
  level <- c(ok = 0, caution = 1, poor = 2)
  chk <- as.matrix(t[trust_checks$check])
  t$verdict <- apply(chk, 1, function(x) names(level)[max(level[x], na.rm = TRUE) + 1])
  t$reasons <- apply(chk, 1, function(x) paste(trust_checks$reason[x != "ok"], collapse = "; "))
  t
}
