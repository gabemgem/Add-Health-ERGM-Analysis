# Base R audit: can run without installing analysis packages.
validate_reference <- function() {
  manifest <- read.csv("reference_results/checksums.csv", stringsAsFactors = FALSE)
  paths <- file.path("reference_results", manifest$file)
  if (any(!file.exists(paths))) stop("Missing reference result files.")
  if (!identical(unname(tools::md5sum(paths)), manifest$md5)) stop("Reference checksum mismatch.")
  for (type in c("ergm", "stergm")) {
    defs <- read.csv(paste0("config/", type, "_model_desc.csv"), stringsAsFactors = FALSE)
    run <- read.csv(paste0("reference_results/", type, "_models_run.csv"), stringsAsFactors = FALSE)
    if (!setequal(defs$model_id, run$model_id)) stop("Reference/config model mismatch: ", type)
    fields <- if (type == "ergm") "terms" else c("formation_terms", "dissolution_terms")
    for (field in fields)
      if (!identical(defs[[field]], run[[field]][match(defs$model_id, run$model_id)]))
        stop("Reference/config formula mismatch: ", type, " ", field)
    cc <- read.csv(paste0("reference_results/", type, "_coefs.csv"), stringsAsFactors = FALSE)
    if (anyDuplicated(cc[c("model_id", "community_id", "part", "term")])) stop("Duplicate coefficient rows.")
    if (!all(cc$model_id %in% defs$model_id)) stop("Unselected model coefficients remain.")
  }
  message("Reference checksums, formulas, and coefficient keys verified.")
  invisible(TRUE)
}
