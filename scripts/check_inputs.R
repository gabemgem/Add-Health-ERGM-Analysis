raw_files <- c("Standard/Survey/W1/allwave1.xpt", "Standard/Survey/W2/wave2.xpt",
               "Standard/InSchool/Inschool.xpt", "Standard/InSchool/Schinfo.xpt",
               "Standard/Friendship/hfriend1.xpt", "Standard/Friendship/hfriend2.xpt")

check_supplement <- function(check_raw = FALSE) {
  required <- c("tidyverse", "haven", "network", "sna", "ergm", "tergm", "here", "callr",
                "metafor", "ggraph", "igraph", "patchwork", "knitr", "kableExtra", "scales", "rmarkdown")
  missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) stop("Missing packages: ", paste(missing, collapse = ", "),
                             ". Run Rscript scripts/setup.R", call. = FALSE)
  lock <- jsonlite::read_json("renv.lock")$Packages
  mismatch <- names(lock)[!vapply(lock, function(record) {
    tryCatch(isTRUE(utils::packageVersion(record$Package) == package_version(record$Version)),
             error = function(e) FALSE)
  }, logical(1))]
  if (length(mismatch)) stop("Package versions differ from renv.lock: ", paste(mismatch, collapse = ", "),
                             ". Run Rscript scripts/setup.R", call. = FALSE)
  for (pkg in c("ergm", "tergm")) {
    expected <- c(ergm = "4.6.0", tergm = "4.2.0")[[pkg]]
    if (as.character(packageVersion(pkg)) != expected)
      stop(pkg, " must be ", expected, "; restore renv.lock before running.")
  }
  source("scripts/validate_reference.R", local = TRUE)
  validate_reference()
  if (check_raw) {
    root <- Sys.getenv("RAW_DATA_ROOT")
    if (!nzchar(root)) stop("Set RAW_DATA_ROOT in .Renviron; see data/README.md.")
    absent <- raw_files[!file.exists(file.path(root, raw_files))]
    if (length(absent)) stop("Missing licensed input files: ", paste(absent, collapse = ", "))
    dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
    write.csv(data.frame(file = raw_files,
                        md5 = unname(tools::md5sum(file.path(root, raw_files)))),
              "data/processed/input_checksums.csv", row.names = FALSE)
  }
  message("Supplement preflight passed", if (check_raw) " (licensed inputs present)" else " (saved results)")
  invisible(TRUE)
}
