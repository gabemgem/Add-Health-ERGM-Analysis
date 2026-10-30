# Run once from the supplement root: Rscript scripts/setup.R
# All downloaded packages stay inside this project.
if (!file.exists("AddHealthSupplement.Rproj")) stop("Run setup from the supplement root.")
options(repos = c(CRAN = "https://packagemanager.posit.co/cran/2024-03-01"),
        timeout = 600)
if (!identical(as.character(getRversion()), "4.3.1"))
  stop("Use R 4.3.1 to restore and run the pinned environment.")
dir.create(".r-library", showWarnings = FALSE)
lib <- normalizePath(".r-library", winslash = "/")
.libPaths(c(lib, .Library))
Sys.setenv(RENV_PATHS_ROOT = file.path(getwd(), ".renv-cache"),
           RENV_CONFIG_CACHE_ENABLED = "FALSE")
if (!requireNamespace("renv", quietly = TRUE))
  install.packages("renv", lib = lib)
if (!file.exists("renv.lock")) stop("Missing renv.lock; run from the supplement root.")
renv::restore(project = getwd(), library = lib, prompt = FALSE)
message("Dependencies restored. Run Rscript run_analysis.R --mode=reference")
