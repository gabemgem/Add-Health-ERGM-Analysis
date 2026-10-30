# Every entry point is run from the supplement root, including under Rscript.
# Source files contain UTF-8 labels and the disclosure suppression symbol.
if (.Platform$OS.type == "windows") {
  locale <- Sys.setlocale("LC_CTYPE", ".UTF-8")
  if (!nzchar(locale)) stop("A UTF-8 Windows locale is required; see README.md.")
}
options(encoding = "UTF-8")
if (!file.exists("AddHealthSupplement.Rproj") || !dir.exists("utils"))
  stop("Run this command from the Add Health ERGM Supplement root.", call. = FALSE)
if (dir.exists(".r-library")) .libPaths(c(normalizePath(".r-library"), .Library))
if (!identical(as.character(getRversion()), "4.3.1"))
  stop("This supplement is pinned to R 4.3.1; see README.md.", call. = FALSE)
if (!requireNamespace("here", quietly = TRUE))
  stop("Install the locked dependencies first: Rscript scripts/setup.R", call. = FALSE)
here::i_am("AddHealthSupplement.Rproj")
if (file.exists(".Renviron")) readRenviron(".Renviron")
