# Shared coefficient labels for manuscript tables, report tables, and plots.
# Edit config/coefficient_labels.csv, then rerun the export script or re-knit.
# Keys omit temporal prefixes, so one entry covers ERGM, Form, and Persist.
read_coefficient_labels <- function(path = "config/coefficient_labels.csv") {
  d <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE,
                       fileEncoding = "UTF-8", na.strings = "")
  if (!all(c("term", "label") %in% names(d))) stop(path, " needs term and label columns.")
  d$term <- trimws(d$term); d$label <- trimws(d$label)
  if (anyNA(d[c("term", "label")]) || any(!nzchar(d$term)) || any(!nzchar(d$label)))
    stop(path, " contains blank terms or labels.")
  if (anyDuplicated(d$term)) stop(path, " contains duplicate term keys.")
  setNames(d$label, d$term)
}

COEFFICIENT_LABELS <- read_coefficient_labels()

coefficient_label <- function(x, max_width = Inf, mapping = COEFFICIENT_LABELS) {
  key <- sub("^(?:Form|Diss|Persist)\\(\\d+\\)~", "", x, perl = TRUE)
  label <- unname(mapping[key])
  missing <- is.na(label)
  # Unmapped future terms retain the report's readable fallback labels.
  fallback <- key[missing]
  replacements <- c("^nodeicov\\." = "in-deg cov: ", "^nodeocov\\." = "out-deg cov: ",
    "^nodecov\\." = "cov: ", "^nodematch\\." = "match: ",
    "^nodeifactor\\." = "in-factor: ", "^nodeofactor\\." = "out-factor: ",
    "^mix\\." = "nodemix: ", "^edgecov\\." = "dyadic cov: ",
    "^gwideg\\.fixed\\.([0-9.]+)$" = "GW in-degree (decay=\\1)",
    "^gwodeg\\.fixed\\.([0-9.]+)$" = "GW out-degree (decay=\\1)",
    "^gwesp\\.([A-Z]+)\\.fixed\\.([0-9.]+)$" = "GW shared partners, \\1 (decay=\\2)",
    "^gwdsp\\.([A-Z]+)\\.fixed\\.([0-9.]+)$" = "GW dyadwise shared partners, \\1 (decay=\\2)")
  for (pattern in names(replacements)) fallback <- sub(pattern, replacements[[pattern]], fallback)
  label[missing] <- fallback
  if (is.finite(max_width)) substr(label, 1, max_width) else label
}
