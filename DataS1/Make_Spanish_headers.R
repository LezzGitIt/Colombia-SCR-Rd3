# Regenerate Spanish-headed copies of the DataS1 tables.
#
# The deposited CSVs use English column headers. This script rewrites the headers
# to Spanish using Column_names_ES.csv (english,spanish) and writes the copies to
# DataS1/es/. Run it from the DataS1/ folder with base R -- no packages needed:
#
#   Rscript Make_Spanish_headers.R
#
# Only the column *names* change; the data are untouched.

lookup_path <- "Column_names_ES.csv"
out_dir     <- "es"

if (!file.exists(lookup_path)) stop("Run this script from inside the DataS1/ folder (Column_names_ES.csv not found).")

lookup <- read.csv(lookup_path, stringsAsFactors = FALSE, check.names = FALSE, encoding = "UTF-8")
en_to_es <- setNames(lookup$spanish, lookup$english)

tables <- c("Bird_pcs_all", "Bird_pcs_analysis", "Event_covs",
            "Functional_traits", "Site_covs", "Taxonomy")

dir.create(out_dir, showWarnings = FALSE)

for (tbl in tables) {
  src <- paste0(tbl, ".csv")
  if (!file.exists(src)) { warning("skipping missing ", src); next }
  df <- read.csv(src, stringsAsFactors = FALSE, check.names = FALSE, encoding = "UTF-8")

  hit <- names(df) %in% names(en_to_es)
  names(df)[hit] <- unname(en_to_es[names(df)[hit]])
  if (any(!hit)) {
    message(src, ": no Spanish name for ", sum(!hit), " column(s), left in English -- ",
            paste(names(df)[!hit], collapse = ", "))
  }

  dest <- file.path(out_dir, src)
  write.csv(df, dest, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  cat("wrote", dest, "(", ncol(df), "columns )\n")
}

cat("\nDone. Spanish-headed copies are in", file.path(getwd(), out_dir), "\n")
