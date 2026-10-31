# Build a shareable package from an explicit file list, excluding local market data.
roots <- c("R", "scripts", "tests", "data", "Figures")
files <- c(
  "main.R", "README.md", "LICENSE", "CITATION.cff", "Rcodes_MRR.Rproj", ".gitignore",
  unlist(lapply(roots, list.files, recursive = TRUE, full.names = TRUE, all.files = FALSE))
)
paper <- list.files("manuscript/paper", recursive = TRUE, full.names = TRUE)
# Include source dependencies and the compiled documents, not LaTeX intermediates.
files <- sort(unique(c(files, paper[grepl("[.](tex|bib|bst|cls|sty|pdf|md|csv)$", paper)])))
stopifnot(
  !any(grepl("(^|/)(private_data|[.]git|archives)/", files)),
  !any(grepl("MRR_CDS|credit_risk_indicators", files)), all(file.exists(files))
)
archive <- normalizePath(".", winslash = "/")
archive <- file.path(archive, "replication_package.zip")
if (file.exists(archive)) unlink(archive)
status <- utils::zip(archive, files, flags = "-q")
stopifnot(status == 0L)
contents <- utils::unzip(archive, list = TRUE)$Name
stopifnot(setequal(contents, files), !any(grepl("MRR_CDS|private_data|[.]git/", contents)))
message("Created replication_package.zip with ", length(files), " files; the market workbook is excluded.")
