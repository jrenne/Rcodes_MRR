# Create an archive of the replication code, supplied data and numerical outputs.
roots <- c("R", "scripts", "tests", "data", "Figures")
files <- c(
  "main.R", "README.md", "LICENSE", "CITATION.cff", "Rcodes_MRR.Rproj", ".gitignore",
  unlist(lapply(roots, list.files, recursive = TRUE, full.names = TRUE, all.files = FALSE))
)
files <- sort(unique(files))
stopifnot(
  !any(grepl("(^|/)(private_data|[.]git|archives|manuscript)/", files)),
  all(file.exists(files))
)
archive <- normalizePath(".", winslash = "/")
archive <- file.path(archive, "replication_package.zip")
if (file.exists(archive)) unlink(archive)
status <- utils::zip(archive, files, flags = "-q")
stopifnot(status == 0L)
contents <- utils::unzip(archive, list = TRUE)$Name
stopifnot(setequal(contents, files), !any(grepl("(^|/)(private_data|[.]git|archives|manuscript)/", contents)))
message("Created replication_package.zip with ", length(files), " files.")
