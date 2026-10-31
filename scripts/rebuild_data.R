# Optional rebuild of processed inputs from the supplied public source snapshots.
# A separate output directory lets the caller compare a rebuild before replacing inputs.
args <- commandArgs(trailingOnly = TRUE)
out <- if (length(args)) args[1] else "outputs/data_rebuild"
for (package in c("dplyr", "tidyr", "quadprog", "readxl", "jsonlite")) {
  if (!requireNamespace(package, quietly = TRUE)) stop("Install the R package ", package, ".")
}
dir.create(out, recursive = TRUE, showWarnings = FALSE)
for (file in c("exposures", "capital", "nbfi", "accounts")) source(file.path("R", "data", paste0(file, ".R")))
build_exposure_data(out)
build_capital_data(out)
build_nbfi_data(out)
build_accounting_data(out)
message("Public data rebuilt in ", out, ". Source snapshots were not modified.")
