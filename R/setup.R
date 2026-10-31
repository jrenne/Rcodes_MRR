# Run scripts from the project root. Dependencies for the numerical results are small.
if (!file.exists("data/processed/exposure_matrix.csv")) stop("Run from the repository root.")
for (package in "jsonlite") {
  if (!requireNamespace(package, quietly = TRUE)) stop("Install the R package ", package, ".")
}
for (file in c(
  "common", "bank_reference", "financial_system", "resources", "clearing",
  "floor_capacity", "bond_purchases", "aggregation", "block_aggregation"
)) {
  source(file.path("R", "model", paste0(file, ".R")))
}
dir.create("outputs", showWarnings = FALSE)
dir.create("Figures", showWarnings = FALSE)
source("R/output/tables.R")
