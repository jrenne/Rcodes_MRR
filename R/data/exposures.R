# Reconstruct the end-2024 matrix from the supplied public snapshots.  Creditor rows,
# debtor columns; EUR millions in source layers become EUR billions.
build_exposure_data <- function(output_directory) {
  suppressPackageStartupMessages({
    library(dplyr)
    library(tidyr)
  })
  source("R/data/exposure_helpers.R")
  out <- output_directory
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  for (file in c("reference_layers_2024Q4.csv", "eurostat_raw_stock_reference.csv", "government_nominal_bonds.csv")) {
    file.copy(file.path("data/raw/reference", file), file.path(out, file), overwrite = TRUE)
  }
  # These stages share this function workspace; each writes its source diagnostics.
  for (step in c("01_source_margins", "02_interbank_loans", "03_bank_loans_deposits", "04_bank_securities", "05_nonbank_claims", "06_reconciliation")) {
    sys.source(file.path("R", "data", "exposures", paste0(step, ".R")), envir = environment())
  }
  invisible(list(matrix = mat, exposures = ledger, bank_balance_sheets = bs))
}
