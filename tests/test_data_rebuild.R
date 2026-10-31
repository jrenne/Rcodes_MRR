# Check a full rebuild against the supplied processed inputs, without overwriting them.
source("R/setup.R")
out <- tempfile("mrr_data_rebuild_")
dir.create(out)
for (file in c("exposures", "capital", "nbfi", "accounts")) source(file.path("R", "data", paste0(file, ".R")))
build_exposure_data(out)
build_capital_data(out)
build_nbfi_data(out)
build_accounting_data(out)
files <- c(
  "exposure_matrix.csv", "exposure_register.csv", "bank_balance_sheets.csv", "bank_accounts.csv",
  "firm_loans.csv", "household_loans.csv", "bank_capital_sample.csv", "capital_proxies.csv",
  "capital_proxy_by_bank.csv", "nbfi_buffers.csv", "household_consumption.csv", "stress_loss_rates.csv"
)
for (file in files) {
  before <- read.csv(file.path("data/processed", file), check.names = FALSE)
  after <- read.csv(file.path(out, file), check.names = FALSE)
  if (file == "household_consumption.csv") {
    before <- before[order(before$country), ]
    after <- after[order(after$country), ]
  }
  stopifnot(nrow(before) == nrow(after))
  for (column in intersect(names(before), names(after))) {
    a <- before[[column]]
    b <- after[[column]]
    if (is.numeric(a)) {
      stopifnot(
        identical(is.na(a), is.na(b)),
        all(abs(a - b) < 1e-7 * pmax(1, abs(a)), na.rm = TRUE)
      )
    } else {
      stopifnot(identical(as.character(a), as.character(b)))
    }
  }
}
unlink(out, recursive = TRUE)
cat("Passed the public-source rebuild of 12 input tables.\n")
