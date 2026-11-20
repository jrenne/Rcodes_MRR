source("R/setup.R")
settings <- readRDS("outputs/baseline.rds")$settings
# Each output script reads the saved results and writes its named paper assets.
for (file in c("figures", "exposures", "nbfi_exposures", "two_bank_example", "capital", "dependence", "coverage", "purchase_multiplicity")) {
  source(file.path("R", "output", paste0(file, ".R")), local = new.env())
}
