source("R/setup.R")
manifest <- read.csv("data/source_manifest.csv")
stopifnot(
  all(file.exists(manifest$file)),
  identical(unname(tools::md5sum(manifest$file)), manifest$md5)
)
m <- as.matrix(read.csv("data/processed/exposure_matrix.csv", row.names = 1, check.names = FALSE))
e <- read.csv("data/processed/exposure_register.csv")
rebuilt <- m * 0
for (i in which(e$in_matrix)) rebuilt[e$from[i], e$to[i]] <- rebuilt[e$from[i], e$to[i]] + e$amount_eur_bn[i]
stopifnot(
  identical(dim(m), c(24L, 24L)), all(m >= 0), max(abs(m - rebuilt)) < 1e-8,
  all(nzchar(e$method)), !any(grepl("BIS_CBS", e$source))
)
a <- read.csv("data/processed/bank_accounts.csv")
stopifnot(max(abs(a$observed_equity + a$clearing_pool_debt - a$fixed_net_resources -
  a$interbank_assets - a$sample_sovereign_securities)) < 1e-7)
# Household deposits are measured claims rather than a residual used to close leverage.
raw <- read.csv("data/raw/residence_2024Q4/bsi_monthly_ea.csv")
iso2 <- c(DEU = "DE", FRA = "FR", ITA = "IT", ESP = "ES")
for (country in names(iso2)) {
  row <- raw[raw$REF_AREA == iso2[country] & raw$BS_ITEM == "L20" &
    raw$COUNT_AREA == "U6" & raw$BS_COUNT_SECTOR == "2250", ]
  got <- e[e$from == paste0(country, ":S14") & e$to == paste0(country, ":S122") & e$instrument == "deposits", ]
  stopifnot(nrow(row) == 1, nrow(got) == 1, abs(got$amount_eur_bn - row$OBS_VALUE / 1000) < 1e-9)
}
view <- aggregate_passive_counterparties(m)
core <- setdiff(rownames(m), c("ROW:S2", "UNALLOC:ISSUER", "UNALLOC:HOLDER"))
stopifnot(
  nrow(view$matrix) == 22, all(view$matrix >= 0),
  max(abs(rowSums(view$matrix)[core] - rowSums(m)[core])) < 1e-8,
  max(abs(colSums(view$matrix)[core] - colSums(m)[core])) < 1e-8
)
z <- readRDS("outputs/baseline.rds")$system
full <- build_bank_nbfi_system(z$reference, m, z$public_buffers)
grouped <- build_bank_nbfi_system(z$reference, view$matrix, z$public_buffers)
stopifnot(identical(full, grouped))
# Symmetric institutions reproduce the national block map without own claims.
for (counts in list(rep(2L, 8), rep(3L, 8))) {
  micro <- disaggregate_symmetric_blocks(z, counts)
  k <- repayment_resources(z)
  for (s in 1:16) {
    for (q in readRDS("outputs/baseline.rds")$result$roots[[s]]) {
      R <- c(q$repayment, rep(1, 4))[micro$group]
      V <- as.numeric(k$constant[micro$group, s] / counts[micro$group] + micro$claims %*% R)
      mapped <- ifelse(V < micro$cutoff - 1e-9, micro$lambda * V / micro$debt, 1)
      stopifnot(max(abs(R - mapped)) < 1e-10, all(diag(micro$claims) == 0))
    }
  }
}
cat("Passed source controls, exposure reconstruction, accounting and aggregation checks.\n")
