# Economic claims used in the abstract, conclusion and purchase-figure discussion.
source("R/setup.R")
losses <- read.csv("outputs/nonbank_creditor_receipt_gaps.csv")
france <- losses[losses$sovereign_defaults == "FRA", ]
stopifnot(
  round(france$total_max_shortfall_eur_bn) == 196,
  round(max(losses$total_max_shortfall_eur_bn)) == 396,
  all(losses$total_max_shortfall_eur_bn[losses$num_equilibria == 1] == 0)
)

# The fine recovery grid supplies a sufficient coverage level, not a continuous minimum.
coverage <- read.csv("outputs/sensitivities/coverage_refinement.csv")
stopifnot(abs(min(coverage$coverage_eta[coverage$multiple_states == 0]) - .36) < 1e-12)

# More equilibria need not mean more defaults: check both plotted default bounds.
sweep <- read.csv("outputs/bond_purchases/multiplicity_sweep.csv")
stopifnot(nrow(sweep) == 606, length(unique(sweep$scheme)) == 6)
for (scheme in unique(sweep$scheme)) {
  d <- sweep[sweep$scheme == scheme, ]
  d <- d[order(d$purchase_fraction), ]
  stopifnot(
    all(diff(d$mean_minimum_bank_defaults) <= 1e-12),
    all(diff(d$mean_maximum_bank_defaults) <= 1e-12)
  )
}
d <- sweep[sweep$scheme == "All", ]
stopifnot(any(diff(d$multiple_states) > 0))

# Independent clearing of the Spanish example: one default, coexistence, no default.
b <- readRDS("outputs/baseline.rds")
expected <- list(1L, c(0L, 1L), 0L)
for (i in seq_along(expected)) {
  purchase <- apply_eurosystem_sovereign_purchases(
    b$system, b$matrix, c(.60, .65, .70)[i]
  )
  result <- solve_clearing(purchase$system)
  s <- which(result$by_state$sovereign_defaults == "ESP")
  counts <- sort(vapply(result$roots[[s]], function(r) sum(r$default), integer(1)))
  stopifnot(identical(counts, expected[[i]]))
}
cat("Passed creditor-loss magnitudes, refined coverage and the non-monotonic multiplicity interpretation.\n")
