source("R/setup.R")
x <- readRDS("outputs/bond_purchases/bond_purchases.rds")
b <- readRDS("outputs/baseline.rds")
z <- b$system
C <- length(z$countries)
zero <- x$cases$All_issuers_000
stopifnot(
  identical(zero$purchase$matrix, b$matrix),
  identical(zero$solution$roots, b$result$roots),
  identical(zero$solution$by_bank, b$result$by_bank)
)
for (name in names(x$cases)) {
  case <- x$cases[[name]]
  q <- case$purchase
  k <- repayment_resources(q$system)
  before <- repayment_resources(z)
  direct_gain <- q$system$resources[seq_len(C)] - z$resources[seq_len(C)] -
    q$purchases %*% t(k$G)
  stopifnot(
    max(abs(k$G - before$G)) == 0,
    max(abs(k$constant[seq_len(C), ] - before$constant[seq_len(C), ] - direct_gain)) < 1e-8,
    max(abs(colSums(direct_gain) - case$results$by_state$extra_eurosystem_net_cash_shortfall_eur_bn)) < 1e-8,
    identical(q$system$equity, z$equity), identical(q$system$headroom, z$headroom),
    identical(q$system$debt, z$debt), identical(q$system$cutoff, z$cutoff),
    identical(q$system$reference$private_loss_matrix, z$reference$private_loss_matrix)
  )
  # Check the critical capacity and its endpoint for the binding state.
  ss <- case$results$by_state
  s <- which.max(ss$minimum_floor_capacity_eur_bn)
  critical <- ss$minimum_floor_capacity_eur_bn[s]
  if (critical > 0) {
    roots <- case$solution$roots[[s]]
    Q <- roots[[which.max(vapply(roots, function(r) sum(r$repayment), numeric(1)))]]$repayment
    a <- k$constant[seq_len(C), s] + rowSums(q$system$K)
    curves <- minimum_floor_branches(
      q$system$inputs$interbank, q$system$debt[seq_len(C)],
      q$system$lambda[seq_len(C)], q$system$cutoff[seq_len(C)], a, Q
    )
    for (delta in c(-1e-5, 0, 1e-5)) {
      capped <- minimum_floor_capped_roots(curves, Q, critical + delta)
      expected_unique <- delta > 0 || (delta == 0 && ss$floor_unique_at_critical_capacity[s])
      stopifnot((length(capped) == 1L) == expected_unique)
    }
  }
}
# Verify the reduced policy solver against full eight-node enumeration.
checked <- 0L
for (name in c("All_issuers_050", "France_only_050", "Italy_and_Spain_075")) {
  case <- x$cases[[name]]
  full <- solve_bank_nbfi_default(case$purchase$system)
  stopifnot(
    identical(full$by_state$num_equilibria, case$solution$by_state$num_equilibria),
    all(full$by_node$repayment[full$by_node$sector == "nbfi"] == 1)
  )
  for (s in 1:16) {
    nodes <- full$by_node[full$by_node$state == s & full$by_node$sector == "bank", ]
    full_R <- split(nodes$repayment, nodes$equilibrium)
    reduced_R <- lapply(case$solution$roots[[s]], function(r) r$repayment)
    for (r in full_R) stopifnot(any(vapply(reduced_R, function(v) max(abs(v - r)) < 1e-9, logical(1))))
    checked <- checked + 1L
  }
}
stopifnot(
  inherits(try(apply_eurosystem_sovereign_purchases(z, b$matrix, 1.01), silent = TRUE), "try-error"),
  inherits(try(apply_eurosystem_sovereign_purchases(z, b$matrix, .5, "XXX"), silent = TRUE), "try-error")
)
cat("Passed bond-purchase accounting, risk transfer, capacity endpoints and", checked, "independent eight-node checks.\n")
