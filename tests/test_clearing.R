source("R/setup.R")
x <- readRDS("outputs/equilibria.rds")
b <- x$baseline
z <- b$system
g <- x$backstop_grid
no_cost <- z
no_cost$lambda[1:4] <- 1
no_cost$params$lambda_B <- 1
no_cost$cutoff <- pmin(no_cost$debt + no_cost$equity - no_cost$headroom, no_cost$debt / no_cost$lambda)
en <- solve_clearing(no_cost)
stopifnot(all(en$by_state$num_equilibria == 1), all(en$by_bank$repayment == 1))
# Verify the open-bank-block uniqueness condition stated in the paper.
# Pi has debtor rows: its row sums are internal creditor shares of debt.
BB <- z$inputs$interbank
DD <- z$debt[1:4]
Pi <- sweep(t(BB), 1, DD, "/")
constants <- repayment_resources(z)$constant[1:4, , drop = FALSE] + rowSums(z$K)
stopifnot(
  all(constants > 0), all(rowSums(Pi) < 1),
  abs(max(rowSums(Pi)) - .17007154) < 1e-7
)
for (s in 1:16) {
  for (start in c(0, 1)) {
    p <- start * DD
    for (iter in 1:1000) {
      q <- pmin(DD, as.numeric(constants[, s] + t(Pi) %*% p))
      if (max(abs(q - p)) < 1e-10) break
      p <- q
    }
    stopifnot(iter < 1000, max(abs(q - DD)) < 1e-9)
  }
}
write.csv(
  data.frame(
    country = z$countries, clearing_debt_eur_bn = DD,
    bank_creditor_debt_eur_bn = colSums(BB), internal_debt_share = rowSums(Pi),
    outside_debt_share = 1 - rowSums(Pi),
    minimum_external_receipts_eur_bn = apply(constants, 1, min)
  ),
  "outputs/bank_creditor_shares.csv",
  row.names = FALSE
)
stopifnot(all(g$multiple_equilibrium_states[g$backstop_coverage_eta == .5] == 0))
checked <- 0L
for (y in c(list(b$result, x$floor_policy), x$backstop_policies)) {
  k <- repayment_resources(y$system)
  for (s in 1:16) {
    constant <- k$constant[1:4, s] + rowSums(z$K)
    receipt <- function(R) {
      if (y$kind == "none") {
        return(R)
      }
      if (y$kind == "floor") {
        return(pmax(R, y$floor[, s]))
      }
      coverage <- rep(y$eta, 4)
      pmin(1, R * (1 + coverage * (1 - z$lambda[1:4]) / z$lambda[1:4]))
    }
    map <- function(R) {
      V <- as.numeric(constant + z$inputs$interbank %*% receipt(R))
      ifelse(V < z$cutoff[1:4] - 1e-9, z$lambda[1:4] * V / z$debt[1:4], 1)
    }
    roots <- y$roots[[s]]
    if (y$kind == "none") {
      for (q in roots) {
        stopifnot(all(!q$default | k$delta[s, ] == 1))
      }
    }
    for (q in roots) {
      stopifnot(
        max(abs(map(q$repayment) - q$repayment)) < 1e-10,
        max(abs(receipt(q$repayment) - q$creditor_receipt)) < 1e-10,
        all(q$creditor_receipt <= 1 + 1e-10), all(q$creditor_receipt >= q$repayment - 1e-10),
        max(abs(q$payment_by_debtor - colSums(z$inputs$interbank) * (q$creditor_receipt - q$repayment))) < 1e-8
      )
    }
    for (start in c(0, 1)) {
      R <- rep(start, 4)
      for (iter in 1:1000) {
        next_R <- map(R)
        if (max(abs(R - next_R)) < 1e-13) break
        R <- next_R
      }
      vectors <- lapply(roots, `[[`, "repayment")
      endpoint <- if (start == 0) Reduce(pmin, vectors) else Reduce(pmax, vectors)
      stopifnot(iter < 1000, max(abs(next_R - endpoint)) < 1e-10)
    }
    checked <- checked + 1L
  }
}
initial <- b$reference$first_round
stopifnot(
  sum(initial$default_at_par) == 16,
  sum(b$reference$by_state$bank_defaults_at_par > 0) == 12,
  identical(as.integer(table(factor(initial$bank[initial$default_at_par],
    levels = z$countries
  ))), c(0L, 0L, 8L, 8L))
)
# Independently compute household euro gaps from measured bank claims.
for (s in 1:16) {
  roots <- b$result$roots[[s]]
  gap <- do.call(cbind, lapply(roots, function(q) {
    as.numeric(x$household_bank_claims %*% (x$floors[, s] - q$repayment))
  }))
  row <- x$household[s, ]
  stopifnot(
    max(abs(apply(gap, 1, max) - as.numeric(row[paste0(z$countries, "_max_shortfall_eur_bn")]))) < 1e-8,
    abs(max(colSums(gap)) - row$total_max_shortfall_eur_bn) < 1e-8
  )
}
# A bank can enter distress with positive equity. Full recovery-cost coverage
# must cap creditor payments at par, not transfer the remaining shareholder equity.
inp <- list(
  country_labels = c("A", "B"), interbank = matrix(c(0, 10, 10, 0), 2, 2),
  sovereign = matrix(0, 2, 2), debt = c(100, 100), equity = c(10, 10), fixed_resources = c(100, 100)
)
p <- list(r = 1, rR = 1, rB_bar = 1, kappa = 0, haircut_G = .5, pi_G = 0, lambda_B = .9)
loans <- matrix(c(6, 0, 0, 0), 2, 2)
ref <- prepare_bank_reference(inp, p, c(5, 5), loans, private_loss_matrix = loans)
nodes <- c("A:S122", "B:S122", "A:S12nbfi", "B:S12nbfi", "A:S13", "B:S13")
m <- matrix(0, 6, 6, dimnames = list(nodes, nodes))
m[1:2, 1:2] <- inp$interbank
toy <- build_bank_nbfi_system(ref, m, data.frame(
  country = c("A", "B"),
  debt_pool_eur_bn = c(100, 100), loss_absorption_buffer_eur_bn = c(100, 100)
))
t <- solve_clearing(toy, "recovery", eta = 1)
d <- t$by_bank[t$by_bank$sovereign_defaults == "A" & t$by_bank$bank == "A", ]
stopifnot(
  nrow(d) == 1, abs(d$repayment - .936) < 1e-12, d$creditor_receipt == 1,
  abs(d$payment - .64) < 1e-12, d$assets > 100
)
cat("Passed OCR publication checks, including", checked, "state and policy comparisons and synthetic no-overpayment test.\n")
# Creditor assets can exceed that creditor's own debt: only debtor-column
# liabilities are bounded by D. This valid asymmetric system must be accepted.
input_asym <- inp
input_asym$interbank <- matrix(c(0, 150, 10, 0), 2, byrow = TRUE)
input_asym$debt <- c(100, 1000)
input_asym$equity <- c(100, 100)
input_asym$fixed_resources <- input_asym$debt + input_asym$equity - rowSums(input_asym$interbank)
ref_asym <- prepare_bank_reference(input_asym, p, c(5, 5), matrix(0, 2, 2))
m_asym <- m
m_asym[1:2, 1:2] <- input_asym$interbank
asym <- build_bank_nbfi_system(ref_asym, m_asym, data.frame(
  country = c("A", "B"),
  debt_pool_eur_bn = c(100, 100), loss_absorption_buffer_eur_bn = c(100, 100)
))
stopifnot(
  any(rowSums(input_asym$interbank) / input_asym$debt > 1),
  all(colSums(input_asym$interbank) / input_asym$debt < 1),
  all(solve_clearing(asym)$by_state$num_equilibria >= 1)
)
# NBFI solvency is an enforced reduction condition, not an assumption silently
# imposed on a system whose NBFIs actually default in a sovereign crisis.
m_bad <- m
m_bad[3, 5] <- 40
bad_nbfi <- build_bank_nbfi_system(ref, m_bad, data.frame(
  country = c("A", "B"),
  debt_pool_eur_bn = c(100, 100), loss_absorption_buffer_eur_bn = c(5, 100)
))
stopifnot(
  any(solve_bank_nbfi_default(bad_nbfi)$by_node$repayment < 1),
  inherits(try(solve_clearing(bad_nbfi), silent = TRUE), "try-error")
)
cat("Passed: debtor-column debt-share guard and rejection of uncertified NBFI reduction.\n")
