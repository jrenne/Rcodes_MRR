# Reweight saved equilibria; preserve marginal default probabilities and payoffs.
x <- readRDS("outputs/equilibria.rds")
z <- x$baseline$system
k <- repayment_resources(z)
p <- align_country_vector(z$params$pi_G, z$countries, "pi_G")
delta <- k$delta
independent <- apply(delta, 1, function(d) prod(ifelse(d == 1, p, 1 - p)))
# Shared uniform shock: country i defaults iff U < p_i.
# Mixing this distribution with independence preserves every marginal.
shared <- rep(0, nrow(delta))
knots <- sort(unique(c(0, p, 1)))
for (j in seq_len(length(knots) - 1L)) {
  d <- as.integer((knots[j] + knots[j + 1]) / 2 < p)
  state <- which(apply(delta, 1, function(a) all(a == d)))
  stopifnot(length(state) == 1L)
  shared[state] <- shared[state] + knots[j + 1] - knots[j]
}
crisis <- rowSums(delta) > 0
D <- sum(z$debt[seq_along(z$countries)])
L <- colSums(z$inputs$interbank)
state_means <- function(sol) {
  do.call(rbind, lapply(sol$roots, function(roots) {
    w <- equilibrium_weights(roots, .2)
    c(
      payment = sum(w * vapply(roots, function(r) sum(r$payment_by_debtor), numeric(1))),
      loss = sum(w * vapply(roots, function(r) sum(L * (1 - r$creditor_receipt)) / sum(L), numeric(1)))
    )
  }))
}
base <- state_means(x$backstop_policies$common_000)
policy <- state_means(x$backstop_policies$common_050)
ans <- do.call(rbind, lapply(c(0, .5, 1), function(alpha) {
  q <- (1 - alpha) * independent + alpha * shared
  stopifnot(all(q >= 0), abs(sum(q) - 1) < 1e-12, max(abs(colSums(delta * q) - p)) < 1e-12)
  avg <- function(v) sum(q[crisis] * v[crisis]) / sum(q[crisis])
  data.frame(
    shared_shock_weight = alpha, crisis_probability_pct = 100 * sum(q[crisis]),
    no_backstop_loss_bps = 1e4 * avg(base[, "loss"]),
    half_backstop_loss_bps = 1e4 * avg(policy[, "loss"]),
    half_backstop_payment_pct_debt = 100 * avg(policy[, "payment"]) / D,
    max_state_expected_payment_pct_debt = 100 * max(policy[, "payment"]) / D
  )
}))
ref <- summarise_backstop(x$backstop_policies$common_050, .2)
stopifnot(
  abs(ans$half_backstop_payment_pct_debt[1] - ref$conditional_payment_pct_aggregate_bank_debt) < 1e-10,
  abs(ans$half_backstop_loss_bps[1] - ref$conditional_bank_to_bank_credit_loss_bps) < 1e-10
)
write.csv(ans, "outputs/sensitivities/sovereign_dependence.csv", row.names = FALSE)
rows <- vapply(seq_len(nrow(ans)), function(j) {
  r <- ans[j, ]
  sprintf(
    "%s & %.2f & %.1f & %.1f & %.2f \\\\",
    c("Independent", "50\\% shared-shock mixture", "Fully shared shock")[j], r$crisis_probability_pct,
    r$no_backstop_loss_bps, r$half_backstop_loss_bps, r$half_backstop_payment_pct_debt
  )
}, character(1))
writeLines(
  c(
    "\\begin{tabular*}{\\textwidth}{@{\\extracolsep{\\fill}}lrrrr@{}}", "\\toprule",
    "Dependence & \\shortstack{Crisis probability\\\\(\\%)} & \\shortstack{B--B loss\\\\$\\eta=0$ (bp)} & \\shortstack{B--B loss\\\\$\\eta=.50$ (bp)} & \\shortstack{Payment\\\\$\\eta=.50$ (\\% debt)} \\\\",
    "\\midrule", rows, "\\bottomrule", "\\end{tabular*}"
  ),
  "Figures/sovereign_dependence_sensitivity.tex"
)
