source("R/setup.R")
# Analytic one-bank example: adverse branch ends at R=.9, theta=.8,
# B=.8*.5*(1-.9)=.04. Equality at the cutoff entails par repayment.
B <- matrix(.5, 1, 1)
D <- Q <- T <- 1
lambda <- .9
a <- .51
curves <- minimum_floor_branches(B, D, lambda, T, a, Q)
stopifnot(
  length(curves) == 1, abs(curves[[1]]$theta - .8) < 1e-11,
  abs(curves[[1]]$budget - .04) < 1e-11, !curves[[1]]$attained,
  length(minimum_floor_capped_roots(curves, Q, .04)) == 1
)
for (budget in c(0, .01, .039, .041)) {
  roots <- minimum_floor_capped_roots(curves, Q, budget)
  it <- minimum_floor_capped_least(B, D, lambda, T, a, Q, budget)
  stopifnot(
    length(roots) == if (budget < .04) 2L else 1L,
    any(vapply(roots, function(r) max(abs(it$repayment - r)) < 1e-10, logical(1)))
  )
}
# Reject apparent convergence to a non-fixed left limit at the jump.
stopifnot(inherits(try(minimum_floor_capped_least(B, D, lambda, T, a, Q, .04), silent = TRUE), "try-error"))
# Already unique par state; and a fundamental default which the floor retains.
stopifnot(length(minimum_floor_branches(B, D, lambda, T, 1, Q)) == 0)
Q <- .09 / .55
stopifnot(
  length(minimum_floor_branches(B, D, lambda, T, .1, Q)) == 0,
  abs(minimum_floor_capped_least(B, D, lambda, T, .1, Q, 0)$repayment - Q) < 1e-11
)
# An asymmetric two-bank network tests off-diagonal polynomial determinants
# and column-based creditor weights against independent capped iterations.
B <- matrix(c(.2, .3, .1, .25), 2, byrow = TRUE)
D <- T <- Q <- rep(1, 2)
lambda <- c(.9, .85)
a <- c(.51, .66)
curves <- minimum_floor_branches(B, D, lambda, T, a, Q)
stopifnot(length(curves) > 0)
for (budget in c(0, .001, .01, .1)) {
  roots <- minimum_floor_capped_roots(curves, Q, budget)
  it <- minimum_floor_capped_least(B, D, lambda, T, a, Q, budget)
  stopifnot(any(vapply(roots, function(r) max(abs(it$repayment - r)) < 1e-10, logical(1))))
  for (r in roots) {
    gap <- pmax(Q - r, 0)
    H <- sum(colSums(B) * gap)
    t <- if (H <= budget) 1 else budget / H
    V <- as.numeric(a + B %*% (r + t * gap))
    stopifnot(max(abs(ifelse(V < T, lambda * V / D, 1) - r)) < 1e-10)
  }
}
cat("Passed analytic floor thresholds, exact cutoffs, discontinuity detection and asymmetric networks.\n")
