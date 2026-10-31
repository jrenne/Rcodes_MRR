source("R/setup.R")
# Compare the polynomial solver with independent matrix solves and derivatives.
x <- readRDS("outputs/equilibria.rds")
z <- x$baseline$system
k <- repayment_resources(z)
constants <- k$constant[1:4, , drop = FALSE] + rowSums(z$K)
checks <- list()
count <- 0L
max_residual <- 0
max_derivative_error <- 0
# This solve uses no polynomial algebra.
direct_roots <- function(B, D, lambda, T, a, Q, theta) {
  n <- length(D)
  roots <- list()
  sets <- as.matrix(expand.grid(rep(list(c(FALSE, TRUE)), n)))
  for (h in seq_len(nrow(sets))) {
    J <- which(sets[h, ])
    S <- setdiff(seq_len(n), J)
    R <- rep(1, n)
    if (length(J)) {
      A <- sweep(B[J, J, drop = FALSE], 1, lambda[J] / D[J], "*")
      rhs <- (lambda[J] / D[J]) * (a[J] + rowSums(B[J, S, drop = FALSE]) + theta * as.numeric(B[J, J, drop = FALSE] %*% Q[J]))
      R[J] <- solve(diag(length(J)) - (1 - theta) * A, rhs)
    }
    V <- as.numeric(a + B %*% ((1 - theta) * R + theta * Q))
    if (all(R >= -1e-9) && all(R <= Q + 1e-9) && all(V[J] < T[J] - 1e-8) && all(V[S] >= T[S] + 1e-8)) roots[[length(roots) + 1L]] <- R
  }
  roots
}
audit_case <- function(B, D, lambda, T, a, Q, id, theta_grid) {
  curves <- minimum_floor_branches(B, D, lambda, T, a, Q)
  critical <- max(c(0, vapply(curves, `[[`, numeric(1), "budget")))
  w <- colSums(B)
  nu <- max(lambda * w / D)
  for (v in curves) {
    if (v$right - v$left > 1e-7) {
      for (theta in seq(v$left, v$right, length.out = 7)[2:6]) {
        J <- v$default
        A <- sweep(B[J, J, drop = FALSE], 1, lambda[J] / D[J], "*")
        M <- diag(length(J)) - (1 - theta) * A
        R <- v$repayment(theta)
        g <- Q[J] - R[J]
        V <- as.numeric(a + B %*% ((1 - theta) * R + theta * Q))
        residual <- max(abs(ifelse(V < T, lambda * V / D, 1) - R))
        max_residual <<- max(max_residual, residual)
        stopifnot(residual < 1e-9)
        predicted <- as.numeric(t(w[J]) %*% (diag(length(J)) - A) %*% solve(M, g))
        eps <- min(1e-5, (v$right - theta) / 10, (theta - v$left) / 10)
        observed <- (v$budget_at(theta + eps) - v$budget_at(theta - eps)) / (2 * eps)
        err <- abs(predicted - observed) / max(1, abs(predicted))
        max_derivative_error <<- max(max_derivative_error, err)
        stopifnot(predicted >= -1e-9, err < 1e-6, all(as.numeric(t(w[J]) %*% A) <= nu * w[J] + 1e-10))
      }
    }
  }
  for (theta in theta_grid) {
    direct <- direct_roots(B, D, lambda, T, a, Q, theta)
    branch <- lapply(Filter(function(v) theta > v$left + 1e-8 && theta < v$right - 1e-8 && v$valid(theta), curves), function(v) v$repayment(theta))
    direct_adverse <- Filter(function(R) max(abs(R - Q)) > 1e-7, direct)
    stopifnot(length(direct_adverse) == length(branch))
    for (R in direct_adverse) {
      stopifnot(any(vapply(branch, function(p) max(abs(p - R)) < 1e-8, logical(1))))
      H <- sum(w * pmax(Q - R, 0))
      cap <- theta * H
      stopifnot(cap <= critical + 1e-7)
      count <<- count + 1L
    }
  }
  data.frame(case = id, critical_budget = critical, branches = length(curves), theta_checks = length(theta_grid))
}
for (s in 1:16) checks[[length(checks) + 1L]] <- audit_case(z$inputs$interbank, z$debt[1:4], z$lambda[1:4], z$cutoff[1:4], constants[, s], x$floors[, s], paste0("state_", s), seq(.0001, .9999, length.out = 301))
# Random asymmetric networks test the algebra beyond the calibrated matrix.
set.seed(30102026)
for (h in 1:24) {
  n <- 2L + (h %% 3L)
  B <- matrix(runif(n * n), n)
  B <- sweep(B, 2, colSums(B), "/") * .6
  D <- rep(1, n)
  lambda <- runif(n, .8, .98)
  T <- pmin(runif(n, 1, 1.12), .99 * D / lambda)
  a <- runif(n, .35, .8)
  original <- direct_roots(B, D, lambda, T, a, rep(1, n), 0)
  stopifnot(length(original) > 0)
  Q <- Reduce(pmax, original)
  stopifnot(any(vapply(original, function(r) max(abs(Q - r)) < 1e-8, logical(1))))
  checks[[length(checks) + 1L]] <- audit_case(B, D, lambda, T, a, Q, paste0("random_", h), seq(.001, .999, length.out = 101))
}
# Independently recover the eight critical endpoints by a bracketed root of
# the binding resource cutoff, using matrix solves rather than polynomials.
states <- read.csv("outputs/floor_capacity/by_state.csv")
boundaries <- lapply(which(states$critical_budget_eur_bn > 0), function(s) {
  J <- match(strsplit(states$critical_default_banks[s], ",", fixed = TRUE)[[1]], z$countries)
  S <- setdiff(1:4, J)
  exit <- match(states$critical_exit_banks[s], z$countries)
  B <- z$inputs$interbank
  D <- z$debt[1:4]
  lambda <- z$lambda[1:4]
  T <- z$cutoff[1:4]
  a <- constants[, s]
  Q <- x$floors[, s]
  evaluate <- function(theta) {
    A <- sweep(B[J, J, drop = FALSE], 1, lambda[J] / D[J], "*")
    R <- rep(1, 4)
    R[J] <- solve(
      diag(length(J)) - (1 - theta) * A,
      (lambda[J] / D[J]) * (a[J] + rowSums(B[J, S, drop = FALSE]) + theta * as.numeric(B[J, J, drop = FALSE] %*% Q[J]))
    )
    list(R = R, V = as.numeric(a + B %*% ((1 - theta) * R + theta * Q)))
  }
  guess <- states$critical_paid_fraction[s]
  theta <- uniroot(function(t) evaluate(t)$V[exit] - T[exit],
    c(max(0, guess - 1e-3), min(1, guess + 1e-3)),
    tol = 1e-13
  )$root
  p <- evaluate(theta)
  capacity <- theta * sum(colSums(B) * (Q - p$R))
  error <- abs(capacity - states$critical_budget_eur_bn[s])
  stopifnot(
    error < 1e-7, abs(p$V[exit] - T[exit]) < 1e-8,
    evaluate(theta - 1e-7)$V[exit] < T[exit], evaluate(theta + 1e-7)$V[exit] > T[exit]
  )
  data.frame(state = s, theta_direct = theta, capacity_direct = capacity, capacity_difference = error)
})
boundaries <- do.call(rbind, boundaries)
cat("Audit passed:", length(checks), "cases;", count, "independent adverse roots; maximum residual", max_residual, "; derivative error", max_derivative_error, "\n")
