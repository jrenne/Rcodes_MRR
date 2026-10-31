# Policy clearing for the residence-based specification. All amounts are EUR bn.
# Only bank creditor receipts are altered by these guarantees.
solve_clearing <- function(system, kind = c("none", "floor", "recovery"), floor = NULL, eta = 0) {
  kind <- match.arg(kind)
  z <- system
  C <- length(z$countries)
  B <- z$inputs$interbank
  D <- z$debt[1:C]
  lam <- z$lambda[1:C]
  T <- z$cutoff[1:C]
  k <- repayment_resources(z)
  S <- nrow(k$delta)
  # B has creditor rows and debtor columns. Internal creditor shares of each debtor's
  # debt therefore use column sums, including domestic claims.
  stopifnot(
    all(colSums(B) / D < 1), all(z$lambda[C + seq_len(C)] == 1), length(eta) == 1,
    eta >= 0, eta <= 1
  )
  # This reduced solver fixes NBFIs at par. Certify that reduction rather than silently
  # applying it to systems with insufficient NBFI buffers.
  banks <- seq_len(C)
  nbfi <- C + banks
  lower_B <- pmin(sweep(k$constant[banks, , drop = FALSE], 1, lam / D, "*"), 1)
  stopifnot(all(k$constant[banks, ] > 0), all(k$constant[nbfi, , drop = FALSE] + z$W %*%
    lower_B >= z$debt[nbfi] - 1e-09))
  if (kind == "floor") {
    stopifnot(all(dim(floor) == c(C, S)), all(floor >= 0 & floor <= 1))
  }
  sets <- k$delta == 1
  states <- nodes <- list()
  all_roots <- vector("list", S)
  for (s in 1:S) {
    constant <- as.numeric(k$constant[1:C, s] + rowSums(z$K))
    roots <- list()
    for (j in 1:nrow(sets)) {
      J <- sets[j, ]
      nj <- sum(J)
      regions <- if (kind == "none" || !nj) {
        matrix(FALSE, 1, nj)
      } else {
        as.matrix(expand.grid(rep(list(c(FALSE, TRUE)), nj)))
      }
      for (t in 1:nrow(regions)) {
        active <- as.logical(regions[t, ])
        # Within a binding region, creditor repayment is intercept + slope * R.
        slope <- rep(0, C)
        intercept <- rep(1, C)
        if (nj) {
          slope[J] <- 1
          intercept[J] <- 0
          if (kind == "floor") {
            slope[which(J)[active]] <- 0
            intercept[which(J)[active]] <- floor[which(J)[active], s]
          }
          if (kind == "recovery") {
            coverage <- rep(eta, C)
            slope[J] <- 1 + coverage[J] * (1 - lam[J]) / lam[J]
            slope[which(J)[active]] <- 0
            intercept[which(J)[active]] <- 1
          }
        }
        R <- rep(1, C)
        if (nj) {
          scale <- lam[J] / D[J]
          lhs <- diag(nj) - sweep(
            sweep(B[J, J, drop = FALSE], 2, slope[J], "*"), 1,
            scale, "*"
          )
          stopifnot(rcond(lhs) > 1e-12)
          R[J] <- solve(lhs, scale * (constant[J] + as.numeric(B[J, , drop = FALSE] %*%
            intercept)))
        }
        if (any(R < 0 | R > 1) || any(R[J] >= 1)) {
          next
        }
        credited <- R
        if (kind == "floor") {
          credited <- pmax(R, floor[, s])
        }
        if (kind == "recovery" && nj) {
          credited[J] <- pmin(1, R[J] * (1 + coverage[J] * (1 - lam[J]) / lam[J]))
        }
        if (max(abs(credited - (slope * R + intercept))) > 1e-09) {
          next
        }
        V <- as.numeric(constant + B %*% credited)
        trigger <- V < T - 1e-09
        mapped <- ifelse(trigger, lam * V / D, 1)
        if (!identical(unname(trigger), unname(J)) || max(abs(mapped - R)) > 1e-10) {
          next
        }
        if (any(vapply(roots, function(q) max(abs(q$repayment - R)) < 1e-09, logical(1)))) {
          next
        }
        roots[[length(roots) + 1L]] <- list(
          default_set = j, repayment = R, creditor_receipt = credited,
          assets = V, default = J, payment_by_debtor = colSums(B) * (credited - R),
          residual = max(abs(mapped - R))
        )
      }
    }
    if (!length(roots)) {
      stop("No clearing equilibrium in state ", s)
    }
    all_roots[[s]] <- roots
    states[[s]] <- data.frame(
      state = s, sovereign_defaults = k$labels[s], num_equilibria = length(roots),
      maximum_bank_defaults = max(vapply(roots, function(r) sum(r$default), numeric(1)))
    )
    for (e in seq_along(roots)) {
      r <- roots[[e]]
      nodes[[length(nodes) + 1L]] <- data.frame(
        state = s, sovereign_defaults = k$labels[s],
        equilibrium = e, default_set = r$default_set, bank = z$countries, repayment = r$repayment,
        creditor_receipt = r$creditor_receipt, credit_default = r$default, assets = r$assets,
        headroom = z$headroom[1:C], cutoff = T, payment = r$payment_by_debtor, map_residual = r$residual
      )
    }
  }
  list(
    roots = all_roots, by_state = do.call(rbind, states), by_bank = do.call(rbind, nodes),
    kind = kind, floor = floor, eta = eta, system = z
  )
}

equilibrium_weights <- function(roots, rho = 0.2) {
  stopifnot(rho >= 0, rho <= 1)
  if (length(roots) == 1) {
    return(1)
  }
  preferred <- which.max(vapply(roots, function(r) sum(r$repayment), numeric(1)))
  w <- rep(rho / (length(roots) - 1), length(roots))
  w[preferred] <- 1 - rho
  w
}

summarise_backstop <- function(solution, rho = 0.2) {
  z <- solution$system
  k <- repayment_resources(z)
  pi <- align_country_vector(z$params$pi_G, z$countries, "pi_G")
  probability <- apply(k$delta, 1, function(d) prod(ifelse(d == 1, pi, 1 - pi)))
  crisis <- rowSums(k$delta) > 0
  L <- colSums(z$inputs$interbank)
  D <- sum(z$debt[seq_along(z$countries)])
  state <- do.call(rbind, lapply(seq_along(solution$roots), function(s) {
    r <- solution$roots[[s]]
    w <- equilibrium_weights(r, rho)
    pay <- vapply(r, function(a) sum(a$payment_by_debtor), numeric(1))
    loss <- vapply(r, function(a) sum(L * (1 - a$creditor_receipt)) / sum(L), numeric(1))
    data.frame(
      state = s, expected_payment = sum(w * pay), maximum_payment = max(pay),
      interbank_loss = sum(w * loss)
    )
  }))
  data.frame(
    backstop_coverage_eta = solution$eta, adverse_equilibrium_probability = rho,
    multiple_equilibrium_states = sum(solution$by_state$num_equilibria > 1), total_bank_equilibria = sum(solution$by_state$num_equilibria),
    expected_payment_pct_aggregate_bank_debt = 100 * sum(probability * state$expected_payment) / D,
    conditional_payment_pct_aggregate_bank_debt = 100 * sum(probability[crisis] * state$expected_payment[crisis]) / sum(probability[crisis]) / D,
    worst_case_payment_pct_aggregate_bank_debt = 100 * max(state$expected_payment) / D,
    maximum_individual_equilibrium_payment_pct_debt = 100 * max(state$maximum_payment) / D,
    conditional_bank_to_bank_credit_loss_bps = 10000 * sum(probability[crisis] * state$interbank_loss[crisis]) / sum(probability[crisis])
  )
}
