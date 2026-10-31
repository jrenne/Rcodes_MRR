# Pre-shock book-value exchange of bank-held sovereign bonds for reserves.  The
# reporting database and reference calibration are inputs, never overwritten.
apply_eurosystem_sovereign_purchases <- function(system, matrix, fraction, issuers = system$countries) {
  z <- system
  C <- length(z$countries)
  banks <- seq_len(C)
  bank_nodes <- paste0(z$countries, ":S122")
  gov_nodes <- paste0(z$countries, ":S13")
  stopifnot(length(fraction) == 1, is.finite(fraction), fraction >= 0, fraction <= 1, length(issuers) >
    0, all(issuers %in% z$countries), !anyDuplicated(issuers), all(c(
    bank_nodes, gov_nodes,
    "EA:S121"
  ) %in% rownames(matrix)))
  N <- system$inputs$sovereign
  P <- fraction * sweep(N, 2, as.numeric(z$countries %in% issuers), "*")
  m <- matrix
  m[bank_nodes, gov_nodes] <- m[bank_nodes, gov_nodes] - P
  m["EA:S121", gov_nodes] <- m["EA:S121", gov_nodes] + colSums(P)
  m[bank_nodes, "EA:S121"] <- m[bank_nodes, "EA:S121"] + rowSums(P)
  z$inputs$sovereign <- N - P
  z$sovereign[banks, ] <- z$sovereign[banks, ] - P
  z$resources[banks] <- z$resources[banks] + rowSums(P)
  z$inputs$fixed_resources <- z$inputs$fixed_resources + rowSums(P)
  if (!is.null(z$inputs$accounting)) {
    a <- z$inputs$accounting
    a$sample_sovereign_securities <- rowSums(N - P)
    a$fixed_net_resources <- z$inputs$fixed_resources
    a$principal_value_identity_error <- z$inputs$fixed_resources + rowSums(z$inputs$interbank) +
      rowSums(N - P) - z$inputs$debt - z$inputs$equity
    z$inputs$accounting <- a
  }
  # Retain the private-credit scenario but discard stale pre-purchase outcomes.
  z$reference <- list(
    inputs = z$inputs, params = z$params, private_loss_matrix = system$reference$private_loss_matrix,
    private_claims = system$reference$private_claims, headroom = z$headroom[banks], default_asset_cutoff = z$cutoff[banks]
  )
  z$purchase_experiment <- list(
    timing = "Before the sovereign state; no price or credit-supply response",
    fraction = fraction, issuers = issuers, price = 1, reserve_repayment = 1, total_principal = sum(P),
    bank_sales = rowSums(P), issuer_sales = colSums(P)
  )
  stopifnot(
    min(m) >= -1e-08, max(abs(colSums(m[, gov_nodes, drop = FALSE]) - colSums(matrix[,
      gov_nodes,
      drop = FALSE
    ]))) < 1e-07, max(abs(rowSums(m[bank_nodes, , drop = FALSE]) -
      rowSums(matrix[bank_nodes, , drop = FALSE]))) < 1e-07, identical(m[, bank_nodes,
      drop = FALSE
    ], matrix[, bank_nodes, drop = FALSE]), abs(sum(m["EA:S121", ]) - sum(matrix["EA:S121", ]) - sum(P)) < 1e-07, abs(sum(m[, "EA:S121"]) - sum(matrix[, "EA:S121"]) - sum(P)) <
      1e-07, max(abs(z$resources + rowSums(z$claims) + rowSums(z$sovereign) - z$debt -
      z$equity)) < 1e-08, identical(z$debt, system$debt), identical(z$equity, system$equity),
    identical(z$headroom, system$headroom), identical(z$cutoff, system$cutoff), identical(
      z$claims,
      system$claims
    ), identical(z$lambda, system$lambda), identical(z$params, system$params)
  )
  list(system = z, matrix = m, purchases = P, units = "EUR bn")
}

summarise_eurosystem_purchase <- function(purchase, solution, household_bank_claims) {
  z <- purchase$system
  C <- length(z$countries)
  banks <- seq_len(C)
  k <- repayment_resources(z)
  S <- nrow(k$delta)
  D <- z$debt[banks]
  B <- z$inputs$interbank
  L <- colSums(B)
  T <- z$cutoff[banks]
  initial <- sweep(
    k$constant[banks, , drop = FALSE] + rowSums(z$claims[banks, , drop = FALSE]),
    1, T, "<"
  )
  state_rows <- vector("list", S)
  bank_rows <- vector("list", S)
  expected_R <- matrix(0, C, S)
  for (s in seq_len(S)) {
    roots <- solution$roots[[s]]
    R <- do.call(cbind, lapply(roots, function(r) r$repayment))
    Q <- roots[[which.max(colSums(R))]]$repayment
    gap <- pmax(Q - R, 0)
    household <- household_bank_claims %*% gap
    sufficient <- max(colSums(L * gap))
    branches <- if (length(roots) > 1) {
      minimum_floor_branches(
        B, D, z$lambda[banks], T, k$constant[banks, s] + rowSums(z$K),
        Q
      )
    } else {
      list()
    }
    capacity <- max(c(0, vapply(branches, function(v) v$budget, numeric(1))))
    maxima <- Filter(function(v) abs(v$budget - capacity) < 1e-07, branches)
    unique_at_capacity <- !any(vapply(maxima, function(v) v$attained, logical(1)))
    expected_R[, s] <- as.numeric(R %*% equilibrium_weights(roots, 0.2))
    state_rows[[s]] <- data.frame(
      state = s, sovereign_defaults = k$labels[s], bank_defaults_at_par = sum(initial[
        ,
        s
      ]), equilibria = ncol(R), maximum_equilibrium_bank_events = max(vapply(
        roots,
        function(r) sum(r$default), integer(1)
      )), minimum_bank_repayment = min(R), minimum_floor_capacity_eur_bn = capacity,
      floor_unique_at_critical_capacity = unique_at_capacity, sufficient_floor_capacity_eur_bn = sufficient,
      maximum_household_equilibrium_gap_eur_bn = max(colSums(household)), extra_eurosystem_sovereign_credit_loss_eur_bn = sum(colSums(purchase$purchases) *
        pmax(1 - k$G[s, ], 0)), extra_eurosystem_net_cash_shortfall_eur_bn = sum(colSums(purchase$purchases) *
        (1 - k$G[s, ]))
    )
    bank_rows[[s]] <- do.call(rbind, lapply(seq_along(roots), function(e) {
      data.frame(
        state = s,
        sovereign_defaults = k$labels[s], equilibrium = e, country = z$countries, repayment = R[
          ,
          e
        ], bank_credit_event = roots[[e]]$default, default_at_par = initial[, s],
        household_equilibrium_gap_eur_bn = household[, e]
      )
    }))
  }
  states <- do.call(rbind, state_rows)
  nodes <- do.call(rbind, bank_rows)
  pi <- align_country_vector(z$params$pi_G, z$countries, "pi_G")
  prob <- apply(k$delta, 1, function(d) prod(ifelse(d == 1, pi, 1 - pi)))
  expected_bank_R <- as.numeric(expected_R %*% prob)
  stopifnot(abs(sum(prob * states$extra_eurosystem_net_cash_shortfall_eur_bn)) < 1e-08)
  summary <- data.frame(
    purchase_fraction = z$purchase_experiment$fraction, purchase_eur_bn = sum(purchase$purchases),
    default_states_at_par = sum(states$bank_defaults_at_par > 0), bank_defaults_at_par = sum(states$bank_defaults_at_par),
    states_with_some_equilibrium_bank_default = sum(states$maximum_equilibrium_bank_events >
      0), multiple_states = sum(states$equilibria > 1), total_equilibria = sum(states$equilibria),
    lowest_bank_repayment = min(states$minimum_bank_repayment), minimum_floor_capacity_eur_bn = max(states$minimum_floor_capacity_eur_bn),
    floor_unique_at_common_critical_capacity = all(states$floor_unique_at_critical_capacity[abs(states$minimum_floor_capacity_eur_bn -
      max(states$minimum_floor_capacity_eur_bn)) < 1e-07]), sufficient_floor_capacity_eur_bn = max(states$sufficient_floor_capacity_eur_bn),
    maximum_household_equilibrium_gap_eur_bn = max(states$maximum_household_equilibrium_gap_eur_bn),
    maximum_extra_eurosystem_sovereign_credit_loss_eur_bn = max(states$extra_eurosystem_sovereign_credit_loss_eur_bn),
    expected_bank_shortfall_bps = 10000 * sum(D * (1 - expected_bank_R)) / sum(D)
  )
  list(summary = summary, by_state = states, by_bank = nodes)
}
