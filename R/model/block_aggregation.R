# Exact symmetric disaggregation, for an aggregation diagnostic only.  Off-diagonal
# within-sector claims are between distinct institutions; no institution owes itself.
# No empirical heterogeneity or new model is fitted.
disaggregate_symmetric_blocks <- function(system, counts = rep(2L, length(system$debt))) {
  z <- system
  n <- length(z$debt)
  stopifnot(length(counts) == n, all(counts >= 2), all(counts == as.integer(counts)))
  group <- rep(seq_len(n), counts)
  indices <- split(seq_along(group), group)
  claims <- matrix(0, length(group), length(group))
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      I <- indices[[i]]
      J <- indices[[j]]
      if (i != j) {
        claims[I, J] <- z$claims[i, j] / (counts[i] * counts[j])
      } else {
        a <- matrix(z$claims[i, i] / (counts[i] * (counts[i] - 1)), counts[i], counts[i])
        diag(a) <- 0
        claims[I, I] <- a
      }
    }
  }
  split_amount <- function(v) v[group] / counts[group]
  m <- list(
    group = group, counts = counts, claims = claims, debt = split_amount(z$debt),
    equity = split_amount(z$equity), headroom = split_amount(z$headroom), resources = split_amount(z$resources),
    cutoff = split_amount(z$cutoff), lambda = z$lambda[group], sovereign = sweep(z$sovereign[group, ,
      drop = FALSE
    ], 1, counts[group], "/")
  )
  reconstructed <- matrix(0, n, n)
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      reconstructed[i, j] <- sum(claims[indices[[i]],
        indices[[j]],
        drop = FALSE
      ])
    }
  }
  stopifnot(all(diag(claims) == 0), max(abs(reconstructed - z$claims)) < 1e-08, max(abs(m$resources +
    rowSums(m$claims) + rowSums(m$sovereign) - m$debt - m$equity)) < 1e-08)
  m
}
