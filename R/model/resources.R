# Appendix B resources and the Appendix D sensitivity to NBFI buffer availability.
apply_nbfi_buffer <- function(system, fraction = 1) {
  stopifnot(length(fraction) == 1, is.finite(fraction), fraction > 0, fraction <= 1)
  z <- system
  nbfi <- length(z$countries) + seq_along(z$countries)
  removed <- (1 - fraction) * z$equity[nbfi]
  z$equity[nbfi] <- fraction * z$equity[nbfi]
  z$headroom[nbfi] <- fraction * z$headroom[nbfi]
  z$resources[nbfi] <- z$resources[nbfi] - removed
  z$cutoff <- pmin(z$debt + z$equity - z$headroom, z$debt / z$lambda)
  z
}

repayment_resources <- function(base) {
  C <- length(base$countries)
  delta <- binary_configurations(C)
  pi <- align_country_vector(base$params$pi_G, base$countries, "pi_G")
  G <- t(matrix(vapply(seq_len(nrow(delta)), function(s) {
    (1 - base$params$haircut_G * delta[s, ]) / (1 - base$params$haircut_G * pi)
  }, numeric(C)), nrow = C))
  L <- rbind(base$reference$private_loss_matrix, matrix(0, C, C))
  constant <- base$resources + base$sovereign %*% t(G) - L %*% t(delta)
  list(
    delta = delta, G = G, constant = constant,
    labels = apply(delta, 1, function(d) {
      if (any(d)) {
        paste(base$countries[d == 1], collapse = "+")
      } else {
        "None"
      }
    })
  )
}
