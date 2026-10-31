# Assemble the bank accounting reference and the two-test distress cutoff.
prepare_bank_reference <- function(
  inputs, params, headroom, private_claims, private_loss_rate = 0,
  private_loss_matrix = NULL
) {
  C <- length(inputs$country_labels)
  H <- align_country_vector(headroom, inputs$country_labels, "headroom")
  lambda <- align_country_vector(params$lambda_B, inputs$country_labels, "lambda_B")
  pi <- align_country_vector(params$pi_G, inputs$country_labels, "pi_G")
  B <- inputs$interbank
  N <- inputs$sovereign
  D <- inputs$debt
  E <- inputs$equity
  b <- inputs$fixed_resources
  P <- private_claims
  L <- if (is.null(private_loss_matrix)) {
    private_loss_rate * P
  } else {
    private_loss_matrix
  }
  stopifnot(
    all(is.finite(c(B, N, D, E, b, H, P, lambda))), all(c(B, N, b, P, H) >= 0),
    all(D > 0), all(H <= E), all(lambda > 0 & lambda <= 1), length(private_loss_rate) ==
      1L, is.finite(private_loss_rate), private_loss_rate >= 0, private_loss_rate <=
      1, all(dim(P) == c(C, C)), all(dim(L) == c(C, C)), all(is.finite(L)), all(L >=
      0), all(L <= P + 1e-09), all(b - rowSums(L) >= 0), max(abs(b + rowSums(B) + rowSums(N) -
      D - E)) < 1e-08, all(pi >= 0 & pi < 1), params$haircut_G >= 0, params$haircut_G <=
      1
  )
  cutoff <- pmin(D + E - H, D / lambda)
  list(
    inputs = inputs, headroom = H, params = params, private_claims = P,
    private_loss_matrix = L, default_asset_cutoff = cutoff
  )
}
