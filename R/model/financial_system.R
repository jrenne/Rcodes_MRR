# Common binary credit-event enumeration for banks and resident NBFI sectors.
# Matrix rows are creditors, columns debtors. All amounts are EUR bn.
enumerate_financial_credit_roots <- function(
  receipts, claims, debt, cutoff,
  lambda, node_labels, tolerance = 1e-9
) {
  n <- length(debt)
  stopifnot(
    n <= 12, all(dim(claims) == c(n, n)), length(receipts) == n,
    length(cutoff) == n, length(lambda) == n, length(node_labels) == n,
    all(is.finite(c(receipts, claims, debt, cutoff, lambda))),
    all(receipts >= 0), all(claims >= 0), all(debt > 0),
    all(lambda > 0 & lambda <= 1), all(cutoff <= debt / lambda + 1e-8)
  )
  sets <- binary_configurations(n) == 1L
  roots <- list()
  candidates <- vector("list", nrow(sets))
  for (k in seq_len(nrow(sets))) {
    J <- sets[k, ]
    R <- rep(1, n)
    if (any(J)) {
      scale <- lambda[J] / debt[J]
      lhs <- diag(sum(J)) - sweep(claims[J, J, drop = FALSE], 1, scale, "*")
      if (rcond(lhs) < 1e-12) stop("Singular candidate; enumeration not certified.")
      R[J] <- solve(lhs, scale * (receipts[J] + rowSums(claims[J, !J, drop = FALSE])))
    }
    V <- as.numeric(receipts + claims %*% R)
    triggered <- V < cutoff - tolerance
    mapped <- ifelse(triggered, lambda * V / debt, 1)
    retained <- all(R >= 0 & R <= 1) && identical(unname(J), unname(triggered)) &&
      all(R[J] < 1) && max(abs(R - mapped)) < 1e-10
    candidates[[k]] <- data.frame(
      candidate = k,
      candidate_defaults = if (any(J)) paste(node_labels[J], collapse = "+") else "None",
      retained = retained
    )
    if (retained) {
      roots[[length(roots) + 1L]] <- list(
        candidate = k, repayment = R,
        assets = V, default = J, map_residual = max(abs(R - mapped))
      )
    }
  }
  if (!length(roots)) stop("No repayment equilibrium.")
  list(roots = roots, candidates = do.call(rbind, candidates))
}

build_bank_nbfi_system <- function(
  bank_reference, matrix, public_buffers,
  lambda_nbfi = 1
) {
  inp <- bank_reference$inputs
  p <- bank_reference$params
  c <- inp$country_labels
  C <- length(c)
  bank_nodes <- paste0(c, ":S122")
  nbfi_nodes <- paste0(c, ":S12nbfi")
  gov_nodes <- paste0(c, ":S13")
  d <- public_buffers[match(c, public_buffers$country), ]
  stopifnot(identical(as.character(d$country), c))
  K <- unname(matrix[bank_nodes, nbfi_nodes, drop = FALSE])
  W <- unname(matrix[nbfi_nodes, bank_nodes, drop = FALSE])
  Q <- unname(matrix[nbfi_nodes, nbfi_nodes, drop = FALSE])
  U <- unname(matrix[nbfi_nodes, gov_nodes, drop = FALSE])
  A <- rbind(cbind(inp$interbank, K), cbind(W, Q))
  N <- rbind(inp$sovereign, U)
  DM <- d$debt_pool_eur_bn
  EM <- d$loss_absorption_buffer_eur_bn
  # Bank b already includes par NBFI receipts: remove them before adding K*R_M.
  bB <- inp$fixed_resources - rowSums(K)
  bM <- DM + EM - rowSums(W) - rowSums(Q) - rowSums(U)
  D <- c(inp$debt, DM)
  E <- c(inp$equity, EM)
  b <- c(bB, bM)
  lamB <- align_country_vector(p$lambda_B, c, "lambda_B")
  lamM <- align_country_vector(lambda_nbfi, c, "lambda_nbfi")
  H <- c(bank_reference$headroom, EM)
  cutoff <- pmin(D + E - H, D / c(lamB, lamM))
  # The NBFI debt pool covers all observed loans and debt securities.
  stopifnot(
    all(b >= 0), all(D > 0), all(EM >= 0),
    max(abs(b + rowSums(A) + rowSums(N) - D - E)) < 1e-8,
    all(DM + 1e-6 >= colSums(matrix[, nbfi_nodes, drop = FALSE])),
    max(abs(cutoff[seq_len(C)] - bank_reference$default_asset_cutoff)) < 1e-8,
    max(abs(cutoff[C + seq_len(C)] - DM)) < 1e-8
  )
  list(
    inputs = inp, params = p, reference = bank_reference, countries = c,
    node_labels = c(bank_nodes, nbfi_nodes), claims = A, sovereign = N,
    debt = D, equity = E, resources = b, headroom = H, cutoff = cutoff,
    lambda = c(lamB, lamM), K = K, W = W, Q = Q, U = U, public_buffers = d,
    nbfi_buffer_above_total_endogenous_claims = EM - rowSums(W) - rowSums(Q) - rowSums(U)
  )
}

solve_bank_nbfi_default <- function(system) {
  z <- system
  C <- length(z$countries)
  iB <- seq_len(C)
  iM <- C + iB
  states <- binary_configurations(C)
  pi <- align_country_vector(z$params$pi_G, z$countries, "pi_G")
  L <- z$reference$private_loss_matrix
  details <- summary <- candidates <- first <- list()
  for (s in seq_len(nrow(states))) {
    delta <- states[s, ]
    label <- if (any(delta)) paste(z$countries[delta == 1], collapse = "+") else "None"
    G <- (1 - z$params$haircut_G * delta) / (1 - z$params$haircut_G * pi)
    loss <- c(as.numeric(L %*% delta), rep(0, C))
    constant <- as.numeric(z$resources + z$sovereign %*% G - loss)
    y <- enumerate_financial_credit_roots(
      constant, z$claims, z$debt, z$cutoff,
      z$lambda, z$node_labels
    )
    cc <- y$candidates
    cc$state <- s
    cc$sovereign_defaults <- label
    candidates[[s]] <- cc
    v0 <- constant + rowSums(z$claims)
    first[[s]] <- data.frame(
      state = s, sovereign_defaults = label, bank = z$countries,
      default_at_par = v0[iB] < z$cutoff[iB] - 1e-9,
      loss_at_par_eur_bn = z$debt[iB] + z$equity[iB] - v0[iB]
    )
    d <- do.call(rbind, lapply(y$roots, function(r) {
      data.frame(
        state = s, sovereign_defaults = label, equilibrium = r$candidate,
        node = z$node_labels, country = rep(z$countries, 2),
        sector = rep(c("bank", "nbfi"), each = C), credit_default = r$default,
        repayment = r$repayment, total_loss_eur_bn = z$debt + z$equity - r$assets,
        creditor_shortfall_eur_bn = z$debt * (1 - r$repayment),
        map_residual = r$map_residual
      )
    }))
    details[[s]] <- d
    summary[[s]] <- data.frame(
      state = s, sovereign_defaults = label,
      num_equilibria = length(y$roots), bank_defaults_at_par = sum(v0[iB] < z$cutoff[iB] - 1e-9),
      maximum_bank_defaults = max(vapply(y$roots, function(r) sum(r$default[iB]), numeric(1))),
      maximum_nbfi_defaults = max(vapply(y$roots, function(r) sum(r$default[iM]), numeric(1))),
      lowest_bank_repayment = min(d$repayment[d$sector == "bank"]),
      lowest_nbfi_repayment = min(d$repayment[d$sector == "nbfi"])
    )
  }
  list(
    system = z, by_state = do.call(rbind, summary), by_node = do.call(rbind, details),
    candidates = do.call(rbind, candidates), first_round = do.call(rbind, first)
  )
}
