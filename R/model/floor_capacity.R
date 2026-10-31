# Numerical default-regime enumeration for a proportionally capped repayment floor.
# Polynomials are numeric vectors in ascending powers; only base R is needed.
floor_poly_add <- function(a, b) {
  n <- max(length(a), length(b))
  length(a) <- length(b) <- n
  a[is.na(a)] <- 0
  b[is.na(b)] <- 0
  a + b
}
floor_poly_sub <- function(a, b) floor_poly_add(a, -b)
floor_poly_mul <- function(a, b) {
  v <- numeric(length(a) + length(b) - 1L)
  for (i in seq_along(a)) v[i + seq_along(b) - 1L] <- v[i + seq_along(b) - 1L] + a[i] * b
  v
}
floor_poly_eval <- function(p, t) {
  v <- 0
  for (i in rev(seq_along(p))) v <- v * t + p[i]
  v
}
floor_poly_deriv <- function(p) if (length(p) > 1L) p[-1L] * seq_len(length(p) - 1L) else 0
floor_poly_det <- function(m) {
  n <- nrow(m)
  if (n == 1L) {
    return(m[[1L]])
  }
  v <- 0
  for (j in seq_len(n)) {
    v <- floor_poly_add(v, (-1)^(1L + j) * floor_poly_mul(
      m[[1L, j]],
      floor_poly_det(m[-1L, -j, drop = FALSE])
    ))
  }
  v
}
floor_unit_roots <- function(p) {
  while (length(p) > 1L && abs(tail(p, 1)) < 1e-13 * max(1, abs(p))) p <- head(p, -1L)
  if (length(p) <= 1L) {
    return(numeric())
  }
  r <- polyroot(p)
  sort(Re(r)[abs(Im(r)) < 1e-7 & Re(r) > 1e-10 & Re(r) < 1 - 1e-10])
}

minimum_floor_branches <- function(B, D, lambda, T, a, Q) {
  n <- length(D)
  scale <- lambda / D
  result <- list()
  theta <- c(0, 1)
  stopifnot(
    all(dim(B) == c(n, n)), length(a) == n, length(Q) == n,
    all(is.finite(c(B, D, lambda, T, a, Q))), all(B >= 0), all(D > 0),
    all(lambda > 0 & lambda <= 1), all(a >= 0), all(Q >= 0 & Q <= 1),
    all(T > 0 & T <= D / lambda + 1e-10), max(lambda * colSums(B) / D) < 1
  )
  for (size in seq_len(n)) {
    for (J in combn(n, size, simplify = FALSE)) {
      S <- setdiff(seq_len(n), J)
      if (length(S) && any(Q[S] < 1 - 1e-10)) next
      A <- sweep(B[J, J, drop = FALSE], 1, scale[J], "*")
      m <- matrix(vector("list", size * size), size, size)
      for (i in seq_len(size)) for (j in seq_len(size)) m[[i, j]] <- c(as.numeric(i == j) - A[i, j], A[i, j])
      den <- floor_poly_det(m)
      rhs0 <- scale[J] * (a[J] + rowSums(B[J, S, drop = FALSE]))
      rhs1 <- scale[J] * as.numeric(B[J, J, drop = FALSE] %*% Q[J])
      nums <- rep(list(den), n)
      for (j in seq_along(J)) {
        replaced <- m
        for (i in seq_len(size)) replaced[[i, j]] <- c(rhs0[i], rhs1[i])
        nums[[J[j]]] <- floor_poly_det(replaced)
      }
      effective <- lapply(seq_len(n), function(j) {
        floor_poly_add(
          floor_poly_mul(c(1, -1), nums[[j]]), floor_poly_mul(theta, Q[j] * den)
        )
      })
      inequalities <- lapply(seq_len(n), function(i) {
        v <- a[i] * den
        for (j in seq_len(n)) v <- floor_poly_add(v, B[i, j] * effective[[j]])
        floor_poly_sub(v, T[i] * den)
      })
      upper <- lapply(seq_len(n), function(i) floor_poly_sub(Q[i] * den, nums[[i]]))
      points <- sort(unique(round(c(0, 1, unlist(lapply(c(inequalities, nums, upper), floor_unit_roots))), 13)))
      demand <- 0
      for (j in seq_len(n)) demand <- floor_poly_add(demand, colSums(B)[j] * upper[[j]])
      bn <- floor_poly_mul(theta, demand)
      deriv <- floor_poly_sub(
        floor_poly_mul(floor_poly_deriv(bn), den),
        floor_poly_mul(bn, floor_poly_deriv(den))
      )
      # Capture each regime in its own environment before advancing the loop.
      curve <- local({
        nums <- nums
        den <- den
        J <- J
        S <- S
        bn <- bn
        deriv <- deriv
        repayment <- function(t) vapply(nums, floor_poly_eval, numeric(1), t = t) / floor_poly_eval(den, t)
        valid <- function(t, tol = 1e-8) {
          R <- repayment(t)
          V <- as.numeric(a + B %*% ((1 - t) * R + t * Q))
          all(R >= -tol) && all(R <= Q + tol) && all(V[J] < T[J] - tol) &&
            all(V[S] >= T[S] - tol) && max(abs(R - Q)) > 1e-8
        }
        list(
          default = J, repayment = repayment, valid = valid, budget_num = bn,
          denominator = den, derivative_num = deriv,
          budget_at = function(t) floor_poly_eval(bn, t) / floor_poly_eval(den, t)
        )
      })
      for (i in seq_len(length(points) - 1L)) {
        left <- points[i]
        right <- points[i + 1L]
        if (!curve$valid((left + right) / 2)) next
        # Capacity is nondecreasing on each admissible branch under the
        # weighted internal-exposure bound checked above. Its supremum is
        # therefore the right endpoint; evaluate the strict cutoff separately.
        peak <- right
        result[[length(result) + 1L]] <- c(curve, list(
          left = left, right = right, theta = peak,
          budget = curve$budget_at(peak), attained = peak < 1 && curve$valid(peak), R = curve$repayment(peak)
        ))
      }
      # Weak survival inequalities may admit an isolated boundary point.
      for (point in head(points, -1L)) {
        if (curve$valid(point) &&
          !any(vapply(result, function(v) {
            identical(v$default, J) &&
              v$left <= point && v$right >= point
          }, logical(1)))) {
          result[[length(result) + 1L]] <- c(curve, list(
            left = point, right = point, theta = point,
            budget = curve$budget_at(point), attained = TRUE, R = curve$repayment(point)
          ))
        }
      }
    }
  }
  result
}

minimum_floor_capped_roots <- function(branches, Q, budget) {
  stopifnot(length(budget) == 1L, is.finite(budget), budget >= 0)
  roots <- list(Q)
  for (v in branches) {
    eq <- floor_poly_sub(v$budget_num, budget * v$denominator)
    candidates <- floor_unit_roots(eq)
    if (abs(floor_poly_eval(eq, 0)) < 1e-10) candidates <- c(candidates, 0)
    for (t in candidates) {
      if (t >= v$left - 1e-9 && t <= v$right + 1e-9 && v$valid(t)) {
        R <- v$repayment(t)
        if (!any(vapply(roots, function(r) max(abs(R - r)) < 1e-9, logical(1)))) roots[[length(roots) + 1L]] <- R
      }
    }
  }
  roots
}

minimum_floor_capped_least <- function(B, D, lambda, T, a, Q, budget, iterations = 10000L) {
  R <- numeric(length(D))
  for (step in seq_len(iterations)) {
    gap <- pmax(Q - R, 0)
    H <- sum(colSums(B) * gap)
    theta <- if (H <= budget) 1 else budget / H
    V <- as.numeric(a + B %*% (R + theta * gap))
    updated <- ifelse(V < T, lambda * V / D, 1)
    if (max(abs(updated - R)) < 1e-13) {
      # An upward sequence can stall at a non-fixed left limit of a jump.
      # Iteration cannot certify this endpoint: require regime enumeration.
      near_jump <- abs(V - T) < 1e-11 * pmax(1, abs(T)) & 1 - lambda * T / D > 1e-8 & updated < 1 - 1e-8
      if (any(near_jump)) stop("Iteration approaches a distress discontinuity; use regime enumeration.")
      return(list(repayment = updated, steps = step))
    }
    R <- updated
  }
  stop("Capped iteration did not converge.")
}
