source("R/setup.R")
# Independent checks of the abstract floor propositions. These discontinuous
# isotone maps are unrelated to the bank solver and include zero claim weights.
grid <- c(0, .5, 1)
eq <- function(x, y) max(abs(x - y)) < 1e-10
claim <- function(R, Q, w) sum(w * pmax(Q - R, 0))
effective <- function(R, Q, w, B) {
  H <- claim(R, Q, w)
  theta <- if (H <= B) 1 else B / H
  R + theta * pmax(Q - R, 0)
}
fixed <- function(F, candidates, transform = identity) {
  Filter(function(R) eq(F(transform(R)), R), candidates)
}
count_cases <- count_capacities <- count_pairs <- 0L
audit <- function(F, n, w, breaks = grid) {
  candidates <- lapply(seq_len(3^n), function(i) {
    as.numeric(as.matrix(expand.grid(rep(list(grid), n)))[i, ])
  })
  original <- fixed(F, candidates)
  stopifnot(length(original) > 0)
  Q <- Reduce(pmax, original)
  lower <- Reduce(pmin, original)
  stopifnot(
    eq(F(Q), Q), eq(F(lower), lower),
    length(fixed(F, candidates, function(R) pmax(R, Q))) == 1L
  )
  # Locate all fixed-fraction intervals by the map's known input breakpoints,
  # then compare their budget supremum with direct capped fixed-point solves.
  gamma <- list()
  for (R in candidates) {
    h <- pmax(Q - R, 0)
    H <- sum(w * h)
    if (H <= 0) next # Zero claims use theta_B=1, not an arbitrary fixed fraction.
    crossings <- unlist(lapply(which(h > 0), function(j) (breaks - R[j]) / h[j]))
    points <- sort(unique(c(0, 1, crossings[crossings > 0 & crossings < 1])))
    valid <- function(theta) theta < 1 && eq(F(R + theta * h), R)
    for (t in head(points, -1L)) {
      if (valid(t)) {
        gamma[[length(gamma) + 1L]] <- c(budget = t * H, attained = 1)
      }
    }
    for (i in seq_len(length(points) - 1L)) {
      a <- points[i]
      b <- points[i + 1L]
      if (valid((a + b) / 2)) {
        gamma[[length(gamma) + 1L]] <-
          c(budget = b * H, attained = as.integer(valid(b)))
      }
    }
  }
  critical <- max(c(0, vapply(gamma, function(g) g["budget"], numeric(1))))
  attained <- any(vapply(gamma, function(g) {
    abs(g["budget"] - critical) < 1e-10 && g["attained"] == 1
  }, logical(1)))
  stopifnot(critical <= claim(lower, Q, w) + 1e-10)
  budgets <- sort(unique(c(
    0, .005, .05, .2, .5, 2, critical,
    pmax(0, critical - 1e-7), critical + 1e-7
  )))
  adverse <- logical(length(budgets))
  for (i in seq_along(budgets)) {
    B <- budgets[i]
    roots <- fixed(F, candidates, function(R) effective(R, Q, w, B))
    stopifnot(length(roots) > 0, any(vapply(roots, eq, logical(1), y = Q)))
    adverse[i] <- length(roots) > 1
    predicted <- B < critical - 1e-10 || (abs(B - critical) < 1e-10 && attained)
    stopifnot(adverse[i] == predicted)
    for (R in roots) {
      stopifnot(
        all(R <= Q + 1e-10), all(R >= lower - 1e-10),
        claim(R, Q, w) <= claim(lower, Q, w) + 1e-10
      )
      if (!eq(R, Q)) {
        H <- claim(R, Q, w)
        theta <- B / H
        stopifnot(H > B, theta < 1, eq(F(R + theta * pmax(Q - R, 0)), R))
      }
    }
    for (R in candidates) {
      for (S in candidates) {
        if (all(R <= S)) {
          stopifnot(all(effective(R, Q, w, B) <= effective(S, Q, w, B) + 1e-10))
          count_pairs <<- count_pairs + 1L
        }
      }
    }
  }
  stopifnot(all(diff(as.integer(adverse)) <= 0))
  count_cases <<- count_cases + 1L
  count_capacities <<- count_capacities + length(budgets)
  list(critical = critical, attained = attained)
}
# The same threshold may or may not implement at equality. These examples
# differ only in whether the upward jump includes its resource boundary.
closed_exit <- audit(function(x) ifelse(x >= .75, 1, .5), 1, 1, c(0, .75, 1))
open_exit <- audit(function(x) ifelse(x > .75, 1, .5), 1, 1, c(0, .75, 1))
stopifnot(
  abs(closed_exit$critical - .25) < 1e-12, !closed_exit$attained,
  abs(open_exit$critical - .25) < 1e-12, open_exit$attained
)
# With zero weights, no genuine claim is rationed and every capacity uses
# the fully honoured map. Excluding zero-claim fixed-fraction pairs is essential.
zero <- audit(function(x) ifelse(x >= .75, 1, .5), 1, 0, c(0, .75, 1))
stopifnot(zero$critical == 0, !zero$attained)
# Random monotone two-coordinate step maps test the order comparisons beyond
# the financial model. Both left- and right-continuous jumps are represented.
set.seed(31102026)
for (h in 1:40) {
  tables <- lapply(1:2, function(j) {
    m <- matrix(sample(0:2, 9, replace = TRUE), 3, 3)
    for (i in 1:3) m[i, ] <- cummax(m[i, ])
    for (i in 1:3) m[, i] <- cummax(m[, i])
    m / 2
  })
  for (side in c("left", "right")) {
    F <- local({
      a <- tables
      direction <- side
      function(x) {
        indices <- if (direction == "left") ceiling(2 * x - 1e-12) else floor(2 * x + 1e-12)
        indices <- pmax(0, pmin(2, indices)) + 1L
        vapply(a, function(m) m[indices[1], indices[2]], numeric(1))
      }
    })
    for (w in list(c(1, 2), c(0, 1), c(0, 0))) audit(F, 2, w)
  }
}
cat(
  "Passed abstract floor-order checks:", count_cases, "maps/weight cases;",
  count_capacities, "capacity checks;", count_pairs, "ordered receipt comparisons;",
  "zero claims and both endpoint conventions.\n"
)
