# Compare computed results with the reference values reported in the paper.
source("R/setup.R")
reference <- readRDS("tests/fixtures/paper_results.rds")
x <- readRDS("outputs/equilibria.rds")
b <- x$baseline
stopifnot(identical(b$result$by_state$num_equilibria, reference$equilibrium_counts))
for (s in 1:16) {
  for (r in b$result$roots[[s]]) {
    stopifnot(any(vapply(reference$repayments[[s]], function(v) {
      max(abs(v - r$repayment)) < 1e-10
    }, logical(1))))
  }
}
compare <- function(a, b) {
  stopifnot(nrow(a) == nrow(b), setequal(names(a), names(b)))
  for (name in intersect(names(a), names(b))) {
    if (is.numeric(a[[name]])) {
      stopifnot(max(abs(a[[name]] - b[[name]])) < 1e-8)
    } else {
      stopifnot(identical(as.character(a[[name]]), as.character(b[[name]])))
    }
  }
}
for (name in c("dispersion", "compensation", "household")) compare(x[[name]], reference[[name]])
compare(read.csv("outputs/floor_capacity/by_state.csv"), reference$floor)
new <- readRDS("outputs/bond_purchases/bond_purchases.rds")$summary
old <- reference$purchases[match(new$case, reference$purchases$case), ]
compare(new, old)
compare(x$backstop_grid, reference$backstop)
stopifnot(
  max(abs(b$system$claims - reference$claims)) < 1e-9,
  max(abs(b$system$resources - reference$resources)) < 1e-9,
  max(abs(b$system$headroom - reference$headroom)) < 1e-9,
  max(abs(b$system$cutoff - reference$cutoff)) < 1e-9
)
cat("Passed the paper reference: every baseline root, household loss, policy grid, floor threshold and purchase case.\n")
