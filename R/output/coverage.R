# Coverage robustness for the common backstop; enumerate all equilibria at
# each tested value. This refines a numerical sweep, not a minimum theorem.
x <- readRDS("outputs/equilibria.rds")
z <- x$baseline$system
rows <- lapply(settings$coverage_refinement, function(eta) {
  p <- solve_clearing(z, "recovery", eta = eta)
  data.frame(
    coverage_eta = eta, multiple_states = sum(p$by_state$num_equilibria > 1),
    equilibria = sum(p$by_state$num_equilibria), max_equation_residual = max(p$by_bank$map_residual)
  )
})
d <- do.call(rbind, rows)
ref <- subset(x$backstop_grid, rule == "Common recovery rule" & abs(adverse_equilibrium_probability - settings$rho) < 1e-12)
for (i in seq_len(nrow(ref))) {
  j <- which.min(abs(d$coverage_eta - ref$backstop_coverage_eta[i]))
  if (abs(d$coverage_eta[j] - ref$backstop_coverage_eta[i]) > 1e-10) next
  stopifnot(
    d$multiple_states[j] == ref$multiple_equilibrium_states[i], d$equilibria[j] == ref$total_bank_equilibria[i]
  )
}
stopifnot(max(d$max_equation_residual) < 1e-10)
write.csv(d, "outputs/sensitivities/coverage_refinement.csv", row.names = FALSE)
