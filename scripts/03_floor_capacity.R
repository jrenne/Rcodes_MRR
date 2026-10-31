source("R/setup.R")
# Section 5: evaluate each admissible default-regime endpoint, including equality.
x <- readRDS("outputs/equilibria.rds")
z <- x$baseline$system
k <- repayment_resources(z)
banks <- seq_along(z$countries)
B <- z$inputs$interbank
D <- z$debt[banks]
lambda <- z$lambda[banks]
T <- z$cutoff[banks]
constants <- k$constant[banks, , drop = FALSE] + rowSums(z$K)
weights <- colSums(B)
records <- intervals <- list()
for (s in seq_along(k$labels)) {
  a <- constants[, s]
  Q <- x$floors[, s]
  original <- x$baseline$result$roots[[s]]
  sufficient <- sum(weights * (Q - Reduce(pmin, lapply(original, `[[`, "repayment"))))
  curves <- minimum_floor_branches(B, D, lambda, T, a, Q)
  critical <- max(c(0, vapply(curves, `[[`, numeric(1), "budget")))
  maxima <- Filter(function(v) abs(v$budget - critical) < 1e-7, curves)
  unique_at_threshold <- !any(vapply(maxima, `[[`, logical(1), "attained"))
  best <- if (length(maxima)) maxima[[1]] else NULL
  exit_banks <- if (is.null(best)) {
    ""
  } else {
    paste(z$countries[best$default[
      abs(as.numeric(a + B %*% ((1 - best$theta) * best$R + best$theta * Q)) - T)[best$default] < 1e-7
    ]], collapse = ",")
  }
  records[[s]] <- data.frame(
    state = s, sovereign_defaults = k$labels[s],
    critical_budget_eur_bn = critical, sufficient_budget_eur_bn = sufficient,
    unique_at_critical = unique_at_threshold, critical_paid_fraction = if (is.null(best)) 0 else best$theta,
    critical_exit_banks = exit_banks,
    critical_default_banks = if (is.null(best)) "" else paste(z$countries[best$default], collapse = ","),
    admissible_branch_intervals = length(curves)
  )
  for (v in curves) {
    intervals[[length(intervals) + 1L]] <- data.frame(
      state = s,
      defaults = paste(z$countries[v$default], collapse = ","), theta_left = v$left, theta_right = v$right,
      theta_at_supremum = v$theta, budget_supremum_eur_bn = v$budget, supremum_attained = v$attained
    )
  }
  stopifnot(critical <= sufficient + 1e-7)
}
records <- do.call(rbind, records)
dir.create("outputs/floor_capacity", recursive = TRUE, showWarnings = FALSE)
write.csv(records, "outputs/floor_capacity/by_state.csv", row.names = FALSE)
write.csv(do.call(rbind, intervals), "outputs/floor_capacity/branch_suprema.csv", row.names = FALSE)
maximum <- records[which.max(records$critical_budget_eur_bn), ]
summary <- list(
  common_budget_threshold_eur_bn = maximum$critical_budget_eur_bn,
  common_budget_threshold_pct_bank_debt = 100 * maximum$critical_budget_eur_bn / sum(D),
  common_budget_unique_at_threshold = maximum$unique_at_critical, binding_state = maximum$sovereign_defaults,
  sufficient_bound_eur_bn = max(records$sufficient_budget_eur_bn)
)
jsonlite::write_json(summary, "outputs/floor_capacity/summary.json", auto_unbox = TRUE, pretty = TRUE, digits = 12)
positive <- records[records$critical_budget_eur_bn > 0, ]
rows <- sprintf(
  "%s & %.1f & %.1f", positive$sovereign_defaults,
  positive$critical_budget_eur_bn, positive$sufficient_budget_eur_bn
)
zero_states <- sum(records$critical_budget_eur_bn <= 0)
other_label <- paste("Other", if (zero_states == 8) "eight" else zero_states, "states & 0 & 0")
table_tex(
  "Figures/minimum_floor_budget_table.tex",
  "Sovereign defaults & Minimum capacity & Sufficient bound", c(rows, paste0("\\midrule\n", other_label)), "lrr"
)
message("Repayment-floor capacity: EUR ", round(maximum$critical_budget_eur_bn, 1), " bn.")
