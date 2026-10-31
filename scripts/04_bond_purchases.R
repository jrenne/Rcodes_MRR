source("R/setup.R")
# Pre-shock exposure counterfactual; does not update the baseline.
destination <- "outputs/bond_purchases"
dir.create(destination, recursive = TRUE, showWarnings = FALSE)
baseline <- readRDS("outputs/baseline.rds")
z <- baseline$system
HH <- baseline$matrix[paste0(z$countries, ":S14"), paste0(z$countries, ":S122"), drop = FALSE]
settings <- baseline$settings
cases <- settings$purchases
results <- list()
summary <- states <- nodes <- list()
for (name in names(cases)) {
  fraction <- cases[[name]]$fraction
  scheme <- sub("_[0-9]+$", "", name)
  purchase <- apply_eurosystem_sovereign_purchases(z, baseline$matrix, fraction, cases[[name]]$issuers)
  solution <- solve_clearing(purchase$system)
  out <- summarise_eurosystem_purchase(purchase, solution, HH, rho = settings$rho)
  results[[name]] <- list(purchase = purchase, solution = solution, results = out)
  out$summary$scheme <- scheme
  out$summary$case <- name
  summary[[name]] <- out$summary
  out$by_state$case <- name
  states[[name]] <- out$by_state
  out$by_bank$case <- name
  nodes[[name]] <- out$by_bank
}
summary <- do.call(rbind, summary)
rownames(summary) <- NULL
write.csv(summary, file.path(destination, "summary.csv"), row.names = FALSE)
write.csv(do.call(rbind, states), file.path(destination, "by_state.csv"), row.names = FALSE)
write.csv(do.call(rbind, nodes), file.path(destination, "by_bank.csv"), row.names = FALSE)
for (name in names(results)) {
  m <- results[[name]]$purchase$matrix
  write.csv(data.frame(creditor = rownames(m), m, check.names = FALSE),
    file.path(destination, paste0("matrix_", name, ".csv")),
    row.names = FALSE
  )
}
saveRDS(
  list(
    units = "EUR bn",
    summary = summary, cases = results
  ),
  file.path(destination, "bond_purchases.rds")
)

# Report the configured purchase cases.
s <- summary
stopifnot(!anyNA(s$case), all(s$floor_unique_at_common_critical_capacity))
country_names <- c(DEU = "Germany", FRA = "France", ITA = "Italy", ESP = "Spain")
labels <- vapply(cases, function(case) {
  if (case$fraction == 0) {
    return("No purchases")
  }
  issuers <- if (setequal(case$issuers, z$countries)) "All issuers" else if (identical(case$issuers, "FRA")) "France only" else paste(country_names[case$issuers], collapse = " and ")
  sprintf("%s: %.0f\\%%", issuers, 100 * case$fraction)
}, character(1))
amount <- function(x, digits = 0) ifelse(abs(x) < 1e-8, "$-$", formatC(x, digits = digits, format = "f", big.mark = ","))
rows <- paste(labels, amount(s$purchase_eur_bn), s$default_states_at_par,
  s$multiple_states, s$total_equilibria, amount(s$minimum_floor_capacity_eur_bn, 1),
  sep = " & "
)
writeLines(
  c(
    "\\begin{tabularx}{\\textwidth}{@{}>{\\raggedright\\arraybackslash}Xrrrrr@{}}",
    "\\toprule",
    r"(Purchased holdings & \shortstack{Purchase\\principal} & \shortstack{Default states\\under full\\interbank repayment} & \shortstack{Multiple\\states} & Equilibria & \shortstack{Floor\\capacity} \\)",
    "\\midrule", paste0(rows, " \\\\"), "\\bottomrule", "\\end{tabularx}"
  ),
  "Figures/eurosystem_purchase_table.tex"
)
cat("Eurosystem purchase table generated from", nrow(s), "counterfactuals.\n")
