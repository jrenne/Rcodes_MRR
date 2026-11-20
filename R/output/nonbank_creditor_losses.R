# Gross direct-bank receipt losses of households, NBFIs and nonfinancial firms.
baseline <- readRDS("outputs/baseline.rds")
z <- baseline$system
r <- baseline$result
countries <- z$countries
bank_nodes <- paste0(countries, ":S122")
sectors <- c(Households = "S14", NBFIs = "S12nbfi", Corporates = "S11")
claims_by_sector <- lapply(sectors, function(sector) {
  claims <- baseline$matrix[paste0(countries, ":", sector), bank_nodes, drop = FALSE]
  dimnames(claims) <- list(countries, countries)
  claims
})
claims <- Reduce(`+`, claims_by_sector)
register <- read.csv("data/processed/exposure_register.csv")
for (sector in sectors) for (i in seq_along(countries)) for (j in seq_along(countries)) {
  rows <- register$from == paste0(countries[i], ":", sector) & register$to == bank_nodes[j] & register$in_matrix
  stopifnot(abs(sum(register$amount_eur_bn[rows]) - baseline$matrix[paste0(countries[i], ":", sector), bank_nodes[j]]) < 1e-8)
}
summary_rows <- sector_rows <- equilibrium_rows <- list()
for (s in seq_along(r$roots)) {
  roots <- r$roots[[s]]
  R <- do.call(cbind, lapply(roots, `[[`, "repayment"))
  target <- R[, which.max(colSums(R))]
  stopifnot(all(target - R >= -1e-10))
  gaps <- pmax(target - R, 0)
  losses_by_sector <- lapply(claims_by_sector, function(X) X %*% gaps)
  losses <- claims %*% gaps
  stopifnot(max(abs(losses - Reduce(`+`, losses_by_sector))) < 1e-9,
    all(losses >= -1e-10))
  country_max <- apply(losses, 1, max)
  summary_rows[[s]] <- data.frame(
    state = s, sovereign_defaults = r$by_state$sovereign_defaults[s], num_equilibria = length(roots),
    t(setNames(country_max, paste0(countries, "_max_shortfall_eur_bn"))),
    total_max_shortfall_eur_bn = max(colSums(losses)), check.names = FALSE
  )
  if (length(roots) == 1L) stopifnot(max(abs(losses)) < 1e-10)
  for (e in seq_along(roots)) {
    equilibrium_rows[[length(equilibrium_rows) + 1L]] <- data.frame(
      state = s, sovereign_defaults = r$by_state$sovereign_defaults[s], equilibrium = e,
      country = countries, gross_nonbank_receipt_shortfall_eur_bn = losses[, e]
    )
    for (sector in names(sectors)) sector_rows[[length(sector_rows) + 1L]] <- data.frame(
      state = s, sovereign_defaults = r$by_state$sovereign_defaults[s], equilibrium = e,
      country = countries, sector = sector,
      gross_receipt_shortfall_eur_bn = losses_by_sector[[sector]][, e]
    )
  }
}
nonbank_losses <- do.call(rbind, summary_rows)
write.csv(nonbank_losses, "outputs/nonbank_creditor_receipt_gaps.csv", row.names = FALSE)
write.csv(do.call(rbind, equilibrium_rows), "outputs/nonbank_creditor_losses_by_equilibrium.csv", row.names = FALSE)
write.csv(do.call(rbind, sector_rows), "outputs/nonbank_creditor_losses_by_sector.csv", row.names = FALSE)
cell <- function(v) ifelse(v < 1e-10, "$-$", ifelse(v < 1, "$<1$", formatC(v, format = "f", digits = 0, big.mark = ",")))
rowend <- "\\\\"
lines <- c(
  "\\begin{tabular*}{\\textwidth}{@{\\extracolsep{\\fill}}lrrrrrr@{}}", "\\toprule",
  paste(paste(c("Sovereign defaults", "Eq.", countries, "Total"), collapse = " & "), rowend), "\\midrule"
)
for (s in seq_len(nrow(nonbank_losses))) {
  values <- as.numeric(nonbank_losses[s, c(paste0(countries, "_max_shortfall_eur_bn"), "total_max_shortfall_eur_bn")])
  lines <- c(lines, paste(paste(c(nonbank_losses$sovereign_defaults[s], nonbank_losses$num_equilibria[s], cell(values)), collapse = " & "), rowend))
}
writeLines(c(lines, "\\bottomrule", "\\end{tabular*}"), "Figures/nonbank_creditor_losses_table.tex")
cat("Nonbank creditor losses: 48 claim cells reconciled; sector totals verified in all 25 equilibria.\n")
