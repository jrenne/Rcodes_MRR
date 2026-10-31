# Rebuild the capital table from public bank disclosures and the
# country transfer from supervisory capital to resident equity. Ratios are bank medians;
# amounts and headroom/equity use the resident model blocks.
capital_summary <- local({
  banks <- read.csv("data/processed/bank_capital_sample.csv")
  adopted <- readRDS("outputs/baseline.rds")$system
  codes <- c(DEU = "DE", FRA = "FR", ITA = "IT", ESP = "ES")
  labels <- c(DEU = "Germany", FRA = "France", ITA = "Italy", ESP = "Spain")
  rows <- lapply(seq_along(adopted$countries), function(i) {
    country <- adopted$countries[i]
    sample <- banks[banks$country == codes[[country]], ]
    stopifnot(
      nrow(sample) > 0, all(sample$rwa_native_currency > 0),
      all(sample$leverage_exposure_native_currency > 0)
    )
    data.frame(
      country = labels[[country]], banks = nrow(sample),
      cet1_ratio = 100 * median(sample$cet1_ratio_from_amounts),
      leverage_ratio = 100 * median(sample$tier1_native_currency / sample$leverage_exposure_native_currency),
      equity = adopted$equity[i], headroom = adopted$headroom[i],
      headroom_pct_equity = 100 * adopted$headroom[i] / adopted$equity[i]
    )
  })
  summary <- do.call(rbind, rows)
  stopifnot(identical(summary$banks, c(23L, 11L, 12L, 10L)))
  rowend <- paste0("\\", "\\")
  body <- vapply(seq_len(nrow(summary)), function(i) {
    r <- summary[i, ]
    paste(sprintf(
      "%s & %.1f & %.1f & %.1f & %.1f & %.1f",
      r$country, r$cet1_ratio, r$leverage_ratio, r$equity, r$headroom, r$headroom_pct_equity
    ), rowend)
  }, character(1))
  writeLines(c(
    "\\begin{tabular*}{\\textwidth}{@{\\extracolsep{\\fill}}lrrrrr@{}}",
    "\\toprule", paste(r"(Country & \shortstack{CET1 ratio\\(\%)} & \shortstack{Leverage ratio\\(\%)} & \shortstack{Equity $E_i$\\(EUR bn)} & \shortstack{Headroom $H_i$\\(EUR bn)} & \shortstack{Headroom\\(\% of equity)})", rowend),
    "\\midrule", body, "\\bottomrule", "\\end{tabular*}"
  ), "Figures/capital_headroom_summary.tex")
  summary
})
cat("Capital/headroom summary generated from country inputs.\n")
