# Rebuild the capital table from public bank disclosures and the
# country transfer from supervisory capital to resident equity. Ratios are bank medians;
# headroom/assets uses reported resident bank assets, as in the exposure table.
capital_summary <- local({
  banks <- read.csv("data/processed/bank_capital_sample.csv")
  adopted <- readRDS("outputs/baseline.rds")$system
  codes <- c(DEU = "DE", FRA = "FR", ITA = "IT", ESP = "ES")
  labels <- c(DEU = "Germany", FRA = "France", ITA = "Italy", ESP = "Spain")
  rows <- lapply(seq_along(adopted$countries), function(i) {
    country <- adopted$countries[i]
    sample <- banks[banks$country == codes[[country]], ]
    reported_assets <- adopted$inputs$accounting$reported_assets[
      match(country, adopted$inputs$accounting$country)
    ]
    stopifnot(is.finite(reported_assets), reported_assets > 0)
    stopifnot(
      nrow(sample) > 0, all(sample$rwa_native_currency > 0),
      all(sample$leverage_exposure_native_currency > 0)
    )
    data.frame(
      country = labels[[country]], banks = nrow(sample),
      cet1_ratio = 100 * median(sample$cet1_ratio_from_amounts),
      leverage_ratio = 100 * median(sample$tier1_native_currency / sample$leverage_exposure_native_currency),
      equity = adopted$equity[i], headroom = adopted$headroom[i],
      headroom_pct_assets = 100 * adopted$headroom[i] / reported_assets
    )
  })
  summary <- do.call(rbind, rows)
  stopifnot(identical(summary$banks, c(23L, 11L, 12L, 10L)))
  rowend <- paste0("\\", "\\")
  body <- vapply(seq_len(nrow(summary)), function(i) {
    r <- summary[i, ]
    paste(sprintf(
      "%s & %.1f & %.1f & %.1f & %.1f & %.1f",
      r$country, r$cet1_ratio, r$leverage_ratio, r$equity, r$headroom, r$headroom_pct_assets
    ), rowend)
  }, character(1))
  writeLines(c(
    "\\begin{tabular*}{\\textwidth}{@{\\extracolsep{\\fill}}lrrrrr@{}}",
    "\\toprule", paste(r"(Country & \shortstack{CET1 ratio\\(\%)} & \shortstack{Leverage ratio\\(\%)} & \shortstack{Equity $E_i$\\(EUR bn)} & \shortstack{Headroom $H_i$\\(EUR bn)} & \shortstack{Headroom\\(\% of assets)})", rowend),
    "\\midrule", body, "\\bottomrule", "\\end{tabular*}"
  ), "Figures/capital_headroom_summary.tex")
  summary
})
cat("Capital/headroom summary generated from country inputs.\n")
