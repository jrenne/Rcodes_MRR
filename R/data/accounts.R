# Appendix B: provisions, fixed settlements, private loans and consumption.
build_accounting_data <- function(output_directory) {
  out <- output_directory
  countries <- c("DEU", "FRA", "ITA", "ESP")
  iso2 <- c("DE", "FR", "IT", "ES")
  bs <- read.csv(file.path(out, "bank_balance_sheets.csv"))
  bs <- bs[match(countries, bs$country), ]
  m <- as.matrix(read.csv(file.path(out, "exposure_matrix.csv"), row.names = 1, check.names = FALSE))
  components <- read.csv("data/raw/reference/bsi_capital_components.csv")
  provisions <- vapply(iso2, function(country) {
    row <- components[components$REF_AREA == country & components$BS_ITEM == "L60E" &
      components$TIME_PERIOD == "2024-12", ]
    stopifnot(nrow(row) == 1L)
    row$OBS_VALUE / 1000
  }, numeric(1))
  equity <- bs$reported_capital - provisions
  outside <- bs$net_group_funding_bridge + bs$unallocated_other_funding + bs$outside_other_funding +
    bs$central_bank_funding
  debt <- bs$adjusted_liabilities - outside
  banks <- paste0(countries, ":S122")
  sovereigns <- paste0(countries, ":S13")
  fixed <- debt + equity - rowSums(m[banks, banks]) - rowSums(m[banks, sovereigns])
  accounts <- data.frame(
    country = countries, reported_assets = bs$reported_assets, statistical_capital = bs$reported_capital,
    asset_provisions_deducted = provisions, observed_equity = equity, adjusted_assets_less_provisions = bs$adjusted_assets -
      provisions, fixed_outside_obligations = outside, clearing_pool_debt = debt, interbank_assets = rowSums(m[
      banks,
      banks
    ]), sample_sovereign_securities = rowSums(m[banks, sovereigns]), fixed_net_resources = fixed
  )
  stopifnot(max(abs(bs$adjusted_assets - provisions - outside - debt - equity)) < 1e-07)
  write.csv(accounts, file.path(out, "bank_accounts.csv"), row.names = FALSE)
  register <- read.csv(file.path(out, "exposure_register.csv"))
  loans <- register[register$in_matrix & register$instrument == "loans", ]
  for (sector in c(firm_loans = "S11", household_loans = "S14")) {
    claims <- matrix(0, 4, 4, dimnames = list(countries, countries))
    for (i in 1:4) {
      for (j in 1:4) {
        claims[i, j] <- sum(loans$amount_eur_bn[loans$from ==
          banks[i] & loans$to == paste0(countries[j], ":", sector)])
      }
    }
    file <- if (sector == "S11") {
      "firm_loans.csv"
    } else {
      "household_loans.csv"
    }
    write.csv(claims, file.path(out, file))
  }
  source <- jsonlite::read_json("data/raw/reference/household_consumption.json", simplifyVector = TRUE)
  stopifnot(identical(source$id, c("freq", "unit", "na_item", "geo", "time")), identical(
    as.integer(source$size),
    c(1L, 1L, 1L, 4L, 1L)
  ))
  index <- source$dimension$geo$category$index
  values <- vapply(
    iso2, function(country) source$value[[as.character(index[[country]])]],
    numeric(1)
  )
  write.csv(data.frame(
    country = countries, year = 2024, consumption_eur_million = values,
    unit = "CP_MEUR", na_item = "P31_S14_S15", dataset = "nama_10_gdp"
  ), file.path(
    out,
    "household_consumption.csv"
  ), row.names = FALSE)
  # The four portfolio observations are read from ECB Chart 3.8, not estimated.
  portfolios <- read.csv("data/raw/reference/stress_portfolios.csv")
  rates <- do.call(rbind, lapply(c("firms", "households"), function(sector) {
    d <- portfolios[grepl(if (sector == "firms") {
      "corporate"
    } else {
      "retail"
    }, portfolios$portfolio), ]
    adverse <- weighted.mean(d$adverse_cumulative_rate, d$exposure_share_pct)
    baseline <- weighted.mean(d$baseline_cumulative_rate, d$exposure_share_pct)
    data.frame(
      model_sector = sector, pooled_exposure_share_pct = sum(d$exposure_share_pct),
      pooled_adverse_rate = adverse, pooled_baseline_rate = baseline, pooled_incremental_rate = adverse -
        baseline, crisis_loan_loss_rate = round(adverse - baseline, 3)
    )
  }))
  write.csv(rates, file.path(out, "stress_loss_rates.csv"), row.names = FALSE)
}
