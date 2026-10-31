# End-2024 public Pillar 3 ratios and the consolidated-to-resident bridge.
build_capital_data <- function(output_directory) {
  out <- output_directory
  SOURCE <- "data/raw/regulatory_headroom_2024Q4/ecb_pillar3_2024.xlsx"
  x <- as.data.frame(readxl::read_excel(SOURCE, sheet = "P3 data", trim_ws = FALSE))
  x$excel_row <- seq_len(nrow(x)) + 1L
  links <- as.data.frame(readxl::read_excel(SOURCE, sheet = "Links"))
  x <- x[x[[3]] %in% c("DE", "FR", "IT", "ES") & as.Date(x[[4]]) == as.Date("2024-12-31"), ]
  required <- c(
    "1", "2", "3", "4", "5", "6", "7", "EU 7b", "EU 7c", "EU 7d", "11", "EU 11a",
    "12", "13", "14", "EU 14e"
  )
  unit_factor <- c(
    Millions = 1e+06, Million = 1e+06, Thousands = 1000, Thousand = 1000,
    Billions = 1e+09, Units = 1
  )
  banks <- inputs <- list()
  for (lei in unique(x[[1]])) {
    d <- x[x[[1]] == lei, ]
    stopifnot(!anyDuplicated(d[[6]]), all(required %in% d[[6]]))
    rownames(d) <- d[[6]]
    val <- function(k) as.numeric(d[k, 9])
    pct <- if (val("5") > 1) {
      0.01
    } else {
      1
    }
    rate <- function(k) {
      val(k) * if (isTRUE(d[k, 8] == "Percent")) {
        0.01
      } else {
        pct
      }
    }
    amount <- function(k) {
      unit <- trimws(d[k, 8])
      stopifnot(unit %in% names(unit_factor))
      val(k) * unit_factor[[unit]]
    }
    cet1 <- amount("1")
    tier1 <- amount("2")
    total <- amount("3")
    rwa <- amount("4")
    exposure <- amount("13")
    error <- max(abs(c(cet1, tier1, total) / rwa - vapply(c("5", "6", "7"), rate, numeric(1))))
    stack_error <- abs(rate("EU 11a") - rate("EU 7d") - rate("11"))
    stopifnot(
      cet1 > 0, rwa > 0, exposure > 0, cet1 <= tier1 + 1, tier1 <= total + 1,
      error <= 6e-04, stack_error <= 6e-04
    )
    h_tscr <- rate("12") * rwa
    h_rw <- (rate("12") - rate("11")) * rwa
    usable <- rate("EU 7b") >= 0 && rate("EU 7b") <= rate("EU 7d") - 0.08 + 6e-04 &&
      rate("EU 7c") >= 0 && rate("EU 7c") <= rate("EU 7d") - 0.08 + 6e-04
    gap <- if (usable) {
      100 * (h_tscr - min(cet1 - (0.045 + rate("EU 7b")) * rwa, tier1 - (0.06 + rate("EU 7c")) *
        rwa, total - rate("EU 7d") * rwa)) / rwa
    } else {
      NA_real_
    }
    stopifnot(rate("EU 14e") >= 0.03, rate("EU 14e") < 0.15)
    h_lr <- tier1 - rate("EU 14e") * exposure
    h <- min(h_rw, h_lr)
    link <- links[links[[1]] == lei, ]
    stopifnot(nrow(link) == 1)
    banks[[length(banks) + 1L]] <- list(
      lei = lei, bank = d[[2]][1], country = d[[3]][1],
      report_pdf = link[[4]], report_excel = link[[5]], website = link[[6]], cet1_source_units = d[
        "1",
        8
      ], percentage_scale = pct, cet1_native_currency = cet1, tier1_native_currency = tier1,
      total_capital_native_currency = total, rwa_native_currency = rwa, leverage_exposure_native_currency = exposure,
      cet1_ratio_from_amounts = cet1 / rwa, cet1_above_45pct_native_currency = cet1 -
        0.045 * rwa, headroom_tscr_reported_native_currency = h_tscr, headroom_rw_ocr_native_currency = h_rw,
      headroom_leverage_native_currency = h_lr, headroom_rw_and_leverage_native_currency = h,
      headroom_rw_ocr_percentage_points_rwa = 100 * h_rw / rwa, headroom_rw_lr_share_cet1 = h / cet1,
      headroom_rw_lr_share_leverage_exposure = h / exposure, leverage_tighter_than_rw = h_lr <
        h_rw, reported_ratio_error_percentage_points = 100 * error, ocr_stack_error_percentage_points = 100 *
        stack_error, tscr_reconstruction_gap_percentage_points = gap, p2r_composition_usable_for_reconstruction = usable,
      reconstruction_gap_exceeds_005pp = usable && abs(gap) > 0.05, source_sheet = "P3 data",
      source_row_start = d["1", "excel_row"], source_row_end = d["EU 14e", "excel_row"]
    )
    for (metric in required) {
      inputs[[length(inputs) + 1L]] <- list(
        lei = lei, bank = d[[2]][1],
        country = d[[3]][1], metric = metric, value = val(metric), unit = d[metric, 8],
        variable = d[metric, 7], excel_row = d[metric, "excel_row"]
      )
    }
  }
  banks <- do.call(rbind, lapply(banks, as.data.frame))
  stopifnot(nrow(banks) == 56)
  country <- lapply(c("DE", "FR", "IT", "ES"), function(c) {
    d <- banks[banks$country == c, ]
    list(
      country = c, banks = nrow(d), median_rw_headroom_percentage_points_rwa = median(d$headroom_rw_ocr_percentage_points_rwa),
      median_rw_lr_headroom_pct_cet1 = 100 * median(d$headroom_rw_lr_share_cet1), banks_leverage_tighter_than_rw = sum(d$leverage_tighter_than_rw),
      reconstruction_gap_flags = sum(d$reconstruction_gap_exceeds_005pp), unusable_composition_rows = sum(!d$p2r_composition_usable_for_reconstruction)
    )
  })
  public_export(out, "bank_capital_sample.csv", banks)
  public_export(out, "country_unweighted_medians.csv", country)
  public_export(out, "source_cells.csv", inputs)
  sbs <- public_sbs(c(
    Equity = "sbs_accounting_equity_eur_bn", `CET1 amount` = "sbs_cet1_eur_bn",
    `Total risk exposure amount` = "sbs_rwa_eur_bn", `Number of entities` = "sbs_banks"
  ))
  km1path <- file.path(out, "bank_capital_sample.csv")
  if (!file.exists(km1path)) {
    stop("Run scripts/examine_regulatory_headroom.R first.")
  }
  banks <- read.csv(km1path)
  x <- as.data.frame(readxl::read_excel(SOURCE, sheet = "P3 data"))
  x$excel_row <- seq_len(nrow(x)) + 1L
  x <- x[x[[3]] %in% c("DE", "FR", "IT", "ES") & as.Date(x[[4]]) == as.Date("2024-12-31") &
    x[[6]] == "EU 14c", ]
  map <- c(DE = "DEU", FR = "FRA", IT = "ITA", ES = "ESP")
  ratios <- list()
  for (i in seq_len(nrow(banks))) {
    b <- banks[i, ]
    lr <- x[x[[1]] == b$lei, ]
    stopifnot(nrow(lr) == 1)
    minimum_lr <- lr[[9]] * if (isTRUE(lr[[8]] == "Percent")) {
      0.01
    } else {
      b$percentage_scale
    }
    stopifnot(minimum_lr >= 0.03, minimum_lr < 0.15)
    minimum <- min(b$headroom_tscr_reported_native_currency, b$tier1_native_currency -
      minimum_lr * b$leverage_exposure_native_currency) / b$cet1_native_currency
    isa <- b$cet1_above_45pct_native_currency / b$cet1_native_currency
    ocr <- b$headroom_rw_and_leverage_native_currency / b$cet1_native_currency
    stopifnot(ocr > 0, ocr <= minimum + 1e-10, minimum <= isa + 1e-10, isa <= 1)
    ratios[[i]] <- list(
      country = unname(map[b$country]), lei = b$lei, bank = b$bank,
      isa_share_cet1 = isa, minimum_share_cet1 = minimum, ocr_share_cet1 = ocr, minimum_leverage_rate = minimum_lr,
      leverage_source_excel_row = lr$excel_row, km1_source_row_start = b$source_row_start,
      km1_source_row_end = b$source_row_end, report_pdf = b$report_pdf
    )
  }
  ratios <- do.call(rbind, lapply(ratios, as.data.frame))
  proxies <- list()
  for (short in names(map)) {
    a <- sbs$data[[short]]
    stopifnot(length(a) == 4)
    bridge <- a$sbs_cet1_eur_bn / a$sbs_accounting_equity_eur_bn
    sample <- ratios[ratios$country == map[[short]], ]
    isa <- (a$sbs_cet1_eur_bn - 0.045 * a$sbs_rwa_eur_bn) / a$sbs_accounting_equity_eur_bn
    p <- c(list(country = map[[short]]), a, list(
      km1_banks = nrow(sample), sbs_cet1_share_accounting_equity = bridge,
      isa_headroom_share_equity = isa
    ))
    for (metric in c("isa", "minimum", "ocr")) {
      for (label in c("q25", "median", "q75")) {
        q <- c(q25 = 0.25, median = 0.5, q75 = 0.75)[[label]]
        value <- unname(quantile(sample[[paste0(metric, "_share_cet1")]], q, type = 7))
        p[[paste0(metric, "_", label, "_share_cet1")]] <- value
        p[[paste0(metric, "_", label, "_headroom_share_equity")]] <- bridge * value
      }
    }
    stopifnot(p$ocr_median_headroom_share_equity > 0, p$ocr_median_headroom_share_equity <=
      p$minimum_median_headroom_share_equity, p$minimum_median_headroom_share_equity <=
      isa, isa <= 1)
    proxies[[length(proxies) + 1L]] <- p
  }
  public_export(out, "capital_proxies.csv", proxies)
  public_export(out, "capital_proxy_by_bank.csv", ratios)
  public_export(out, "sbs_source_cells.csv", sbs$cells)
}
public_export <- function(out, name, rows) {
  if (is.list(rows) && !is.data.frame(rows)) {
    rows <- do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors = FALSE))
  }
  write.csv(rows, file.path(out, name), row.names = FALSE, na = "")
}
public_sbs <- function(metrics) {
  x <- as.data.frame(readxl::read_excel("data/raw/regulatory_headroom_2024Q4/ecb_sbs_2024Q4.xlsx",
    sheet = "Data"
  ))
  selected <- which(x[[22]] == 20244 & x[[10]] %in% c("DE", "FR", "IT", "ES") & x[[14]] ==
    "_T" & x[[3]] %in% names(metrics))
  data <- cells <- list()
  map <- c(DE = "DEU", FR = "FRA", IT = "ITA", ES = "ESP")
  for (i in selected) {
    country <- x[[10]][i]
    key <- metrics[[x[[3]][i]]]
    value <- as.numeric(x[[7]][i])
    if (!is.null(data[[country]][[key]])) {
      stopifnot(data[[country]][[key]] == value)
      next
    }
    data[[country]][[key]] <- value
    cells[[length(cells) + 1L]] <- list(
      country = unname(map[country]), source_workbook = "ecb_sbs_2024Q4.xlsx",
      sheet = "Data", excel_row = i + 1L, series = x[[1]][i], metric = x[[3]][i], value = value,
      unit = if (x[[3]][i] == "Number of entities") "banks" else "EUR bn (publisher layout)"
    )
  }
  list(data = data, cells = cells)
}
