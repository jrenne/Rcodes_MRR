# Bank and Eurosystem creditor columns; full-precision inputs remain unchanged.
write_bank_exposure_table <- function(baseline) {
  z <- baseline$system$inputs
  countries <- z$country_labels
  debtors <- c(paste0(countries, ":S13"), paste0(countries, ":S122"), "EA:S121")
  creditors <- c(paste0(countries, ":S122"), "EA:S121")
  claims <- baseline$matrix[creditors, debtors, drop = FALSE]
  eurosystem <- jsonlite::read_json("data/raw/reference/eurosystem_balance_sheet_2024.json", simplifyVector = TRUE)
  assets <- c(z$accounting$reported_assets, eurosystem$total_assets_eur_bn)
  shares <- sweep(claims, 1, assets, "/")
  stopifnot(
    identical(z$accounting$country, countries),
    all(is.finite(assets)), all(assets > 0), all(is.finite(claims)), all(claims >= 0),
    max(abs(unname(claims[1:4, 1:4]) - z$sovereign)) < 1e-10,
    max(abs(unname(claims[1:4, 5:8]) - z$interbank)) < 1e-10,
    max(abs(sweep(shares, 1, assets, "*") - claims)) < 1e-10
  )
  register <- read.csv("data/processed/exposure_register.csv")
  for (i in seq_along(creditors)) for (j in seq_along(debtors)) {
    rows <- register$from == creditors[i] & register$to == debtors[j] & register$in_matrix
    stopifnot(abs(sum(register$amount_eur_bn[rows]) - claims[i, j]) < 1e-8)
  }
  amount <- function(x) formatC(x, format = "f", digits = 0, big.mark = ",")
  cell <- function(value, share) paste0(amount(value), " (", sprintf("%.0f", 100 * share), "\\%)")
  rowend <- "\\\\"
  lines <- c(
    "\\begin{tabular*}{\\textwidth}{@{\\extracolsep{\\fill}}lrrrrr@{}}", "\\toprule",
    paste(" & Germany & France & Italy & Spain & Eurosystem", rowend), "\\midrule",
    paste("\\multicolumn{6}{@{}l}{\\textit{Sovereign bonds}}", rowend)
  )
  labels <- c("Germany", "France", "Italy", "Spain")
  row_labels <- c(labels, labels, "Eurosystem")
  for (j in seq_along(debtors)) {
    if (j == 5L) lines <- c(lines, "\\addlinespace", paste(
      "\\multicolumn{6}{@{}l}{\\textit{Claims on banking systems and the Eurosystem}}", rowend
    ))
    lines <- c(lines, paste0(
      row_labels[j], " & ",
      paste(vapply(seq_along(creditors), function(i) {
        if (creditors[i] == "EA:S121" && debtors[j] == "EA:S121") "$-$" else cell(claims[i, j], shares[i, j])
      }, character(1)), collapse = " & "),
      " ", rowend
    ))
  }
  lines <- c(lines, "\\midrule", paste0("Total assets & ", paste(amount(assets), collapse = " & "), " ", rowend),
    "\\bottomrule", "\\end{tabular*}")
  writeLines(lines, "Figures/bank_exposure_table.tex")
  detail <- expand.grid(creditor = creditors, debtor = debtors, stringsAsFactors = FALSE)
  detail$claim_eur_bn <- as.vector(claims)
  detail$creditor_total_assets_eur_bn <- rep(assets, length(debtors))
  detail$share_of_creditor_assets_pct <- 100 * as.vector(shares)
  write.csv(detail, "Figures/bank_and_eurosystem_exposure_table_data.csv", row.names = FALSE)
  cat(sprintf("Bank and Eurosystem exposure table: %d claims verified against the source register.\n", length(claims)))
  invisible(detail)
}
