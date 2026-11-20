# Resident NBFIs as creditors, in the same layout as the bank-exposure table.
baseline <- readRDS("outputs/baseline.rds")
countries <- baseline$system$countries
matrix <- baseline$matrix
nbfi_nodes <- paste0(countries, ":S12nbfi")
debtors <- c(paste0(countries, ":S13"), paste0(countries, ":S122"))
claims <- matrix[nbfi_nodes, debtors, drop = FALSE]
buffers <- read.csv("data/processed/nbfi_buffers.csv")
assets <- buffers$financial_assets_eur_bn[match(countries, buffers$country)]
shares <- sweep(claims, 1, assets, "/")
stopifnot(
  all(is.finite(assets)), all(assets > 0),
  all(is.finite(claims)), all(claims >= 0), all(rowSums(shares) < 1),
  max(abs(sweep(shares, 1, assets, "*") - claims)) < 1e-10,
  max(abs(unname(claims[, 1:4]) - baseline$system$U)) < 1e-10,
  max(abs(unname(claims[, 5:8]) - baseline$system$W)) < 1e-10
)
# Independently reconcile each displayed claim to its source-register components.
register <- read.csv("data/processed/exposure_register.csv")
for (i in 1:4) for (j in 1:8) {
  rows <- register$from == nbfi_nodes[i] & register$to == debtors[j] & register$in_matrix
  stopifnot(abs(sum(register$amount_eur_bn[rows]) - claims[i, j]) < 1e-8)
  allowed <- if (j <= 4) "debt_securities" else c("deposits", "debt_securities")
  stopifnot(all(register$instrument[rows] %in% allowed))
}
amount <- function(x) formatC(x, format = "f", digits = 0, big.mark = ",")
cell <- function(value, share) {
  pct <- sprintf("%.0f", 100 * share)
  paste0(amount(value), " (", pct, "\\%)")
}
rowend <- "\\\\"
lines <- c(
  "\\begin{tabular*}{\\textwidth}{@{\\extracolsep{\\fill}}lrrrr@{}}",
  "\\toprule", paste(" & Germany & France & Italy & Spain", rowend), "\\midrule",
  paste("\\multicolumn{5}{@{}l}{\\textit{Sovereign bonds}}", rowend)
)
labels <- c("Germany", "France", "Italy", "Spain")
for (j in 1:8) {
  if (j == 5L) lines <- c(lines, "\\addlinespace", paste(
    "\\multicolumn{5}{@{}l}{\\textit{Claims on banking systems}}", rowend
  ))
  lines <- c(lines, paste0(
    labels[(j - 1L) %% 4L + 1L], " & ",
    paste(vapply(1:4, function(i) cell(claims[i, j], shares[i, j]), character(1)), collapse = " & "),
    " ", rowend
  ))
}
lines <- c(lines, "\\midrule", paste0(
  "Total NBFI financial assets & ", paste(amount(assets), collapse = " & "), " ", rowend
), "\\bottomrule", "\\end{tabular*}")
writeLines(lines, "Figures/nbfi_exposure_table.tex")
detail <- expand.grid(creditor = countries, debtor = debtors, stringsAsFactors = FALSE)
detail$claim_eur_bn <- as.vector(claims)
detail$creditor_financial_assets_eur_bn <- rep(assets, 8)
detail$share_of_creditor_financial_assets_pct <- 100 * as.vector(shares)
write.csv(detail, "Figures/nbfi_exposure_table_data.csv", row.names = FALSE)
cat("NBFI exposure table: 32 claims reconciled to the matrix and source register.\n")
