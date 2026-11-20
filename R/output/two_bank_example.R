# Per-bank accounts for the two-bank sovereign-default illustration.
D <- 100
safe <- 41
sovereign_bonds <- 20
interbank_claim <- 50
lambda <- 0.90
recovery <- c(1, 0.50, 0.50)
a <- safe + sovereign_bonds * recovery
repayment <- c(1, 1, lambda * a[3] / (D - lambda * interbank_claim))
assets <- a + interbank_claim * repayment
cost <- c(0, 0, (1 - lambda) * assets[3])
equity <- assets - D
payment <- D * repayment
stopifnot(
  abs(safe + sovereign_bonds + interbank_claim - D - 11) < 1e-12,
  all(assets[1:2] >= D), assets[3] < D,
  max(abs(payment - pmin(D, assets - cost))) < 1e-12,
  max(abs(assets - (D / 2 + D / 2 + equity))) < 1e-12
)
# Enumerate all four bank-default sets in each sovereign state.
for (state in 1:2) {
  roots <- list()
  for (mask in 0:3) {
    default <- as.logical(intToBits(mask)[1:2])
    slope <- ifelse(default, lambda / D, 0)
    intercept <- ifelse(default, lambda * a[state] / D, 1)
    B <- matrix(c(0, interbank_claim, interbank_claim, 0), 2, 2)
    R <- as.numeric(solve(diag(2) - diag(slope) %*% B, intercept))
    A <- a[state] + as.numeric(B %*% R)
    if (all(R >= 0 & R <= 1) && identical(unname(A < D), unname(default))) roots[[length(roots) + 1L]] <- R
  }
  stopifnot(length(roots) == state)
  if (state == 2) stopifnot(any(vapply(roots, function(R) max(abs(R - repayment[3])) < 1e-12, logical(1))))
}
values <- rbind(
  `Safe assets` = rep(safe, 3), `Sovereign bond receipts` = sovereign_bonds * recovery,
  `Interbank receipts` = interbank_claim * repayment,
  `Total assets before costs $A_i$` = assets,
  `Debt to the other bank` = rep(D / 2, 3), `Debt to nonbank creditors` = rep(D / 2, 3),
  `Remaining pre-cost equity $A_i-D_i$` = equity,
  `Bank distress cost` = cost, `Total debt payment $D_iR_i$` = payment,
  `Repayment rate $R_i$` = 100 * repayment
)
colnames(values) <- c('No sovereign default', 'Sovereign default: full bank repayment', 'Sovereign default: both banks default')
number <- function(x) {
  out <- formatC(x, format = 'f', digits = 1)
  out <- sub('[.]0$', '', out)
  out
}
rowend <- '\\\\'
lines <- c(
  '\\begin{tabular*}{\\textwidth}{@{\\extracolsep{\\fill}}lrrr@{}}', '\\toprule',
  paste(r'( & \shortstack{No sovereign\\default} & \multicolumn{2}{c}{Sovereign default})', rowend),
  '\\cmidrule(lr){3-4}',
  paste(r'( & \shortstack{Unique\\equilibrium} & \shortstack{Full bank\\repayment} & \shortstack{Both banks\\default})', rowend),
  '\\midrule'
)
for (i in seq_len(nrow(values))) {
  if (i == 1) lines <- c(lines, paste('\\multicolumn{4}{@{}l}{\\textit{Assets}}', rowend))
  if (i == 5) lines <- c(lines, '\\addlinespace', paste('\\multicolumn{4}{@{}l}{\\textit{Debt and remaining equity}}', rowend))
  if (i == 8) lines <- c(lines, '\\midrule')
  cells <- if (i == 10) paste0(sprintf('%.1f', values[i, ]), '\\%') else number(values[i, ])
  lines <- c(lines, paste0(rownames(values)[i], ' & ', paste(cells, collapse = ' & '), ' ', rowend))
}
writeLines(c(lines, '\\bottomrule', '\\end{tabular*}'), 'Figures/two_bank_sovereign_balance_sheets.tex')
write.csv(data.frame(item = rownames(values), values, check.names = FALSE),
  'Figures/two_bank_sovereign_balance_sheets_data.csv', row.names = FALSE)
cat('Verified one equilibrium before sovereign default and two afterwards; all balance sheets reconcile.\n')
