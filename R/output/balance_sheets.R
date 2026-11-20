# Same four-country balance-sheet layout for the French-only and joint-default states.
write_balance_sheet_figures <- function(z, r) {
  D <- z$debt[seq_along(z$countries)]
  rowend <- "\\\\"
  rate <- function(v) ifelse(abs(v - 1) < 1e-10, "100\\%", sprintf("%.1f\\%%", 100 * v))
  cases <- c(FRA = "french_default_balance_sheets.tex",
    `DEU+FRA+ITA+ESP` = "joint_sovereign_default_balance_sheets.tex")
  for (state in names(cases)) {
    s <- which(r$by_state$sovereign_defaults == state)
    stopifnot(length(s) == 1L)
    fixed <- rbind(
      `Debt $D_i$` = D,
      `Opening equity $E_i$` = z$equity[1:4],
      `Headroom $H_i$` = z$headroom[1:4],
      `B--B claims` = rowSums(z$inputs$interbank)
    )
    fixed_lines <- c(
      "\\begingroup\\small",
      "\\begin{tabular*}{\\textwidth}{@{\\extracolsep{\\fill}}lrrrr@{}}",
      paste("\\multicolumn{5}{@{}l}{\\textbf{Common inputs across equilibria}}", rowend),
      "\\toprule", paste(paste(c("", z$countries), collapse = " & "), rowend), "\\midrule",
      vapply(seq_len(nrow(fixed)), function(i) paste0(
        rownames(fixed)[i], " & ", paste(sprintf("%.1f", fixed[i, ]), collapse = " & "), " ", rowend
      ), character(1)),
      "\\bottomrule", "\\end{tabular*}", "\\endgroup", "\\par\\medskip"
    )
    boxes <- c()
    for (e in seq_along(r$roots[[s]])) {
      q <- r$roots[[s]][[e]]
      boxes <- c(boxes, sprintf("\\multicolumn{4}{l}{\\textbf{Equilibrium %s}} %s", LETTERS[e], rowend))
      cells <- vapply(1:4, function(i) {
        paste0(
          "\\fcolorbox{black}{", if (q$default[i]) "black!10" else "white",
          "}{\\parbox[t]{0.212\\textwidth}{\\centering\\scriptsize\\textbf{", z$countries[i], "}\\par ",
          if (q$default[i]) "Credit event" else "Full repayment", "\\par ", rate(q$repayment[i]),
          "\\par\\smallskip\\begin{tabular}{@{}lr@{}}",
          paste(sprintf(
            "%s & %.1f", c("Resources", "B--B loss"),
            c(q$assets[i],
              as.numeric(z$inputs$interbank[i, , drop = FALSE] %*% (1 - q$repayment))
            )
          ), collapse = paste0(rowend, "\n")),
          "\\end{tabular}}}"
        )
      }, character(1))
      boxes <- c(boxes, paste(paste(cells, collapse = " & "), rowend), "\\noalign{\\vskip 8pt}")
    }
    writeLines(c(fixed_lines, "\\begin{tabular*}{\\textwidth}{@{\\extracolsep{\\fill}}llll@{}}", boxes, "\\end{tabular*}"), file.path("Figures", cases[[state]]))
  }
  invisible(NULL)
}
