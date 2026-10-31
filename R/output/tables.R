table_tex <- function(path, header, rows, alignment) {
  rowend <- "\\\\"
  writeLines(c(
    paste0("\\begin{tabular*}{\\textwidth}{@{\\extracolsep{\\fill}}", alignment, "@{}}"),
    "\\toprule", paste(header, rowend), "\\midrule", paste(rows, rowend), "\\bottomrule", "\\end{tabular*}"
  ), path)
}
