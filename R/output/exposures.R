# Bank-to-sovereign and bank-to-bank claims in the baseline calibration.
# Edge widths use creditor banks' reported total assets, not clearing debt.
z <- readRDS("outputs/equilibria.rds")$baseline$system$inputs
countries <- c("DEU", "FRA", "ITA", "ESP")
stopifnot(
  identical(as.character(z$accounting$country), countries),
  all(is.finite(z$accounting$reported_assets)),
  all(z$accounting$reported_assets > 0)
)
claims <- cbind(z$sovereign, z$interbank)
dimnames(claims) <- list(countries, c(
  paste0(countries, " sovereign"),
  paste0(countries, " banks")
))
shares <- sweep(claims, 1, z$accounting$reported_assets, "/")
stopifnot(
  all(is.finite(shares)), all(shares >= 0), all(rowSums(shares) < 1),
  max(abs(sweep(shares, 1, z$accounting$reported_assets, "*") - claims)) < 1e-10
)
register <- expand.grid(
  creditor = countries, debtor = colnames(claims),
  stringsAsFactors = FALSE
)
register$claim_eur_bn <- as.vector(claims)
register$creditor_reported_assets_eur_bn <- rep(z$accounting$reported_assets, 8)
register$share_of_creditor_assets_pct <- 100 * as.vector(shares)
dir.create("Figures", showWarnings = FALSE)
write.csv(register, "Figures/bank_exposure_network_data.csv", row.names = FALSE)

# Same positions, transposed for columns identifying creditor banking systems.
amount <- function(x) formatC(x, format = "f", digits = 1, big.mark = ",")
cell <- function(value, share) {
  pct <- if (share > 0 && round(100 * share, 1) == 0) "$<0.1$" else sprintf("%.1f", 100 * share)
  paste0(amount(value), " (", pct, "\\%)")
}
table_lines <- c(
  "\\begin{tabular*}{\\textwidth}{@{\\extracolsep{\\fill}}lrrrr@{}}",
  "\\toprule", " & Germany & France & Italy & Spain \\\\", "\\midrule",
  "\\multicolumn{5}{@{}l}{\\textit{Sovereign bonds}} \\\\"
)
names_full <- c("Germany", "France", "Italy", "Spain")
for (j in 1:8) {
  if (j == 5L) {
    table_lines <- c(
      table_lines, "\\addlinespace",
      "\\multicolumn{5}{@{}l}{\\textit{Claims on banking systems}} \\\\"
    )
  }
  table_lines <- c(table_lines, paste0(
    names_full[(j - 1L) %% 4L + 1L], " & ",
    paste(vapply(1:4, function(i) cell(claims[i, j], shares[i, j]), character(1)),
      collapse = " & "
    ), " \\\\"
  ))
}
table_lines <- c(
  table_lines, "\\midrule", paste0(
    "Total bank assets & ",
    paste(amount(z$accounting$reported_assets), collapse = " & "), " \\\\"
  ),
  "\\bottomrule", "\\end{tabular*}"
)
writeLines(table_lines, "Figures/bank_exposure_table.tex")

cols <- c(DEU = "#346A93", FRA = "#338461", ITA = "#B85448", ESP = "#CB942E")
# Banks occupy the centre; their sovereigns are passive outer nodes.
banks <- rbind(
  c(-1.55, 1.05), c(1.55, 1.05),
  c(-1.55, -1.05), c(1.55, -1.05)
)
govts <- rbind(
  c(-4.1, 2.05), c(4.1, 2.05),
  c(-4.1, -2.05), c(4.1, -2.05)
)
radius <- .43
# Exact proportionality, including small claims: no minimum line-width offset.
edge_width <- function(share) 48 * share
bezier <- function(points, n = 180L) {
  t <- seq(0, 1, length.out = n)
  if (nrow(points) == 3L) {
    outer((1 - t)^2, points[1, ]) + outer(2 * t * (1 - t), points[2, ]) + outer(t^2, points[3, ])
  } else {
    outer((1 - t)^3, points[1, ]) + outer(3 * t * (1 - t)^2, points[2, ]) +
      outer(3 * t^2 * (1 - t), points[3, ]) + outer(t^3, points[4, ])
  }
}
edge <- function(path, share, colour, alpha = .65) {
  if (share <= 0) {
    return(invisible(NULL))
  }
  lw <- edge_width(share)
  colour <- adjustcolor(colour, alpha.f = alpha)
  lines(path[, 1], path[, 2], col = colour, lwd = lw)
  # Keep arrow heads legible for thin exposures without changing shaft widths.
  j <- nrow(path)
  arrows(path[j - 5, 1], path[j - 5, 2], path[j, 1], path[j, 2],
    col = colour, lwd = max(.45, lw), length = .065, angle = 22
  )
}
curve <- function(from, to, bend, sovereign = FALSE) {
  delta <- to - from
  perp <- c(-delta[2], delta[1]) / sqrt(sum(delta^2))
  middle <- (from + to) / 2 + bend * perp
  start <- from + radius * (middle - from) / sqrt(sum((middle - from)^2))
  direction <- middle - to
  if (sovereign) {
    # Intersect the rectangular node's boundary, with a small outside gap.
    end <- to + direction * min(c(.60, .37) / abs(direction))
  } else {
    end <- to + radius * direction / sqrt(sum(direction^2))
  }
  bezier(rbind(start, middle, end))
}
pdf("Figures/bank_exposure_network.pdf",
  width = 9, height = 5.3,
  family = "Helvetica", useDingbats = FALSE
)
par(mar = c(.2, .2, .2, .2), xpd = NA)
plot.new()
plot.window(xlim = c(-5.05, 5.05), ylim = c(-3.3, 2.55), asp = 1)

# Draw small exposures first so the main domestic links remain visible.
order_g <- order(as.vector(shares[, 1:4]))
for (k in order_g) {
  i <- (k - 1L) %% 4L + 1L
  g <- (k - 1L) %/% 4L + 1L
  edge(
    curve(banks[i, ], govts[g, ], if (i == g) 0 else .28, sovereign = TRUE),
    shares[i, g], cols[i], if (i == g) .85 else .45
  )
}
for (i in 1:4) {
  for (j in 1:4) {
    if (i != j) {
      edge(curve(banks[i, ], banks[j, ], .35), shares[i, j + 4L], cols[i], .7)
    }
  }
}

# Domestic interbank claims are between distinct institutions within a block.
for (i in 1:4) {
  side <- if (i <= 2L) 1 else -1
  xy <- banks[i, ]
  path <- bezier(rbind(
    xy + c(-.28, side * .33),
    xy + c(-1.05, side * 1.55),
    xy + c(1.05, side * 1.55),
    xy + c(.28, side * .33)
  ))
  edge(path, shares[i, i + 4L], cols[i], .85)
  text(xy[1], xy[2] + side * 1.08,
    sprintf("%.1f%%", 100 * shares[i, i + 4L]),
    cex = .82, col = cols[i]
  )
}

for (i in 1:4) {
  xy <- banks[i, ]
  symbols(xy[1], xy[2],
    circles = radius, inches = FALSE, add = TRUE,
    bg = cols[i], fg = "white", lwd = 1.4
  )
  text(xy[1], xy[2], paste(countries[i], "banks", sep = "\n"),
    cex = .89, col = "white", font = 2
  )
  xy <- govts[i, ]
  rect(xy[1] - .57, xy[2] - .34, xy[1] + .57, xy[2] + .34,
    col = adjustcolor(cols[i], alpha.f = .12), border = cols[i], lwd = 1.3
  )
  text(xy[1], xy[2], paste(countries[i], "sovereign", sep = "\n"),
    cex = .88, col = cols[i], font = 2
  )
  # Label domestic sovereign exposures as percentages of bank assets.
  mid <- (banks[i, ] + govts[i, ]) / 2
  text(mid[1], mid[2] + if (i <= 2L) -.19 else .19,
    sprintf("%.1f%%", 100 * shares[i, i]),
    cex = .82, col = cols[i]
  )
}
text(0, -2.7, "Arrow width: share of creditor banks' total assets", cex = .86)
for (k in 1:3) {
  share <- c(.01, .05, .10)[k]
  x <- c(-2.3, 0, 2.3)[k]
  arrows(x - .53, -3.02, x + .23, -3.02,
    lwd = edge_width(share),
    col = "#555555", length = .08, angle = 22
  )
  text(x + .45, -3.02, paste0(100 * share, "%"), pos = 4, cex = .86)
}
invisible(dev.off())
cat("Eight-node bank exposure figure, amount/share table and all 32 underlying positions generated.\n")
