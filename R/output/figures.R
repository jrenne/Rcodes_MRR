b <- readRDS("outputs/equilibria.rds")
reporting_matrix <- b$baseline$matrix
presentation <- aggregate_passive_counterparties(reporting_matrix)
M <- presentation$matrix
dir.create("outputs/model_presentation", showWarnings = FALSE)
saveRDS(presentation, "outputs/model_presentation/model_presentation.rds")
write.csv(M, "outputs/model_presentation/model_matrix_eur_bn.csv")
write.csv(presentation$node_map, "outputs/model_presentation/node_map.csv", row.names = FALSE)
# Base PDF graphics avoid platform-specific Cairo dependencies.
pdf("Figures/figure_network_2024-10-01.pdf", width = 9, height = 8.5)
par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
n <- nrow(M)
theta <- seq(0, 2 * base::pi, length.out = n + 1)[-(n + 1)]
xy <- cbind(cos(theta), sin(theta))
plot(xy, type = "n", xlim = c(-1.25, 1.25), ylim = c(-1.2, 1.2), axes = FALSE, xlab = "", ylab = "", asp = 1)
cols <- c(DEU = "#346A93", FRA = "#338461", ITA = "#B85448", ESP = "#CB942E", EA = "#8C68A5", OTHER = "#777777")
country <- sub(":.*", "", rownames(M))
colour <- cols[country]
for (i in 1:n) {
  j <- order(M[i, ], decreasing = TRUE)
  j <- head(j[M[i, j] > 0 & j != i], 5)
  for (k in j) {
    start <- xy[i, ] * .98
    end <- xy[k, ] * .96
    delta <- end - start
    control <- (start + end) / 2 + .18 * c(-delta[2], delta[1])
    t <- seq(0, 1, length.out = 80)
    curve <- outer((1 - t)^2, start) + outer(2 * t * (1 - t), control) + outer(t^2, end)
    edge_col <- adjustcolor(if (country[i] == country[k]) colour[i] else "grey50", alpha.f = .48)
    edge_width <- .6 + 5 * sqrt(M[i, k] / max(M))
    lines(curve, col = edge_col, lwd = edge_width)
    arrows(curve[77, 1], curve[77, 2], end[1], end[2],
      length = .075,
      angle = 25, col = edge_col, lwd = edge_width
    )
  }
}
points(xy, pch = 21, bg = colour, col = "white", cex = 1.5)
sector_names <- c(S11 = "Firm", S122 = "Bk", S12nbfi = "NBFI", S13 = "Gvt", S14 = "HH", S121 = "CB")
labels <- paste(country, sector_names[sub(".*:", "", rownames(M))], sep = ": ")
labels[country == "EA"] <- "Eurosystem"
labels[country == "OTHER"] <- "Other\ncounterparties"
text(1.13 * xy, labels = labels, cex = .78, col = colour)
invisible(dev.off())
# Cells can contain multiple instruments/sources; label mixtures explicitly.
# The provenance figure retains all 24 reporting accounts for auditability.
M <- reporting_matrix
n <- nrow(M)
e <- read.csv("data/processed/exposure_register.csv")
e <- e[e$in_matrix & e$amount_eur_bn > 0, ]
sources <- matrix(NA_character_, n, n, dimnames = dimnames(M))
for (i in 1:n) {
  for (j in 1:n) {
    if (M[i, j] > 0) {
      a <- unique(e$source[e$from == rownames(M)[i] & e$to == colnames(M)[j]])
      sources[i, j] <- if (!length(a)) "Reconciliation" else if (length(a) > 1) "Multiple sources" else a
    }
  }
}
levels <- sort(unique(na.omit(as.vector(sources))))
Z <- matrix(match(sources, levels), n, n)
pdf("Figures/active_source_matrix_2024-10-01.pdf", width = 9, height = 9.5)
layout(matrix(c(1, 2), 2, 1), heights = c(7.7, 1.8))
par(mar = c(7.5, 8, .5, .5))
# Distinct, moderately saturated source colours.
palette <- c(
  BSI_S122_SECURITIES_CONTROL = "#4F80AE", ECB_BSI = "#93BDDB",
  ECB_BSI_CPIS_ALLOCATION = "#9EB96D", ECB_SHSS_BSI = "#5B9D89",
  EUROSTAT_NASQ_10_F_CP = "#D49D56", EUROSTAT_NASQ_STK_ASS = "#E3C67F",
  IMF_CPIS_RESIDENCE_RECONCILED = "#A283B5", ISSUER_STOCK_RECONCILIATION = "#CA8273",
  "Multiple sources" = "#AAAAAA"
)[levels]
source_labels <- c(
  BSI_S122_SECURITIES_CONTROL = "ECB BSI: securities control", ECB_BSI = "ECB BSI",
  ECB_BSI_CPIS_ALLOCATION = "ECB BSI / IMF CPIS allocation", ECB_SHSS_BSI = "ECB SHSS / BSI",
  EUROSTAT_NASQ_10_F_CP = "Eurostat: financial accounts", EUROSTAT_NASQ_STK_ASS = "Eurostat: asset stocks",
  IMF_CPIS_RESIDENCE_RECONCILED = "IMF CPIS: reconciled", ISSUER_STOCK_RECONCILIATION = "Issuer-stock reconciliation",
  "Multiple sources" = "Multiple sources"
)[levels]
stopifnot(!anyNA(palette))
image(1:n, 1:n, t(Z[n:1, ]), col = palette, zlim = c(.5, length(levels) + .5), axes = FALSE, xlab = "", ylab = "")
axis(1, at = 1:n, labels = colnames(M), las = 2, cex.axis = .9)
axis(2, at = 1:n, labels = rev(rownames(M)), las = 2, cex.axis = .9)
par(mar = c(0, 0, 0, 0))
plot.new()
legend("center", legend = source_labels, fill = palette, bty = "n", cex = .95, ncol = 2)
invisible(dev.off())
g <- b$backstop_grid
g <- g[g$rule == "Common recovery rule", ]
pdf("Figures/interbank_recovery_backstop_averages.pdf", width = 9, height = 6.5)
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1), mgp = c(2.5, .7, 0), las = 1)
fields <- c(
  "multiple_equilibrium_states", "conditional_bank_to_bank_credit_loss_bps",
  "conditional_payment_pct_aggregate_bank_debt", "worst_case_payment_pct_aggregate_bank_debt"
)
titles <- c(
  "(a) States with multiplicity", "(b) Interbank credit loss in crises (bp)",
  "(c) Payments in crises (% bank debt)", "(d) Largest state-expected payment (%)"
)
colours <- c("#338461", "#346A93", "#B85448")
for (i in 1:4) {
  plot(NA,
    xlim = c(0, 1), ylim = range(c(0, g[[fields[i]]])) * c(1, 1.1),
    xlab = "Coverage (%)", ylab = "", main = titles[i], cex.main = .9, xaxt = "n"
  )
  axis(1, at = seq(0, 1, by = .1), labels = seq(0, 100, by = 10))
  for (j in 1:3) {
    d <- g[abs(g$adverse_equilibrium_probability - c(.1, .2, .3)[j]) < 1e-10, ]
    lines(d$backstop_coverage_eta, d[[fields[i]]], type = "b", col = colours[j], pch = 15 + j, lwd = 1.4)
  }
  if (i == 1) legend("topright", paste("rho =", c(.1, .2, .3)), col = colours, lty = 1, pch = 16:18, bty = "n", cex = .8)
}
invisible(dev.off())

# Main-text panels use statewise quantities, with no selection weights.
d <- g[abs(g$adverse_equilibrium_probability - .2) < 1e-10, ]
pdf("Figures/interbank_recovery_backstop_grid.pdf", width = 9, height = 3.7)
par(mfrow = c(1, 2), mar = c(4, 4, 3, 1), mgp = c(2.5, .7, 0), las = 1)
plot(d$backstop_coverage_eta, d$multiple_equilibrium_states,
  type = "b", pch = 16,
  col = "#346A93", lwd = 1.4, xlim = c(0, 1), ylim = c(0, 8.5), xaxt = "n",
  xlab = "Coverage (%)", ylab = "", main = "(a) States with multiplicity", cex.main = .95
)
axis(1, at = seq(0, 1, by = .1), labels = seq(0, 100, by = 10))
plot(d$backstop_coverage_eta, d$maximum_individual_equilibrium_payment_pct_debt,
  type = "b", pch = 16, col = "#346A93", lwd = 1.4, xlim = c(0, 1), xaxt = "n",
  xlab = "Coverage (%)", ylab = "", main = "(b) Maximum payment (% bank debt)", cex.main = .95,
  ylim = range(c(0, d$maximum_individual_equilibrium_payment_pct_debt)) * c(1, 1.1)
)
axis(1, at = seq(0, 1, by = .1), labels = seq(0, 100, by = 10))
invisible(dev.off())
