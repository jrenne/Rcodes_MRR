# Multiplicity and bank defaults under purchases alone, with issuer composition held fixed per curve.
baseline <- readRDS("outputs/baseline.rds")
z <- baseline$system
schemes <- list(
  All = z$countries, Germany = "DEU", France = "FRA", Italy = "ITA", Spain = "ESP",
  Italy_Spain = c("ITA", "ESP")
)
fractions <- seq(0, 1, by = 0.01)
rows <- state_rows <- list()
for (scheme in names(schemes)) {
  issuers <- schemes[[scheme]]
  for (fraction in fractions) {
    purchase <- apply_eurosystem_sovereign_purchases(z, baseline$matrix, fraction, issuers)
    solution <- solve_clearing(purchase$system)
    counts <- solution$by_state$num_equilibria
    minimum_defaults <- vapply(solution$roots, function(roots) {
      min(vapply(roots, function(root) sum(root$default), integer(1)))
    }, integer(1))
    maximum_defaults <- solution$by_state$maximum_bank_defaults
    bank_default <- maximum_defaults > 0
    residual <- max(solution$by_bank$map_residual)
    stopifnot(length(counts) == 16L, all(counts >= 1), residual < 1e-10)
    rows[[length(rows) + 1L]] <- data.frame(
      scheme = scheme, issuers = paste(issuers, collapse = "+"), purchase_fraction = fraction,
      purchase_eur_bn = sum(purchase$purchases), multiple_states = sum(counts > 1),
      states_with_some_equilibrium_bank_default = sum(bank_default),
      mean_minimum_bank_defaults = mean(minimum_defaults),
      mean_maximum_bank_defaults = mean(maximum_defaults),
      total_equilibria = sum(counts), additional_equilibria = sum(counts - 1),
      max_equation_residual = residual
    )
    state_rows[[length(state_rows) + 1L]] <- data.frame(
      scheme = scheme, purchase_fraction = fraction, purchase_eur_bn = sum(purchase$purchases),
      state = solution$by_state$state, sovereign_defaults = solution$by_state$sovereign_defaults,
      equilibria = counts, minimum_bank_defaults = minimum_defaults,
      maximum_bank_defaults = maximum_defaults,
      some_equilibrium_bank_default = bank_default
    )
  }
  cat("Purchase sweep completed:", scheme, "\n")
}
d <- do.call(rbind, rows)
by_state <- do.call(rbind, state_rows)
stopifnot(all(d$multiple_states[d$purchase_fraction == 0] == 8),
  all(d$total_equilibria[d$purchase_fraction == 0] == 25),
  all(d$multiple_states <= d$states_with_some_equilibrium_bank_default),
  all(d$mean_minimum_bank_defaults >= 0), all(d$mean_maximum_bank_defaults <= 4),
  all(d$mean_minimum_bank_defaults <= d$mean_maximum_bank_defaults),
  all(d$mean_minimum_bank_defaults[d$multiple_states == 0] ==
    d$mean_maximum_bank_defaults[d$multiple_states == 0]))
# Cross-check every purchase case already reported in the paper.
reference <- read.csv("outputs/bond_purchases/summary.csv")
for (name in names(baseline$settings$purchases)) {
  case <- baseline$settings$purchases[[name]]
  match_scheme <- names(schemes)[vapply(schemes, function(v) setequal(v, case$issuers), logical(1))]
  point <- d[d$scheme == match_scheme & abs(d$purchase_fraction - case$fraction) < 1e-12, ]
  original <- reference[reference$case == name, ]
  stopifnot(nrow(point) == 1L, nrow(original) == 1L,
    abs(point$purchase_eur_bn - original$purchase_eur_bn) < 1e-8,
    point$multiple_states == original$multiple_states,
    point$states_with_some_equilibrium_bank_default == original$states_with_some_equilibrium_bank_default,
    point$total_equilibria == original$total_equilibria)
}
write.csv(d, "outputs/bond_purchases/multiplicity_sweep.csv", row.names = FALSE)
write.csv(by_state, "outputs/bond_purchases/multiplicity_by_state.csv", row.names = FALSE)
write.csv(d, "Figures/eurosystem_purchase_multiplicity_data.csv", row.names = FALSE)

plot_purchase_multiplicity <- function(d) {
  labels <- c(All = "All four issuers", Germany = "Germany only", France = "France only",
    Italy = "Italy only", Spain = "Spain only", Italy_Spain = "Italy and Spain")
  colours <- c(All = "#202020", Germany = "#346A93", France = "#338461",
    Italy = "#B85448", Spain = "#B08020", Italy_Spain = "#885EA5")
  symbols <- c(All = 16, Germany = 1, France = 15, Italy = 2, Spain = 5, Italy_Spain = 6)
  layout(matrix(1:3, ncol = 1), heights = c(0.65, 3.35, 3.35))
  par(mar = rep(0, 4), family = "sans", las = 1, cex = 1)
  plot.new()
  legend("center", legend = unname(labels), col = colours, lty = 1, pch = symbols,
    lwd = c(2.3, rep(1.8, 5)), bty = "n", cex = 0.95, ncol = 3, x.intersp = 1.3)
  panels <- c(multiple_states = "(a) Multiple banking equilibria",
    mean_maximum_bank_defaults = "(b) Mean number of banking systems in default")
  for (metric in names(panels)) {
    bottom <- metric == "mean_maximum_bank_defaults"
    ticks <- if (bottom) seq(0, 2, by = 0.5) else seq(0, 16, by = 4)
    par(mar = c(if (bottom) 4.2 else 2.5, 4.6, 2.3, 0.8), mgp = c(2.8, 0.7, 0))
    plot(NA, xlim = c(0, max(d$purchase_eur_bn)), ylim = c(0, if (bottom) 2.05 else 16.5),
      xaxs = "i", yaxs = "i", axes = FALSE,
      xlab = if (bottom) "Sovereign bonds purchased (EUR billions)" else "",
      ylab = if (bottom) "Number of banking systems" else "Number of sovereign states", cex.lab = 1)
    title(main = panels[metric], font.main = 1, cex.main = 1)
    abline(h = ticks, col = "#E5E5E5", lwd = 0.7)
    axis(1, at = seq(0, 1200, by = 200))
    axis(2, at = ticks)
    box(bty = "l", col = "#555555")
    # Shading uses the same step convention as the boundary curves.
    if (bottom) {
      for (scheme in names(labels)) {
        q <- d[d$scheme == scheme, ]
        step_x <- rep(q$purchase_eur_bn, each = 2)[-1]
        lower <- head(rep(q$mean_minimum_bank_defaults, each = 2), -1)
        upper <- head(rep(q$mean_maximum_bank_defaults, each = 2), -1)
        polygon(c(step_x, rev(step_x)), c(lower, rev(upper)),
          col = adjustcolor(colours[scheme], alpha.f = 0.12), border = NA)
      }
    }
    for (scheme in names(labels)) {
      q <- d[d$scheme == scheme, ]
      if (bottom) {
        lines(q$purchase_eur_bn, q$mean_minimum_bank_defaults, type = "s",
          col = colours[scheme], lty = 2, lwd = 1.4)
      }
      lines(q$purchase_eur_bn, q[[metric]], type = "s", col = colours[scheme],
        lty = 1, lwd = if (scheme == "All") 2.3 else 1.8)
      marked <- which(abs(q$purchase_fraction * 10 - round(q$purchase_fraction * 10)) < 1e-10)
      points(q$purchase_eur_bn[marked], q[[metric]][marked], col = colours[scheme], pch = symbols[scheme], cex = 0.65)
    }
    if (bottom) {
      legend("topright", legend = c("Mean maximum", "Mean minimum"),
        col = "#333333", lty = c(1, 2), lwd = c(1.8, 1.4), bty = "n", cex = 0.9)
    }
  }
}
pdf("Figures/eurosystem_purchase_multiplicity.pdf", width = 9, height = 8.1)
plot_purchase_multiplicity(d)
invisible(dev.off())
png("Figures/eurosystem_purchase_multiplicity.png", width = 1800, height = 1620, res = 200)
plot_purchase_multiplicity(d)
invisible(dev.off())
cat("Purchase multiplicity figure generated from", nrow(d), "counterfactuals; all six published cases agree.\n")
