source("R/setup.R")
# Regenerate every active publication table from the calibration.
baseline_path <- "outputs/baseline.rds"
b <- readRDS(baseline_path)
z <- b$system
r <- b$result
k <- repayment_resources(z)
C <- 4L
S <- 16L
D <- z$debt[1:4]
L <- colSums(z$inputs$interbank)
preferred <- vapply(r$roots, function(a) which.max(vapply(a, function(t) sum(t$repayment), numeric(1))), integer(1))
floors <- vapply(1:S, function(s) r$roots[[s]][[preferred[s]]]$repayment, numeric(4))
floor_policy <- solve_clearing(z, "floor", floor = floors)
stopifnot(all(floor_policy$by_state$num_equilibria == 1), max(floor_policy$by_bank$payment) < 1e-8)
zero_floor <- solve_clearing(z, "floor", floor = matrix(0, 4, 16))
stopifnot(identical(zero_floor$by_state$num_equilibria, r$by_state$num_equilibria))
household_bank_claims <- b$matrix[paste0(z$countries, ":S14"), paste0(z$countries, ":S122"), drop = FALSE]
cons <- read.csv("data/processed/household_consumption.csv")
consumption <- setNames(cons$consumption_eur_million / 1000, cons$country)[z$countries]
dispersion <- compensation <- household_results <- equilibrium_losses <- list()
for (s in 1:S) {
  a <- r$roots[[s]]
  R <- do.call(cbind, lapply(a, `[[`, "repayment"))
  g <- floors[, s]
  stopifnot(all(g - R >= -1e-10), max(abs(floor_policy$roots[[s]][[1]]$repayment - g)) < 1e-10)
  gap <- g - R
  gap[gap < 0] <- 0
  bank_cost <- colSums(gap * L)
  all_cost <- colSums(gap * D)
  aggregate <- colSums(R * D) / sum(D)
  loc <- which(R == min(R), arr.ind = TRUE)[1, ]
  dispersion[[s]] <- data.frame(
    state = s, sovereign_defaults = k$labels[s], num_equilibria = ncol(R),
    aggregate_recovery_min = min(aggregate), aggregate_recovery_max = max(aggregate),
    minimum_recovery_country = z$countries[loc[1]], minimum_recovery_rate = R[loc[1], loc[2]],
    minimum_country_preferred_recovery = g[loc[1]]
  )
  compensation[[s]] <- data.frame(
    state = s, sovereign_defaults = k$labels[s], num_equilibria = ncol(R),
    banks_only_max_eur_bn = max(bank_cost), all_creditors_max_eur_bn = max(all_cost),
    max_compensation_pct_bank_debt = 100 * max(bank_cost) / sum(D),
    all_creditors_max_pct_bank_debt = 100 * max(all_cost) / sum(D)
  )
  loss <- household_bank_claims %*% gap
  v <- apply(loss, 1, max)
  total <- max(colSums(loss))
  household_results[[s]] <- data.frame(
    state = s, sovereign_defaults = k$labels[s], num_equilibria = ncol(R),
    t(setNames(v, paste0(z$countries, "_max_shortfall_eur_bn"))), total_max_shortfall_eur_bn = total,
    t(setNames(100 * v / consumption, paste0(z$countries, "_max_shortfall_pct_2024_consumption"))),
    total_max_shortfall_pct_2024_consumption = 100 * total / sum(consumption), check.names = FALSE
  )
  for (e in seq_along(a)) {
    equilibrium_losses[[length(equilibrium_losses) + 1L]] <- data.frame(
      state = s,
      sovereign_defaults = k$labels[s], equilibrium = e, country = z$countries,
      preferred_repayment = g, repayment = R[, e],
      gross_household_receipt_shortfall_eur_bn = loss[, e],
      bank_creditor_compensation_eur_bn = bank_cost[e], all_creditor_compensation_eur_bn = all_cost[e]
    )
  }
}
dispersion <- do.call(rbind, dispersion)
compensation <- do.call(rbind, compensation)
household_results <- do.call(rbind, household_results)
write.csv(dispersion, "outputs/recovery_by_state.csv", row.names = FALSE)
write.csv(compensation, "outputs/creditor_compensation.csv", row.names = FALSE)
write.csv(household_results, "outputs/household_receipt_gaps.csv", row.names = FALSE)
write.csv(do.call(rbind, equilibrium_losses), "outputs/household_and_creditor_losses.csv", row.names = FALSE)
rowend <- paste0("\\", "\\")

rate <- function(x) ifelse(abs(x - 1) < 1e-10, "100\\%", sprintf("%.1f\\%%", 100 * x))
range <- ifelse(abs(dispersion$aggregate_recovery_max - dispersion$aggregate_recovery_min) < 1e-12,
  rate(dispersion$aggregate_recovery_min), paste0(rate(dispersion$aggregate_recovery_min), "--", rate(dispersion$aggregate_recovery_max))
)
table_tex(
  "Figures/bank_recovery_dispersion_table.tex",
  "Sovereign defaults & Equilibria & Aggregate recovery range & Lowest bank recovery",
  sprintf(
    "%s & %d & %s & %s", k$labels, dispersion$num_equilibria, range,
    ifelse(abs(dispersion$aggregate_recovery_min - 1) < 1e-10 & abs(dispersion$aggregate_recovery_max - 1) < 1e-10,
      "$-$", paste0(
        sprintf("%s/%s", dispersion$minimum_recovery_country, rate(dispersion$minimum_recovery_rate)),
        ifelse(dispersion$num_equilibria > 1, paste0(" (", rate(dispersion$minimum_country_preferred_recovery), ")"), "")
      )
    )
  ), "lrrl"
)
money <- function(pct, amount) ifelse(abs(amount) < 1e-10, "$-$", sprintf("%.2f\\%% (\\euro{}%.0f bn)", pct, amount))
table_tex(
  "Figures/ex_post_compensation_table.tex", "Sovereign defaults & Equilibria & Banks only & All creditors",
  sprintf(
    "%s & %d & %s & %s", k$labels, compensation$num_equilibria,
    money(compensation$max_compensation_pct_bank_debt, compensation$banks_only_max_eur_bn),
    money(compensation$all_creditors_max_pct_bank_debt, compensation$all_creditors_max_eur_bn)
  ), "lrrr"
)
fmt_hh <- function(v, pct) {
  ifelse(v < 1e-10, "$-$", paste0(
    ifelse(v < 1, "$<1$", sprintf("%.0f", v)),
    " (", ifelse(pct > 0 & pct < .005, "$<0.01$", sprintf("%.2f", pct)), "\\%)"
  ))
}
table_tex(
  "Figures/household_consumption_changes_table.tex", "Sovereign defaults & Equilibria & DEU & FRA & ITA & ESP & Total",
  vapply(1:S, function(s) {
    paste(c(
      k$labels[s], household_results$num_equilibria[s],
      fmt_hh(
        as.numeric(household_results[s, c(paste0(z$countries, "_max_shortfall_eur_bn"), "total_max_shortfall_eur_bn")]),
        as.numeric(household_results[s, c(paste0(z$countries, "_max_shortfall_pct_2024_consumption"), "total_max_shortfall_pct_2024_consumption")])
      )
    ), collapse = " & ")
  }, character(1)), "lrrrrrr"
)
# Section 6: common recovery backstop, in five-percentage-point steps.
policies <- list()
gridrows <- list()
for (eta in seq(0, 1, by = .05)) {
  name <- sprintf("common_%03d", round(eta * 100))
  y <- solve_clearing(z, "recovery", eta = eta)
  policies[[name]] <- y
  for (rho in c(.1, .2, .3)) {
    d <- summarise_backstop(y, rho)
    d$rule <- "Common recovery rule"
    gridrows[[length(gridrows) + 1L]] <- d
  }
}
grid <- do.call(rbind, gridrows)
write.csv(grid, "outputs/recovery_backstop.csv", row.names = FALSE)
g <- grid[abs(grid$adverse_equilibrium_probability - .2) < 1e-10 & grid$rule == "Common recovery rule", ]
eta_first <- min(g$backstop_coverage_eta[g$multiple_equilibrium_states == 0])
rob <- b$robustness
select_names <- c("ISA_lambda0900", "half_ISA_lambda0900", "baseline", "OCR_q25_lambda0900", "OCR_q75_lambda0900", "minimum_median_lambda0875", "sovereign_haircut45", "sovereign_haircut55", "nbfi_half")
rob <- rob[match(select_names, rob$name), ]
labels <- c(
  "ISA headroom, $\\lambda=.90$", "Half ISA headroom, full equity", "Public median OCR (baseline)",
  "Lower-quartile OCR", "Upper-quartile OCR", "Minimum requirements, $\\lambda=.875$", "Sovereign recovery 55\\%", "Sovereign recovery 45\\%", "NBFI availability 50\\%"
)
table_tex(
  "Figures/OCR_robustness_table.tex", "Specification & Default states & Bank--state events & Multiple states",
  sprintf("%s & %d & %d & %d", labels, rob$default_states_at_par, rob$bank_defaults_at_par, rob$multiple_equilibrium_states), "lrrr"
)
head <- read.csv("outputs/sensitivities/country_headroom.csv")
table_tex(
  "Figures/OCR_headroom_table.tex", "Country & Accounting equity & Clearing debt & OCR headroom & ISA headroom",
  sprintf("%s & %.1f & %.1f & %.1f & %.1f", head$country, head$accounting_equity, D, head$OCR, head$ISA), "lrrrr"
)
# A compact, fully reconciled French-only balance-sheet illustration.
s <- which(k$labels == "FRA")
boxes <- c()
for (e in seq_along(r$roots[[s]])) {
  q <- r$roots[[s]][[e]]
  boxes <- c(boxes, sprintf("\\multicolumn{4}{l}{\\textbf{Equilibrium %s}} %s", LETTERS[e], rowend))
  cells <- vapply(1:4, function(i) {
    paste0(
      "\\fcolorbox{black}{", if (q$default[i]) "black!10" else "white",
      "}{\\parbox[t]{.212\\textwidth}{\\centering\\scriptsize\\textbf{", z$countries[i], "}\\par ",
      if (q$default[i]) "Credit event" else "Full repayment", "\\par ", rate(q$repayment[i]),
      "\\par\\smallskip\\begin{tabular}{@{}lr@{}}",
      paste(sprintf(
        "%s & %.1f", c("Estate", "Debt", "Equity", "Headroom", "B--B claims", "B--B loss"),
        c(
          q$assets[i], D[i], z$equity[i], z$headroom[i], sum(z$inputs$interbank[i, ]),
          as.numeric(z$inputs$interbank[i, , drop = FALSE] %*% (1 - q$repayment))
        )
      ), collapse = paste0(rowend, "\n")),
      "\\end{tabular}}}"
    )
  }, character(1))
  boxes <- c(boxes, paste(paste(cells, collapse = " & "), rowend), "\\noalign{\\vskip 8pt}")
}
writeLines(c("\\begin{tabular*}{\\textwidth}{@{\\extracolsep{\\fill}}llll@{}}", boxes, "\\end{tabular*}"), "Figures/french_default_balance_sheets.tex")
pi <- align_country_vector(z$params$pi_G, z$countries, "pi_G")
prob <- apply(k$delta, 1, function(d) prod(ifelse(d == 1, pi, 1 - pi)))
expected <- rep(0, 4)
for (s in 1:S) expected <- expected + prob[s] * do.call(cbind, lapply(r$roots[[s]], `[[`, "repayment")) %*% equilibrium_weights(r$roots[[s]], .2)
statistics <- list(
  baseline = "Public OCR/full equity/lambda .90", units = "EUR bn",
  multiple_states = sum(dispersion$num_equilibria > 1), total_equilibria = sum(dispersion$num_equilibria),
  default_states_at_par = sum(b$reference$by_state$bank_defaults_at_par > 0),
  max_compensation_eur_bn = max(compensation$banks_only_max_eur_bn), max_compensation_pct_debt = max(compensation$max_compensation_pct_bank_debt),
  total_bank_debt = sum(D), expected_debt_weighted_shortfall_bp = 1e4 * sum(D * (1 - expected)) / sum(D),
  common_first_unique_eta = eta_first,
  france_only_household_shortfalls = as.list(household_results[which(k$labels == "FRA"), paste0(z$countries, "_max_shortfall_eur_bn")])
)
jsonlite::write_json(statistics, "outputs/summary.json", auto_unbox = TRUE, pretty = TRUE, digits = 12)
saveRDS(list(
  baseline_md5 = tools::md5sum(baseline_path), baseline = b, dispersion = dispersion, compensation = compensation,
  household = household_results, household_bank_claims = household_bank_claims, floors = floors, floor_policy = floor_policy,
  backstop_policies = policies, backstop_grid = grid, statistics = statistics
), "outputs/equilibria.rds")
cat("All active publication tables regenerated from paper calibration.\n")
