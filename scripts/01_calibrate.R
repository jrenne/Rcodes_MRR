# Sections 2--3: accounting, repayment resources and the crisis specification.
source("R/setup.R")
if (!exists("settings")) stop("Set the calibration in main.R before running this stage.")
if (!identical(settings$countries, c("DEU", "FRA", "ITA", "ESP"))) {
  stop("The supplied data and publication tables require DEU, FRA, ITA, ESP in that order.")
}
stopifnot(
  length(settings$bank_lambda) == 1, settings$bank_lambda > 0, settings$bank_lambda <= 1,
  length(settings$sovereign_recovery) == 1, settings$sovereign_recovery >= 0, settings$sovereign_recovery < 1,
  identical(names(settings$loan_loss_rates), c("firms", "households")),
  all(is.finite(settings$loan_loss_rates)), all(settings$loan_loss_rates >= 0 & settings$loan_loss_rates <= 1),
  length(settings$rho) == 1, settings$rho >= 0, settings$rho <= 1,
  all(is.finite(settings$rho_values)), all(settings$rho_values >= 0 & settings$rho_values <= 1),
  !anyDuplicated(settings$rho_values), any(abs(settings$rho_values - settings$rho) < 1e-12),
  length(settings$dependence_coverage) == 1, settings$dependence_coverage >= 0, settings$dependence_coverage <= 1,
  all(is.finite(settings$dependence_weights)), all(settings$dependence_weights >= 0 & settings$dependence_weights <= 1),
  !anyDuplicated(settings$dependence_weights)
)
for (grid in list(settings$coverage_grid, settings$coverage_refinement)) {
  stopifnot(
    length(grid) > 0, all(is.finite(grid)), all(grid >= 0 & grid <= 1),
    !anyDuplicated(grid), !is.unsorted(grid)
  )
}
stopifnot(
  any(abs(settings$coverage_grid) < 1e-12),
  any(abs(settings$coverage_grid - settings$dependence_coverage) < 1e-12)
)
countries <- settings$countries
C <- length(countries)
banks_index <- seq_len(C)
read_matrix <- function(file) unname(as.matrix(read.csv(file, row.names = 1, check.names = FALSE)))
accounts <- read.csv("data/processed/bank_accounts.csv")
accounts <- accounts[match(countries, accounts$country), ]
capital <- read.csv("data/processed/capital_proxies.csv")
capital <- capital[match(countries, capital$country), ]
sovereign <- read.csv("data/sovereign_spreads.csv")
sovereign <- sovereign[match(countries, sovereign$country), ]
matrix <- as.matrix(read.csv("data/processed/exposure_matrix.csv", row.names = 1, check.names = FALSE))
banks <- paste0(countries, ":S122")
governments <- paste0(countries, ":S13")
inputs <- list(
  country_labels = countries,
  interbank = unname(matrix[banks, banks]), sovereign = unname(matrix[banks, governments]),
  debt = accounts$clearing_pool_debt, equity = accounts$observed_equity,
  fixed_resources = accounts$fixed_net_resources, accounting = accounts
)
# With loss given default LGD, (1 - LGD*pi)*(1 + spread) = 1.
# The CDS means enter as one-period yield proxies, not a full CDS pricing model.
params <- list(
  haircut_G = 1 - settings$sovereign_recovery,
  pi_G = (sovereign$spread_basis_points / 10000) / ((1 - settings$sovereign_recovery) * (1 + sovereign$spread_basis_points / 10000)),
  lambda_B = settings$bank_lambda
)
firms <- read_matrix("data/processed/firm_loans.csv")
households <- read_matrix("data/processed/household_loans.csv")
loss_rates <- settings$loan_loss_rates
headroom <- capital$ocr_median_headroom_share_equity * inputs$equity
bank_reference <- prepare_bank_reference(inputs, params, headroom, firms + households,
  private_loss_matrix = loss_rates[["firms"]] * firms + loss_rates[["households"]] * households
)
system <- build_bank_nbfi_system(bank_reference, matrix, read.csv("data/processed/nbfi_buffers.csv"))
solution <- solve_clearing(system)
full_solution <- solve_bank_nbfi_default(system)
stopifnot(
  identical(solution$by_state$num_equilibria, full_solution$by_state$num_equilibria),
  all(full_solution$by_node$repayment[full_solution$by_node$sector == "nbfi"] == 1)
)

# Appendix D: headroom, default-cost and private-credit sensitivities.
make_sensitivity <- function(
  name, headroom = system$headroom[banks_index], lambda = settings$bank_lambda, haircut = 1 - settings$sovereign_recovery,
  firm_rate = loss_rates[["firms"]], household_rate = loss_rates[["households"]], nbfi_factor = 1
) {
  z <- apply_nbfi_buffer(system, nbfi_factor)
  z$headroom[banks_index] <- headroom
  z$lambda[banks_index] <- lambda
  z$params$lambda_B <- lambda
  z$params$haircut_G <- haircut
  z$reference$private_loss_matrix <- firm_rate * firms + household_rate * households
  z$cutoff <- pmin(z$debt + z$equity - z$headroom, z$debt / z$lambda)
  y <- solve_clearing(z)
  constants <- repayment_resources(z)$constant[banks_index, , drop = FALSE] + rowSums(z$K)
  defaults_at_par <- constants + rowSums(z$inputs$interbank) < z$cutoff[banks_index] - 1e-9
  data.frame(
    name = name, default_states_at_par = sum(colSums(defaults_at_par) > 0),
    bank_defaults_at_par = sum(defaults_at_par), multiple_equilibrium_states = sum(y$by_state$num_equilibria > 1),
    total_equilibria = sum(y$by_state$num_equilibria)
  )
}
isa <- capital$isa_headroom_share_equity * inputs$equity
minimum <- capital$minimum_median_headroom_share_equity * inputs$equity
robustness <- do.call(rbind, list(
  make_sensitivity("ISA", isa), make_sensitivity("half_ISA", .5 * isa),
  make_sensitivity("baseline", headroom),
  make_sensitivity("OCR_q25", capital$ocr_q25_headroom_share_equity * inputs$equity),
  make_sensitivity("OCR_q75", capital$ocr_q75_headroom_share_equity * inputs$equity),
  make_sensitivity("minimum_requirements", minimum, lambda = settings$sensitivity$minimum_requirement_lambda),
  make_sensitivity("sovereign_recovery_high", headroom, haircut = 1 - settings$sensitivity$sovereign_recoveries[["high"]]),
  make_sensitivity("sovereign_recovery_low", headroom, haircut = 1 - settings$sensitivity$sovereign_recoveries[["low"]]),
  make_sensitivity("nbfi_reduced", headroom, nbfi_factor = settings$sensitivity$nbfi_buffer_fraction)
))
local_grid <- expand.grid(
  headroom_multiplier = settings$sensitivity$headroom_multipliers,
  lambda = settings$sensitivity$bank_lambdas, credit_loss_change = settings$sensitivity$loan_loss_changes
)
local_results <- do.call(rbind, lapply(seq_len(nrow(local_grid)), function(i) {
  p <- local_grid[i, ]
  cbind(p, make_sensitivity(paste0("local_", i), p$headroom_multiplier * headroom,
    lambda = p$lambda, firm_rate = loss_rates[["firms"]] + p$credit_loss_change, household_rate = loss_rates[["households"]] + p$credit_loss_change
  ))
}))
bank_proxies <- read.csv("data/processed/capital_proxy_by_bank.csv")
buffer_results <- do.call(rbind, lapply(settings$sensitivity$additional_buffer_fractions, function(fraction) {
  ratio <- vapply(countries, function(country) {
    p <- bank_proxies[bank_proxies$country == country, ]
    median(p$ocr_share_cet1 + fraction * (p$minimum_share_cet1 - p$ocr_share_cet1))
  }, numeric(1))
  cbind(fraction, make_sensitivity(
    "additional_buffer",
    ratio * capital$sbs_cet1_share_accounting_equity * inputs$equity
  ))
}))
# Netting domestic bank claims removes the same amount of assets and debt.
net <- system
removed <- diag(net$inputs$interbank)
diag(net$inputs$interbank) <- 0
diag(net$claims[banks_index, 1:4]) <- 0
net$inputs$debt <- net$inputs$debt - removed
net$debt[banks_index] <- net$debt[banks_index] - removed
net$cutoff <- pmin(net$debt + net$equity - net$headroom, net$debt / net$lambda)
netting <- solve_clearing(net)
dir.create("outputs/sensitivities", recursive = TRUE, showWarnings = FALSE)
write.csv(local_results, "outputs/sensitivities/local_grid.csv", row.names = FALSE)
write.csv(buffer_results, "outputs/sensitivities/additional_buffers.csv", row.names = FALSE)
write.csv(netting$by_state, "outputs/sensitivities/domestic_netting.csv", row.names = FALSE)
write.csv(robustness, "outputs/sensitivities/crisis_specification.csv", row.names = FALSE)
write.csv(data.frame(
  country = countries, accounting_equity = inputs$equity,
  ISA = isa, minimum = minimum, OCR = headroom
), "outputs/sensitivities/country_headroom.csv", row.names = FALSE)
baseline <- list(
  units = "EUR bn", date = settings$reference_date, settings = settings, system = system,
  matrix = matrix, result = solution, reference = full_solution, robustness = robustness,
  controls = list(lambda_B = settings$bank_lambda, bank_equity_availability = 1)
)
saveRDS(baseline, "outputs/baseline.rds")
message("Calibration: ", sum(solution$by_state$num_equilibria), " equilibria in 16 sovereign states.")
