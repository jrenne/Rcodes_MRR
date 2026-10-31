# Sections 2--3: accounting, repayment resources and the crisis specification.
source("R/setup.R")
countries <- c("DEU", "FRA", "ITA", "ESP")
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
# With a 50% default loss, (1 - 0.5*pi)*(1 + spread) = 1.
# The CDS means enter as one-period yield proxies, not a full CDS pricing model.
params <- list(
  haircut_G = .5,
  pi_G = (sovereign$spread_basis_points / 10000) / (.5 * (1 + sovereign$spread_basis_points / 10000)), lambda_B = .9
)
firms <- read_matrix("data/processed/firm_loans.csv")
households <- read_matrix("data/processed/household_loans.csv")
loss_data <- read.csv("data/processed/stress_loss_rates.csv")
loss_rates <- setNames(loss_data$crisis_loan_loss_rate, loss_data$model_sector)
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

# Appendix D: compute the reported sensitivities and 75 local combinations.
make_sensitivity <- function(
  name, headroom = system$headroom[1:4], lambda = .9, haircut = .5,
  firm_rate = loss_rates[["firms"]], household_rate = loss_rates[["households"]], nbfi_factor = 1
) {
  z <- apply_nbfi_buffer(system, nbfi_factor)
  z$headroom[1:4] <- headroom
  z$lambda[1:4] <- lambda
  z$params$lambda_B <- lambda
  z$params$haircut_G <- haircut
  z$reference$private_loss_matrix <- firm_rate * firms + household_rate * households
  z$cutoff <- pmin(z$debt + z$equity - z$headroom, z$debt / z$lambda)
  y <- solve_clearing(z)
  constants <- repayment_resources(z)$constant[1:4, , drop = FALSE] + rowSums(z$K)
  defaults_at_par <- constants + rowSums(z$inputs$interbank) < z$cutoff[1:4] - 1e-9
  data.frame(
    name = name, default_states_at_par = sum(colSums(defaults_at_par) > 0),
    bank_defaults_at_par = sum(defaults_at_par), multiple_equilibrium_states = sum(y$by_state$num_equilibria > 1),
    total_equilibria = sum(y$by_state$num_equilibria)
  )
}
isa <- capital$isa_headroom_share_equity * inputs$equity
minimum <- capital$minimum_median_headroom_share_equity * inputs$equity
robustness <- do.call(rbind, list(
  make_sensitivity("ISA_lambda0900", isa), make_sensitivity("half_ISA_lambda0900", .5 * isa),
  make_sensitivity("baseline", headroom),
  make_sensitivity("OCR_q25_lambda0900", capital$ocr_q25_headroom_share_equity * inputs$equity),
  make_sensitivity("OCR_q75_lambda0900", capital$ocr_q75_headroom_share_equity * inputs$equity),
  make_sensitivity("minimum_median_lambda0875", minimum, lambda = .875),
  make_sensitivity("sovereign_haircut45", headroom, haircut = .45),
  make_sensitivity("sovereign_haircut55", headroom, haircut = .55),
  make_sensitivity("nbfi_half", headroom, nbfi_factor = .5)
))
local_grid <- expand.grid(
  headroom_multiplier = seq(.9, 1.1, length.out = 5),
  lambda = seq(.89, .91, length.out = 5), credit_loss_change = c(-.001, 0, .001)
)
local_results <- do.call(rbind, lapply(seq_len(nrow(local_grid)), function(i) {
  p <- local_grid[i, ]
  cbind(p, make_sensitivity(paste0("local_", i), p$headroom_multiplier * headroom,
    lambda = p$lambda, firm_rate = loss_rates[["firms"]] + p$credit_loss_change, household_rate = loss_rates[["households"]] + p$credit_loss_change
  ))
}))
bank_proxies <- read.csv("data/processed/capital_proxy_by_bank.csv")
buffer_results <- do.call(rbind, lapply(c(.25, .5, .75, 1), function(fraction) {
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
diag(net$claims[1:4, 1:4]) <- 0
net$inputs$debt <- net$inputs$debt - removed
net$debt[1:4] <- net$debt[1:4] - removed
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
  units = "EUR bn", date = as.Date("2024-12-31"), system = system,
  matrix = matrix, result = solution, reference = full_solution, robustness = robustness,
  controls = list(lambda_B = .9, bank_equity_availability = 1)
)
saveRDS(baseline, "outputs/baseline.rds")
message("Calibration: ", sum(solution$by_state$num_equilibria), " equilibria in 16 sovereign states.")
