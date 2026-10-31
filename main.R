# Calibration and experiments for the end-2024 data supplied with the paper.
# The input data and publication tables use these four countries in this order.
countries <- c("DEU", "FRA", "ITA", "ESP")
settings <- list(
  countries = countries,
  reference_date = as.Date("2024-12-31"),
  bank_lambda = .90,
  sovereign_recovery = .50,
  loan_loss_rates = c(firms = .018, households = .017),

  # Equilibrium weights and recovery-backstop coverage.
  rho = .20,
  rho_values = c(.10, .20, .30),
  coverage_grid = seq(0, 1, by = .05),
  coverage_refinement = seq(0, 1, by = .01),
  dependence_weights = c(0, .50, 1),
  dependence_coverage = .50,

  # Appendix D sensitivities.
  sensitivity = list(
    headroom_multipliers = seq(.90, 1.10, length.out = 5),
    bank_lambdas = seq(.89, .91, length.out = 5),
    loan_loss_changes = c(-.001, 0, .001),
    additional_buffer_fractions = c(.25, .50, .75, 1),
    minimum_requirement_lambda = .875,
    sovereign_recoveries = c(low = .45, high = .55),
    nbfi_buffer_fraction = .50
  ),

  # Each purchase fraction applies to all bank holdings of the selected issuers.
  purchases = list(
    All_issuers_000 = list(issuers = countries, fraction = 0),
    All_issuers_025 = list(issuers = countries, fraction = .25),
    All_issuers_050 = list(issuers = countries, fraction = .50),
    All_issuers_075 = list(issuers = countries, fraction = .75),
    France_only_050 = list(issuers = "FRA", fraction = .50),
    Italy_and_Spain_075 = list(issuers = c("ITA", "ESP"), fraction = .75)
  )
)

# Each stage has its own workspace and saves the results used by the next stage.
source("scripts/01_calibrate.R", local = new.env())
source("scripts/02_equilibria.R", local = new.env())
source("scripts/03_floor_capacity.R", local = new.env())
source("scripts/04_bond_purchases.R", local = new.env())
source("scripts/05_figures_tables.R", local = new.env())
source("scripts/06_sync_manuscript.R", local = new.env())

message("Replication complete. Results are in outputs/ and Figures/.")
