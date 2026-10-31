# Reproduce the tables and figures from the supplied end-2024 inputs.  Each stage has
# its own workspace and saves the results used by the next stage.
scripts <- c(
  "01_calibrate.R", "02_equilibria.R", "03_floor_capacity.R", "04_bond_purchases.R",
  "05_figures_tables.R", "06_sync_manuscript.R"
)
for (script in scripts) {
  message("\n", script)
  sys.source(file.path("scripts", script), envir = new.env(parent = globalenv()))
}
message("\nReplication complete. Results are in outputs/ and Figures/.")
