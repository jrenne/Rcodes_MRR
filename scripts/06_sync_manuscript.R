# Copy the generated assets actually used by the current paper.
assets <- c(
  "figure_network_2024-10-01.pdf", "active_source_matrix_2024-10-01.pdf",
  "bank_exposure_network.pdf", "bank_exposure_network_data.csv", "bank_exposure_table.tex",
  "interbank_recovery_backstop_grid.pdf", "interbank_recovery_backstop_averages.pdf",
  "bank_recovery_dispersion_table.tex", "ex_post_compensation_table.tex",
  "household_consumption_changes_table.tex", "french_default_balance_sheets.tex",
  "OCR_headroom_table.tex", "OCR_robustness_table.tex", "minimum_floor_budget_table.tex",
  "eurosystem_purchase_table.tex", "capital_headroom_summary.tex", "sovereign_dependence_sensitivity.tex"
)
stopifnot(all(file.exists(file.path("Figures", assets))))
stopifnot(all(file.copy(file.path("Figures", assets), "manuscript/paper/Figures", overwrite = TRUE)))
