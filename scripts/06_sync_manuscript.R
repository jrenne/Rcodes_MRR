# Copy the generated figures and tables into the manuscript.
assets <- c(
  "figure_network_2024-10-01.pdf", "active_source_matrix_2024-10-01.pdf",
  "bank_exposure_network.pdf", "bank_exposure_network_data.csv", "bank_exposure_table.tex",
  "bank_and_eurosystem_exposure_table_data.csv",
  "nbfi_exposure_table.tex", "nbfi_exposure_table_data.csv",
  "interbank_recovery_backstop_grid.pdf", "interbank_recovery_backstop_averages.pdf",
  "bank_recovery_dispersion_table.tex", "bank_recovery_dispersion_table_data.csv",
  "ex_post_compensation_table.tex",
  "nonbank_creditor_losses_table.tex", "french_default_balance_sheets.tex",
  "joint_sovereign_default_balance_sheets.tex",
  "two_bank_sovereign_balance_sheets.tex", "two_bank_sovereign_balance_sheets_data.csv",
  "OCR_headroom_table.tex", "OCR_robustness_table.tex", "minimum_floor_budget_table.tex",
  "eurosystem_purchase_table.tex", "eurosystem_purchase_multiplicity.pdf",
  "eurosystem_purchase_multiplicity_data.csv",
  "capital_headroom_summary.tex", "sovereign_dependence_sensitivity.tex"
)
stopifnot(all(file.exists(file.path("Figures", assets))))
stopifnot(all(file.copy(file.path("Figures", assets), "manuscript/paper/Figures", overwrite = TRUE)))
