# Sovereign Risk Spillovers across Countries and Sectors

R code and data for the [paper](manuscript/paper/main.pdf) and [Online Appendix](manuscript/paper/online_appendix.pdf) by Benoît Mojon, Xavier Ragot and Jean-Paul Renne. The data cover Germany, France, Italy and Spain at the end of 2024.

## Reproduce the results

Use R from the repository root, or open `Rcodes_MRR.Rproj` in RStudio. The calculations require `jsonlite` and have been tested with R 4.5.1:

```r
install.packages("jsonlite")
```

Run:

```sh
Rscript main.R
```

All required inputs are included. Numerical results are written to `outputs/`, and figures and tables to `Figures/`.

**Runtime:** a complete run took approximately **11 seconds** on an Apple M4 Mac with 24 GB RAM and R 4.5.1. This includes calibration, equilibria, sensitivities, policy experiments, figures and tables, starting from the supplied processed data. It excludes package installation, data reconstruction, validation tests and PDF compilation. Allow roughly a minute on a recent computer; the actual time depends on the hardware.

## Parameters and code

The `settings` block at the beginning of `main.R` defines default costs, sovereign recovery, private-loan loss rates, equilibrium-selection weights, coverage grids, sensitivity ranges and bond-purchase experiments. Edit this block to change the exercise. Country coverage, country order (DEU, FRA, ITA, ESP) and the reference date are tied to the supplied data.

The calculation scripts follow the paper:

| Script | Calculations | Results |
|---|---|---|
| `scripts/01_calibrate.R` | Accounting, crisis parameters and sensitivities (Sections 2–3 and Appendix D) | `outputs/baseline.rds`, `outputs/sensitivities/` |
| `scripts/02_equilibria.R` | Equilibria, household receipt gaps, compensation and recovery backstop (Sections 4–6) | `outputs/equilibria.rds`, CSV summaries |
| `scripts/03_floor_capacity.R` | Minimum repayment-floor capacity (Section 5 and Online Appendix) | `outputs/floor_capacity/` |
| `scripts/04_bond_purchases.R` | Sovereign-bond purchase experiments (Section 7) | `outputs/bond_purchases/` |
| `scripts/05_figures_tables.R` | Figures, exposure and capital tables, and dependence sensitivity | `Figures/` |

`R/model/` contains the clearing and policy calculations, `R/output/` produces the figures and tables, and `R/data/` reconstructs the input data.

Matrix **rows are creditors and columns are debtors**. Model amounts are **EUR billions**.

## Check the replication

The paper's calibration gives 25 banking equilibria across 16 sovereign states, with multiplicity in eight states. The proportional repayment floor requires about EUR 60.6 billion of common capacity and pays nothing in equilibrium. The common recovery backstop removes multiplicity at 40% on the five-percentage-point grid and at 36% on the one-percentage-point grid; these grid results do not establish a continuous minimum.

For the validation tests, also install:

```r
install.packages(c("dplyr", "tidyr", "quadprog", "readxl"))
```

After running `main.R`, run:

```sh
Rscript scripts/run_tests.R
```

The tests check accounting identities, data reconstruction, clearing calculations, repayment-floor thresholds and agreement with the paper's numerical results. They use the paper's default parameter values and include a reconstruction of the database. Test logs are in `outputs/tests/`.

## Data

`data/processed/` contains the CSV model inputs. The exposure register records each claim's source, instrument, allocation method and reconciliation. Missing bilateral positions are allocated within observed totals. Related-office loans are removed, while domestic claims between distinct banks are retained.

`data/raw/` contains the source snapshots and reference extracts. The [data guide](data/README.md) gives definitions, units, transformations and source links; `data/source_manifest.csv` lists the supplied files and their checksums.

To reconstruct the processed data from these snapshots:

```sh
Rscript scripts/rebuild_data.R outputs/data_rebuild
```

This requires the additional packages listed above. The supplied inputs define the paper's data vintage; new downloads may contain provider revisions.

### Redistribution

The MIT licence in `LICENSE` applies to the replication code. Statistical data retain the source-specific reuse conditions listed below.

| Source | Included data and reuse conditions |
|---|---|
| [ECB](https://www.ecb.europa.eu/services/using-our-site/disclaimer/html/index.en.html) and [ECB Banking Supervision](https://www.bankingsupervision.europa.eu/services/disclaimer/html/index.en.html) | Bank balance sheets, securities holdings, exchange rates and public capital disclosures. Reuse requires source attribution, accurate reproduction and identification of modifications. |
| [Eurostat](https://ec.europa.eu/eurostat/help/copyright-notice) | Financial accounts, government debt and household consumption. Reuse is allowed with attribution. Our allocations, conversions and pooling are identified as authors' calculations; Eurostat is not responsible for them. |
| [IMF](https://www.imf.org/en/about/copyright-and-terms) | Public Portfolio Investment Positions, formerly CPIS. The statistical-data terms allow redistribution with attribution and identification of transformations. Commercial reuse requires checking the IMF's permission requirements. |
| Authors' calibration inputs | `data/sovereign_spreads.csv` provides the four mean sovereign CDS spreads used to normalize bond payments. The [data guide](data/README.md) gives their definitions, sample period and units. |

The linked provider terms govern reuse of third-party data.
