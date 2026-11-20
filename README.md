# Sovereign Risk Spillovers across Countries and Sectors

Replication code and public-source data for *Sovereign Risk Spillovers across Countries and Sectors* by [Benoît Mojon](https://www.bis.org/author/benoit-mojon), [Xavier Ragot](https://xavier-ragot.fr/) and [Jean-Paul Renne](https://www.jprenne.com/). The network covers Germany, France, Italy and Spain at 31 December 2024. Results are model-based scenario calculations conditional on the reconstructed exposures and crisis assumptions.

## Reproduce the results

Run from the repository root, or open `Rcodes_MRR.Rproj` in RStudio. R 4.5.1 was used for validation. The main calculations require only `jsonlite`:

```r
install.packages("jsonlite")
```

```sh
Rscript main.R
```

All inputs are included; no download is needed. The run writes numerical results to `outputs/` and tables and figures to `Figures/`. A full run from the supplied inputs took about 20 seconds in the validation environment; reconstruction, tests and PDF compilation are separate.

## Settings and files

The `settings` block in `main.R` specifies default costs, sovereign recovery, private-loan losses, equilibrium-selection weights, sensitivities and purchase schemes. The supplied data and publication tables use the fixed country order DEU, FRA, ITA, ESP.

| Script | Calculations | Main outputs |
|---|---|---|
| `scripts/01_calibrate.R` | Accounting, equilibrium baseline and crisis sensitivities | `outputs/baseline.rds`, `outputs/sensitivities/` |
| `scripts/02_equilibria.R` | Equilibrium losses, compensation and recovery backstop | `outputs/equilibria.rds`, loss and policy CSVs |
| `scripts/03_floor_capacity.R` | Minimum capacity and endpoint tests | `outputs/floor_capacity/` |
| `scripts/04_bond_purchases.R` | Selected sovereign-bond purchase experiments | `outputs/bond_purchases/` |
| `scripts/05_figures_tables.R` | Publication assets, dependence sensitivity and 606 purchase counterfactuals | `Figures/`, purchase and sensitivity CSVs |

`R/model/` contains the clearing and policy calculations, `R/output/` produces publication assets, and `R/data/` reconstructs the inputs. Matrix rows are **creditors**, columns are **debtors**; model amounts are **EUR billions**.

## Validation

The validation suite checks data reconstruction, accounting consistency and the model's numerical results against the reference results supplied with the replication package. It uses the default parameters in `main.R`.

Install the additional packages required for data reconstruction:

```r
install.packages(c("dplyr", "tidyr", "quadprog", "readxl"))
```

After running `main.R`, execute:

```sh
Rscript scripts/run_tests.R
```

A summary and individual test logs are saved in `outputs/tests/`.

## Data and optional reconstruction

`data/processed/` contains the model inputs. The exposure register identifies sources, instruments, bilateral allocations and reconciliation adjustments. Identified related-office loans are removed; gross claims between distinct domestic banks are retained. The [data guide](data/README.md) documents coverage, units and transformations. `data/source_manifest.csv` records the supplied raw files and their checksums.

To reconstruct the inputs from the supplied source snapshots without overwriting them:

```sh
Rscript scripts/rebuild_data.R outputs/data_rebuild
```

The validation suite compares this reconstruction with the supplied inputs. The source snapshots define the paper's data vintage; fresh provider downloads may contain revisions.

### Redistribution

The MIT licence in `LICENSE` applies to the replication code. Statistical data retain the source-specific reuse conditions listed below.

| Source | Included data and reuse conditions |
|---|---|
| [ECB](https://www.ecb.europa.eu/services/using-our-site/disclaimer/html/index.en.html) and [ECB Banking Supervision](https://www.bankingsupervision.europa.eu/services/disclaimer/html/index.en.html) | Bank balance sheets, securities holdings, exchange rates and public capital disclosures. Reuse requires source attribution, accurate reproduction and identification of modifications. |
| [Eurostat](https://ec.europa.eu/eurostat/help/copyright-notice) | Financial accounts, government debt and household consumption. Reuse is allowed with attribution. Our allocations, conversions and pooling are identified as authors' calculations; Eurostat is not responsible for them. |
| [IMF](https://www.imf.org/en/about/copyright-and-terms) | Public Portfolio Investment Positions, formerly CPIS. The statistical-data terms allow redistribution with attribution and identification of transformations. Commercial reuse requires checking the IMF's permission requirements. |
| Authors' calibration inputs | `data/sovereign_spreads.csv` provides the four mean sovereign CDS spreads used to normalize bond payments. The [data guide](data/README.md) gives their definitions, sample period and units. |

The linked provider terms govern reuse of third-party data.
