# Sovereign Risk Spillovers across Countries and Sectors

Replication code for Benoît Mojon, Xavier Ragot and Jean-Paul Renne.
Run all commands from the repository root. The current paper uses public residence-based data for Germany, France, Italy and Spain at end-2024.

## Run the paper

Open `Rcodes_MRR.Rproj`, or start R from this directory. Numerical replication uses R (tested with 4.5.1) and `jsonlite`. To run both the replication and the full test suite, which also rebuilds the public data, install:

```r
install.packages(c("jsonlite", "dplyr", "tidyr", "quadprog", "readxl"))
```

Then run:

```sh
Rscript main.R
Rscript scripts/run_tests.R
```

The supplied inputs are sufficient: no download, CDS workbook, Python or C++ compiler is needed. Results go to `outputs/`; generated figures and tables go to `Figures/` and are copied into the local manuscript. `main.R` does not change an external Overleaf project.

## Where to start reading

`main.R` runs six scripts in order. Each stage has its own workspace and reads the previous stage's saved results.

| Script | Paper content | Main output |
|---|---|---|
| `scripts/01_calibrate.R` | Sections 2–3 and Appendix D: accounting, crisis parameters and reported sensitivities | `outputs/baseline.rds`, `outputs/sensitivities/` |
| `scripts/02_equilibria.R` | Sections 4–6: repayment equilibria, household receipt gaps, creditor compensation and common recovery backstop | `outputs/equilibria.rds`, CSV summaries |
| `scripts/03_floor_capacity.R` | Section 5 and Online Appendix: minimum capacity of the proportional repayment floor | `outputs/floor_capacity/` |
| `scripts/04_bond_purchases.R` | Section 7: the six purchase cases in the paper | `outputs/bond_purchases/` |
| `scripts/05_figures_tables.R` | Network charts, exposure and capital tables, backstop plots and dependence sensitivity | `Figures/` |
| `scripts/06_sync_manuscript.R` | Copy the generated assets used by the paper | `manuscript/paper/Figures/` |

The reusable mathematics is in `R/model/`: `clearing.R` enumerates bank default sets and guarantee regions; `floor_capacity.R` evaluates the capped-floor threshold; `bond_purchases.R` changes sovereign holdings into reserves. `financial_system.R` provides the independent bank-and-NBFI enumeration. `R/output/` contains the plotting and table routines. `R/data/` reconstructs the inputs from public snapshots.

Matrix **rows are creditors and columns are debtors**. Model amounts are **EUR billions**. Countries are ordered DEU, FRA, ITA, ESP. Dates such as `2024-10-01` in source labels and figure filenames identify the fourth-quarter snapshot, whose reference date is 31 December 2024.

## Calibration and checks

Banks retain their full accounting equity. Loss headroom is a country proxy based on public overall capital and leverage requirements. Bank default destroys 10% of the remaining repayment estate (`lambda = 0.90`). Sovereign default pays 50% of contractual bond payments. Additional borrower-country loan losses are 1.8% for firms and 1.7% for households, applied once for the crisis episode. These rates are calculated from the ECB stress-test portfolio observations.

The baseline has 25 banking equilibria across 16 sovereign states, with multiplicity in eight states. The proportional repayment floor requires about EUR 60.6 billion of common capacity and pays nothing in equilibrium. The common recovery backstop removes multiplicity at 40% on the five-percentage-point grid and at 36% on the supplementary one-percentage-point grid; neither grid establishes a continuous minimum.

The tests check source/accounting identities, the full public-data rebuild, independent clearing calculations, analytic floor examples, polynomial derivatives and cutoff endpoints, bond-purchase accounting, and numerical agreement with the paper before the code cleanup. `tests/fixtures/paper_results.rds` contains that compact numerical reference; it contains no original market time series. Test logs are saved under `outputs/tests/`.

## Data and rebuilding

`data/processed/` contains the readable CSV inputs used by `main.R`. The exposure register records each claim's source, instrument, allocation method and reconciliation. Missing bilateral positions are allocated within observed totals; they are not treated as direct observations. Related-office loans are removed, while domestic claims between distinct banks are retained.

`data/raw/` contains the corresponding public snapshots and reference extracts. The [data guide](data/README.md) lists the files, units, transformations and source links. `data/source_manifest.csv` records the supplied files and their checksums.

To reconstruct the processed tables from the public snapshots, write a separate rebuild:

```sh
Rscript scripts/rebuild_data.R outputs/data_rebuild
```

The tests compare this rebuild with `data/processed/`. Source files are left unchanged. Refreshing data from provider websites can change historical values; the supplied snapshots define the paper's vintage.

### Redistribution

The public statistical inputs can accompany a **non-commercial research replication package**, subject to the source conditions below. The MIT licence in `LICENSE` applies to our code, not to third-party data.

| Source | Included data and reuse conditions |
|---|---|
| [ECB](https://www.ecb.europa.eu/services/using-our-site/disclaimer/html/index.en.html) and [ECB Banking Supervision](https://www.bankingsupervision.europa.eu/services/disclaimer/html/index.en.html) | Bank balance sheets, securities holdings, exchange rates and public capital disclosures. Reuse requires source attribution, accurate reproduction and identification of modifications. The package includes statistical material; separately authored publications are not redistributed. |
| [Eurostat](https://ec.europa.eu/eurostat/help/copyright-notice) | Financial accounts, government debt and household consumption. Reuse is allowed with attribution. Our allocations, conversions and pooling are identified as authors' calculations; Eurostat is not responsible for them. |
| [IMF](https://www.imf.org/en/about/copyright-and-terms) | Public Portfolio Investment Positions, formerly CPIS. The statistical-data terms allow redistribution with attribution and identification of transformations. Commercial reuse requires checking the IMF's permission requirements. |
| Authors' calibration inputs | `data/sovereign_spreads.csv` contains only the four average spreads reported in the paper, at the precision used in computation. They determine the one-period sovereign payoff normalization; they are not a redistributed CDS history. |

The original `MRR_CDS.xlsx` workbook has **unverified redistribution rights**. It is kept separately in the authors' private working files and is not included in this repository or the replication archive. It is not read by the replication or data-rebuild scripts. Its source observations and other sheets are not redistributed.

Use `Rscript scripts/package_replication.R` to create `replication_package.zip` from the listed code, public inputs and manuscript files. This excludes `private_data/`, Git history, old backups and generated numerical outputs. This public repository has a clean history containing only the distributable files; the earlier private working history is not included. Provider terms were checked on 31 October 2026; the links above govern reuse.

## Manuscript

Both documents compile from `manuscript/paper/`:

```sh
latexmk -cd -pdf manuscript/paper/main.tex
latexmk -cd -pdf manuscript/paper/online_appendix.tex
```

Numerical replication does not require LaTeX. The source files for the paper and Online Appendix share the same figure directory and caption style.
