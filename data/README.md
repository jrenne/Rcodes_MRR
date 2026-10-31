# Data guide

The reference date is 31 December 2024. The four banking systems are resident offices in Germany, France, Italy and Spain, including foreign-owned offices and excluding offices abroad. Public supervisory capital data use consolidated groups; the country bridge to resident equity is a modelling proxy.

## Inputs used by the model

| File in `processed/` | Contents and units |
|---|---|
| `exposure_matrix.csv` | 24-account claim matrix, creditor rows and debtor columns; EUR bn. Rest-of-world and unallocated accounts are grouped only for the 22-node presentation. |
| `exposure_register.csv` | Instrument-level claims, source keys, observed/imputed status and reconciliation methods; EUR bn. |
| `bank_balance_sheets.csv` | Published bank controls, related-office adjustments and the reconciliation to the exposure network; EUR bn. |
| `bank_accounts.csv` | Provisions, accounting equity, fixed outside settlements, clearing debt and fixed repayment resources; EUR bn. |
| `firm_loans.csv`, `household_loans.csv` | Bank loans by creditor country and borrower country; EUR bn. Securities are excluded from these loan shocks. |
| `capital_proxies.csv`, `capital_proxy_by_bank.csv` | Ratios used to transfer public bank capital and leverage headroom to resident accounting equity. |
| `bank_capital_sample.csv` | The 56-bank public Pillar 3 sample; native-currency amounts, units, source rows and report links. Ratios are dimensionless. |
| `nbfi_buffers.csv` | Pooled nonbank financial assets, debt and buffers above non-equity liabilities; EUR bn. |
| `household_consumption.csv` | Observed 2024 household/NPISH consumption; EUR million, converted to EUR bn by the reporting script. |
| `stress_loss_rates.csv` | Exposure-weighted adverse-minus-baseline loan impairments, rounded to 1.8% for firms and 1.7% for households. Rates cover a three-year episode and are applied once. |

`sovereign_spreads.csv` contains four calibration values: full-sample mean five-year sovereign CDS spreads, December 2007–November 2025, in basis points. They are approximately 14.8, 27.9, 102.5 and 83.9 for DEU, FRA, ITA and ESP. These are pricing inputs, not end-2024 default forecasts. With spread `s` in decimal units and a 50% sovereign loss, the paper uses `pi = s / (0.5 * (1 + s))`. This makes the one-period expected contractual bond payment equal to one. The full market workbook is excluded from redistribution.

## Public source snapshots

- `raw/residence_2024Q4/`: [ECB BSI](https://data.ecb.europa.eu/data/datasets/BSI) and [SHSS](https://data.ecb.europa.eu/data/datasets/SHSS) CSV extracts, with source structures. BSI sector B supplies resident-bank controls; sector A supplies detailed MFI positions and sector F supplies money-market-fund adjustments. Source monetary amounts are EUR million. `retrieval_manifest.json` records the original query URLs and retrieval checksums.
- `raw/regulatory_headroom_2024Q4/`: public [Pillar 3 disclosures](https://www.bankingsupervision.europa.eu/framework/statistics/html/index.en.html) and [Supervisory Banking Statistics](https://www.bankingsupervision.europa.eu/framework/statistics/html/index.en.html) workbooks. The parser preserves source units and checks the reported ratios and capital stacks.
- `raw/nbfi_buffers_2024Q4/`: [Eurostat annual non-consolidated financial balance sheets](https://ec.europa.eu/eurostat/databrowser/view/nasa_10_f_bs/default/table?lang=en) for DE, FR, IT and ES. S123–S129 are pooled without adding overlapping parent sectors. Equity and fund shares are excluded from ordinary debt.
- `raw/reference/eurostat_raw_stock_reference.csv`: end-2024 [quarterly financial-account counterpart stocks](https://ec.europa.eu/eurostat/databrowser/view/nasq_10_f_cp/default/table?lang=en), in EUR million, retaining the original sector, instrument and financial-position codes.
- `raw/reference/government_nominal_bonds.csv`: [Eurostat government debt](https://ec.europa.eu/eurostat/databrowser/view/gov_10q_ggdebt/default/table?lang=en), the nominal debt-security control. Government loans are kept outside the sovereign-bond shock.
- `raw/reference/reference_layers_2024Q4.csv`: selected Eurostat domestic nonbank claims and an ECB refinancing diagnostic, prepared from the source vintage. This is a reference extract, not a separate set of observations to add to the bank controls.
- `raw/reference/pip_disjoint_observations_eur_million.csv`: [IMF Portfolio Investment Positions](https://data.imf.org/Datasets/PIP) converted using the closing exchange rate. Financial parent and child sectors are selected without double counting. Missing issuer/holder detail is allocated by `R/data/exposures.R`.
- `raw/reference/ecb_closing_fx_2024.csv`: [ECB foreign exchange reference rates](https://data.ecb.europa.eu/data/datasets/EXR), including the year-end USD/EUR conversion (1.0389).
- `raw/reference/bsi_capital_components.csv`: public ECB BSI capital components, including provisions. Only December observations are used; monthly stocks are never added across months.
- `raw/reference/household_consumption.json`: [Eurostat national accounts](https://ec.europa.eu/eurostat/databrowser/view/nama_10_gdp/default/table?lang=en), `P31_S14_S15`, current prices, EUR million, 2024.
- `raw/reference/stress_portfolios.csv`: four portfolio observations transcribed or digitised from [ECB, 2025 stress test results](https://www.bankingsupervision.europa.eu/ecb/pub/pdf/ssm.Report_2025_Stress_Test_202508.en.pdf), Chart 3.8, printed page 28. Rates and exposure weights are pooled by borrower sector. The data retain the original chart/image coordinates for traceability; the PDF itself is not needed or redistributed.

ECB and IMF exposure snapshots were acquired on 21 October 2026. Reference extracts retain the earlier source vintage used in the paper; the included source manifest identifies the actual supplied files. A new provider download can reflect revisions.

## Transformations and reuse

The authors convert units/currencies, select disjoint sectors, allocate missing bilateral positions, reconcile holder/issuer totals, remove related-office loans, and pool capital and stress-loss measures. Processed data and model results are authors' calculations based on the cited sources; they are not official ECB, Eurostat or IMF estimates. These institutions do not endorse the calculations. Eurostat is not responsible for the transformations or conclusions.

The root [README](../README.md#redistribution) gives the source-specific redistribution conditions. Public statistical data retain their provider terms and attribution; they are not relicensed under the code's MIT licence. `private_data/` is excluded from any distributable archive. Only four reported sovereign spread calibration inputs are supplied in its place.
