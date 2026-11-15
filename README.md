# Empirical Finance – Assignment 2: Multi-Factor Pricing, Yield-Curve Estimation and Factor-Model Stability

Reproducibility guide for the calculations behind our slides. Every number on the slides can be regenerated from the raw data in `Data/` using the code in `Q2/` and `Q3/` (R) and the workbook in `Q1/` (Excel).

> **AI-use disclosure.** AI assistance was used for code structure, debugging, model-selection design, diagnostics and drafting interpretations, in line with the course AI policy. The group validated the calculations and financial reasoning. The independent numerical checks that document this are listed in Section 8 and are built into the scripts (`stopifnot(...)` assertions in `Q3_parts_1-3.R`, `Q3_part4.R`, `Q3_parts_5-6.R`), and the pre-stated model-selection rule and caveats for Q3.4 are written to `Q3/results/q3_part4_model_audit.txt`.

---

## 1. Repository layout

```
.
├── Data/                                       Raw inputs (never edited by the scripts)
│   ├── Multifactor_Model.xlsx                      Q1: monthly Fama-French data, Jan 1947 – Aug 2020 (884 months)
│   ├── Yield_Curve.xlsx                            Q2: 52 Treasury bonds, settlement 12 Jan 2018
│   ├── Treasury_Par_Yield_Curve_2026-09-30.csv     Q3.5: Treasury par yields, 30 Sep 2026 (14 tenors)
│   ├── latest_french/                              Q3: refreshed Kenneth French files (monthly returns in PERCENT)
│   │   ├── 25_Portfolios_5x5.csv
│   │   ├── F-F_Research_Data_Factors.csv
│   │   ├── Portfolios_Formed_on_ME.csv
│   │   └── Portfolios_Formed_on_BE-ME.csv
│   ├── refresh_french_data.R / .py                 Re-download the four French files into latest_french/
│   └── 25_Portfolios_5x5.CSV, F-F_Research_Data_Factors.CSV,
│       Portfolios_Formed_on_ME.CSV, Portfolios_Formed_on_BE-ME.CSV
│                                                   Older (legacy) copies; not read by any script
├── Q1/
│   └── Empiricial Finance Assignment 2 Excel.xlsx  Q1.1–Q1.5 workings (live Excel formulas) and written answers
├── Q2/
│   └── Assignment 2 Question 2.R                   Q2.1–Q2.6 yield-curve estimation and bond pricing
├── Q3/
│   ├── Q3_parts_1-3.R                              Q3.1–Q3.3  CAPM vs FF3, subperiods, rolling regressions
│   ├── Q3_part4.R                                  Q3.4  interaction model, chronological holdout
│   ├── Q3_parts_5-6.R                              Q3.5–Q3.6  2026 par curve, forwards, audit checks
│   └── results/                                    27 files written by the Q3 scripts (CSV, PNG, one TXT)
├── emp-finance-assignment-2.Rproj
└── README.md
```

## 2. Requirements

| Part | Tool | Needed |
|---|---|---|
| Q1 | Microsoft Excel | Nothing beyond built-in functions (`INTERCEPT`, `SLOPE`, `RSQ`, `STEYX`, `LINEST`) |
| Q2 | R (tested on 4.3.3) | `readxl`, `dplyr`, `lubridate`, `ggplot2`, `scales` |
| Q3.1–Q3.6 | R (tested on 4.3.3) | **Base R only** – no packages |
| Optional data refresh | R or Python 3 | Internet access (downloads from Kenneth French's data library) |

```r
install.packages(c("readxl", "dplyr", "lubridate", "ggplot2", "scales"))
```

## 3. How to reproduce

**Q3 scripts – run from the repository root** (the folder containing `Data/`, `Q2/`, `Q3/`). Run them in this order, because Part 4 reads a file written by Parts 1–3:

```bash
Rscript Q3/Q3_parts_1-3.R     # writes Q3/results/q3_part2_* and q3_part3_* (CSV + PNG)
Rscript Q3/Q3_part4.R         # needs q3_part2_matched_monthly_data.csv from the previous step
Rscript Q3/Q3_parts_5-6.R     # independent of the other two; writes q3_part5_* (CSV + PNG)
```

**Q2 script – run from inside `Data/`.** It opens `Yield_Curve.xlsx` by bare file name, so it fails from the repository root (`path does not exist: 'Yield_Curve.xlsx'`):

```bash
cd Data
Rscript "../Q2/Assignment 2 Question 2.R"     # results print to the console; the fitted-curve plot goes to Rplots.pdf
```

In RStudio, open `emp-finance-assignment-2.Rproj` (this sets the working directory to the repo root) and *Source* each Q3 script; for Q2 first run `setwd("Data")`.

**Q1** is an Excel workbook; open it and read the sheets listed in Section 4. All regressions are live formulas on the `Excess` sheet.

**Optional – refresh the French data:** `Rscript Data/refresh_french_data.R` (or `python Data/refresh_french_data.py`) re-downloads the four files into `Data/latest_french/`. The reported Q3 numbers use the files supplied in the repository, which end in September 2026; a refresh will add later months and change every Q3 number.

Conventions common to the Q3 scripts: monthly returns and factors are in **percent** (1.0 = 1%); HAC standard errors are Newey–West with 6 monthly lags (Bartlett weights, no pre-whitening, n/(n−k) correction), implemented in base R inside the script; significance means a two-sided test at 5%.

---

## 4. Question 1 – Multi-factor asset pricing (`Q1/Empiricial Finance Assignment 2 Excel.xlsx`)

**Input:** `Data/Multifactor_Model.xlsx` (Kenneth French data, monthly, Jan 1947 – Aug 2020, **884 months**, returns in % per month). Portfolio excess return = portfolio return − RF.

| Sheet | Contents |
|---|---|
| `README` | Short description of the workbook |
| `Data` | Original series: Mkt−RF, SMB, HML, RF, size deciles, book-to-market deciles and the 25 size × B/M portfolios |
| `Excess` | Portfolio excess returns R<sub>p</sub> − R<sub>f</sub> (formulas) |
| `Q1.1 CAPM` | Alpha, beta, t-statistics, predicted vs actual mean return, SML chart |
| `Q1.2 Variance` | Total, systematic (β²·Var(Mkt)) and idiosyncratic variance and shares |
| `Q1.3 Size&Value` | CAPM alphas by size and B/M decile and the 5×5 alpha grid |
| `Q1.4 FF3` | Three-factor regressions for all 45 portfolios (`LINEST`) |
| `Q1.4 Compare25` | CAPM vs FF3 for the 25 portfolios: alphas, adjusted R², residual volatility, GRS test |
| `Answers` | Written answers to Q1.1–Q1.5 |

t-statistics in Q1 use ordinary OLS standard errors (`LINEST`); HAC standard errors are used only in Q3.

**Key results (Q1, 1947–Aug 2020):**

| Item | Value |
|---|---|
| Market excess return | mean 0.658% / month, SD 4.28% / month |
| Mean SMB / HML | 0.11% / 0.27% per month |
| Size deciles (smallest → largest) | beta 1.101 → 0.939; mean excess return 0.796% → 0.622% per month: consistent with CAPM |
| B/M deciles (growth → value) | mean excess return 0.626% → 0.923% per month while beta stays flat (≈ 0.91–1.16): not explained by CAPM |
| CAPM → FF3, 25 portfolios: mean \|α\| | 0.163% → 0.093% per month |
| Alphas with \|t\| > 1.96 | 13 → 7 |
| Mean adjusted R² | 0.747 → 0.909 (rises for all 25 portfolios) |
| Mean residual SD | 2.72% → 1.59% per month |
| GRS test (α = 0 jointly, 25 portfolios) | CAPM F = 4.25; FF3 F = 3.85; both reject |

**Note on the GRS statistics.** Excel has no multivariate alpha test, so the two GRS values were computed in Python (`q1_analysis.py`, referred to in the workbook's `README` sheet) and pasted into the workbook as inputs. That script is **not** in this repository, so these two numbers cannot be regenerated from it; everything else in Q1 is a live formula.

---

## 5. Question 2 – Yield-curve estimation and bond pricing (`Q2/Assignment 2 Question 2.R`)

**Input:** sheet `Interpolating the Yield Curve` of `Data/Yield_Curve.xlsx`: **52 Treasury bonds** (maturity, coupon, asked price, last coupon date), maturing between 15 Feb 2018 and 15 Aug 2040. Settlement date **12 Jan 2018**. Prices are per $100 face value; coupons are semi-annual.

**Model.** Continuously compounded yield as a cubic polynomial in time to payment *t* (years, actual/365.25): y(t) = a₀ + a₁t + a₂t² + a₃t³ (floored at 0.000001). Discount factor = exp(−y(t)·t).

| Part | What the code does |
|---|---|
| 2.1 | Accrued interest = (coupon/2) × (days since last coupon ÷ days in the coupon period); full (dirty) price = asked + accrued |
| 2.4 (run first) | `optim` (Nelder–Mead, 10,000 iterations) minimizes the sum of squared differences between dirty and model prices over (a₀, a₁, a₂, a₃), starting from (0.016, 0.002, −0.0001, 0.000002). Run first because 2.2 needs the optimized parameters |
| 2.2 | For every payment date 15 Feb 2018 – 15 Aug 2040 (6-monthly): time to payout, discount rate y(t) and price of a $1 zero-coupon bond |
| 2.3 | Present value of every coupon and principal payment; model price = sum of PVs |
| 2.4 | Pricing errors (dirty − model), in dollars and percent |
| 2.5 | Plot of the fitted curve for 0–25 years |
| 2.6 | (a) `summary()` of asked prices; (d) 1-year forward rate starting Aug 2020; (e) real return on the Aug 2030 zero with 2% inflation; (f) price of a default-free 15-year 10% bond; (g) price with a constant default intensity λ = 3% added to the discount rate |

**Expected output:**

| Item | Value |
|---|---|
| Optimal (a₀, a₁, a₂, a₃) | 0.017722, 0.001558, −0.000100, 0.0000023 (convergence code 0) |
| Dollar pricing errors | min −0.24, max +0.29, mean 0.02, **RMSE 0.12** |
| Percent pricing errors | −0.20% to **+0.22%** |
| Asked prices (2.6a) | min 92.06, median 105.98, mean 111.70, max 135.57 (most bonds trade at a premium) |
| 1-year forward rate starting Aug 2020 (2.6d) | **2.47%** vs 1-year spot (zero maturing Aug 2021) 2.21% |
| Yield on the Aug 2030 zero / exact real return (2.6e) | 2.61% / **0.60%** = (1 + y)/(1.02) − 1 |
| 15-year 10% default-free bond (2.6f) | **$193.69** (30 semi-annual coupons of $5, plus $100) |
| Same bond, λ = 3% (2.6g) | **$147.11** |

Because the fitted curve is a flexible cubic that is estimated on the same 52 prices it is then scored on, the small errors above are in-sample fit, not evidence of future pricing accuracy.

---

## 6. Question 3 – Factor-model stability, model selection and a current yield curve (`Q3/`)

### 6.1 Q3.1–Q3.3: `Q3_parts_1-3.R`

**Inputs:** `Data/latest_french/25_Portfolios_5x5.csv` (section "Average Value Weighted Returns -- Monthly") and `Data/latest_french/F-F_Research_Data_Factors.csv` (the first, monthly, table). The French CSVs contain several tables, so the script reads only the monthly rows (six-digit YYYYMM) and joins by date, never by row position.

**Five test portfolios** (four corners and the centre of the 5×5 size × B/M grid): Small Growth (`SMALL LoBM`), Small Value (`SMALL HiBM`), Middle (`ME3 BM3`), Big Growth (`BIG LoBM`), Big Value (`BIG HiBM`).

**Sample:** Jan 1947 – Sep 2026, **N = 957** months; mean Mkt−RF 0.686, SMB 0.084, HML 0.300 (% per month). To use the full history set `sample_start <- "192607"`; to test the lag choice set `nw_lag <- 12L`.

| Step | Method |
|---|---|
| Q3.2 | For each portfolio, CAPM `excess ~ Mkt_RF` and FF3 `excess ~ Mkt_RF + SMB + HML` by OLS; Newey–West HAC standard errors and p-values; adjusted R²; residual SD |
| Q3.3 A | The same two models in four subperiods: 1947–1999 (636 months), 2000–2009 (120), 2010–2019 (120), 2020–2026 (81); range (max − min) of each FF3 loading across subperiods |
| Q3.3 B | Rolling 60-month FF3 regressions (898 windows per portfolio; first window ends Dec 1951, last Sep 2026); each window uses only data through its end date |
| Check | The Small Value CAPM alpha is recomputed without `lm()` as mean(y) − [Cov(y, m)/Var(m)]·mean(m); the script stops if it differs from the regression by more than 1e-10 |

Subperiod and rolling differences are **descriptive**; they are not formal tests of structural breaks.

**Expected output – CAPM vs FF3 (HAC p-values in parentheses):**

| Portfolio | CAPM α (%/month) | FF3 α (%/month) | Adj. R² CAPM → FF3 | Residual SD reduction |
|---|---|---|---|---|
| Small Growth | −0.575 (<0.001) | −0.458 (<0.001) | 0.595 → 0.856 | 40.4% |
| Small Value | +0.408 (0.004) | +0.181 (<0.001) | 0.585 → 0.913 | 54.2% |
| Middle | +0.093 (0.275) | −0.034 (0.528) | 0.799 → 0.903 | 30.7% |
| Big Growth | +0.011 (0.854) | +0.132 (<0.001) | 0.890 → 0.948 | 31.5% |
| Big Value | +0.167 (0.200) | −0.152 (0.097) | 0.626 → 0.808 | 28.3% |

Significant alphas at 5%: CAPM 2 portfolios (Small Growth, Small Value); FF3 3 portfolios (Small Growth, Small Value, Big Growth). **No significant CAPM alpha disappears under FF3.** Independent check: Small Value CAPM β = 1.063106, α = +0.408385% / month.

**FF3 loadings, full sample (HAC standard errors in parentheses):**

| Portfolio | Market β | SMB | HML |
|---|---|---|---|
| Small Growth | 1.117 (0.026) | +1.429 (0.042) | −0.189 (0.043) |
| Small Value | 0.952 (0.015) | +1.103 (0.044) | +0.705 (0.026) |
| Middle | 0.974 (0.019) | +0.467 (0.063) | +0.382 (0.047) |
| Big Growth | 1.003 (0.011) | −0.232 (0.018) | −0.346 (0.018) |
| Big Value | 1.135 (0.032) | −0.094 (0.045) | +0.846 (0.032) |

**Stability results:**

| Item | Value |
|---|---|
| Largest subperiod range | Big Value SMB: 0.540 (−0.380 in 2000–09 to +0.160 in 2020–26); the only loading that changes sign across subperiods |
| Most stable | Big Growth (all three ranges about 0.11 or less); Small Value HML stays between 0.691 and 0.717 |
| Highest rolling SD | Small Growth HML: 0.302 across 898 windows (range −0.641 to +0.877) |
| Rolling loadings that change sign | Small Growth HML, Middle HML, Big Value SMB |

Outputs: `q3_part2_all_regression_results.csv`, `q3_part2_capm_vs_ff3_comparison.csv`, `q3_part2_matched_monthly_data.csv`, `q3_part2_adjusted_R2.png`, `q3_part2_alpha_HAC_95CI.png`, `q3_part3_subperiod_CAPM_FF3.csv`, `q3_part3_subperiod_FF3_loadings.csv`, `q3_part3_subperiod_loading_ranges.csv`, `q3_part3_rolling_60m_FF3.csv`, `q3_part3_rolling_stability_summary.csv`, and five `q3_part3_rolling_<portfolio>.png` charts.

### 6.2 Q3.4: `Q3_part4.R`

**Input:** `Q3/results/q3_part2_matched_monthly_data.csv` (run `Q3_parts_1-3.R` first).

**Question:** do two market-state interaction terms improve on FF3? Richer model: `excess ~ Mkt_RF + SMB + HML + Mkt_RF×SMB + Mkt_RF×HML`. The interactions are conditional exposures, not tradable factors.

**Selection rule (written before the results were examined, thresholds not changed afterwards):** estimate on 1947–1999 (636 months) only, evaluate on 2000–Sep 2026 (321 months) with no re-fitting; adopt the interaction model only if the **pooled** holdout RMSE falls by at least 2% **and** at least 3 of the 5 portfolios improve. Full-sample HAC tests (including a joint Wald F test of both interaction coefficients) are secondary and cannot overturn the rule.

**Expected output:**

| Portfolio | FF3 RMSE | Interaction RMSE | Change | Joint interaction HAC p (full sample) |
|---|---|---|---|---|
| Small Growth | 3.0973 | 3.1249 | −0.89% (worse) | 0.378 |
| Small Value | 2.5316 | 2.5366 | −0.20% (worse) | 0.706 |
| Middle | 1.8761 | 1.8818 | −0.31% (worse) | 0.079 |
| Big Growth | 0.8741 | 0.9159 | −4.78% (worse) | 0.010 |
| Big Value | 3.3401 | 3.3338 | +0.19% (better) | 0.409 |
| **Pooled** | **2.5077** | **2.5177** | **−0.40% (worse)** | |

Portfolios improved: **1 of 5**. Decision: **retain standard FF3**. Big Growth shows a significant in-sample interaction (p = 0.010) yet the largest holdout deterioration, which is why in-sample p-values are not used for selection. Independent matrix-based check: Small Growth FF3 holdout RMSE = 3.097271% (the script stops if it differs from the `lm()` value by more than 1e-8).

**Caveats (also in `q3_part4_model_audit.txt`):** the 2000–2026 period was already inspected in Q3.3, so this is a chronological pseudo-out-of-sample test, not an untouched one; realized same-month factors enter the fitted return, so the RMSE measures conditional fit, not an ex-ante forecast; two extra regressors were tested on five portfolios, so full-sample p-values are descriptive only.

Outputs: `q3_part4_holdout_comparison.csv`, `q3_part4_holdout_errors.csv`, `q3_part4_full_sample_model_results.csv`, `q3_part4_coefficients_HAC.csv`, `q3_part4_holdout_RMSE_improvement.png`, `q3_part4_model_audit.txt`.

### 6.3 Q3.5–Q3.6: `Q3_parts_5-6.R`

**Input:** `Data/Treasury_Par_Yield_Curve_2026-09-30.csv` (wide format, one row, tenors 1 Mo – 30 Yr, yields in percent). The 2018-01-12 comparison curve is read from `Data/Treasury_Par_Yield_Curve_2018-01-12_reference.csv` if that file exists; it is **not** in the repository, so the script uses the Treasury par yields hard-coded inside it (1M 1.31 … 30Y 2.85).

**Important:** these are Treasury **par** yields, not zero-coupon spot rates. The forward rates below treat par yields as annual-effective spot-rate proxies, with D(t) = (1 + y)^(−t) and forward = [D(t₁)/D(t₂)]^(1/(t₂−t₁)) − 1. They are transparent approximations, not bootstrapped forwards.

**Expected output:**

| Item | Value |
|---|---|
| 2026 3-month / 10-year / 20-year / 30-year par yield | 4.20% / 5.29% / 5.68% / 5.64% |
| 10Y − 3M slope, 2026 vs 2018 | +1.09 pp vs +1.12 pp |
| Change in 10-year par yield, 2018 → 2026 | +2.74 pp (2.55% → 5.29%) |
| Curvature proxy 2·5Y − 2Y − 10Y | 0.010 pp |
| Approx. forward 1Y → 2Y | 5.221% |
| Approx. forward 2Y → 3Y | 5.240% |
| Approx. forward 3Y → 5Y | 5.362% simple per year / 5.225% effective annual |

Independent check: the 1Y→2Y forward is computed by two algebraic routes (discount-factor ratio and (1 + y₂)²/(1 + y₁) − 1); the script stops unless they agree to 1e-10. Q3.6 audit points printed at the end of the script: FF3 raises R² but significant alphas remain; the interaction model does not beat FF3 out of sample; in-sample curve-fitting errors are not forecast errors; forwards are par-yield proxies.

Outputs: `q3_part5_2026_par_curve.csv`, `q3_part5_2018_par_reference.csv`, `q3_part5_approx_forward_rates.csv`, `q3_part5_key_facts.csv`, `q3_part5_2026_par_yield_curve.png`, `q3_part5_par_curve_2018_vs_2026.png`.

---

## 7. Notes and known limitations

* **Two different samples.** Q1 uses the older workbook (Jan 1947 – Aug 2020, 884 months); Q3 uses refreshed French files (Jan 1947 – Sep 2026, 957 months). Q1 and Q3 numbers (for example CAPM alphas on similar portfolios) are therefore not directly comparable.
* **Q2 is console-only.** The script prints its results and shows the plot but saves no files, so the numbers in Section 5 come from the console.
* **Q2 working directory.** Run it from `Data/` (Section 3).
* **Q1 GRS values** are pasted inputs from a Python script that is not in the repository (Section 4).
* **Legacy files.** The four `Data/*.CSV` files in upper case are earlier copies of the French data; the Q3 scripts read only `Data/latest_french/`.
* The code comment in Q2 describes the yield function as a "Nelson–Siegel simplified form"; it is a cubic polynomial in maturity, as described in Section 5.

## 8. How the results were checked

* The three Q3 scripts were re-run from a clean copy of the repository (R 4.3.3). All 16 numeric CSV files agree with the committed versions to floating-point precision (largest difference 3e-14); the committed `q3_part4_model_audit.txt` is byte-identical, and the matched-data and par-curve CSVs are identical.
* The Q2 script was re-run from `Data/` and reproduces the parameters, RMSE (0.12), maximum errors (+0.29, 0.22%), forward rate (2.47%), real return (0.60%) and bond prices ($193.69, $147.11) in Section 5.
* Built-in checks that stop the scripts if they fail: Small Value CAPM alpha without `lm()` (`Q3_parts_1-3.R`); Small Growth FF3 holdout RMSE from the OLS normal equations (`Q3_part4.R`); 1Y→2Y forward by two routes (`Q3_parts_5-6.R`).
* The Q1 workbook's headline figures (market mean and SD, SMB/HML means, and the CAPM → FF3 changes in mean |α|, significant alphas, adjusted R² and residual SD over the 25 portfolios) were recomputed independently from `Data` with NumPy and agree. The GRS statistics could not be recomputed (Section 4).
