# ============================================================================
# Empirical Finance Assignment 2 -- Question 3, Parts 1, 2 and 3
# Five size x book-to-market portfolios: CAPM vs FF3 and factor stability
#
# Run this script with the R working directory set to your REPOSITORY ROOT.
# Expected input files:
#   Data/latest_french/25_Portfolios_5x5.csv
#   Data/latest_french/F-F_Research_Data_Factors.csv
#
# Only base R is required; no additional packages are needed.
# Source: Kenneth R. French Data Library. Monthly returns are in PERCENT.
# ============================================================================

# STEP 1: Settings -------------------------------------------------------------
portfolio_file <- "Data/latest_french/25_Portfolios_5x5.csv"
factor_file    <- "Data/latest_french/F-F_Research_Data_Factors.csv"
output_dir     <- "Q3/results"

sample_start <- "194701"    # Align with the legacy workbook (Jan 1947)
sample_end   <- "202609"    # Last month in the supplied refreshed CSVs
nw_lag       <- 6L          # Six monthly lags; Bartlett/Newey-West HAC

# Optional: change sample_start to "192607" for the full-sample robustness check.
# Optional: change nw_lag to 12L to check sensitivity of statistical significance.

if (!file.exists(portfolio_file) || !file.exists(factor_file)) {
  stop(paste0(
    "Cannot find one or both CSV files. Set R's working directory to the ",
    "repository root (see getwd()), or edit portfolio_file/factor_file above."
  ))
}
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# STEP 2: Read ONLY the monthly table from each Kenneth French CSV -------------
# French CSVs include introductory text, annual data, and (for portfolios)
# additional equal-weighted returns. read.csv(file) alone is NOT sufficient.
read_french_monthly <- function(filename, section_heading = NULL) {
  lines <- readLines(filename, warn = FALSE, encoding = "UTF-8")

  if (is.null(section_heading)) {
    # This header occurs twice (monthly and annual). We need the FIRST one.
    header_matches <- which(grepl(
      "^\\s*,Mkt-RF\\s*,SMB\\s*,HML\\s*,RF\\s*$", lines
    ))
    if (length(header_matches) == 0L) stop("Factor header not found")
    header_line <- header_matches[1L]
  } else {
    section_matches <- which(trimws(lines) == section_heading)
    if (length(section_matches) == 0L) {
      stop(paste("Portfolio section not found:", section_heading))
    }
    header_line <- section_matches[1L] + 1L
  }

  # The next rows must begin with a SIX-DIGIT YYYYMM code.
  # Stop at the first blank row or the next section (annual/equal weighted).
  row_ids <- integer(0)
  i <- header_line + 1L
  while (i <= length(lines) && grepl("^\\s*[0-9]{6}\\s*,", lines[i])) {
    row_ids <- c(row_ids, i)
    i <- i + 1L
  }
  if (length(row_ids) == 0L) stop("No monthly rows found in ", filename)

  names_from_file <- trimws(strsplit(
    lines[header_line], ",", fixed = TRUE
  )[[1L]])
  names_from_file[1L] <- "YYYYMM"
  csv_text <- paste(c(
    paste(names_from_file, collapse = ","), lines[row_ids]
  ), collapse = "\n")
  df <- read.csv(
    text = csv_text, check.names = FALSE, stringsAsFactors = FALSE,
    strip.white = TRUE
  )

  # Keep YYYYMM as a sortable six-character string.
  df$YYYYMM <- as.character(df$YYYYMM)
  if (anyDuplicated(df$YYYYMM)) stop("Duplicate monthly dates in ", filename)

  # Replace the dataset's missing-value codes, if any.
  for (nm in setdiff(names(df), "YYYYMM")) {
    df[[nm]][df[[nm]] %in% c(-99.99, -999)] <- NA_real_
  }
  df
}

portfolios <- read_french_monthly(
  portfolio_file, "Average Value Weighted Returns -- Monthly"
)
factors <- read_french_monthly(factor_file)
names(factors)[names(factors) == "Mkt-RF"] <- "Mkt_RF"

# STEP 3: Choose five test assets (Q3 Part 1) ----------------------------------
# Four corners of the 5x5 size-B/M grid, plus its middle (3,3).
portfolio_names <- c(
  "Small Growth" = "SMALL LoBM",  # Size 1, book/market 1
  "Small Value"  = "SMALL HiBM",  # Size 1, book/market 5
  "Middle"       = "ME3 BM3",    # Size 3, book/market 3
  "Big Growth"   = "BIG LoBM",   # Size 5, book/market 1
  "Big Value"    = "BIG HiBM"    # Size 5, book/market 5
)
needed_portfolios <- unname(portfolio_names)
needed_factors <- c("YYYYMM", "Mkt_RF", "SMB", "HML", "RF")
stopifnot(all(needed_portfolios %in% names(portfolios)))
stopifnot(all(needed_factors %in% names(factors)))

# Join on dates, NEVER on row position. This prevents date misalignment.
dat <- merge(
  factors[, needed_factors],
  portfolios[, c("YYYYMM", needed_portfolios)],
  by = "YYYYMM", sort = TRUE
)
dat <- dat[
  dat$YYYYMM >= sample_start & dat$YYYYMM <= sample_end,
  , drop = FALSE
]
# Use the same complete-case sample for all portfolios and both models.
dat <- dat[complete.cases(dat), , drop = FALSE]
if (nrow(dat) < 60L) stop("Insufficient matched monthly observations")

cat("Final sample:", dat$YYYYMM[1L], "to",
    dat$YYYYMM[nrow(dat)], "; N =", nrow(dat), "months\n")
cat("Means of market, SMB, HML (% monthly):",
    round(mean(dat$Mkt_RF), 3), round(mean(dat$SMB), 3),
    round(mean(dat$HML), 3), "\n")

# STEP 4: Implement Newey-West HAC standard errors in base R ------------------
# Uses Bartlett weights w_l = 1 - l/(L+1), no prewhitening, and n/(n-k)
# small-sample adjustment. Coefficients still come from ordinary OLS.
newey_west_vcov <- function(model, lag = 6L) {
  X <- model.matrix(model)
  u <- as.numeric(residuals(model))
  n <- nrow(X)
  k <- ncol(X)
  L <- min(as.integer(lag), n - 1L)
  Z <- X * u                         # Each row of X multiplied by residual u_t
  S <- crossprod(Z)                 # Contemporaneous score covariance sum

  if (L > 0L) {
    for (ell in seq_len(L)) {
      gamma <- crossprod(
        Z[(ell + 1L):n, , drop = FALSE],
        Z[seq_len(n - ell), , drop = FALSE]
      )
      weight <- 1 - ell / (L + 1)
      S <- S + weight * (gamma + t(gamma))
    }
  }

  XX_inv <- solve(crossprod(X))
  V <- XX_inv %*% S %*% XX_inv * (n / (n - k))
  dimnames(V) <- list(colnames(X), colnames(X))
  V
}

summarize_model <- function(model, portfolio_name, model_name, nw_lag) {
  beta <- coef(model)
  nw_se <- sqrt(pmax(0, diag(newey_west_vcov(model, lag = nw_lag))))
  # Make coefficient labels explicit (do not rely on pmax() retaining names).
  names(nw_se) <- names(beta)
  nw_t <- beta / nw_se
  nw_p <- 2 * pt(-abs(nw_t), df = df.residual(model))
  names(nw_p) <- names(beta)

  get <- function(x, key) if (key %in% names(x)) unname(x[key]) else NA_real_
  data.frame(
    Portfolio = portfolio_name,
    Model = model_name,
    N_months = nobs(model),
    Alpha_pct_month = get(beta, "(Intercept)"),
    Alpha_HAC_SE_pct = get(nw_se, "(Intercept)"),
    Alpha_HAC_t = get(nw_t, "(Intercept)"),
    Alpha_HAC_p = get(nw_p, "(Intercept)"),
    Market_beta = get(beta, "Mkt_RF"),
    Market_HAC_SE = get(nw_se, "Mkt_RF"),
    SMB_loading = get(beta, "SMB"),
    SMB_HAC_SE = get(nw_se, "SMB"),
    HML_loading = get(beta, "HML"),
    HML_HAC_SE = get(nw_se, "HML"),
    Adjusted_R2 = summary(model)$adj.r.squared,
    Residual_SD_pct_month = summary(model)$sigma,
    stringsAsFactors = FALSE
  )
}

# STEP 5: Estimate CAPM and FF3 for each of the five portfolios (Q3 Part 2) ---
results_list <- list()
for (portfolio_label in names(portfolio_names)) {
  column <- portfolio_names[[portfolio_label]]
  d <- dat
  d$portfolio_excess <- d[[column]] - d$RF

  # R_pt - RF_t = alpha + beta_M * (Mkt - RF)_t + error
  capm <- lm(portfolio_excess ~ Mkt_RF, data = d)

  # R_pt - RF_t = alpha + beta_M * (Mkt - RF)_t
  #                           + s * SMB_t + h * HML_t + error
  ff3 <- lm(portfolio_excess ~ Mkt_RF + SMB + HML, data = d)

  results_list[[paste0(portfolio_label, " CAPM")]] <- summarize_model(
    capm, portfolio_label, "CAPM", nw_lag
  )
  results_list[[paste0(portfolio_label, " FF3")]] <- summarize_model(
    ff3, portfolio_label, "FF3", nw_lag
  )
}
results <- do.call(rbind, results_list)
rownames(results) <- NULL

# STEP 6: Compare model fit and changes in alpha -------------------------------
capm_rows <- results[results$Model == "CAPM", ]
ff3_rows  <- results[results$Model == "FF3", ]
stopifnot(identical(capm_rows$Portfolio, ff3_rows$Portfolio))
comparison <- data.frame(
  Portfolio = capm_rows$Portfolio,
  CAPM_alpha = capm_rows$Alpha_pct_month,
  CAPM_alpha_p = capm_rows$Alpha_HAC_p,
  FF3_alpha = ff3_rows$Alpha_pct_month,
  FF3_alpha_p = ff3_rows$Alpha_HAC_p,
  CAPM_adjusted_R2 = capm_rows$Adjusted_R2,
  FF3_adjusted_R2 = ff3_rows$Adjusted_R2,
  CAPM_residual_SD = capm_rows$Residual_SD_pct_month,
  FF3_residual_SD = ff3_rows$Residual_SD_pct_month,
  Residual_SD_reduction_pct = 100 * (
    1 - ff3_rows$Residual_SD_pct_month / capm_rows$Residual_SD_pct_month
  ),
  CAPM_alpha_significant_5pct = capm_rows$Alpha_HAC_p < 0.05,
  FF3_alpha_significant_5pct = ff3_rows$Alpha_HAC_p < 0.05
)
comparison$CAPM_alpha_disappears <- (
  comparison$CAPM_alpha_significant_5pct &
  !comparison$FF3_alpha_significant_5pct
)

# STEP 7: Export tables for slides, appendix, and future analysis --------------
write.csv(results,
          file.path(output_dir, "q3_part2_all_regression_results.csv"),
          row.names = FALSE)
write.csv(comparison,
          file.path(output_dir, "q3_part2_capm_vs_ff3_comparison.csv"),
          row.names = FALSE)
write.csv(dat,
          file.path(output_dir, "q3_part2_matched_monthly_data.csv"),
          row.names = FALSE)

cat("\n===== REGRESSIONS (monthly returns in percent) =====\n")
print(results, row.names = FALSE, digits = 4)
cat("\n===== CAPM VERSUS FF3 COMPARISON =====\n")
print(comparison, row.names = FALSE, digits = 4)
cat("\n===== ALPHAS THAT DISAPPEAR AT THE 5% SIGNIFICANCE LEVEL =====\n")
disappeared <- comparison$Portfolio[comparison$CAPM_alpha_disappears]
if (length(disappeared) == 0L) {
  cat("NONE for this selection and sample.\n")
} else {
  cat(paste(disappeared, collapse = ", "), "\n")
}
cat("\nCSV results written to:", normalizePath(output_dir), "\n")

# STEP 8 (optional): Full-sample robustness -----------------------------------
# Set sample_start <- "192607" and rerun the script.
# This uses the entire overlapping history in the supplied 2026 files.
# You can also compare p-values with nw_lag <- 12L.

# STEP 9: Independently verify one CAPM result without lm() --------------------
# For a univariate CAPM regression, beta = Cov(Y,M)/Var(M) and
# alpha = mean(Y) - beta*mean(M). We verify the Small Value CAPM alpha.
small_value_y <- dat[["SMALL HiBM"]] - dat$RF
beta_manual <- cov(small_value_y, dat$Mkt_RF) / var(dat$Mkt_RF)
alpha_manual <- mean(small_value_y) - beta_manual * mean(dat$Mkt_RF)
alpha_from_regression <- comparison$CAPM_alpha[
  comparison$Portfolio == "Small Value"
]
stopifnot(isTRUE(all.equal(
  as.numeric(alpha_manual), as.numeric(alpha_from_regression),
  tolerance = 1e-10
)))
cat("\nIndependent check, Small Value CAPM:\n")
cat("  Beta =", round(beta_manual, 6),
    "; Alpha =", round(alpha_manual, 6), "% per month\n")

# STEP 10: Generate two slide-ready plots (base R; no additional packages) ----
# Chart A: Does adjusted R-squared improve after adding SMB and HML?
png(file.path(output_dir, "q3_part2_adjusted_R2.png"),
    width = 1250, height = 800, res = 150)
par(mar = c(8, 5, 4, 2))
fit_values <- rbind(
  CAPM = comparison$CAPM_adjusted_R2,
  FF3  = comparison$FF3_adjusted_R2
)
barplot(
  fit_values, beside = TRUE,
  names.arg = comparison$Portfolio,
  las = 2,
  ylim = c(0, 1),
  col = c("#3B82F6", "#F59E0B"),
  ylab = "Adjusted R-squared",
  main = "CAPM vs FF3: Model fit (1947-2026)",
  legend.text = c("CAPM", "FF3"),
  args.legend = list(x = "topleft", bty = "n")
)
dev.off()

# Chart B: Alphas and 95% confidence intervals using HAC standard errors.
capm_critical <- qt(0.975, df = capm_rows$N_months - 2L)
ff3_critical  <- qt(0.975, df = ff3_rows$N_months - 4L)
capm_ci <- capm_critical * capm_rows$Alpha_HAC_SE_pct
ff3_ci  <- ff3_critical * ff3_rows$Alpha_HAC_SE_pct
x <- seq_along(comparison$Portfolio)
y_range <- range(c(
  capm_rows$Alpha_pct_month - capm_ci,
  capm_rows$Alpha_pct_month + capm_ci,
  ff3_rows$Alpha_pct_month - ff3_ci,
  ff3_rows$Alpha_pct_month + ff3_ci,
  0
))

png(file.path(output_dir, "q3_part2_alpha_HAC_95CI.png"),
    width = 1250, height = 800, res = 150)
par(mar = c(8, 5, 4, 2))
plot(x - 0.08, capm_rows$Alpha_pct_month,
     xlim = c(0.5, length(x) + 0.5), ylim = y_range,
     xaxt = "n", pch = 16, col = "#3B82F6",
     xlab = "", ylab = "Alpha (% per month)",
     main = "CAPM vs FF3: HAC 95% confidence intervals")
axis(1, at = x, labels = comparison$Portfolio, las = 2)
abline(h = 0, lty = 2, col = "gray40")
arrows(x - 0.08, capm_rows$Alpha_pct_month - capm_ci,
       x - 0.08, capm_rows$Alpha_pct_month + capm_ci,
       angle = 90, code = 3, length = 0.05, col = "#3B82F6")
points(x + 0.08, ff3_rows$Alpha_pct_month,
       pch = 16, col = "#F59E0B")
arrows(x + 0.08, ff3_rows$Alpha_pct_month - ff3_ci,
       x + 0.08, ff3_rows$Alpha_pct_month + ff3_ci,
       angle = 90, code = 3, length = 0.05, col = "#F59E0B")
legend("topright", legend = c("CAPM", "FF3"),
       col = c("#3B82F6", "#F59E0B"), pch = 16, bty = "n")
dev.off()

cat("\nSaved two figures in", output_dir, "\n")

# ============================================================================
# QUESTION 3, PART 3: TIME-VARYING FACTOR LOADINGS
# ============================================================================
# We use TWO complementary approaches:
#   A. Separate CAPM and FF3 regressions for four historical subperiods.
#   B. Rolling 60-month FF3 regressions, using information available through
#      the END of each window (no future observations enter that regression).
#
# A range or a sign change is DESCRIPTIVE evidence of different estimates.
# It is NOT a formal statistical test that two population betas differ.
# ============================================================================

# STEP 11: Specify the subperiods ----------------------------------------------
# The assignment suggests pre-2000, 2000-2009, 2010-2019, 2020 onward.
# The first period begins at sample_start (194701 in the main analysis).
subperiods <- data.frame(
  Period = c(
    paste0(substr(sample_start, 1L, 4L), "-1999"),
    "2000-2009", "2010-2019", "2020-2026"
  ),
  Start = c(sample_start, "200001", "201001", "202001"),
  End   = c("199912", "200912", "201912", sample_end),
  stringsAsFactors = FALSE
)

# STEP 12: Run both models in each subperiod ----------------------------------
subperiod_results <- list()
index <- 0L
for (j in seq_len(nrow(subperiods))) {
  label <- subperiods$Period[j]
  d_period <- dat[
    dat$YYYYMM >= subperiods$Start[j] &
      dat$YYYYMM <= subperiods$End[j],
    , drop = FALSE
  ]

  # Ensure a reasonable minimum number of monthly observations.
  if (nrow(d_period) < 60L) {
    stop("Subperiod ", label, " has fewer than 60 complete months.")
  }

  for (portfolio_label in names(portfolio_names)) {
    column <- portfolio_names[[portfolio_label]]
    d <- d_period
    d$portfolio_excess <- d[[column]] - d$RF

    capm_period <- lm(portfolio_excess ~ Mkt_RF, data = d)
    ff3_period <- lm(portfolio_excess ~ Mkt_RF + SMB + HML, data = d)

    for (model_name in c("CAPM", "FF3")) {
      fit <- if (model_name == "CAPM") capm_period else ff3_period
      row <- summarize_model(fit, portfolio_label, model_name, nw_lag)
      row$Period <- label
      row$Start_YYYYMM <- d_period$YYYYMM[1L]
      row$End_YYYYMM <- d_period$YYYYMM[nrow(d_period)]
      index <- index + 1L
      subperiod_results[[index]] <- row
    }
  }
}
subperiod_results <- do.call(rbind, subperiod_results)
rownames(subperiod_results) <- NULL

# FF3-only coefficient comparison, organized as one portfolio-period per row.
ff3_subperiod <- subperiod_results[
  subperiod_results$Model == "FF3",
  c("Portfolio", "Period", "Start_YYYYMM", "End_YYYYMM", "N_months",
    "Market_beta", "SMB_loading", "HML_loading", "Alpha_pct_month",
    "Alpha_HAC_p", "Adjusted_R2", "Residual_SD_pct_month")
]

# STEP 13: Describe which FF3 loadings vary most across the four periods -------
# The range is max(beta across periods) minus min(beta across periods).
# Large ranges are not necessarily statistically significant changes.
factor_vars <- c("Market_beta", "SMB_loading", "HML_loading")
subperiod_ranges <- do.call(rbind, lapply(names(portfolio_names), function(p) {
  v <- ff3_subperiod[ff3_subperiod$Portfolio == p, , drop = FALSE]
  data.frame(
    Portfolio = p,
    Market_beta_range = diff(range(v$Market_beta)),
    SMB_loading_range = diff(range(v$SMB_loading)),
    HML_loading_range = diff(range(v$HML_loading)),
    Market_beta_same_sign = all(v$Market_beta > 0) || all(v$Market_beta < 0),
    SMB_loading_same_sign = all(v$SMB_loading > 0) || all(v$SMB_loading < 0),
    HML_loading_same_sign = all(v$HML_loading > 0) || all(v$HML_loading < 0),
    stringsAsFactors = FALSE
  )
}))
rownames(subperiod_ranges) <- NULL

# Identify the largest and smallest subperiod ranges overall.
range_long <- do.call(rbind, lapply(factor_vars, function(factor_name) {
  data.frame(
    Portfolio = subperiod_ranges$Portfolio,
    Factor = factor_name,
    Range = subperiod_ranges[[paste0(factor_name, "_range")]]
  )
}))
range_long <- range_long[order(-range_long$Range), , drop = FALSE]

# STEP 14: Estimate 60-month rolling FF3 regressions ---------------------------
rolling_window <- 60L
if (nrow(dat) < rolling_window) stop("Not enough data for rolling regressions")

# Each window contains 60 consecutive months, ending at Window_end_YYYYMM.
# For example, a window ending 202609 uses 202110-202609.
rolling_records <- vector(
  "list", length(portfolio_names) * (nrow(dat) - rolling_window + 1L)
)
k <- 0L
for (portfolio_label in names(portfolio_names)) {
  column <- portfolio_names[[portfolio_label]]
  for (end_index in rolling_window:nrow(dat)) {
    start_index <- end_index - rolling_window + 1L
    window_data <- dat[start_index:end_index, , drop = FALSE]
    window_data$portfolio_excess <- window_data[[column]] - window_data$RF

    fit <- lm(portfolio_excess ~ Mkt_RF + SMB + HML, data = window_data)
    b <- coef(fit)

    k <- k + 1L
    rolling_records[[k]] <- data.frame(
      Portfolio = portfolio_label,
      Window_start_YYYYMM = dat$YYYYMM[start_index],
      Window_end_YYYYMM = dat$YYYYMM[end_index],
      N_months = rolling_window,
      Alpha_pct_month = unname(b["(Intercept)"]),
      Market_beta = unname(b["Mkt_RF"]),
      SMB_loading = unname(b["SMB"]),
      HML_loading = unname(b["HML"]),
      Adjusted_R2 = summary(fit)$adj.r.squared,
      Residual_SD_pct_month = summary(fit)$sigma,
      stringsAsFactors = FALSE
    )
  }
}
rolling_results <- do.call(rbind, rolling_records)
rownames(rolling_results) <- NULL

# STEP 15: Rolling-coefficient stability summary -------------------------------
# Summarize the dispersion of each coefficient across ALL rolling windows.
rolling_stability <- do.call(rbind, lapply(names(portfolio_names), function(p) {
  x <- rolling_results[rolling_results$Portfolio == p, , drop = FALSE]
  do.call(rbind, lapply(factor_vars, function(factor_name) {
    v <- x[[factor_name]]
    data.frame(
      Portfolio = p,
      Factor = factor_name,
      Rolling_windows = length(v),
      Mean = mean(v),
      SD_across_windows = sd(v),
      Min = min(v),
      P10 = as.numeric(quantile(v, 0.10)),
      P90 = as.numeric(quantile(v, 0.90)),
      Max = max(v),
      Most_recent = tail(v, 1L),
      Same_sign_all_windows = all(v > 0) || all(v < 0),
      stringsAsFactors = FALSE
    )
  }))
}))
rownames(rolling_stability) <- NULL

# STEP 16: Save every Part 3 output --------------------------------------------
write.csv(subperiod_results,
          file.path(output_dir, "q3_part3_subperiod_CAPM_FF3.csv"),
          row.names = FALSE)
write.csv(ff3_subperiod,
          file.path(output_dir, "q3_part3_subperiod_FF3_loadings.csv"),
          row.names = FALSE)
write.csv(subperiod_ranges,
          file.path(output_dir, "q3_part3_subperiod_loading_ranges.csv"),
          row.names = FALSE)
write.csv(rolling_results,
          file.path(output_dir, "q3_part3_rolling_60m_FF3.csv"),
          row.names = FALSE)
write.csv(rolling_stability,
          file.path(output_dir, "q3_part3_rolling_stability_summary.csv"),
          row.names = FALSE)

cat("\n===== PART 3: SUBPERIOD FF3 LOADINGS =====\n")
print(ff3_subperiod[, c("Portfolio", "Period", "N_months",
                         "Market_beta", "SMB_loading", "HML_loading")],
      row.names = FALSE, digits = 4)
cat("\n===== PART 3: RANGES ACROSS SUBPERIODS =====\n")
print(subperiod_ranges, row.names = FALSE, digits = 4)
cat("\n===== PART 3: FIVE LARGEST CHANGES (DESCRIPTIVE RANGES) =====\n")
print(head(range_long, 5L), row.names = FALSE, digits = 4)
cat("\n===== PART 3: 60-MONTH ROLLING STABILITY =====\n")
print(rolling_stability, row.names = FALSE, digits = 4)

# STEP 17: One rolling-loading chart per portfolio -----------------------------
# Chart includes market beta, SMB, and HML. Lines are descriptive estimates,
# not confidence bands. The horizontal zero line highlights sign changes.
rolling_dates <- as.Date(
  paste0(rolling_results$Window_end_YYYYMM, "01"), format = "%Y%m%d"
)
for (portfolio_label in names(portfolio_names)) {
  take <- rolling_results$Portfolio == portfolio_label
  d <- rolling_results[take, , drop = FALSE]
  dates <- rolling_dates[take]
  file_label <- gsub(" ", "_", portfolio_label, fixed = TRUE)
  filename <- file.path(output_dir,
                        paste0("q3_part3_rolling_", file_label, ".png"))

  png(filename, width = 1400, height = 850, res = 150)
  par(mar = c(5, 5, 4, 2))
  y_limits <- range(c(d$Market_beta, d$SMB_loading, d$HML_loading, 0))
  plot(dates, d$Market_beta, type = "l", lwd = 2, lty = 1, col = 1,
       xlab = "Rolling window end date", ylab = "Estimated FF3 loading",
       main = paste(portfolio_label, "-- 60-month rolling FF3 loadings"),
       ylim = y_limits)
  lines(dates, d$SMB_loading, lwd = 2, lty = 2, col = 2)
  lines(dates, d$HML_loading, lwd = 2, lty = 3, col = 4)
  abline(h = 0, lty = 3, col = "gray50")
  legend("topright", legend = c("Market beta", "SMB", "HML"),
         lty = 1:3, col = c(1, 2, 4), lwd = 2, bty = "n")
  dev.off()
}

cat("\nPart 3 finished: CSV tables and five rolling charts saved to ",
    normalizePath(output_dir), "\n", sep = "")
