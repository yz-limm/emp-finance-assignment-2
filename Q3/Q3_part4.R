# ============================================================================
# Empirical Finance Assignment 2 -- Question 3, Part 4
# Does an INTERACTION extension improve on Fama-French three-factor (FF3)?
#
# Run from the root of the repository:
#     Rscript Q3/Q3_part4.R
# or in the RStudio Console:
#     source("Q3/Q3_part4.R")
#
# FIRST run Q3_parts_1-3.R, which generates the required matched data file:
#     Q3/results/q3_part2_matched_monthly_data.csv
#
# Uses only base R. No new downloads or packages.
# Monthly returns and factors are in PERCENT, not decimal units.
# ============================================================================

# STEP 1. Research question and pre-stated model selection rules --------------
# Economic motivation (Week 2: time variation and factor models):
# Part 3 documents variation in market, size, and value loadings over time.
# Conditional exposures may change with contemporaneous market conditions.
# We test two interaction terms, not additional tradable factor portfolios.
#
# Benchmark: FF3
#   excess = alpha + beta*Mkt_RF + s*SMB + h*HML + error
# Richer model: FF3 + interactions
#   excess = alpha + beta*Mkt_RF + s*SMB + h*HML
#                  + delta_s*(Mkt_RF*SMB) + delta_h*(Mkt_RF*HML) + error
#
# INTERPRETATION:
#   effective sensitivity to SMB = s + delta_s * Mkt_RF
#   effective sensitivity to HML = h + delta_h * Mkt_RF
#   These interactions capture nonlinear CONDITIONAL CO-MOVEMENT;
#   they do NOT establish that a new traded risk factor earns a premium.
#
# Model selection rule (written BEFORE examining the output in this script):
#  1. Use 1947-1999 only to estimate candidate parameters.
#  2. Evaluate factor-conditioned fitted errors over 2000-2026.
#  3. Prefer interactions only if pooled test RMSE falls >= 2% relative to FF3
#     AND at least 3 of the 5 portfolios show lower test RMSE.
#  4. Supplement with HAC-robust interaction tests and adjusted R-squared;
#     these are SECONDARY and must not overturn criterion 3.
#
# AUDIT WARNINGS:
# - The 2000-2026 test period was already inspected in Part 3. Therefore this
#   split is *chronological pseudo-out-of-sample*, NOT a pristine untouched
#   test set. Do not describe it as independent validation.
# - Today's Mkt_RF / SMB / HML factors are used to explain today's returns.
#   The resulting error is a held-out CONDITIONAL FIT error, NOT an ex-ante
#   tradable return forecast or investment strategy backtest.
# - Do not select interaction terms or reselect the cutoff after viewing test
#   performance. Report ALL 5 portfolios, including failures.
# ============================================================================

matched_file <- "Q3/results/q3_part2_matched_monthly_data.csv"
output_dir <- "Q3/results"
nw_lag <- 6L
train_end <- "199912"
test_start <- "200001"
min_pooled_rmse_reduction_pct <- 2
min_portfolios_improved <- 3L

if (!file.exists(matched_file)) {
  stop(paste0(
    "Matched data not found: ", matched_file, "\n",
    "First run: Rscript Q3/Q3_parts_1-3.R\n",
    "Run both scripts from your repository ROOT (check getwd())."
  ))
}
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# STEP 2. Import EXACTLY the same data used in Parts 1-3 -----------------------
dat <- read.csv(matched_file, check.names = FALSE, stringsAsFactors = FALSE)
dat$YYYYMM <- sprintf("%06d", as.integer(dat$YYYYMM))
dat <- dat[order(dat$YYYYMM), , drop = FALSE]
portfolio_names <- c(
  "Small Growth" = "SMALL LoBM",
  "Small Value"  = "SMALL HiBM",
  "Middle"       = "ME3 BM3",
  "Big Growth"   = "BIG LoBM",
  "Big Value"    = "BIG HiBM"
)
required <- c("YYYYMM", "Mkt_RF", "SMB", "HML", "RF",
              unname(portfolio_names))
if (!all(required %in% names(dat))) {
  stop("Missing columns in matched dataset: ",
       paste(setdiff(required, names(dat)), collapse = ", "))
}
dat <- dat[complete.cases(dat[, required]), , drop = FALSE]
stopifnot(!anyDuplicated(dat$YYYYMM))

# No lagged interaction was invented after looking at data. These two fixed
# contemporaneous interactions are constructed BEFORE the model is estimated.
# Because factor returns are measured in percentage points, interactions
# have units of (percentage points)^2.
dat$Mkt_x_SMB <- dat$Mkt_RF * dat$SMB
dat$Mkt_x_HML <- dat$Mkt_RF * dat$HML

cat("\n===== PART 4: DATA AND PRE-SPECIFIED MODEL =====\n")
cat("Matched sample:", dat$YYYYMM[1L], "to", dat$YYYYMM[nrow(dat)],
    "; N =", nrow(dat), "monthly observations\n")
cat("Training through", train_end, "; evaluation from", test_start, "\n")
cat("HAC lags:", nw_lag, "; required pooled RMSE reduction:",
    min_pooled_rmse_reduction_pct, "% ; minimum portfolios improved:",
    min_portfolios_improved, "\n")

# STEP 3. Same Newey-West HAC estimator/conventions as in Parts 1-3 -----------
# Bartlett weights, 6 monthly lags, HC-style n/(n-k) finite-sample correction,
# no prewhitening. OLS coefficient estimates are NOT changed.
newey_west_vcov <- function(model, lag = 6L) {
  X <- model.matrix(model)
  u <- as.numeric(residuals(model))
  n <- nrow(X)
  k <- ncol(X)
  L <- min(as.integer(lag), n - 1L)
  Z <- sweep(X, 1L, u, FUN = "*")
  S <- crossprod(Z)
  if (L > 0L) {
    for (ell in seq_len(L)) {
      G <- crossprod(
        Z[(ell + 1L):n, , drop = FALSE],
        Z[seq_len(n - ell), , drop = FALSE]
      )
      w <- 1 - ell / (L + 1)
      S <- S + w * (G + t(G))
    }
  }
  XTX_inv <- solve(crossprod(X))
  V <- XTX_inv %*% S %*% XTX_inv * (n / (n - k))
  dimnames(V) <- list(colnames(X), colnames(X))
  V
}

model_formulas <- list(
  FF3 = portfolio_excess ~ Mkt_RF + SMB + HML,
  FF3_interactions = portfolio_excess ~ Mkt_RF + SMB + HML +
    Mkt_x_SMB + Mkt_x_HML
)
added_terms <- c("Mkt_x_SMB", "Mkt_x_HML")

full_sample_list <- list()
coef_list <- list()
holdout_list <- list()
comparison_list <- list()

# STEP 4. Model selection uses FIXED pre-2000 training fits --------------------
for (portfolio_label in names(portfolio_names)) {
  d <- dat
  d$portfolio_excess <- d[[portfolio_names[[portfolio_label]]]] - d$RF

  train <- d[d$YYYYMM <= train_end, , drop = FALSE]
  test  <- d[d$YYYYMM >= test_start, , drop = FALSE]
  if (nrow(train) < 60L || nrow(test) < 12L) {
    stop("Not enough training or test observations for ", portfolio_label)
  }

  rmse_by_model <- numeric(0)

  for (model_name in names(model_formulas)) {
    formula_used <- model_formulas[[model_name]]

    # Train once on pre-2000 data; do not re-fit with test observations.
    train_fit <- lm(formula_used, data = train)
    predicted_test <- predict(train_fit, newdata = test)
    error <- test$portfolio_excess - predicted_test

    test_rmse <- sqrt(mean(error^2))
    rmse_by_model[model_name] <- test_rmse
    holdout_list[[paste(portfolio_label, model_name)]] <- data.frame(
      Portfolio = portfolio_label,
      Model = model_name,
      Train_start = train$YYYYMM[1L],
      Train_end = tail(train$YYYYMM, 1L),
      N_train = nrow(train),
      Test_start = test$YYYYMM[1L],
      Test_end = tail(test$YYYYMM, 1L),
      N_test = nrow(test),
      RMSE_pct_month = test_rmse,
      MAE_pct_month = mean(abs(error)),
      Mean_error_pct_month = mean(error),
      stringsAsFactors = FALSE
    )

    # Full-sample coefficients are a SECONDARY, descriptive exercise.
    # They are NOT the coefficients used to evaluate holdout performance.
    full_fit <- lm(formula_used, data = d)
    b <- coef(full_fit)
    V <- newey_west_vcov(full_fit, lag = nw_lag)
    se <- sqrt(pmax(0, diag(V)))
    names(se) <- names(b)
    pval <- 2 * pt(-abs(b / se), df = df.residual(full_fit))

    for (name in names(b)) {
      coef_list[[paste(portfolio_label, model_name, name)]] <- data.frame(
        Portfolio = portfolio_label,
        Model = model_name,
        Coefficient = name,
        Estimate = unname(b[name]),
        HAC_SE = unname(se[name]),
        HAC_t = unname(b[name] / se[name]),
        HAC_p = unname(pval[name]),
        stringsAsFactors = FALSE
      )
    }

    joint_p <- NA_real_
    if (model_name == "FF3_interactions") {
      b_added <- b[added_terms]
      V_added <- V[added_terms, added_terms, drop = FALSE]
      q <- length(added_terms)
      # HAC-robust joint Wald F test: H0 delta_s = delta_h = 0.
      joint_F <- as.numeric(t(b_added) %*% solve(V_added, b_added)) / q
      joint_p <- pf(joint_F, df1 = q, df2 = df.residual(full_fit),
                    lower.tail = FALSE)
    }

    full_sample_list[[paste(portfolio_label, model_name)]] <- data.frame(
      Portfolio = portfolio_label,
      Model = model_name,
      N_months = nobs(full_fit),
      Alpha_pct_month = unname(b["(Intercept)"]),
      Alpha_HAC_SE = unname(se["(Intercept)"]),
      Alpha_HAC_p = unname(pval["(Intercept)"]),
      Adjusted_R2 = summary(full_fit)$adj.r.squared,
      Residual_SD_pct_month = summary(full_fit)$sigma,
      Interaction_joint_HAC_p = joint_p,
      stringsAsFactors = FALSE
    )
  }

  comparison_list[[portfolio_label]] <- data.frame(
    Portfolio = portfolio_label,
    FF3_RMSE = unname(rmse_by_model["FF3"]),
    Interaction_RMSE = unname(rmse_by_model["FF3_interactions"]),
    RMSE_reduction_pct = 100 * (
      1 - rmse_by_model["FF3_interactions"] / rmse_by_model["FF3"]
    ),
    stringsAsFactors = FALSE
  )
}

full_sample <- do.call(rbind, full_sample_list)
coefficients <- do.call(rbind, coef_list)
holdout <- do.call(rbind, holdout_list)
comparison <- do.call(rbind, comparison_list)
rownames(full_sample) <- NULL
rownames(coefficients) <- NULL
rownames(holdout) <- NULL
rownames(comparison) <- NULL

# STEP 5. Apply model selection rule, WITHOUT changing any thresholds ---------
pooled_ff3_rmse <- sqrt(mean(comparison$FF3_RMSE^2))
pooled_int_rmse <- sqrt(mean(comparison$Interaction_RMSE^2))
pooled_reduction <- 100 * (1 - pooled_int_rmse / pooled_ff3_rmse)
n_improved <- sum(comparison$RMSE_reduction_pct > 0)
adopt_interactions <- pooled_reduction >= min_pooled_rmse_reduction_pct &&
  n_improved >= min_portfolios_improved

cat("\n===== PRIMARY TEST: HOLDOUT 2000-2026 (no test-period re-fitting) =====\n")
print(comparison, row.names = FALSE, digits = 5)
cat(sprintf("\nPooled holdout RMSE: FF3 = %.5f ; Interactions = %.5f\n",
            pooled_ff3_rmse, pooled_int_rmse))
cat(sprintf("Pooled RMSE change (positive = improvement): %.3f%%\n",
            pooled_reduction))
cat("Portfolios with lower RMSE:", n_improved, "out of 5\n")
cat("Pre-stated model-selection conclusion:",
    if (adopt_interactions) "ADOPT INTERACTION MODEL" else "RETAIN STANDARD FF3",
    "\n")

cat("\n===== SECONDARY: FULL-SAMPLE FIT / ALPHA / HAC INFERENCE =====\n")
print(full_sample, row.names = FALSE, digits = 5)
cat("\n===== SECONDARY: COEFFICIENTS AND HAC STANDARD ERRORS =====\n")
print(coefficients, row.names = FALSE, digits = 5)

# STEP 5A. Independently check one substantive result without lm() ----------
# Recalculate Small Growth's baseline holdout RMSE with the OLS normal
# equations, rather than using lm() or predict(). This is an independently
# reproducible numerical check for the AI-use disclosure.
train_check <- dat[dat$YYYYMM <= train_end, , drop = FALSE]
test_check  <- dat[dat$YYYYMM >= test_start, , drop = FALSE]
X_train <- cbind(1, train_check$Mkt_RF, train_check$SMB, train_check$HML)
X_test  <- cbind(1, test_check$Mkt_RF, test_check$SMB, test_check$HML)
y_train <- train_check[["SMALL LoBM"]] - train_check$RF
y_test  <- test_check[["SMALL LoBM"]] - test_check$RF
beta_direct <- solve(crossprod(X_train), crossprod(X_train, y_train))
rmse_direct <- sqrt(mean((y_test - drop(X_test %*% beta_direct))^2))
rmse_lm <- comparison$FF3_RMSE[comparison$Portfolio == "Small Growth"]
if (length(rmse_lm) != 1L || abs(rmse_direct - rmse_lm) > 1e-8) {
  stop("Independent Small Growth FF3 holdout RMSE check failed")
}
cat(sprintf("Independent matrix-based check, Small Growth FF3 holdout RMSE: %.6f%%\n",
            rmse_direct))

# STEP 6. Save tables, chart, and a model decision note -------------------------
write.csv(full_sample,
          file.path(output_dir, "q3_part4_full_sample_model_results.csv"),
          row.names = FALSE)
write.csv(coefficients,
          file.path(output_dir, "q3_part4_coefficients_HAC.csv"),
          row.names = FALSE)
write.csv(holdout,
          file.path(output_dir, "q3_part4_holdout_errors.csv"),
          row.names = FALSE)
write.csv(comparison,
          file.path(output_dir, "q3_part4_holdout_comparison.csv"),
          row.names = FALSE)

png(file.path(output_dir, "q3_part4_holdout_RMSE_improvement.png"),
    width = 1200, height = 700, res = 150)
par(mar = c(5, 11, 5, 2))
barplot(
  rev(comparison$RMSE_reduction_pct),
  names.arg = rev(comparison$Portfolio),
  horiz = TRUE, las = 1, col = "gray65",
  main = "Holdout performance: FF3 + interactions vs FF3",
  sub = "Train 1947-1999; evaluate 2000-Sep 2026",
  xlab = "RMSE reduction (%)  --  positive = better; negative = worse"
)
abline(v = 0, lwd = 2)
dev.off()

note <- c(
  "Q3 Part 4 -- Interaction-model audit",
  "====================================================",
  "Model choice: FF3 + Mkt_RF*SMB + Mkt_RF*HML",
  "Motivation: allow size and value exposures to vary with market conditions.",
  "These are conditional co-movement terms, not tradable risk factors.",
  "Selection criterion: pooled held-out RMSE reduction >= 2% AND",
  "  improvement for at least 3 of 5 prespecified test portfolios.",
  "Train: 194701-199912; test: 200001-202609.",
  sprintf("Pooled FF3 RMSE (pct monthly) = %.6f", pooled_ff3_rmse),
  sprintf("Pooled interaction RMSE (pct monthly) = %.6f", pooled_int_rmse),
  sprintf("Pooled RMSE improvement = %.3f%%", pooled_reduction),
  sprintf("Improved portfolios = %d/5", n_improved),
  paste("Decision:", if (adopt_interactions) "Adopt interactions" else "Keep FF3"),
  "CAVEAT 1: Test period was already inspected in Q3 Part 3; not pristine.",
  "CAVEAT 2: Current realized factor returns enter the fitted return.",
  "  This measures held-out factor-conditioned fit, not ex-ante forecast skill.",
  "CAVEAT 3: Two extra regressors tested across five portfolios. Do not",
  "  select a winner after seeing these results or tune the test threshold.",
  "CAVEAT 4: Full-sample p-values are descriptive and not model-selection", 
  "  evidence. Multiple testing and overlapping market regimes can mislead."
)
writeLines(note, file.path(output_dir, "q3_part4_model_audit.txt"))
cat("\nSaved Part 4 results and chart under:", normalizePath(output_dir), "\n")
