# ============================================================================
# EMPIRICAL FINANCE — ASSIGNMENT 2 — Q3 PARTS 5 AND 6
# 2026 Treasury par-yield curve, approximate forward rates, historical comparison
# and a numerical verification to document AI auditing for Part 6.
#
# Keep this separate from Q3_parts_1-3.R and Q3_part4.R.
# Run from the REPOSITORY ROOT in the RStudio terminal:
#     Rscript Q3/Q3_parts_5-6.R
# or from the RStudio Console:
#     source("Q3/Q3_parts_5-6.R")
#
# Inputs:
#     Data/Treasury_Par_Yield_Curve_2026-09-30.csv
#     Data/Treasury_Par_Yield_Curve_2018-01-12_reference.csv [optional]
# Output files are placed in Q3/results/
#
# IMPORTANT:
# 2026 yields are Treasury PAR yields. They are NOT exact zero-coupon spots.
# This exercise reports simple, transparent forward-rate APPROXIMATIONS
# by treating par yields as annual-effective spot-rate proxies.
# A true arbitrage-consistent forward curve requires bootstrapping discount
# factors from coupon-bond quotes using the actual payment conventions.
# ============================================================================

file_2026 <- "Data/Treasury_Par_Yield_Curve_2026-09-30.csv"
file_2018 <- "Data/Treasury_Par_Yield_Curve_2018-01-12_reference.csv"
out_dir <- "Q3/results"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(file_2026)) {
  stop(paste(
    "Missing:", file_2026,
    "\nSave the provided 2026 Treasury CSV under Data/.",
    "\nCheck that your working directory is the repository root: getwd()"
  ))
}

# Convert tenor labels such as 3M, 1.5M, 2Y, '3 Mo', or '10 Yr' into years.
tenor_to_years <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  raw <- toupper(gsub("[[:space:]_/-]", "", as.character(x)))
  z <- suppressWarnings(as.numeric(sub("(MONTHS?|MOS?|MO|M|YEARS?|YRS?|YR|Y)$", "", raw)))
  unit_month <- grepl("(MONTHS?|MOS?|MO|M)$", raw)
  unit_year <- grepl("(YEARS?|YRS?|YR|Y)$", raw)
  numeric_only <- !unit_month & !unit_year & !is.na(suppressWarnings(as.numeric(raw)))
  out <- rep(NA_real_, length(raw))
  out[unit_month] <- z[unit_month] / 12
  out[unit_year | numeric_only] <- z[unit_year | numeric_only]
  out
}

# Accept either long-format CSV (Date, Maturity_Years, Par_Yield_Percent)
# or Treasury-style wide-format CSV (Date, '1 Mo', '3 Mo', '1 Yr', ...).
read_par_curve <- function(path, selected_date = NULL) {
  raw <- read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  names(raw) <- trimws(names(raw))
  clean_names <- tolower(gsub("[^a-z0-9]", "", names(raw)))

  # Keep the intended date if a date column is present.
  ix_date <- which(clean_names %in% c("date", "observationdate", "quotedate"))
  if (length(ix_date) > 0L && !is.null(selected_date)) {
    dates <- as.character(raw[[ix_date[1L]]])
    dates <- ifelse(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", dates),
                    dates, as.character(as.Date(dates, format = "%m/%d/%Y")))
    if (!any(dates == selected_date)) {
      stop("Date ", selected_date, " not found in ", path)
    }
    raw <- raw[dates == selected_date, , drop = FALSE]
  }

  nm <- tolower(gsub("[^a-z0-9]", "", names(raw)))
  maturity_ix <- which(nm %in% c("maturityyears", "years", "tenor", "maturity", "term"))
  yield_ix <- which(nm %in% c("paryieldpercent", "paryieldpct", "yieldpercent",
                            "yieldpct", "paryield", "yield", "rate"))

  if (length(maturity_ix) && length(yield_ix)) {
    # Prefer explicit numeric years over tenor strings.
    preferred <- which(nm == "maturityyears")
    mi <- if (length(preferred)) preferred[1L] else maturity_ix[1L]
    yi <- yield_ix[1L]
    t <- tenor_to_years(raw[[mi]])
    y <- suppressWarnings(as.numeric(raw[[yi]]))
  } else {
    # Wide-format input: recognize a column as a Treasury maturity.
    years <- tenor_to_years(names(raw))
    good_columns <- which(!is.na(years) & years > 0 & years <= 100)
    if (!length(good_columns)) {
      stop("Cannot find maturities/yields in ", path,
           ". Expected either Maturity_Years/Par_Yield_Percent columns",
           " or Treasury-style tenor column headings.")
    }
    if (nrow(raw) != 1L) {
      stop("Wide-format file must contain exactly one row for the requested date.")
    }
    t <- years[good_columns]
    y <- suppressWarnings(as.numeric(unlist(raw[1L, good_columns, drop = TRUE])))
  }

  res <- data.frame(Maturity_Years = t, Par_Yield_Percent = y)
  res <- res[is.finite(res$Maturity_Years) & is.finite(res$Par_Yield_Percent), ]
  res <- res[order(res$Maturity_Years), ]
  if (!nrow(res)) stop("No valid yield observations found in ", path)
  if (anyDuplicated(round(res$Maturity_Years, 5L))) {
    stop("Duplicate maturities found in ", path)
  }
  # Explicitly expect percentage points: 4.54, not 0.0454.
  if (min(res$Par_Yield_Percent) <= 0 || max(res$Par_Yield_Percent) < 1) {
    stop("Unexpected yield units: provide par yields in PERCENT (e.g. 4.54).")
  }
  rownames(res) <- NULL
  res
}

curve26 <- read_par_curve(file_2026, "2026-09-30")

# Official Treasury 2018 reference values (January 12, 2018), for comparison
# between PAR curves. If an accompanying reference CSV exists, use that.
if (file.exists(file_2018)) {
  curve18 <- read_par_curve(file_2018, "2018-01-12")
} else {
  curve18 <- data.frame(
    Maturity_Years = c(1/12, 0.25, 0.5, 1, 2, 3, 5, 7, 10, 20, 30),
    Par_Yield_Percent = c(1.31, 1.43, 1.59, 1.78, 1.99, 2.12,
                          2.35, 2.48, 2.55, 2.71, 2.85)
  )
}

get_yield <- function(curve, years) {
  idx <- which(abs(curve$Maturity_Years - years) < 0.00001)
  if (length(idx) != 1L) {
    stop("Required maturity ", years, " years is absent in supplied data.")
  }
  curve$Par_Yield_Percent[idx]
}

# STEP 1 — Describe level, slope, and shape ---------------------------
y3m <- get_yield(curve26, 0.25)
y2y <- get_yield(curve26, 2)
y5y <- get_yield(curve26, 5)
y10y <- get_yield(curve26, 10)
y20y <- get_yield(curve26, 20)
y30y <- get_yield(curve26, 30)
slope26 <- y10y - y3m
slope18 <- get_yield(curve18, 10) - get_yield(curve18, 0.25)
curvature_proxy <- 2*y5y - y2y - y10y # in percentage points (illustrative)

cat("\n===== Q3 PART 5: TREASURY PAR YIELDS (SEP 30, 2026) =====\n")
print(curve26, row.names = FALSE, digits = 6)
cat(sprintf("3M = %.2f%% ; 10Y = %.2f%% ; 20Y = %.2f%% ; 30Y = %.2f%%\n",
            y3m, y10y, y20y, y30y))
cat(sprintf("2026 10Y - 3M slope = %.2f percentage points\n", slope26))
cat(sprintf("2018 10Y - 3M slope = %.2f percentage points\n", slope18))
cat(sprintf("2026 curvature proxy [2*5Y-2Y-10Y] = %.3f pp\n",
            curvature_proxy))

# STEP 2 — Approximate forward rates from adjacent quoted maturities -------
# No interpolation is used for 1Y->2Y, 2Y->3Y, or 3Y->5Y.
# Annual-effective compounding; yields are divided by 100 before calculation.
# PAR yields are used as PROXIES for continuously observable spot zero rates.
#   D_proxy(t) = (1 + y_par(t))^(-t)
#   simple_forward = [D_proxy(t1) / D_proxy(t2) - 1] / (t2 - t1)
#   effective_annual = [D_proxy(t1) / D_proxy(t2)]^(1/(t2 - t1)) - 1
approx_forward <- function(t1, t2) {
  if (t2 <= t1) stop("Forward end must exceed forward start.")
  y1 <- get_yield(curve26, t1) / 100
  y2 <- get_yield(curve26, t2) / 100
  D1 <- (1 + y1)^(-t1)
  D2 <- (1 + y2)^(-t2)
  data.frame(Start_Years = t1, End_Years = t2,
             Start_Par_Yield_pct = y1 * 100,
             End_Par_Yield_pct = y2 * 100,
             Approx_Simple_Forward_pct_per_year = (D1 / D2 - 1) /
               (t2 - t1) * 100,
             Approx_Effective_Annual_Forward_pct =
               ((D1 / D2)^(1 / (t2 - t1)) - 1) * 100)
}
forward <- rbind(approx_forward(1, 2),
                 approx_forward(2, 3),
                 approx_forward(3, 5))

cat("\n===== APPROXIMATE FORWARD RATES (PAR-YIELD PROXY) =====\n")
print(forward, row.names = FALSE, digits = 7)

# STEP 3 — Independently verify the 1-year-forward-starting-in-1-year rate --
# Two independent algebraic routes should agree to machine precision.
# Route 1: direct bond accumulation ratio
f_direct <- ((1 + get_yield(curve26, 2)/100)^2 /
               (1 + get_yield(curve26, 1)/100) - 1) * 100
# Route 2: discount-factor ratio, computed above
f_discount <- forward$Approx_Simple_Forward_pct_per_year[1L]
stopifnot(abs(f_direct - f_discount) < 1e-10)
cat(sprintf("Independent forward check (1Y->2Y): %.6f%% vs %.6f%%; PASS\n",
            f_direct, f_discount))

# STEP 4 — Save figures for presentation ----------------------------------
png(filename = file.path(out_dir, "q3_part5_2026_par_yield_curve.png"),
    width = 1200, height = 700, res = 140)
plot(curve26$Maturity_Years, curve26$Par_Yield_Percent,
     type = "b", pch = 19, lwd = 2, col = "#2457a6",
     xlim = c(0, 30), ylim = range(curve26$Par_Yield_Percent) + c(-0.15,0.15),
     xlab = "Maturity (years)", ylab = "Treasury par yield (%)",
     main = "U.S. Treasury par-yield curve | 30 September 2026",
     xaxt = "n")
axis(1, at = c(0, 1, 2, 3, 5, 7, 10, 20, 30))
grid(col = "grey88")
points(curve26$Maturity_Years, curve26$Par_Yield_Percent,
       col = "#2457a6", pch = 19)
dev.off()

png(filename = file.path(out_dir, "q3_part5_par_curve_2018_vs_2026.png"),
    width = 1200, height = 700, res = 140)
plot(curve26$Maturity_Years, curve26$Par_Yield_Percent,
     type = "b", pch = 19, lwd = 2, col = "#2457a6",
     xlim = c(0, 30), ylim = range(c(curve26$Par_Yield_Percent,
                                      curve18$Par_Yield_Percent)) + c(-0.2,0.2),
     xlab = "Maturity (years)", ylab = "Treasury par yield (%)",
     main = "Treasury par-yield curves | Jan 2018 vs Sep 2026",
     xaxt = "n")
axis(1, at = c(0, 1, 2, 3, 5, 7, 10, 20, 30))
lines(curve18$Maturity_Years, curve18$Par_Yield_Percent,
      type = "b", lwd = 2, pch = 17, col = "#cc6b32")
grid(col = "grey88")
lines(curve26$Maturity_Years, curve26$Par_Yield_Percent,
      type = "b", lwd = 2, pch = 19, col = "#2457a6")
legend("bottomright", legend = c("Sep 30, 2026 par", "Jan 12, 2018 par"),
       pch = c(19,17), lty = 1, lwd = 2, bty = "n",
       col = c("#2457a6", "#cc6b32"))
dev.off()

# STEP 5 — Export data, verification, and one-slide numerical facts ----------
write.csv(curve26, file.path(out_dir, "q3_part5_2026_par_curve.csv"),
          row.names = FALSE)
write.csv(curve18, file.path(out_dir, "q3_part5_2018_par_reference.csv"),
          row.names = FALSE)
write.csv(forward, file.path(out_dir, "q3_part5_approx_forward_rates.csv"),
          row.names = FALSE)

facts <- data.frame(
  Metric = c("2026 3-month par yield (%)", "2026 10-year par yield (%)",
             "2026 10Y-3M slope (pp)", "2018 10Y-3M slope (pp)",
             "Change in 10Y par yield 2018->2026 (pp)",
             "1Y->2Y approximate simple forward (% pa)",
             "2Y->3Y approximate simple forward (% pa)"),
  Value = c(y3m, y10y, slope26, slope18,
            y10y - get_yield(curve18, 10),
            forward$Approx_Simple_Forward_pct_per_year[1],
            forward$Approx_Simple_Forward_pct_per_year[2])
)
write.csv(facts, file.path(out_dir, "q3_part5_key_facts.csv"),
          row.names = FALSE)

cat("\n===== Q3 PART 6: AUDIT POINTS =====\n")
cat("Independent numerical check: 1Y->2Y forward matches by two formulas.\n")
cat("Equity in-sample fit: FF3 R2 improves, but significant alphas can remain.\n")
cat("Equity later-sample fit: Part 4 interactions did not beat standard FF3.\n")
cat("Bond pricing restriction: positive/decreasing discount factors (under positive forwards).\n")
cat("Curve-fitting in-sample errors are NOT future forecast errors.\n")
cat("Warning: reported forwards treat par yields as SPOT PROXIES.\n")
cat("\nOutputs saved to: ", normalizePath(out_dir), "\n", sep = "")
