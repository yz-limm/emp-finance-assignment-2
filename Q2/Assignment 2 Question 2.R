library(dplyr)
library(lubridate)
library(readxl)
library(ggplot2)
library(scales)

bonds_raw <- read_excel("Yield_Curve.xlsx", sheet = "Interpolating the Yield Curve", skip = 8)
dates_range <- read_excel("Yield_Curve.xlsx", sheet = "Interpolating the Yield Curve", range = "B6:AZ6", col_names = FALSE)

settlement_date <- as.Date("2018-01-12")

# ====================================================================
# 0. Core Function Definitions
# ====================================================================

# Yield curve function based on a polynomial model (Nelson-Siegel simplified form)
yield_function <- function(t, params) {
  a0 <- params[1]; a1 <- params[2]; a2 <- params[3]; a3 <- params[4]
  # Use pmax to prevent negative or near-zero rates which cause calculation errors
  y <- pmax(a0 + a1 * t + a2 * (t^2) + a3 * (t^3), 0.000001)
  return(y)
}

# Function to calculate the model price of a single bond
price_bond <- function(maturity_date, coupon_rate, last_coupon_date, params) {
  # Generate future cash flow dates
  cf_dates <- seq(from = last_coupon_date + months(6), 
                  to = maturity_date, by = "6 months")
  
  if(length(cf_dates) == 0) return(0)
  
  # Calculate time to payment in years from settlement date
  t_vec <- as.numeric(difftime(cf_dates, settlement_date, units = "days")) / 365.25
  
  # Calculate cash flows (coupons + principal at maturity)
  cf <- rep(coupon_rate / 2, length(cf_dates))
  cf[length(cf)] <- cf[length(cf)] + 100
  
  # Calculate discount factors
  y_vec <- yield_function(t_vec, params)
  df_vec <- exp(-y_vec * t_vec)
  
  # Return the sum of present values (Model Price)
  return(sum(cf * df_vec))
}

# ====================================================================
# Part 1: Calculate each bond's full (dirty) price including accrued interest
# ====================================================================
cat("--- Part 1: Calculating Dirty Prices ---\n")

bonds <- bonds_raw %>%
  select(Maturity, Coupon, Asked, `Last coupon`) %>%
  rename(Last_Coupon = `Last coupon`) %>%
  filter(!is.na(Maturity)) %>%
  mutate(
    Maturity = as.Date(Maturity),
    Last_Coupon = as.Date(Last_Coupon),
    Coupon = as.numeric(Coupon),
    Asked = as.numeric(Asked),
    
    # Accrued Interest Calculation
    Next_Coupon = Last_Coupon + months(6),
    Days_in_Period = as.numeric(Next_Coupon - Last_Coupon),
    Days_Accrued = as.numeric(settlement_date - Last_Coupon),
    Accrued_Interest = (Coupon / 2) * (Days_Accrued / Days_in_Period),
    
    # Calculate Full (Dirty) Price
    Dirty_Price = Asked + Accrued_Interest
  )

cat("Part 1 Complete. Total bonds processed:", nrow(bonds), "\n")


# ====================================================================
# Part 4: Optimize params (a0, a1, a2, a3) to minimize pricing errors
# (Combined logic here as Part 2 requires the optimized parameters first)
# ====================================================================
cat("--- Part 4: Optimization and Parameter Selection ---\n")

# Define objective function: Sum of Squared Errors (SSE) between actual and model prices
objective_function <- function(params, data) {
  model_prices <- mapply(price_bond, 
                         data$Maturity, data$Coupon, data$Last_Coupon, 
                         MoreArgs = list(params = params))
  
  # Return a large penalty if calculations result in Inf or NaN
  if (any(is.infinite(model_prices)) || any(is.nan(model_prices))) {
    return(1e10)
  }
  
  sse <- sum((data$Dirty_Price - model_prices) ^ 2)
  return(sse)
}

# Initial parameter guesses
init_params <- c(0.016, 0.002, -0.0001, 0.000002)

# Run optimization
opt_result <- optim(init_params, fn = objective_function, data = bonds, 
                    method = "Nelder-Mead", control = list(maxit = 10000))
best_params <- opt_result$par

cat("Optimal Parameters (a0, a1, a2, a3):", round(best_params, 6), "\n")
cat("Convergence code (0 is good):", opt_result$convergence, "\n")


# ====================================================================
# Part 2: For every cash-flow date, calculate time, discount rate, and ZCB price
# ====================================================================
cat("--- Part 2: Generating Zero-Coupon Bond (ZCB) Data ---\n")

# Generate a sequence of standard cash-flow dates (e.g., from 2018 to 2040)
all_payout_dates <- seq(as.Date("2018-02-15"), as.Date("2040-08-15"), by = "6 months")

# Calculate time to payment, model discount rates, and $1 ZCB prices
time_to_payout <- as.numeric(difftime(all_payout_dates, settlement_date, units = "days")) / 365.25
discount_rates <- yield_function(time_to_payout, best_params)
zcb_prices <- exp(-discount_rates * time_to_payout)

# Compile results into a data frame
results_table_part2 <- data.frame(
  Date = format(all_payout_dates, "%m/%d/%y"),
  Time_to_Payout = time_to_payout,
  Discount_Rate = discount_rates,
  Price_of_ZCB = zcb_prices
)

# Preview Part 2 results
cat("\n[Part 2 Results Preview] Top 5 Zero-Coupon Bond Prices:\n")
print(head(results_table_part2, 5))


# ====================================================================
# Part 3: Calculate the present value of every coupon/principal payment
# ====================================================================
cat("\n--- Part 3: Calculating Present Values of Cash Flows ---\n")

# Calculate PV for each individual cash flow and store as a list in the data frame
bonds <- bonds %>%
  rowwise() %>%
  mutate(
    # Future cash flow dates
    cf_dates = list(seq(from = Last_Coupon + months(6), to = Maturity, by = "6 months")),
    # Time to payment in years
    t_vec = list(as.numeric(difftime(unlist(cf_dates), settlement_date, units = "days")) / 365.25),
    # Cash flow values
    cf_values = list({
      cfs <- rep(Coupon / 2, length(t_vec))
      cfs[length(cfs)] <- cfs[length(cfs)] + 100
      cfs
    }),
    # Discount factors
    df_vec = list(exp(-yield_function(unlist(t_vec), best_params) * unlist(t_vec))),
    # Present value of each cash flow
    pv_cashflows = list(unlist(cf_values) * unlist(df_vec))
  ) %>%
  ungroup()

cat("Part 3 Complete. Calculated PVs for", nrow(bonds), "bonds.\n")


# ====================================================================
# Part 4 (Continued): Calculate model price and summarize pricing errors
# ====================================================================
cat("\n--- Part 4: Pricing Errors Summary ---\n")

# Aggregate PVs to get the total model price for each bond
bonds <- bonds %>%
  mutate(Model_Price = sapply(pv_cashflows, sum))

# Calculate errors
errors <- bonds$Dirty_Price - bonds$Model_Price
pct_errors <- (errors / bonds$Dirty_Price) * 100

cat("Dollar Errors: Min =", round(min(errors), 2), 
    "| Max =", round(max(errors), 2), 
    "| Mean =", round(mean(errors), 2), 
    "| RMSE =", round(sqrt(mean(errors ^ 2)), 2), "\n")
cat("Percent Errors: Min =", round(min(pct_errors), 2), "%", 
    "| Max =", round(max(pct_errors), 2), "%\n")


# ====================================================================
# Part 5: Plot the estimated yield curve
# ====================================================================
cat("--- Part 5: Plotting Estimated Yield Curve ---\n")

# Generate a smooth sequence for plotting
tau_seq <- seq(0, 25, by = 0.1)
y_seq <- yield_function(tau_seq, best_params)

plot_data <- data.frame(Tau = tau_seq, Yield = y_seq)

p <- ggplot(plot_data, aes(x = Tau, y = Yield)) +
  geom_line(color = "#2563eb", size = 1.2) + 
  labs(title = "Fitted US Treasury Yield Curve", 
       subtitle = paste0("Settlement Date: ", as.character(settlement_date)),
       x = "Time to Maturity (Years)", 
       y = "Yield") +
  theme_minimal() +
  scale_y_continuous(labels = percent)

print(p)
# ===
# 6
# ===
# 6a 
summary(bonds_raw$Asked)
t1 <- as.numeric(as.Date("2020-08-15") - settlement_date) / 365.25
t2 <- as.numeric(as.Date("2021-08-15") - settlement_date) / 365.25
y1 <- yield_function(t1, best_params)
y2 <- yield_function(t2, best_params)

fwd_rate <- (y2 * t2 - y1 * t1) / (t2 - t1)
cat("\n6(d) 1-Year Forward Rate (starting Aug 2020):", round(fwd_rate * 100, 2), "%\n")
cat("Yield on ZCB maturing Aug 2021 (Spot Rate):", round(y2 * 100, 2), "%\n")

t_2030 <- as.numeric(as.Date("2030-08-15") - settlement_date) / 365.25
y_2030 <- yield_function(t_2030, best_params)
real_return <- ((1 + y_2030) / (1 + 0.02)) - 1
cat("6(e) Exact Real Return (2030 ZCB):", round(real_return * 100, 2), "%\n")

maturity_corp <- as.Date("2032-08-15")
last_coupon_corp <- as.Date("2017-08-15")
cf_dates_corp <- seq(from = last_coupon_corp + months(6), to = maturity_corp, by = "6 months")
t_corp <- as.numeric(cf_dates_corp - settlement_date) / 365.25

cf_corp <- rep(5, length(cf_dates_corp))
cf_corp[length(cf_corp)] <- 105

y_corp <- yield_function(t_corp, best_params)
df_corp <- exp(-y_corp * t_corp)
price_corp <- sum(cf_corp * df_corp)
cat("6(f) Price of Default-Free 15-yr 10% Corp Bond: $", round(price_corp, 2), "\n")

lambda <- 0.03
df_defaultable <- exp(-(y_corp + lambda) * t_corp)
price_defaultable <- sum(cf_corp * df_defaultable)
cat("6(g) Price of Defaultable Corp Bond (3% intensity): $", round(price_defaultable, 2), "\n")

