library(tidyquant)
library(tidyverse)
library(PerformanceAnalytics)

# Function to calculate financial metrics for a given dataframe of stock prices
calculateStatistics <- function(price_data) {
  # Compute daily log returns
  returns_df <- price_data %>%
    group_by(symbol) %>%
    tq_transmute(select     = adjusted,
                 mutate_fun = periodReturn,
                 period     = "daily",
                 type       = "arithmetic",
                 col_rename = "daily_return") %>%
    ungroup()

  # Identify benchmark (SPY) returns for beta calculation
  market_returns <- returns_df %>%
    filter(symbol == "SPY") %>%
    select(date, market_return = daily_return)

  # Join market returns back to the main returns dataframe
  returns_with_market <- returns_df %>%
    left_join(market_returns, by = "date")

  # Compute metrics per ticker
  # Metrics include: average return, volatility, sharpe ratio, skewness, kurtosis, and asset beta
  summary_stats <- returns_with_market %>%
    group_by(symbol) %>%
    summarise(
      avg_return   = mean(daily_return, na.rm = TRUE),
      volatility   = sd(daily_return, na.rm = TRUE),
      sharpe_ratio = if(sd(daily_return, na.rm = TRUE) != 0) mean(daily_return, na.rm = TRUE) / sd(daily_return, na.rm = TRUE) else NA,
      skewness     = PerformanceAnalytics::skewness(daily_return),
      kurtosis     = PerformanceAnalytics::kurtosis(daily_return),
      asset_beta   = cov(daily_return, market_return, use = "complete.obs") / var(market_return, na.rm = TRUE)
    )

  return(summary_stats)
}

