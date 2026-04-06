library(tidyquant)
library(tidyverse)
library(PerformanceAnalytics)

# Function to calculate financial metrics for a given dataframe of stock prices
calculateStatistics <- function(price_data) {
  # Compute daily log returns
  # Using snake_case for the internal returns dataframe as per guidelines
  returns_df <- price_data %>%
    group_by(symbol) %>%
    tq_transmute(select     = adjusted,
                 mutate_fun = periodReturn,
                 period     = "daily",
                 type       = "log",
                 col_rename = "daily_return") %>%
    ungroup()

  # Compute metrics per ticker
  # Metrics include: average return, volatility, sharpe ratio, skewness, and kurtosis
  summary_stats <- returns_df %>%
    group_by(symbol) %>%
    summarise(
      avg_return   = mean(daily_return, na.rm = TRUE),
      volatility   = sd(daily_return, na.rm = TRUE),
      sharpe_ratio = if(sd(daily_return) != 0) mean(daily_return) / sd(daily_return) else NA,
      skewness     = PerformanceAnalytics::skewness(daily_return),
      kurtosis     = PerformanceAnalytics::kurtosis(daily_return)
    )

  return(summary_stats)
}

# Example usage with the tickers previously requested
tickers <- c("SPY", "XLB", "XLC", "XLE", "XLF", "XLI", "XLK", "XLP", "XLRE", "XLV", "XLU", "XLY")

# Pull historical data
historical_prices <- tickers %>%
  tq_get(get = "stock.prices",
         from = "2020-01-01")

# Apply the function
# Results are stored in the ticker_metrics_df snake_case variable
ticker_metrics_df <- calculateStatistics(historical_prices)

# Print results
print(ticker_metrics_df)
