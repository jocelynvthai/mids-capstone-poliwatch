# Outputs:
#   1. stock_prices.csv
#   2. transactions.csv

# 0. Preliminaries -------------------------------------------------------------
library(tidyverse)
library(rjson)
library(quantmod)
library(tidyquant)

# 1. Raw data ------------------------------------------------------------------

# get raw data as json and store as list
# House of Representatives data:
df1 <- rjson::fromJSON(file = "https://house-stock-watcher-data.s3-us-west-2.amazonaws.com/data/all_transactions.json")
# Senate data:
df2 <- rjson::fromJSON(file = "https://senate-stock-watcher-data.s3-us-west-2.amazonaws.com/aggregate/all_transactions.json")

# convert from list to dataframe
df1 <- df1 %>% 
  tibble::enframe(name = "id", value = "data") %>%
  tidyr::unnest_wider(data) %>%
  dplyr::mutate(asset_type = NA,
                comment = NA,
                member_type = "representative") %>%
  dplyr::rename("member" = "representative") %>%
  dplyr::select(-id, -disclosure_year)

df2 <- df2 %>% 
  tibble::enframe(name = "id", value = "data") %>%
  tidyr::unnest_wider(data) %>%
  dplyr::mutate(district = NA,
                cap_gains_over_200_usd = NA,
                member_type = "senator") %>%
  dplyr::rename("member" = "senator") %>%
  dplyr::select(-id)

# merge house and senate data
df <- rbind(df1, df2); rm(df1, df2)

# clean irrelevant rows
df <- df %>%
  dplyr::filter(!is.na(ticker), ticker != "--") %>%
  # convert amount column to integer (floored)
  dplyr::mutate(amount_formatted = str_extract(amount, "\\$[0-9,]+") %>%
                  gsub("[\\$,]", "", .) %>%
                  as.integer() %>%
                  plyr::round_any(10))

# 2. Ticker data ---------------------------------------------------------------

valid_tickers <- intersect(unique(df$ticker), c(tidyquant::tq_exchange("NASDAQ")$symbol, 
                                                tidyquant::tq_exchange("NYSE")$symbol))

# fetch stock prices
# recall that 404 error is page not found -- this means that the ticker is not valid
stock_prices <- tidyquant::tq_get(valid_tickers, get = "stock.prices", 
                                  from = "2010-01-01", to = "2023-09-01")
write.csv(stock_prices, paste0("../data/stock_prices_", gsub("-", "", Sys.Date()), ".csv"), 
          row.names = F)

# if data is already loaded: 
stock_prices <- read.csv("../data/stock_prices_20230914.csv") %>%
  mutate(date = as.Date(date))

# 3. Join transaction data with ticker data ------------------------------------
# Associate transaction price with transactions
df <- df %>%
  dplyr::mutate(transaction_date = base::as.Date(transaction_date)) %>%
  dplyr::left_join(stock_prices, 
                   by = c("ticker" = "symbol", "transaction_date" = "date")) %>%
  # remove rows with NA price values (presumably rows with invalid tickers)
  dplyr::filter(!is.na(open)) %>% 
  # compute total shares purchased.
  # recall that amount_formatted is the minimum amount of the transaction
  dplyr::mutate(shares_purchased = ceiling(amount_formatted/open)) %>%
  dplyr::arrange(desc(transaction_date))

# Associate transactions with ticker industry and country
df <- df %>%
  mutate(transaction_date = as.Date(transaction_date)) %>%
  select(-industry) %>%
  left_join(select(rbind(tidyquant::tq_exchange("NYSE"), tidyquant::tq_exchange("NASDAQ")),
                   symbol, market.cap, country, ipo.year, industry),
            by = c("ticker" = "symbol")) %>%
  select(disclosure_date, transaction_date, owner, member, member_type, amount_formatted, 
         type, ticker, industry, sector, shares_purchased, open, high, low, close, volume, 
         everything())

# 4. Define short and medium term performance metrics --------------------------
# use Yahoo Finance's definition of short term / medium term performance:
#   short: 2W - 6W (14D-42D)
#   medium: 6W-9M (43D - 270D)

# define helper function
performance <- function(ticker, transaction_date, ticker_open, stock_prices,
                        lower = 14, upper = 42) {
  stock_prices %>%
    filter(symbol == ticker, 
           date >= transaction_date + lower, 
           date <= transaction_date + upper) %>%
    summarise(avg_per_change = mean(open - ticker_open)/ticker_open,
              sd_per_change = sd(open - ticker_open)/ticker_open) %>%
    unlist()
}
short_term <- purrr::possibly(.f = performance, c(avg_per_change = NA, sd_per_change = NA))

# test helper function
#view(df[1,])
#performance(df[1,]$ticker, df[1,]$transaction_date, df[1,]$open, stock_prices)

# apply to data frame
df <- df %>%
  rowwise() %>%
  mutate(short_term = list(performance(ticker, transaction_date, open, stock_prices,
                                       lower = 14, upper = 42)),
         med_term = list(performance(ticker, transaction_date, open, stock_prices,
                                     lower = 43, upper = 270))) %>%
  ungroup() %>%
  mutate(short_term.avg_per_change = map_dbl(short_term, ~ .[["avg_per_change"]]),
         short_term.sd_per_change = map_dbl(short_term, ~ .[["sd_per_change"]]),
         med_term.avg_per_change = map_dbl(med_term, ~ .[["avg_per_change"]]),
         med_term.sd_per_change = map_dbl(med_term, ~ .[["sd_per_change"]])) %>%
  select(-short_term, -med_term)

write.csv(df, paste0("../data/transactions_", gsub("-", "", Sys.Date()), ".csv"), 
          row.names = FALSE)
