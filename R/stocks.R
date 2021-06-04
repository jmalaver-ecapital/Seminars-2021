#' ---
#' title: "CAPM"
#' author: "Juan Malaver"
#' date: "6/3/2021"
#' output: github_document
#' ---
#'
#' This is an example data science analysis using the capital asset price model
#' on S&P 500 stocks


# Setup -------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(modelr)
library(broom)

## data import
stocks <- read_csv("../data/stocks.csv", col_types = cols(
  date = col_date(format = "%Y%m%d")))
bigs <-  read_csv("../data/bigstocks.csv", col_names = c("TICKER", "value"))
tbills <- read_csv("../data/tbills.csv")


# Wrangle -----------------------------------------------------------------

## filtering to large stocks
stocks <- stocks %>%
  inner_join(bigs, by = "TICKER") %>%
  select(date, TICKER, RET, sprtrn)

## aggregation function
agg <- function(r) {
  prod(1 + r, na.rm = TRUE) - 1
}
  
## grouping by month
mnthly <- stocks %>%
  mutate(year = year(date), month = month(date)) %>%
  group_by(TICKER, year, month) %>%
  summarize(RET = agg(RET), SNP = agg(sprtrn)) %>%
  unite(date, year, month) %>%
  mutate(date = ymd(date, truncated = 1))

## S&P 500 series
SNP <- mnthly %>%
  filter(TICKER == "AMZN") %>%
  ungroup() %>%
  select(date, SNP)


# Model -------------------------------------------------------------------

## excess and market returns
returns <- mnthly %>%
  inner_join(tbills, by = "date") %>%
  transmute(exr = RET - return,
            mkt = SNP - return)

## grouping by ticker
by_ticker <- returns %>%
  group_by(TICKER) %>%
  nest()

## capm model
ticker_model <- function(df) {
  lm(exr ~ mkt, data = df)
}

## fitting model
ticker_fit <- by_ticker %>%
  mutate(model = map(data, ticker_model),
         tidy = map(model, tidy)) %>%
  unnest(tidy) %>%
  select(TICKER, term, estimate) %>%
  pivot_wider(names_from = term, values_from = estimate) %>%
  rename(alpha = `(Intercept)` , beta = mkt) %>%
  inner_join(bigs, by = "TICKER")