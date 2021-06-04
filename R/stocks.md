CAPM
================
Juan Malaver
6/3/2021

This is an example data science analysis using the capital asset price
model on S&P 500 stocks

``` r
# Setup -------------------------------------------------------------------

library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.3     v purrr   0.3.4
    ## v tibble  3.1.1     v dplyr   1.0.6
    ## v tidyr   1.1.3     v stringr 1.4.0
    ## v readr   1.4.0     v forcats 0.5.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library(modelr)
library(broom)
```

    ## 
    ## Attaching package: 'broom'

    ## The following object is masked from 'package:modelr':
    ## 
    ##     bootstrap

``` r
## data import
stocks <- read_csv("../data/stocks.csv", col_types = cols(
  date = col_date(format = "%Y%m%d")))
```

    ## Warning: 6736 parsing failures.
    ##   row  col expected actual                 file
    ## 42289 RET  a double      C '../data/stocks.csv'
    ## 42289 RETX a double      C '../data/stocks.csv'
    ## 43876 RET  a double      C '../data/stocks.csv'
    ## 43876 RETX a double      C '../data/stocks.csv'
    ## 45441 RET  a double      C '../data/stocks.csv'
    ## ..... .... ........ ...... ....................
    ## See problems(...) for more details.

``` r
bigs <-  read_csv("../data/bigstocks.csv", col_names = c("TICKER", "value"))
```

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   TICKER = col_character(),
    ##   value = col_double()
    ## )

``` r
tbills <- read_csv("../data/tbills.csv")
```

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   date = col_date(format = ""),
    ##   return = col_double()
    ## )

``` r
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
```

    ## `summarise()` has grouped output by 'TICKER', 'year'. You can override using the `.groups` argument.

``` r
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
```
