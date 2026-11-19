library(tidyverse)
library(fpp3)
library(BVAR)

## Part 2

#read in the data
liquor_raw <- read_csv(
  "data/MRTSSM4453USN.csv",
  na = ".",
  col_types = cols(
    observation_date = col_date(format = "%Y-%m-%d"),
    MRTSSM4453USN = col_double()
  )
)

#make tsibble
liquor <- liquor_raw |>
  transmute(Month = yearmonth(observation_date), sales = MRTSSM4453USN) |>
  as_tsibble(index = Month) |>
  filter(Month <= yearmonth("2019 Dec"))

has_gaps(liquor)

#2.1b
autoplot(liquor, sales) +
  labs(title = "Monthly US liquor sales", y = "Sales (millions of dollars)", x = "Year")

gg_season(liquor, log(sales)) +
  labs(title = "Seasonal plot: Monthly US liquor sales", y = "Sales (millions of dollars)", x = "Month")

#The liquor sales data shows both a clear upwards trend over time and seasonality. It appears that sales are highest in later months (November and December), and lowest in earlier months like January.The seasonal swings do appear to increase with time, so using logs may be wise, as it should help to stabilize the seasonal swings.

