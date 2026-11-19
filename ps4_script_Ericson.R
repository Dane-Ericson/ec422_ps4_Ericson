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

#2.2
liquor_fit <- liquor |>
  model(arima = ARIMA(log(sales)),
        ets = ETS(log(sales))
        )

liquor_fit |> select(arima) |> report()
liquor_fit |> select(ets) |> report()

# An ARIMA(1,0,2)(2,1,1) specification was chosen for the ARIMA model, meaning it was seasonally differenced 1 time and nonseasonally differneced 0 times, while an ETS(A,A,A) model was chosen for the ETS model, meaning the error, trend, and soasonal compontents are all additive.

liquor_fit |> select(arima) |> gg_tsresiduals()

#2.2b
liquor_fit |>
  select(arima) |>
  augment() |>
  features(.innov, ljung_box, lag = 24, dof = 6)

# The ljung-box test p-value is 0, meaning we should reject the H0 of white noise residuals and that there is likely remaining autocorrelation struture we could capture.

#2.3

liquor_cv <- liquor |>
  stretch_tsibble(.init = 120, .step = 1) |>
  filter(.id < max(.id))

liquor_cv_fit <- liquor_cv |>
  model(snaive = SNAIVE(sales),
        ets    = ETS(log(sales)),
        arima  = ARIMA(log(sales)))

saveRDS(liquor_cv_fit, "results/liquor_cv_fit.rds")

#There should be 216 forecast origins considered in this TS-CV.

#2.3b

liquor_cv_fc <- liquor_cv_fit |>
  forecast(h = 1)

liquor_cv_fc |>
  accuracy(liquor, measures = list(RMSE = RMSE, MAE = MAE)) |> knitr::kable(digits = 2)


#The first forecast should be in January 2002, while the last should be in December 2019. Both metrics agree that the ARIMA model foretasted the most accurately. 


liquor_errs <- liquor_cv_fc |>
  as_tibble() |>
  left_join(as_tibble(liquor) |> select(Month, actual = sales),
            by = "Month") |>
  mutate(e = actual - .mean) |>
  select(Month, .model, e) |>
  pivot_wider(names_from = .model, values_from = e)

forecast::dm.test(liquor_errs$arima, liquor_errs$ets, h = 1, power = 2)



liquor_errs <- liquor_cv_fc |>
  as_tibble() |>
  left_join(as_tibble(liquor) |> select(Month, actual = sales),
            by = "Month") |>
  mutate(e = actual - .mean) |>
  select(Month, .model, e) |>
  pivot_wider(names_from = .model, values_from = e)

forecast::dm.test(liquor_errs$arima, liquor_errs$snaive, h = 1, power = 2)

#Our D-M test between our ARIMA and ETS models had DM statistic -2.9062, and p-value 0.004042. Our second D-M test between our ARIMA and seasonal random walk models had DM statistic -10.755, and p-value less than 2.2e-16. With our DM test results, we can conclude that the ARIMA model produces more accurate forecasts in a statistically significant manner. If I were the liquor store, I would implement the ARIMA model to best forecast month-ahead sales. 

##Part 3