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

liquor_combo_fc <- liquor_cv_fit |>
  mutate(combo = (arima + ets) / 2) |>
  forecast(h = 1)

liquor_combo_fc |>
  accuracy(liquor, measures = list(RMSE = RMSE, MAE = MAE)) |> knitr::kable(digits = 2)


#The combo model is better than the seasonal random walk and ETS model by both errors, but is worse than the ARIMA model by both errors.

cor(liquor_errs$arima, liquor_errs$ets)

#Our correlation value came back as 0.878, meaning that the errors of the ARIMA and ETS models are very correlated. This, in combination with our DM test results showing that the ARIMA model was more accurate, likely is the reason for the ARIMA beating the combination model, as this means that the errors will likely not cancel one another out due to the errors often being in the same direction. 


##Part 4

#4.1
qd_raw <- read_csv("data/fredqd_2026-09.csv", show_col_types = FALSE) |>
  slice(-(1:2))


growth <- function(x) 100 * difference(log(x))

qd <- qd_raw |>
  mutate(date = as.Date(sasdate, "%m/%d/%Y")) |>
  transmute(date,
            gdp    = growth(GDPC1),     # real GDP
            cons   = growth(PCECC96),   # real consumption
            inv    = growth(GPDIC1),    # real investment
            emp    = growth(PAYEMS),    # payroll employment
            infl   = growth(GDPCTPI),   # GDP price index
            unrate = difference(UNRATE),     # unemployment rate
            ffr    = difference(FEDFUNDS),   # federal funds rate
            gs10   = difference(GS10)) |>    # 10-year Treasury yield
  filter(date >= as.Date("1960-03-01"), date <= as.Date("2019-12-01"))

y <- qd |> select(-date) |> as.matrix()

nrow(y)
head(y)

#4.1c

N <- nrow(y)
origins <- 120:(N - 1)
actual <- y[121:N, ]

#We will produce 120 forecasts for each variable, and our first forecast will be in Q1 of 1990, while our last forecast is in Q4 of 2019.



#4.2

var_ols_fc <- function(z, p) {
  e <- embed(z, p + 1)            # columns: y_t, then its lags 1 to p
  M <- ncol(z)                    # number of variables
  B <- coef(lm(e[, 1:M] ~ e[, -(1:M)]))
  x <- c(1, t(z[nrow(z):(nrow(z) - p + 1), ]))   # constant, latest p lags
  x %*% B
}

fc_var1 <- NULL
fc_var4 <- NULL
for (T in origins) {
  fc_var1 <- rbind(fc_var1, var_ols_fc(y[1:T, ], 1))
  fc_var4 <- rbind(fc_var4, var_ols_fc(y[1:T, ], 4))
}

rmse_by_var <- function(f) sqrt(colMeans((actual - f)^2))

rmse_by_var(fc_var1)
rmse_by_var(fc_var4)


#Our VAR(1) model forecasted better than our VAR(4) model for 7 of the 8 variables (all except inflation). The VAR(1) model may have performed better due to estimation error from overfitting potentially present in the VAR(4) model from its extra parameters. 

#4.3

bvar_cv <- function(lambda) {
  pr <- bv_priors(hyper = "lambda",
                  mn = bv_mn(lambda = bv_lambda(mode = lambda, sd = 0.4,
                                                min = 0.999 * lambda,
                                                max = 1.001 * lambda),
                             b = 0))
  fc <- NULL
  for (T in origins) {
    fit_T <- bvar(y[1:T, ], lags = 4, priors = pr,
                  n_draw = 2000, n_burn = 1000, verbose = FALSE)
    x <- c(1, t(y[T:(T - 3), ]))         # constant and 4 lags of each variable
    fc <- rbind(fc, x %*% coef(fit_T))   # forecast of y_{T+1}
  }
  fc
}

set.seed(422)
fc_bvar_03 <- bvar_cv(0.3)
saveRDS(fc_bvar_03, "results/fc_bvar_03.rds")




set.seed(422)
fc_bvar_005 <- bvar_cv(0.05)

var_results <- tibble(
  Variable = colnames(y),
  `VAR(1)` = rmse_by_var(fc_var1),
  `VAR(4)` = rmse_by_var(fc_var4),
  `BVAR(4), lambda = 0.3` = rmse_by_var(fc_bvar_03),
  `BVAR(4), lambda = 0.05` = rmse_by_var(fc_bvar_005)
)

saveRDS(var_results, "results/var_results.rds")

var_results |> knitr::kable(digits = 3)

#The BVAR(4) with lambda = 0.3 forecasted better than the VAR(4) across all 8 variables. This may be due to the shrinkage in the BVAR model reducing the overfitting from the extra parameters in the VAR(4) by pulling the parameter estimates closer to zero, reducing estimation variance.

#The BVAR(4) model outperformed the VAR(1) model on only three of the eight variables.

#With the increased shrinkage in our BVAR(4) model with lambda 0.05, our RMSE values were unanimously higher than the BVAR with less shrinkage. The winning model by RMSE in this table may not be an accurate forecast of how it may forecast in the future, as it may have benefited from favorable random variation in the evaluation data, underestimating its true forcasting abilities.



#4.3c

e_var4 <- actual[, "gdp"] - fc_var4[, 1]
e_bvar <- actual[, "gdp"] - fc_bvar_03[, 1]

forecast::dm.test(e_var4, e_bvar, h = 1, power = 2)


e_var4 <- actual[, "infl"] - fc_var4[, 5]
e_bvar <- actual[, "infl"] - fc_bvar_03[, 5]

forecast::dm.test(e_var4, e_bvar, h = 1, power = 2)


#Our p-value for GDP growth is 0.035 with a DM stat of 2.14, suggesting that our BVAR was better in a significant way. For inflation, the p-value was 0.1489, indicating that the difference in expected loss is not statistically significant at the 5% level. This reaffirms the statment from 4.3b regarding the results, as it supports the claim that the model with the lower RMSE may not forecast better in the future.
