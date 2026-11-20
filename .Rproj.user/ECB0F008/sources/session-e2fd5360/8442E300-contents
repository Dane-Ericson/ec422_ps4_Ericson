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

#An ARIMA(1,0,2)(2,1,1) specification was chosen for the ARIMA model, meaning it was seasonally differenced 1 time and non-seasonally differenced 0 times, while an ETS(A,A,A) model was chosen for the ETS model, meaning the error, trend, and seasonal components are all additive.


#2.2b
liquor_fit |> select(arima) |> gg_tsresiduals()


liquor_fit |>
  select(arima) |>
  augment() |>
  features(.innov, ljung_box, lag = 24, dof = 6)

#The ljung-box test p-value is 0, meaning we should reject the H0 of white noise residuals and that there is likely remaining autocorrelation structure we could capture.

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


#The first forecast should be in January 2002, while the last should be in December 2019. Both metrics agree that the ARIMA model forecasted the most accurately. 


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


##Part 5

md_raw <- read_csv("data/fredmd_2026-09.csv", show_col_types = FALSE)
tcode <- unlist(md_raw[1, -1])
md <- md_raw[-1, ] |> mutate(date = as.Date(sasdate, "%m/%d/%Y"))

transform_series <- function(x, code) {
  d <- function(v) c(NA, diff(v))
  switch(as.character(code),
         "1" = x,                      # level
         "2" = d(x),                   # first difference
         "3" = d(d(x)),                # second difference
         "4" = log(x),                 # log
         "5" = 100 * d(log(x)),        # growth rate
         "6" = 100 * d(d(log(x))),     # change in the growth rate
         "7" = d(x / dplyr::lag(x) - 1))
}


fm <- map2_dfc(md[names(tcode)], tcode, transform_series) |>
  mutate(date = md$date, .before = 1) |>
  filter(date >= as.Date("1960-03-01"), date <= as.Date("2019-12-01"))

fm <- fm[, colSums(is.na(fm)) == 0]

dim(fm)


tcode[c("INDPRO", "PAYEMS", "CPIAUCSL", "FEDFUNDS")]


#INDPRO is transformed using the first difference of logged values. 100(log(X)t - log(x)t-1).

#PAYEMS is transformed the same way as INDPRO.

#CPIAUCSL is transformed using the second difference of logged values.

#FEDFUNDS is transformed using just the first differences of values, with no log. 

#FEDFUNDS and INDPRO likely use different transformations because INDPRO is a quantity, so we are more interested in the percentage growth rates, while FEDFUNDS is expressed as an interest rate, so we want to observe changes in percentage points rather than percentage growth.


#5.2

Z <- fm |> select(-date) |> as.matrix()

pc <- prcomp(scale(Z))

summary(pc)$importance[, 1:5]


#The first three factors explain 29.45 percent of the variance.


F1 <- pc$x[, 1] * sign(cor(pc$x[, 1], fm$INDPRO))

tibble(date = fm$date, F1 = F1) |>
  ggplot(aes(date, F1)) +
  geom_line() +
  labs(title = "First Factor from FRED-MD", x = NULL, y = NULL) +
  theme_minimal()

sort(abs(cor(pc$x[, 1], Z))[1, ], decreasing = TRUE)[1:8]
sort(abs(cor(pc$x[, 2], Z))[1, ], decreasing = TRUE)[1:8]

#The first component seems to capture economic activity and production, correlating closely with other measures of economic activity like PAYEMS and INDPRO. The second factor seems to capture inflation and price changes, correlation closely with other consumer price variables like CPIAUSL. 

#5.3

ip <- fm$INDPRO
n <- nrow(Z)
init <- which(fm$date == as.Date("1989-12-01"))

e_ar <- NULL
e_fa <- NULL
for (T in init:(n - 1)) {
  F_T <- prcomp(scale(Z[1:T, ]))$x[, 1:3]         # factors using data through T
  d <- data.frame(y_next = ip[2:T], y = ip[1:(T - 1)], F_T[1:(T - 1), ])
  new <- data.frame(y = ip[T], t(F_T[T, ]))
  e_ar <- c(e_ar, ip[T + 1] - predict(lm(y_next ~ y, d), new))
  e_fa <- c(e_fa, ip[T + 1] - predict(lm(y_next ~ ., d), new))
}

c(RMSE_AR = sqrt(mean(e_ar^2)), RMSE_FA = sqrt(mean(e_fa^2)))

# Estimating the factors using the full sample would incorporate information from future periods into prior forecasts, leading to information leakage and making forecasts appear better than they are. 

#5.4

direct_race <- function(series, h = 12, r = 3) {
  lv <- md |>
    mutate(log_x = log(.data[[series]]),
           g = 100 * (log_x - lag(log_x))) |>
    filter(date >= as.Date("1960-03-01"), date <= as.Date("2019-12-01"))
  yh <- 100 * (lead(lv$log_x, h) - lv$log_x)      # growth over the next h months
  g  <- lv$g                                      # growth over the latest month
  
  e_ar <- NULL
  e_fa <- NULL
  for (T in init:(n - h)) {
    F_T <- prcomp(scale(Z[1:T, ]))$x[, 1:r, drop = FALSE]
    i <- 1:(T - h)
    d <- data.frame(yh = yh[i], g = g[i], F_T[i, , drop = FALSE])
    new <- data.frame(g = g[T], F_T[T, , drop = FALSE])
    e_ar <- c(e_ar, yh[T] - predict(lm(yh ~ g, d), new))
    e_fa <- c(e_fa, yh[T] - predict(lm(yh ~ ., d), new))
  }
  tibble(series, forecasts = length(e_ar),
         RMSE_AR = sqrt(mean(e_ar^2)), RMSE_FA = sqrt(mean(e_fa^2)),
         MAE_AR = mean(abs(e_ar)), MAE_FA = mean(abs(e_fa)),
         DM_pvalue = forecast::dm.test(e_ar, e_fa, h = h, power = 2,
                                       varestimator = "bartlett")$p.value)
}

direct_race("CPIAUCSL") |>
  print(width = Inf)

#Both the MAE and RMSE are lowered in Factor Augmented model than the AR model, with a 10.43% reduction in RMSE. These results are statistically significant at the 5% level with a p-value of 0.0004.