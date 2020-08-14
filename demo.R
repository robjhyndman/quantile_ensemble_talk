library(tidyverse)
library(lubridate)
library(fable)
library(distributional)

# Load data
cafe <- readRDS("cafe.rds")

# Plot data
cafe %>%
  autoplot(turnover) +
  scale_y_log10()

# Fit models to training data
fit <- cafe %>%
  filter(year(date) <= 2018) %>%
  model(
    ETS = ETS(turnover),
    ARIMA = ARIMA(turnover ~ pdq(d = 1) + PDQ(D = 1)),
    SNAIVE = SNAIVE(turnover)
  )

# Add combination and produce forecasts
fc <- fit %>%
  mutate(
    COMB = (ETS + ARIMA) / 2
  ) %>%
  forecast(h = "1 year")

# Add ensemble of ARIMA and ETS forecasts
fc_ensemble <- fc %>%
  filter(.model %in% c("ETS","ARIMA")) %>%
  group_by(state) %>%
  summarise(
    turnover = dist_mixture(
      turnover[1], turnover[2],
      weights=c(0.5,0.5))
  ) %>%
  transmute(.model = "ENSEMBLE", turnover) %>%
  as_fable(key = c(state, .model))
fc <- bind_rows(fc, fc_ensemble)

# Check forecast accuracy against 2019 data
fc %>%
  accuracy(
    data = cafe,
    measures = list(
      crps = CRPS,
      rmse = RMSE,
      mase = MASE,
      ss_crps = skill_score(CRPS),
      ss_rmse = skill_score(RMSE)
    )
  ) %>%
  group_by(.model) %>%
  summarise(
    ss_crps = mean(ss_crps) * 100,
    ss_rmse = mean(ss_rmse) * 100,
    mase = mean(mase)
  )
