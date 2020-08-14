library(tidyverse)
library(lubridate)
library(fable)

cafe <- readRDS("cafe.rds")

cafe %>%
  autoplot(turnover) +
  scale_y_log10()

fc <- cafe %>%
  filter(year(date) <= 2018) %>%
  model(
    ETS = ETS(turnover),
    ARIMA = ARIMA(turnover ~ pdq(d = 1) + PDQ(D = 1)),
    SNAIVE = SNAIVE(turnover)
  ) %>%
  mutate(
    COMB = (ETS + ARIMA) / 2
  ) %>%
  forecast(h = "1 year")

fc %>%
  accuracy(
    data = cafe,
    measures = list(
      crps = CRPS,
      rmse = RMSE,
      ss=skill_score(CRPS)
    )
  ) %>%
  group_by(.model) %>%
  summarise(sspc = mean(ss) * 100)
