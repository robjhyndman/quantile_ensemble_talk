library(fable)

cafe <- readRDS("cafe.rds")

cafe %>%
  filter(year(date) <= 2018) %>%
  model(
    ETS = ETS(turnover),
    ARIMA = ARIMA(turnover ~
                    pdq(d=1) + PDQ(D=1)),
    SNAIVE = SNAIVE(turnover)
  ) %>%
  mutate(
    COMB = (ETS+ARIMA)/2
  ) %>%
  forecast(h = "1 year") %>%
  accuracy(
    data = cafe,
    measures = list(crps=CRPS, rmse=RMSE)
  ) ->
  crps

sn_crps <- crps %>%
  filter(.model=="SNAIVE") %>%
  select(state,crps) %>%
  rename(sn_crps = "crps")

crps %>%
  filter(.model != "SNAIVE") %>%
  left_join(sn_crps, by="state") %>%
  mutate(
    skill = 100*(sn_crps - crps)/sn_crps
  ) %>%
  group_by(.model) %>%
  summarise(
    skill = mean(skill)
  )
