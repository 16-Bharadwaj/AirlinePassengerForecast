install.packages("zoo")
library(zoo)
library(forecast)
library(tidyverse)
library(lubridate)

df <- read.csv("C:\\Users\\bhara\\Downloads\\archive (4)\\airline-passengers.csv")

df <- df %>% mutate(month = ym(df$month))

ts_data <- ts(df$total_passengers,
              start = c(year(min(df$month)), month(min(df$month))), frequency = 12)

plot(ts_data, main="Monthly Passenger Count", ylab = "Passengers", xlab = "Time")

decomp <- decompose(ts_data)
plot(decomp)

model_arime <- auto.arima(ts_data)
print(model_arime)
forecast_arima <- forecast(model_arime, h=6)
plot(forecast_arima, main="ARIMA Forecast for Passenger Demand")

model_ets <- ets(ts_data)
forecast_ets <- forecast(model_ets, h = 6)
plot(forecast_ets, main = "ETS Forecast for Passenger Demand")

accuracy(forecast_arima)
accuracy(forecast_ets)


forecast_months <- format(as.yearmon(time(forecast_arima$mean)), "%Y/%m")

forecast_df <- data.frame(
  Month = forecast_months,
  Forecast = round(as.numeric(forecast_arima$mean))
)

write.csv(forecast_df, "forecast_results_final.csv", row.names = FALSE)