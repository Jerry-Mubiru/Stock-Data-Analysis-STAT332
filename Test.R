library(rugarch)
df_monthly$Volume #this is our signal

#Time Series plot
plot(df_monthly$Volume, type='l')
plot(log(df_monthly$Volume), type='l')
plot(diff(log(df_monthly$Volume)), type='l')

#Decomposition
decomp_multiplicative <- decompose(ts(df_monthly$Volume, frequency = 12), type = "multiplicative")
plot(decomp_multiplicative)

decomp_additive <- decompose(ts(df_monthly$Volume, frequency = 12), type = "additive")
plot(decomp_additive)
#ACF
acf(df_monthly$Volume, lag.max = 40)
acf(diff(log(df_monthly$Volume))[-1], lag.max = 40)
#PACF
pacf(diff(log(df_monthly$Volume))[-1], lag.max = 40)
pacf(df_monthly$Volume, lag.max = 40)
#Plot many variables and decide which is the most promising

#If you notice a significant pattern, SARIMA
sarima_model_volume <- auto.arima(df_monthly$Volume)
summary(sarima_model_volume)

#if not, G(ARCH) - If both, first do the SARIMA then do the G(ARCH) to the residuals
(sarima_residuals <- residuals(sarima_model_volume))

#After model decision - test hypotheses to validate stationarity, estimate model parameters

# Example: Simulated training data
set.seed(123)
sample_index <- sample(1:nrow(df_monthly$Volume), 0.7 * nrow(df_monthly$Volume))

# Extract the "Volume" column for both training and testing data
train_data <- df_monthly$Volume[sample_index]
test_data <- df_monthly$Volume[-sample_index]

# Fit the auto.arima model using the training data
arima_model <- auto.arima(train_data)

# Generating predictions for the testing data
(forecast_values <- forecast(arima_model, h = length(test_data)))

plot(forecast_values, main = "Auto ARIMA Forecast", xlab = "Time", ylab = "Values")
lines(test_data, col = "blue")  # Add the testing time series data for comparison
plot(forecast(sarima_model_volume))


#Estimate parameters for the training dataset, then forecast to the test dataset
LSTS::Box.Ljung.Test(diff(log(df_monthly$Volume))[-1])

adf.test(diff(log(df_monthly$Volume))[-1])
adf.test(log(df_monthly$Volume))

# Perform the Ljung-Box test
ljung_box_test <- Box.test(sarima_residuals, lag = 40, type = "Ljung-Box")

