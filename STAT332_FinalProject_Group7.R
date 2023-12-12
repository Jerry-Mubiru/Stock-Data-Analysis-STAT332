# Loading the libraries
library(ggplot2)
library(forecast)
library(tseries)
library(tseries)
library(TSA)
library(readxl)
library(xts)

# Loading the data
df <- read.csv("TeslaStockData.csv", header = TRUE)

df$Date <- as.Date(df$Date)   # Converting 'Date' to Date format


df <- df[complete.cases(df$Date), ]
df_xts <- xts(df[, -1], order.by = df$Date)  # Converting to xts time series
# Assuming numeric columns are from the 2nd column onwards
num_cols <- 2:6
df[, num_cols] <- lapply(df[, num_cols], function(x) as.numeric(gsub("[\\$,]", "", x)))


# Converting to xts time series with numeric columns
df_xts <- xts(df[, num_cols], order.by = df$Date)

# Obtaining monthly average
df_monthly <- as.data.frame(apply.monthly(df_xts, colMeans))
df_monthly$Date <- index(df_monthly)  # Add the 'Date' column back

head(df_monthly)  # Displaying the first few rows
summary(df_monthly)  # Summary statistics

# Boxplots for each variable - possible indicator for extreme values/ peaks in volume
par(mfrow=c(2,2))
boxplot(df_monthly$Open, main="Boxplot - Opening Price")
boxplot(df_monthly$Close, main="Boxplot - Closing price")
boxplot(df_monthly$High, main="Boxplot - High Price")
boxplot(df_monthly$Low, main="Boxplot - Low Price")
boxplot(df_monthly$Volume, main="Boxplot - Volume")

#time series plots
ggplot(df_monthly, aes(x = Date, y = Open)) + geom_line() + labs(title = "Time Series Plot - Monthly Average Opening Price")
ggplot(df_monthly, aes(x = Date, y = Close)) + geom_line() + labs(title = "Time Series Plot - Monthly Average Closing Price")
ggplot(df_monthly, aes(x = Date, y = High)) + geom_line() + labs(title = "Time Series Plot - Monthly Average Highest price")
ggplot(df_monthly, aes(x = Date, y = Low)) + geom_line() + labs(title = "Time Series Plot - Monthly Average Lowest price")
ggplot(df_monthly, aes(x = Date, y = Volume)) + geom_line() + labs(title = "Time Series Plot - Monthly Average Volume")

# Decomposition of the time series plots (additive)
decomp_additive <- decompose(ts(df_monthly$Open, frequency = 12), type = "additive")
plot(decomp_additive)
decomp_additive <- decompose(ts(df_monthly$Close, frequency = 12), type = "additive")
plot(decomp_additive)
decomp_additive <- decompose(ts(df_monthly$High, frequency = 12), type = "additive")
plot(decomp_additive)
decomp_additive <- decompose(ts(df_monthly$Low, frequency = 12), type = "additive")
plot(decomp_additive)
decomp_additive <- decompose(ts(df_monthly$Volume, frequency = 12), type = "additive")
plot(decomp_additive)

# Decomposition of the time series plot (multiplicative)
decomp_multiplicative <- decompose(ts(df_monthly$Open, frequency = 12), type = "multiplicative")
plot(decomp_multiplicative)
decomp_multiplicative <- decompose(ts(df_monthly$Close, frequency = 12), type = "multiplicative")
plot(decomp_multiplicative)
decomp_multiplicative <- decompose(ts(df_monthly$High, frequency = 12), type = "multiplicative")
plot(decomp_multiplicative)
decomp_multiplicative <- decompose(ts(df_monthly$Low, frequency = 12), type = "multiplicative")
plot(decomp_multiplicative)
decomp_multiplicative <- decompose(ts(df_monthly$Volume, frequency = 12), type = "multiplicative")
plot(decomp_multiplicative)

# Autocorrelation plots (ACF)
acf(df_monthly$Open, lag.max=40)
acf(df_monthly$Close, lag.max=40)
acf(df_monthly$High, lag.max=40)
acf(df_monthly$Low, lag.max=40)
acf(df_monthly$Volume, lag.max=40)

# Partial autocorrelation plots (PACF)
pacf(df_monthly$Open, lag.max=40)
pacf(df_monthly$Close, lag.max=40)
pacf(df_monthly$High, lag.max=40)
pacf(df_monthly$Low, lag.max=40)
pacf(df_monthly$Volume, lag.max=40)

#-----------------------------------------------------------------
#CHOOSING VOLUME AS THE DATASET - EXPLORATORY ANALYSIS
#-----------------------------------------------------------------

#Time series plot
ggplot(df_monthly, aes(x = Date, y = Volume)) + geom_line() + labs(title = "Time Series Plot - Monthly Average Volume")
#Decomposition of the time series using multiplicative
decomp_multiplicative <- decompose(ts(df_monthly$Volume, frequency = 12), type = "multiplicative")
plot(decomp_multiplicative)
#ACF Plot
acf(df_monthly$Volume, lag.max=40)
#PACF Plot
pacf(df_monthly$Volume, lag.max=40)

# Splitting:
# Assuming your data is already sorted by date
cutoff_index <- round(0.7 * nrow(df_monthly))
train_data <- df_monthly[1:cutoff_index, ]
test_data <- df_monthly[(cutoff_index + 1):nrow(df_monthly), ]
ggplot(train_data, aes(x = Date, y = Volume)) + geom_line() + labs(title = "Time Series Plot - Monthly Average Volume")
ggplot(test_data, aes(x = Date, y = Volume)) + geom_line() + labs(title = "Time Series Plot - Monthly Average Volume")


#ADF test
adf.test(train_data$Volume)

#Log Operations and differenicing
plot(log(df_monthly$Volume), type = "l")
plot(diff(log(df_monthly$Volume)), type = "l")

#Stationarity test
adf.test(log(df_monthly$Volume))
adf.test(diff(log(df_monthly$Volume)))

#SARIMA model fitting on the train data
arima_model_volume <- auto.arima(train_data$Volume)
print(summary(sarima_model_volume))
# Fitting the SARIMA model
sarima_model_volume <- Arima(train_data$Volume, order = c(1, 0, 0), seasonal = list(order = c(1, 1, 1), period = 12))
print(summary(sarima_model_volume))

# Forecasting future values
#num_steps <- length(test_data)  # Forecast the same number of steps as the length of the test data
#sarima_forecast <- forecast(sarima_model_volume, h = num_steps)
sarima_forecast <- forecast(sarima_model_volume)
# Set up a smaller plotting area
par(mar = c(5, 4, 4, 2) + 0.1)  # Adjust the margin as needed

# Creating a sequence for the forecasted values
(forecast_sequence <- seq_along(sarima_forecast$mean)+cutoff_index)

# Plot both the original data and the forecast
plot(df_monthly$Volume, type = "l", col = "blue", lwd = 2, ylab = "Volume", xlab = "Time", main = "SARIMA Forecast vs. Original Data")
lines(seq_along(df_monthly$Volume)[-(1:cutoff_index)], test_data$Volume, col = "green", lwd = 2)
lines(forecast_sequence, sarima_forecast$mean, col = "red", lwd = 2)
legend("topright", legend = c("Original Data", "Test Data", "Forecast"), col = c("blue", "green", "red"), lty = 1:2, cex = 0.8)

plot(forecast(sarima_model_volume))

# Perform the Ljung-Box test
sarima_residuals <- residuals(sarima_model_volume)
ljung_box_test <- Box.test(sarima_residuals, lag = 40, type = "Ljung-Box")
# Display the test results
print(ljung_box_test)
#-----------------------------------------------------------------


