df_weekly$Volume #this is our signal
plot(diff(df_monthly$Volume), type='l')
plot(diff(log(df_monthly$Volume)), type='l')
LSTS::Box.Ljung.Test(log(df_monthly$Volume))
adf.test(diff(log(df_monthly$Volume))[-1])
decomp_multiplicative <- decompose(ts(df_monthly$Volume, frequency = 12), type = "multiplicative")
plot(decomp_multiplicative)
acf(log(df_monthly$Volume),lag.max = 40)
pacf(log(df_monthly$Volume),lag.max = 40)
