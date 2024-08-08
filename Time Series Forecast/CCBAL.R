# Libraries
# If you don't have these packages installed in R, you need to run
# the install.packages function.
#install.packages("forecast")
#install.packages("tseries")
library(forecast)
library(tseries)

## Modify the setwd line to match your folder location
setwd("C:/Users/83710/Downloads")

## Read files. This file includes the level series. The differencing (one-period and
## seasonal) is done here in the program.
data <- read.csv("CCBAL.csv", header = TRUE)

# Extract the column of interest from the data frame
data_column <- data$CCBAL
head(data_column)

# Make it a time series object, specify appropriate frequency
Original_CCBAL_Series <- ts(data_column, frequency =4)

# Plot the series
plot(Original_CCBAL_Series, type="l",
     main="Original Series",  # Adding a main title
     ylab="Series Values",    # Adding a label to the Y axis
     xlab="Period")           # Adding a label to the X axis

## The series is decomposed through the 'decompose' function, for information
## only, in case is useful. Only for reference.
DEC.Original_CCBAL_Series.TS<-decompose(Original_CCBAL_Series)

## Here we produce the plot of the components, for information
plot(DEC.Original_CCBAL_Series.TS)

# Check the stationarity of the series to see if it needs to be differenced
adf_test <- adf.test(Original_CCBAL_Series, k=0)

# Print the ADF test result with a custom title
cat("ADF Test for the Original Series\n")
print(adf_test)

# Check the ACF and PACF
acf(Original_CCBAL_Series)
pacf(Original_CCBAL_Series)

#Apply a one-period difference to the series as usual to make a series stationary
Differenced_CCBAL_Series <- diff(Original_CCBAL_Series, differences = 1)
plot(Differenced_CCBAL_Series, type="l",
     main="One-Period Difference Series",  # Adding a main title
     ylab="Series Values",    # Adding a label to the Y axis
     xlab="Period")           # Adding a label to the X axis

# Apply ADF to see what it shows, although the series needs an extra seasonal
# differencing step regardless of the ADF result
adf_test <- adf.test(Differenced_CCBAL_Series)

# Print the ADF test result with a custom title
cat("ADF Test for the One-Period Difference Series\n")
print(adf_test)

# Check the ACF and PACF
acf(Differenced_CCBAL_Series)
pacf(Differenced_CCBAL_Series)

# Since the series is seasonal, a seasonal difference is also needed at the 
# appropriate frequency, even if the ADF rejects non-stationarity. The ADF does
# not capture the lack of "seasonal stationarity"
Seasonally_Differenced_CCBAL_Series <- diff(Differenced_CCBAL_Series, differences = 1, lag = 4)
plot(Seasonally_Differenced_CCBAL_Series, type="l",
     main="Seasonally Differenced Series",  # Adding a main title
     ylab="Series Values",    # Adding a label to the Y axis
     xlab="Period")           # Adding a label to the X axis

# Check stationarity on the series after regular and seasonal differencing
adf_test <- adf.test(Seasonally_Differenced_CCBAL_Series)

# Print the ADF test result with a custom title
cat("ADF Test for the Seasonally Differenced Series\n")
print(adf_test)

# Check the ACF and PACF to decide on AR and MA orders
acf_plot <- acf(Seasonally_Differenced_CCBAL_Series)
pacf_plot <- pacf(Seasonally_Differenced_CCBAL_Series)

# Fit the Seasonal ARIMA (SARIMA) model using the original series. Because the
# SARIMA model is fit using the original series, then we need to specify the middle 
# parameter "d" in the c vector to make sure R applies the one-period and seasonal difference. 
# The one-period difference is specified in the first c vector and the seasonal difference
# is specified in the second c vector
sarima_model = forecast::Arima(Original_CCBAL_Series, order = c(1,1,0), seasonal=list(order=c(0,1,1), period=4))
summary(sarima_model)

# Additional residual diagnostics
forecast::checkresiduals(sarima_model)

# Plot the actual and fitted series
plot(sarima_model$x, col='red', type="l",
     main="Actual vs. Fitted",  # Adding a title to the chart
     ylab="Series Values",      # Adding a label to the Y axis
     xlab="Periods")            # Adding a label to the X axis

lines(fitted(sarima_model), col='blue')

# Adding legends for the blue and red lines
legend("topleft", legend=c("Actual", "Fitted"), col=c("red", "blue"), lty=1)


# Produce forecast, in quarters
print(data.frame(predict(sarima_model, n.ahead=12)))

# Different from the output from the linear ARIMA forecast doee, the SARIMA forecast outputs
# produced here by R already comes in levels. This level forecast can be pasted in Excel to 
# produce a chart with a portion of the actual series and the forecast. You can use the SE 
# column to estimate 95% confidence intervals (CI), by calculating:
# Upper CI = forecast + (1.96 * SE)
# Lower CI = forecast - (1.96 * SE)
# These CIs can be added to the forecast plot in Excel.