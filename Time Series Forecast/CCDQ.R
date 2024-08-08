# Libraries
# If you don't have these packages installed in R, you need to run
# the install.packages function.
#install.packages("forecast")
#install.packages("tseries")
library(forecast)
library(tseries)

## Modify the setwd line to match your folder location
setwd("C:/Users/83710/Downloads")
## Read file, which includes the level and difference series CCDQ and CHGCCDQ
data<-read.csv("CCDQ.csv", header=TRUE)

## Plot the original series
plot(data$CCDQ, type="l",
     main="Original Series",  # Adding a main title
     ylab="Series Values",    # Adding a label to the Y axis
     xlab="Period")           # Adding a label to the X axis

## Plot the differenced series
plot(data$CHGCCDQ, type="l",
     main="One-Period Difference Series",  # Adding a main title
     ylab="Series Values",    # Adding a label to the Y axis
     xlab="Period")           # Adding a label to the X axis

## The series is decomposed through the 'decompose' function, for information
## only, in case is useful. Only for reference.
data$CHGDATATS<-ts(data$CHGCCDQ, frequency=4)
DEC.CHGDATATS.TS<-decompose(data$CHGDATATS)

## Here we produce the plot of the components, for information
plot(DEC.CHGDATATS.TS)

## Perform stationarity test on level series included in the dataset
adf_test <- adf.test(data$CCDQ, k=0)

# Print the ADF test result with a custom title
cat("ADF Test for the Original Series\n")
print(adf_test)

# Check the ACF and PACF
acf(data$CCDQ)
pacf(data$CCDQ)

# Perform stationarity tests on differenced series included in the dataset
adf_test <- adf.test(data$CHGCCDQ, k=0)

# Print the ADF test result with a custom title
cat("ADF Test for the Differenced Series\n")
print(adf_test)

# Check the ACF and PACF
acf(data$CHGCCDQ)
pacf(data$CHGCCDQ)

##### FIRST TRY MODEL #####
## Fit Arima model on the differenced series, with 8 AR term
(arima_model <- Arima(data$CHGCCDQ, c(8, 0, 0)))

## Check residuals
checkresiduals(arima_model)

# Plot the actual and fitted series
plot(arima_model$x, col='red', type="l",
     main="Actual vs. Fitted",  # Adding a title to the chart
     ylab="Series Values",      # Adding a label to the Y axis
     xlab="Periods")            # Adding a label to the X axis

lines(fitted(arima_model), col='blue')

# Adding legends for the blue and red lines
legend("topleft", legend=c("Actual", "Fitted"), col=c("red", "blue"), lty=1)

## Produce forecast. Note this is the forecast of the change.
## To see how they affect the original level series, add
## the forecasted change to the series. Can be done in Excel
futureval<-forecast(arima_model,h=12)
futureval


##### SIMPLER MODEL #####
## Fit alternative Arima model with simpler specification using 1 AR term
(arima_model_simpler <- Arima(data$CHGCCDQ, c(1, 0, 0)))

## Check residuals again
checkresiduals(arima_model_simpler)

# Plot the actual and fitted series
plot(arima_model_simpler$x, col='red', type="l",
     main="Actual vs. Fitted",  # Adding a title to the chart
     ylab="Series Values",      # Adding a label to the Y axis
     xlab="Periods")            # Adding a label to the X axis

lines(fitted(arima_model_simpler), col='blue')

# Adding legends for the blue and red lines
legend("topleft", legend=c("Actual", "Fitted"), col=c("red", "blue"), lty=1)

## Produce forecast, in quarters, in quarters. Note this is the forecast of the change.
## To see how they affect the original level series, add
## the forecasted change to the series
futureval<-forecast(arima_model_simpler,h=12)
futureval

## You need to use the change forecast produced here by R to
## build the level forecast in Excel

