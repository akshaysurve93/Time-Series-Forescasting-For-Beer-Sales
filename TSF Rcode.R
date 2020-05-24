setwd("C:/Users/DELL/Desktop/Akshay/Group Assignments/Group Assignment 6 TSF")
getwd()
beer <- read.csv("C:/Users/DELL/Desktop/Akshay/Group Assignments/Group Assignment 6 TSF/beer.csv", sep="")
summary(beer)
str(beer)

## since as mentioned in the probelm statement that it is quarterly data with 72 observations
## Total of 18 years of data is available.
## Assuming these 18 years to be recent i.e. concidering starting year 2001
## converting dataset into Time Series in order to predict out
beer_time_series <- ts(beer, start = 2001, frequency = 4, end = 2019 )
## frequency = 4 since data is for quarters
str(beer_time_series)

## plotting beer time series
plot(beer_time_series, main = "Quarterly beer sales over 18 years", col = "red")
cycle(beer_time_series)
## from plot it can be seen that seasonality of beer sales is almost consistent.
## over a period of 18 years.
## inter quarter/year fluctuations are almost similar accross the years
## but there is seasonality in the data.
## there is slightly increasing trend in the time series which goes on increasing YOY
## also there are no outliers in the Time series
## also there is no diference in the in the time series.
## there is consistency in the time series
## Overall Upward positive trend shows increase in sales with time.
## Seasonality foreseen: Regular pattern which increases slowly and constantly.
## There is Heteroscadasticity: 
## Variation from one season to another seems to increase over time

##################################
## subseries plot
## checking the seasonality in the time series
## taking seasonality component out and plooting data for every season of the series
## checking behaviour of data for each quarter
monthplot(beer_time_series)
## from seasonality plot it can be seen that 
## for quarter 2 ,3 and 4 seasonality is almost similar
## also the beer sales were low at the start of every quarter
## beer sale drops at the end of quarter 1

boxplot(beer_time_series~cycle(beer_time_series),
        main = "Quarterly variation in the beer sales over the years")
## boxplot shows that at the end of each year there are higher sales
## middle quarters shows lower sales compared to quarter 1 and quarter 4
## furthur quarters according to sales can be arranged in decending order. 
## they are: Quarter 4 > Quarter 1 > Quarter 3 > Quarter 2

library(tseries)
library(ggplot2)
library(fpp2)
library(TSA)
## Lag plot
gglagplot(beer_time_series)
## from lag plot it can be seen that lagplot 1, lag 5 and lag 9 shows similar pattern
## lag 4 and lag 8 shows similar patttern
## lag 3 and lag 7 shows similar pattern
## lag 2 and lag 6 exhibit similar pattern
## this furthur proves the presence of seasonality in the time series

##########################################
# AUTO CORELATIION
#Lags are very useful in time series analysis 
#because of a phenomenon called autocorrelation, which is a tendency for
#the values within a time series to be correlated with previous copies of itself

# 1] Auto corelation function
acf(beer_time_series)
## form above graoh it can be seen that 
## the values within a time series are correlated with previous copies of themselves
## and it decreases with time, aslo with advancing time the effect of past observations decreases
## The non-seasonality in time series reflects an AR model.

#2] Partial Autocorelation Function
pacf(beer_time_series)
## Auto coralation outside purple band indicate 
## significant dependency of series on a series with lag 2.
## the above plot indicates that after lag 2, the autocorelation decreases 
## but it pregresses to zero after it.
## also it helps us identify that 2 post period have to included in auto regression model
## There seems presence of both AR(1) and MA(2) model and even presence of some error.

######################################
## CHECKING IF MODEL IS STATIONARY ON NON STATIONARY
######################################
## Only Stationary model can be forecasted in the ARIMA model.
## so if our model isn't stationary we have to stationarize it

## Augmented dickey fuller test can identify whether model is stationary or not
library(tseries)
adf.test(beer_time_series)
## P value in this case is much higher than our considered level of significance which is 0.05
## H0: time series is non stationary 
## H1: time series is stationary 
## The p-value is greater than 0.05. We cannot reject the null hypothesis. 

###############################
## DECTECTING variance of stability and seasonality
###############################
var_beer <- diff(beer_time_series,lag=4)
# checking stationarity of the data
adf.test(var_beer)
BoxCox.lambda(var_beer)
## The "optimal value" in this case,
## which gives best approximation of a normal distribution curve is 1.154986

## checking acf and pacf again on var_beer
acf(var_beer)
pacf(var_beer)
## from these corelation plot it can be seen that corelation of time series with 
## previous copies of themselves is within purple line. 
## indicating independency of series on a series with lag 2

###########################################
## analysing trend and seasonality 
## considering moving average for constructing trend in beer sales
beer_trend <- ma(beer_time_series,order = 4)
beer_trend 
plot(beer_time_series, main = "Quarterly beer sales over 18 years", col = "purple")
lines(beer_trend ,col = "green")

## in order to stabalise the time series, remove trend from the series
## to remove trend from the original time series it must be subtracted
beer_wo_trend <- beer_time_series - beer_trend
beer_wo_trend
plot(beer_wo_trend, col = "purple", main = "Beer time series without trend")

## now creating matrix for beer sales without trend
beer_matrix <- t(matrix(data = beer_wo_trend, nrow = 4))
beer_matrix                 

## seasonality in beer consumption
seasonality_in_beer= colMeans(beer_matrix, na.rm = TRUE)
seasonality_in_beer
## plotting seasonality
plot.ts(rep(seasonality_in_beer,18))

## in this case from the seasonality plot, 
## it can be said that seasonality is constant over time
## thus it indicates presence of additive seasonality

## now calculating the white noise in beer time series
error_rate <- beer_wo_trend - seasonality_in_beer
plot(error_rate, col = "red", main = "error rate in Beer Time Series")

## now using the calculated trend, seasonality and error rate form a new time series
new_beer <- beer_trend + seasonality_in_beer + error_rate
plot(new_beer, col = "purple", main = "New beer TIme Series")

###########################################
## Decomposing beer time series
## It is not possible to estimate the movement of the trend.
## because trend movement is impacted by the seasonality.
beer_Dec <- stl(beer_time_series,s.window = 'p')
## since seasonality is consistant in our case we will put random valuesin s.windows 
beer_Dec
plot(beer_Dec)
## from plot it can be seen that,  scale for trend is bigger,
## indicating volitility in the trend as compared to constant seasonality
## with time trend goes on incresing
## error rates can be calculated in recent years, 
## because as projecting in future the error decreases with older time series data

########################################
## Holt Winters Method
########################################

autoplot(var_beer)
## plot exhibits both trend and seasonality with upward movement,
## also the model variation in the model is cosistent YOY
## there is cosistancy in beer sales 

## since both seasonality and trend are present, holt-winter model can be applied here
holt_winter <- hw(beer_time_series,seasonal = "additive")
summary(holt_winter)
## from summary it can be seen that slope values are quite lower
## therefore, change in the slope will be gradual

############################
## Predicting for next two year
library(forecast)
forecast(holt_winter,h = 8)
## since forecast is for next 2 years, concidering 8 quarters
plot(forecast(holt_winter,h = 8))
## it can be ssen that, beer sales will be slighly lower in the next two years compared 
## to the previous years sales of around 550 thousand dollers
## dark blue coloured shaded area indicate 80% confidence interval
## light blue coloured shaded area indicate 95% confidence interval

checkresiduals(holt_winter)
## pvalue greater than significance level indicate that,
## there will be no effect of past and present data on the future forecast

###########################################
## ARIMA MODEL
###########################################

## beer sale data doesn't requires differenciating of order, hence d = 0
## beer data has AR(1) model indicating p = 1
## it contain MA terms, hence q = 2
## in notation, ARIMA(1,0,2) 

arima_model = arima((beer_time_series), c(1,0, 2),seasonal = list(order = c(0, 1, 2), period = 4, include.constant=FALSE))
arima_model

## AIC for the model
AIC(arima_model)

## BIC for the model
BIC(arima_model)

## lesser the value of AIC, more prefearable is the ARIMA model. 
##therefore, as our model is having least value of AIC
## ARIMA(1,0,2)x(0,1,2)[4] is highly preferrable.

###################################
## Automated ARIMA model
## auto.arima model itself takes appropriate value of (p,d,q)
## also it helps in identifying the model with low value of AIC
ar_model <- auto.arima(beer_time_series,stepwise = FALSE)
ar_model
forecast(ar_model,h=8)
plot(forecast(ar_model,h=8))

## checking residuals
checkresiduals(ar_model)
## pvalue greater than significance level indicate that,
## there will be no effect of past and present data on the future forecast

##################################
## setting up forecast function for winter holt and ARIMA model to find best fit model.
wh <- function(x,h)
{
  forecast(ets(x),h=h)
}

ar <- function(x,h)
{
  forecast(auto.arima(x),h=h)
}

## Calculate Cross Validation errors for Winter Holt's (l1)  and ARIMA (l2)
l1 <- tsCV(beer_time_series,wh,h=1)
l2 <- tsCV(beer_time_series,ar,h=1)

## Mean Square Error of each model

## for winter holt's model
mean(l1^2, na.rm=TRUE)
## for ARIMA model 
mean(l2^2, na.rm=TRUE)
## from the above values, the model with minimum mean square error is considered to be "Optimal Model"
## since ARIMA model gives minimum Mean Square Error value between the two model.
## ARIMA model is selected
