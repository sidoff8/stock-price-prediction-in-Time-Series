library(quantmod)
library(dplyr)
library(PerformanceAnalytics)

tickers <- c("AAPL", "AMZN" , "MSFT" ,"FB")

#Get Prices (can be monthly or weekly)
portfolioPrices <- NULL
for (Ticker in tickers)
  portfolioPrices <- cbind(portfolioPrices,
                           getSymbols.yahoo(Ticker, from="2017-01-01", periodicity = "weekly", auto.assign=FALSE)[,4])

#Delete all dates with no prices
portfolioPrices <- portfolioPrices[apply(portfolioPrices,1,function(x) all(!is.na(x))),]
#Rename Columns
colnames(portfolioPrices) <- tickers

plot(portfolioPrices)
addLegend("topleft", on=0, 
          legend.names = tickers, 
          lty=c(1, 1,1,1), lwd=c(1, 1,1,1)
)


#pridiction for Facebook stock using ARIMA
library(quantmod)
library(dplyr)
library(forecast)
getSymbols("AAPL",src="yahoo", from="2009-01-01", periodicity = "monthly"  )

x<-data.frame(AAPL)
x$date<-rownames(x)
rownames(x)<-NULL

close<-dplyr::select(x, date, AAPL.Close)
write.table(close, file = "AAPLclose.csv", row.names=F, sep = ",")


mydata <- read.csv("AAPLclose.csv",TRUE, ",")
attach(mydata)


library(MASS)
library(tseries)
library(forecast)
#plot and covert to log format
lnstock=log(AAPL.Close[1:96])
lnstock
#returns are in form of percentages

#ACF, partial autocorrelation function (PACF) and Dickey-fuller Test 
acf(lnstock, lag.max=20)
pacf(lnstock, lag.max=20)
difflnstock=diff(lnstock,1)
difflnstock
adf.test(lnstock)
adf.test(difflnstock)


#time series and autoarima
pricearima <- ts(lnstock, start = c(2009,01), frequency =12)
fitlnstock<- auto.arima(pricearima)
fitlnstock
plot(pricearima,type = 'l')
title('AAPL Close')
exp(lnstock)


#Forecasting value from Arima
forecastedvalues_ln=forecast(fitlnstock,27)
forecastedvalues_ln
plot(forecastedvalues_ln)


forecastedvaluesextracted=as.numeric(forecastedvalues_ln$mean)
finalforecastvalues=exp(forecastedvaluesextracted)
finalforecastvalues


#Percentage Error
df<-data.frame(AAPL.Close[96:122],finalforecastvalues)
col_headings<- c("Actual Price","Forecasted Price")
names(df)<-col_headings
attach(df)
percentage_error=((df$`Actual Price` -df$`Forecasted Price`)/(df$`Actual Price`))
percentage_error
mean(percentage_error)

#Ljung Box
Box.test(fitlnstock$residuals, lag = 5, type = "Ljung-Box")
Box.test(fitlnstock$residuals, lag = 10, type = "Ljung-Box")
Box.test(fitlnstock$residuals, lag = 15, type = "Ljung-Box")



fit <- auto.arima(FB$FB.Close, ic="bic")
fit
plot(as.ts(FB$FB.Close) )
lines(fitted(fit), col="red")
fit.forecast<-forecast(fit)
fit.forecast
plot(fit.forecast)

#pridiction for Microsoft croporation stock using ARIMA
library(quantmod)
library(forecast)
getSymbols("MSFT",src="yahoo", from="2016-01-01")
plot(MSFT)
fit <- auto.arima(MSFT$MSFT.Close, ic="bic")
fit
plot(as.ts(MSFT$MSFT.Close) )
lines(fitted(fit), col="red")
fit.forecast<-forecast(fit)
fit.forecast
plot(fit.forecast)

scaledData <- scale(portfolioPrices)
plot(scaledData)
addLegend("topleft", on=0, 
          legend.names = tickers, 
          lty=c(1, 1,1,1), lwd=c(1, 1,1,1)
)