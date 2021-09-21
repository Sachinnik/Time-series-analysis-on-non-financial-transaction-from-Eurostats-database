install.packages("xlsx")
install.packages("readxl")
library("readxl")

setwd("F:/National College of Ireland/Statistics for Data Analytics/Time Series")
getwd()

#Reading Excel data
Non_Financial_Transactions <- read_excel('F:/National College of Ireland/Statistics for Data Analytics/Time Series/Poland.xlsx')
summary(Non_Financial_Transactions)
install.packages("fpp2")
library("fpp2")

#Storing feature value in NFT
NFT <- c(Non_Financial_Transactions$Values)
TFA <- ts(NFT, start= c(1999,1), frequency = 4)  # Setting values according to Quarters using ts object
TFA

plot(TFA)
start(TFA)
end(TFA)
frequency(TFA)
install.packages("ggplot")
library("forecast")
ggseasonplot(TFA, year.labels = TRUE, year.labels.left = TRUE) + ylab(" ??? millions") + ggtitle("Non- Financial Transactions")
ggsubseriesplot(TFA) + ylab("??? millions") + ggtitle("Seasonal Subseries plot : Non- Financial Transactions")

#check the stationary Augmented Dickey-Fuller Test
library(tseries)
adf.test(TFA)


#KPSS TEST..KPS LEVEL STATIONARITY 
kpss.test(TFA, null = "Trend")


#Subsetting the series with the window function > 0.05



TFA.subset <- window(TFA, start=c(1999, 5), end=c(2019, 4))
TFA.subset
autoplot(TFA.subset)



#checking the mean and variance
abline(reg=lm(TFA~time(TFA)))








#show year in year trend
plot(aggregate(TFA, FUN= mean))





#check the dataset class
class(TFA)



#check the cycle
cycle(TFA)
#box plot to give seasonal trends
boxplot(TFA~cycle(TFA))





#transforming the data
#log function to equa the variance
plot(log(TFA))





#smoothing the TA by moving average
plot(ma(TFA, 5))





#using the autolayer, try to smooth
autoplot(TFA)+
  autolayer(ma(TFA,5))+
  autolayer(ma(TFA,3))+
  autolayer(ma(TFA,7))



#plotting the sesonal trends
monthplot(TFA)
seasonplot(TFA)




#Seasonal decomposition
fit.decomadd<-decompose(TFA, type = "additive")
fit.decomadd
plot(fit.decomadd)
summary(fit.decomadd)




#Seasonal decomposition
fit.decommult<-decompose(TFA,type = "multiplicative")
fit.decommult
fit.decomadd$TFA
plot(fit.decommult)




#auto correlation and parcial correlation
ggtsdisplay(TFA)



#AutoARIMA
fit <- auto.arima ((TFA),ic = "aic",trace = TRUE,stepwise = FALSE)
Fit_3 <- arima(TFA , c(2,0,0), seasonal = c(0,1,1))

my_predict <- predict(Fit_3,n.ahead = 12)
my_predict
forecast(Fit_3, 12)
plot(forecast(Fit_3, 12), xlab="Year", ylab="Non- Financial Transactions")

qqnorm(Fit_3$residuals)
qqline(Fit_3$residuals)


Box.test(Fit_3$residuals,type = "Ljung-Box")
checkresiduals(Fit_3)
accuracy(Fit_3)
Fit_3







fit1 <- arima ((TFA),order= c(2,0,0))
fit1



fit2 <- arima ((TFA),order= c(0,1,1))
fit2                  


install.packages("lmtest")
library("lmtest")
model_1 <- arima(TFA, order=c(2,0,0),seasonal = list(order = c(2,0,0), period = 4),method="ML")
coeftest(model_1)





coeftest(fit3)





future_val <- forecast(model_1, h = 3*12, level = c(95.0))
plot(future_val)



#Forecasting with the fitted model



forecast(Fit_3, 8)
plot(forecast(fit1, 8), xlab="Year", ylab="quaterly applications")





qqnorm(model_1$residuals)
qqline(model_1$residuals)




Box.test(model_1$residuals,type = "Ljung-Box")
checkresiduals(model_1)
accuracy(model_1)





model_2 <- arima(TFA, order=c(0,1,1),seasonal = list(order = c(0,1,1), period = 4),method="ML")
coeftest(model_2)





my_predict <- predict(model_2,n.ahead = 6)
my_predict





future_val <- forecast(model_2, h = 3*12, level = c(95.0))
plot(future_val)





qqnorm(model_2$residuals)
qqline(model_2$residuals)




Box.test(model_2$residuals,type = "Ljung-Box")
checkresiduals(model_2)
accuracy(model_2)



# ETS TEST - Exponential  
ets(TFA, model="ZZZ", damped=NULL, alpha=NULL, beta=NULL,
    gamma=NULL, phi=NULL, lambda=NULL, biasadj=FALSE,
    additive.only=FALSE, restrict=TRUE,
    allow.multiplicative.trend=FALSE)

hw(TFA, model="ZZZ", damped=NULL, alpha=NULL, beta=NULL,
    gamma=NULL, phi=NULL, lambda=NULL, biasadj=FALSE,
    additive.only=FALSE, restrict=TRUE,
    allow.multiplicative.trend=FALSE)



fit1 <- ets(TFA)
summary(fit1)
  autoplot(fit1)
  

  ############# Holts winter model
  fit1HW <- hw(TFA, seasonal = "additive", h = 12)
  summary(fit1HW)
  
  fit.hw <- hw(TFA, seasonal = "multiplicative", h = 12)





# residual checking in ETS 
cbind('Residuals' = residuals(fit1),
      'Forecast errors' = residuals(fit1,type='response')) %>%
  autoplot(facet=TRUE) + xlab("Year") + ylab("")





#forecasting the timeseries in ETS
fit1 %>% forecast(h=12) %>%
  autoplot() +
  ylab("Non- Financial Transaaction")


forecast(fit1,12)






# package install forecast
install.packages("forecast")
library(forecast)





# checking the data type for smoothining
hslm <- window(TFA,start=1999)
fit3 <- hw(TFA,seasonal="additive", h=12)
fit4 <- hw(TFA,seasonal="multiplicative", h=12)
summary(fit3)
autoplot(TFA) + autolayer(fit3, series = "Additive", PI = FALSE) + autolayer(fit4, series = "Multiplicative", PI=FALSE) + xlab("Year") +
  ylab("Non-Financial Transactions") + ggtitle("Transaction Estimates") + guides(colour=guide_legend(title = "Forecast"))




hww <- autoplot(TFA) +
  autolayer(fit3, series="HW additive forecasts", PI=FALSE) +
  autolayer(fit4, series="HW multiplicative forecasts",
            PI=FALSE) +
  xlab("Year") +
  ylab("Non- Financial Transactions") +
  ggtitle("Transactions Estimates") +
  guides(colour=guide_legend(title="Forecast"))

summary(hww)

# smoothing the data and checking coefficients
slmforecast <- HoltWinters(hslm, gamma=FALSE)
slmforecast






# holts winters filtering 
slmforecast$SSE
plot(slmforecast)





# time series forecast with confidence interval 095 and time 4 intervals
forecast <- predict(slmforecast, n.ahead = 4, prediction.interval = T, level = 0.95)
plot(slmforecast, forecast)




# ACF AND PCAF plots to check residual
# for additive
acf(fit3$residuals, lag.max=20)
Box.test(fit3, lag=20, type="Ljung-Box")
pacf(fit3$residuals, lag.max=20)
#for multiplicative
acf(fit4$residuals, lag.max=20)
Box.test(fit4, lag=20, type="Ljung-Box")
pacf(fit4$residuals, lag.max=20)


