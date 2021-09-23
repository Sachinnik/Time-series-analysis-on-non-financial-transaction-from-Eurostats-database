Masters of Science (Data Analytics) - Semester 1 project 
Subject : Statistics for data Analysis 
# Time-series-analysis-on-non-financial-transaction-from-Eurostats-database
Time series analysis and study is done on non-financial transaction dataset from europa website

The dataset contains the non-financial transactions of the accounts given quarterly from 1999 to 2019. The non-financial transaction includes production, use of income, income distribution, and balance of primary income.


ANALYSIS: At first the dataset is loaded in to R. The time series 
object is created with ts() function and the graph is 
plot to check the trend and seasonality

Different tests on data :
Augumented Dickey-Fuller Test
Kwiatkowski-Phillips-Schmidt-Shin (KSS)


ETS: ETS comes under Exponential Smoothening models.ETS algorithm is called by the ets() function from the package “forecast”.

Holt Winters: Holt winters is other ETS model [3] which is used for forecasting. This model is executed by using the hw() function.

ARIMA: ARIMA is Auto Regressive Integrated Moving Average model. ARIMA is considered as best forecasting model.

 Box – Ljung Test: After applying the ARIMA, box test is applied to check the auto- correlations between the residuals.
