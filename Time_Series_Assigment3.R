
#source(here("src",'helper_functions.R'))

# LOAD YOUR PACKAGES
library(ggplot2)
library(forecast)
library(plotly)
library(ggfortify)
library(tseries)
library(gridExtra)
library(docstring)
library(readr)
library(here)
here() # Should output current work directory

data_master <- read.csv(here("data", "data_master_1.csv"))
attach(data_master)

# EXPLORE DATA

str(data_master)

#sel_vars <- c("year", "oilPrices", "cpi", "nasdaq", "nyse", "sp_500", "gdp_us", "housingIndex")
#data <- data_master[sel_vars]

data_1 <- data_master[c(1,9,12,17:20,22)]
#str(data_1)

# Check for missing values
data_1[!complete.cases(data_1),]

# Compute Summary Statistics
summary(data_1[2:8])

# Visualize and correlations and relationships between sp_500 with selected economic indicators

# Scatterplot Matrices from the gclus Package
library(gclus)

dta <- data_1[c(2,3,6,7,8)] # get data
dta.r <- abs(cor(dta)) # get correlations
dta.r
dta.col <- dmat.color(dta.r) # get colors
# reorder variables so those with highest correlation
# are closest to the diagonal
dta.o <- order.single(dta.r)
cpairs(dta, dta.o, panel.colors=dta.col, gap=.5,
       main="SP_500 & Economic Variables Ordered and Colored by Correlation" ) 

# From the correlation numbers and plots, it is clear and reasonably expected that the 
# economic indicators are strongly and highly correlated among themselves than with the sp_500 

# Visualize and correlations and relationships among major US stock indices

dta_2 <- data_1[c(4,5,6)] # get data
dta_2.r <- abs(cor(dta_2)) # get correlations
dta_2.r
dta_2.col <- dmat.color(dta_2.r) # get colors
# reorder variables so those with highest correlation
# are closest to the diagonal
dta_2.o <- order.single(dta_2.r)
cpairs(dta_2, dta_2.o, panel.colors=dta_2.col, gap=.5,
       main="Major Indices Ordered and Colored by Correlation" )

# The major US stock indices are highly correlated, ranging from 0.84 between 
# nasdaq and nyse to 0.95 between nasdaq and sp_500

##################################################################

#Load HANG SENG INDEX data and explore relationship with sp_500

#################################################################
data_hseng <- read.csv(here("data", "hang_seng_index.csv"))
str(data_hseng)

data_hseng <- data_hseng[1:nrow(data_master), c(2:4)]

library(dplyr)

data_sp500hseng <- cbind(data_hseng$Hang_Seng, data_master$sp_500)
colnames(data_sp500hseng) <- list("hang_seng", "sp_500")
head(data_sp500hseng) 
colnames(data_sp500hseng)

library(car)
scatterplot(hang_seng ~ sp_500, data=data_sp500hseng)

hseng_sp500.corr <- cor(data_sp500hseng) # get correlations
hseng_sp500.corr 

# The correlation between sp_500 and hang_seng is 0.72 and much less than 
# the minimum correlation among the US stock indices. 


# For this exercise: 
# 1) We will perform a detailed univariate time series analysis of sp_500 and 
# 2) Then attempt a multivariate time series analysis of sp_500 and the economic indicators (oilPrices,
# cpi, gdp_us and housingIndex)
# 3) Lastly, we will peeform clustering on all four indices - sp_500, nasdaq, nyse and hang_seng

# In the future, we will apply the analysis using the underlying stocks or ETFs that make up these indices

#############################################################
# (1) START WITH UNIVARIATE TIME SERIES ANALYSIS
#############################################################

sp_500 <- ts(data$sp_500, start=c(1995, 1), freq=12)

#Here we use our own function called plot_time_series, which does as its name suggests:

# Run helper_functions.R once, to allow you to run most of the functions in this code
plot_time_series(sp_500, 'S&P 500')


#Before we begin any analysis, we will be splitting the data to remove 2015 to use as our test set.

sp500_training <- ts(sp_500, start=c(1995, 1), end=c(2014, 12), freq=12)

#Plotting our Time Series

#Plotting the data is arguably the most critical step in the exploratory analysis phase (We chose to emphasize on the time series object that has intervals from 1995 to 2014, which we will explain later!). This enables us to make inferences about important components of the time-series data, such as trend, seasonality, heteroskedasticity, and stationarity. Here is a quick summary of each:

#Trend: we say that a dataset has a trend when it has either a long-term increase or decrease.
#Seasonality: we say that a dataset has seasonality when it has patterns that repeat over known, fixed periods of time (e.g. monthly, quarterly, yearly).
#Heteroskedasticity: we say that a data is heteroskedastic when its variability is not constant (i.e. its variance increases or decreases as a function of the explanatory variable).
#Stationarity: a stochastic process is called stationary if the mean and variance are constant (i.e. their joint distribution does not change over time).

#we start our analysis by plotting our time series object to give us a visual basis to start our modeling.

plot_time_series(sp500_training, 'S&P 500')


#Testing for Stationarity

#We will utilize a few statistical tests to test for stationarity. We must be weary of our model having a unit root, this will lead to non-stationary processes.

#1. Box-Ljung test... H0 = stationary, and Ha = non-stationary 
Box.test(sp_500, lag = 20, type = 'Ljung-Box')

#Now we will utilize the Augmented Dickey-Fuller (ADF) Test for stationarity. 
#The null hypothesis states that large p-values indicate non-stationarity and smaller p values indicate stationarity 
#i.e. H0 =  non-stationary, and Ha = Stationarity..We will be using 0.05 as our alpha value.

adf.test(sp_500)
adf.test(sp500_training)

#We can see our p-value for the ADF test is relatively high, so we'll do some further visual inspection. But we know we will most likely have to difference our time series for stationarity.

#Decomposing our time-series

#Beyond understanding the trend of our time-series, we want to further understand the anatomy of our data. For this reason we break-down our time-series into its seasonal component, trend, and residuals.

plot_decomp(sp500_training, 'S&P 500')

#The trend line already shows us what we know and we can see that there might be some seasonality in our time series object.


#Model Estimation
#Diagnosing the ACF and PACF Plots of our Time-Series Object

#ACF stands for "autocorrelation function" and PACF stands for "partial autocorrelation function". The ACF and PACF diagnosis is employed over a time-series to determine the order for which we are going to create our model using ARIMA modeling. Loosely speaking, a time-series is stationary when its mean, variance, and autocorrelation remain constant over time.

#These functions help us understand the correlation component of different data points at different time lags. Lag refers to the time difference between one observation and a previous observation in a dataset. Let's examine our plots!

# DIAGNOSING ACF AND PACF PLOTS
plot_acf_pacf(sp500_training, 'S&P 500')


#Transforming our data to adjust for non-stationary

#From visual inspection of the time series object and the previously mentioned statistical tests used for exploratory analysis, we decided it is appropriate to difference our time series object to account for the non-stationarity and see how that fares!
  
#A way to make a time-series stationary is to find the difference across its consecutive values. This helps stabilize the mean, thereby making the time-series object stationary.

#For this we use the diff() method.

tsDiff <- diff(sp500_training)

#Next we plot our transformed time-series:
  
plot_time_series(tsDiff, 'First Difference')

#This plot suggests that our working data is stationary. We want to confirm this running the same tests, and looking at the ACF and PACF diagnostics over the differenced data to find our if we can proceed to estimating a model.

#Testing for Stationarity

#We apply the same tests to our differenced time series object.

Box.test(tsDiff, lag = 20, type = 'Ljung-Box')

#Now let's use the ADF Test

adf.test(tsDiff)

#we can see that the result yields a small p-value which makes us reject the null suggestion stationarity.

#Diagnosing the acf and pacf of our transformed time-series object

#The plot below helps us confirm that we have stationarity and also helps us deduce which model we will use. It is important to keep in mind that we have a difference parameter equal to one (i.e. d = 1) because of the previous transformation we carried out.

plot_acf_pacf(tsDiff, 'First Difference Time Series Object')

#From the above plots we deduce that an MA(1) model (where MA stands for moving average) best fits our data because the ACF cuts off at one significant lag and the PACF shows geometric decay.
#Recall that we are examining the differenced time-series so we have to use the combined model ARIMA (Autoregressive integrated moving average), thus our model so far is ARIMA(0, 1, 1).

#Build Model

#Our findings in the exploratory analysis phase suggest that model ARIMA(0, 1, 1) might be best fit. Fortunately, there is a function in R that we can use to test our findings.

#The auto.arima() method, found within the forecast package, yields the best model for a time-series based on Akaike-Information-Criterion (AIC). The AIC is a measurement of quality used across various models to find the best fit. After running our original and differenced data sets through the auto.arima() method we confirmed that the ARIMA(0, 1, 1) is our best fit model.

#We use the Arima() method to fit our model and include our training data set sp500_training as the first argument.

fit <- Arima(sp500_training, order = c(0,1,1),
             include.drift = TRUE)
summary(fit)

#Our next step is to run a residual diagnostics to ensure our residuals are white noise under our initial assumptions. For this we use the ggtsdiplay() method.

# RESIDUAL DIAGNOSTICS
ggtsdiag_custom(fit, sp500_training) +
  theme(panel.background = element_rect(fill = "gray98"),
        panel.grid.minor = element_blank(),
        axis.line.y = element_line(colour="gray"),
        axis.line.x = element_line(colour="gray"))

residFit <- ggplot(data=fit, aes(residuals(fit))) +
  geom_histogram(aes(y =..density..),  
                 binwidth = 5,
                 col="turquoise4", fill="white") +
  geom_density(col=1) +
  theme(panel.background = element_rect(fill = "gray98"),
        panel.grid.minor = element_blank(),
        axis.line   = element_line(colour="gray"),
        axis.line.x = element_line(colour="gray")) +
  ggtitle("Plot of SP 500 ARIMA Model Residuals")

residFit

#Based on our diagnostic plot, the residuals to seem to display a normal distribution.
#This model appears to confirm all of our assumptions, which means we can continue to the forecasting phase!


#Forecasting

#We proceed to forecasting now that we believe we found the appropriate model!
  
#We utilized the autoplot() function quite heavily on this iteration of our project, since we couldn't find a way of adding the actual values to the plot we used a workaround by borrowing Drew Schmidt's work around to including the actual 2015 values.
#The code can be found in the github repository, so you should run the autoplot.forecast() in order to get the plots we have here. For now we create a time series object that will include the actual 2015 values, where in our function it will be called up by the parameter holdout.
#Next we use the forecast function, ggplot2 and plotly to visualize the predictions for the year 2015! Here within the plots the forecasted values are BLUE, the actual 2015 values are in RED, the 80% Confidence Intervals are encompassed in the YELLOW bands and 95% Confidence Intervals are encompassed in the ORANGE bands respectively.

#Let's create the test set:
sp500_test <- window(sp_500, 2015, c(2015, 12))

#Forcast using the model fitted with Arima earlier
fit_arima <- forecast(fit, h = 12)

#Next we create the autoplot visualization.
autoplot(fit_arima,
         holdout = sp500_test, 
         forc_name = 'ARIMA', 
         ts_object_name = 'S&P 500')

#We can see that the model performs well and within the 80% and 95% confidence intervals. 
#You can forecast values even further into the future by tuning the appropriate parameters. 
#Please note that this forecast project is for educational purposes and we do not recommend investing by using 
#these predictions - remember that the stock market is very volatile.


#Other Forecasting Methods

#In this more interactive iteration of our project, we included other forecasting methods to show 
#the versatility of forecasting methods and to use as comparisons!

#Exponential Smoothing Forecast

#The following forecasting method is far more complex than the previous methods. 
#This forecasting method relies on weighted averages of past observations where the most recent observations hold higher weight! 
#Fortunately for us if we use the ets function, it outputs the method that best fits (much like the auto.arima() function)

#When outputting the summary for the ets model, we receive that our model is ETS(A, Ad, N) which reading more of Hyndman's blog we 
#see that it is equivalent to an ARIMA(1, 1, 2)... interesting to know.

fit_ets <- forecast(ets(sp500_training), h = 12)
autoplot(fit_ets, 
    holdout=sp500_test,
    forc_name = 'Exponential Smoothing',
    ts_object_name = 'S&P 500')

#Interesting that Exponential Smoothing's prediction is still within both prediction intervals, 
#although the bands are noticeably larger than our ARIMA, it will be interesting to see more future predictions for this promising model. 

#Naive Forecast

#The naive forecasting method returns an ARIMA(0, 1, 0) with random walk model that is applied to our time series object. 
#Important to note that Hyndman described this forecasting method as being effective in financial time series objects, and that 
#the forecasting method "... all [...] values are set to be $$y_T$$, where $$y_T$$ is the last observed value"

fit_naive <- naive(sp500_training, h = 12)
autoplot(fit_naive, 
         holdout = sp500_test,
         forc_name = 'Naive Forecast',
         ts_object_name = 'S&P 500') 


#Seasonal Naive Forecast

#For the snaive() method it follows the same principles as the naive method, but works better for very seasonal data!
  
fit_snaive <- snaive(sp500_training, h = 12)
autoplot(fit_snaive, 
         holdout = sp500_test,
         forc_name = 'Seasonal Naive',
         ts_object_name = 'S&P 500')


##The forecasting method we use to find the best model is recieving the lowest MAE and MAPE as described by Rob J. Hyndman
##We run the accuracy function on all the forecast methods and we check which performed best!
  
round(accuracy(fit_arima, sp500_test), 2) # accuracy fxn uses default values in this case...d = 1 and D = 0 for non-seasonal series
round(accuracy(fit_ets, sp500_test), 2)
round(accuracy(fit_naive, sp500_test), 2)
round(accuracy(fit_snaive, sp500_test), 2)

#As we can see from our metrics relating to the 1-year test set, the ARIMA modeled performed better with Exponential Smoothing peforming well. 
#Through the forecast plots however we saw that Exponential Smoothing is still within the prediction intervals, so its a close call.
#We conclude that the Naive models performs best given that it's still inside the 95% prediction intervals and the accuracy metrics performed better than all other models. 

#############################################################################
# (2) Multivariate Time Series Analysis of Sp_500 and the Economic Indicators
#############################################################################

head(dta)
ts_vars_1 <- ts(dta, start=c(1995, 1), freq=12)

# Use Johansen Test to to test for stationarity
# We use ca.jo function from urca library

library(urca)

jotest=ca.jo(data.frame(ts_vars_1), type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest)

# Interprete Johansen Test
# The first section shows the eigenvalues generated by the test. 
# In this instance we have five with the largest approximately equal to 0.1378
# For a series to be stationary, the eigenvalues should be less than 1 in modulus.

# The next section shows the trace test statistic for the four hypotheses of r???4, r???3,r???2, r???1, and r=0
# For each of these three tests we have not only the statistic itself (given under the test column) 
# but also the critical values at certain levels of confidence: 10%, 5% and 1% respectively.

# Each hypothesis tests for the absence (H0) (and hence presence, Ha) of cointegration. 
# Each test statistic is less than the 1% level value, signifying that we have some evidence to reject the null hypothesis of no cointegration. 
# However, the order of the evidence is strongest at r???4, and so can conclude that the rank of the matrix r is at least 4.
# Therefore, we need a linear combination of at least 4 time series to form a stationary series 
# We will keep all 5 times series in this case

# We could use the eigenvector components of the eigenvector associated with the largest eigenvalue.
# We previously mentioned that the largest eigenvalue is approximately 0.1378. It corresponds to the vector given under the 
# column oilPrices.l2, and is approximately equal to (1.000000,7.6048577,0.5332223, -11.3153784, 3.3195011). 
# If we form a linear combination of series using these components, we will receive a stationary series:

# Use VARselect() to determine an optimal lag length according to an information criteria or the 
# final prediction error of an empirical VAR(p)-process.

VARselect(ts_vars_1, lag.max = 5, type = "const")

# The results show that both AIC and FPE recommend 5 while both HQ and SC recommend 4 

# Train-Test Split
#Just as we did in the Univariate TS Analysis, we split the data and keep the last year (12 months) for testing

ts_1_train <- ts(dta, start=c(1995, 1), end=c(2014, 12), freq=12)
ts_1_test <- window(ts_vars_1, 2015, c(2015, 12))

# Fit Model
# VAR(4) is estimated with the function VAR() and as deterministic regressors a constant is included.
var.4_train <- VAR(ts_1_train, p = 4, type = "const")
names(var.4_train)
summary(var.4_train)
plot(var.4_train)

# Compare with VAR(2)
var.2_train <- VAR(ts_1_train, p = 2, type = "const")
names(var.2_train)
summary(var.2_train)
plot(var.2_train)

# forecasting 12 months ahead
var.f12 <- predict(var.4_train, n.ahead = 12, ci = 0.95)
names(var.f12)
plot(var.f12)
fanchart(var.f12)

###############################################################

#3 PERFORM CLUSTERING ON FOUR INDICES (SP_500, NYSE, NASDAQ and HANG_SENG)

###############################################################

# Time Series Clustering

###############################################################

#Time series clustering is to partition time series data into groups based on similarity 
#or distance, so that time series in the same cluster are similar. For time series clustering
#with R, the first step is to work out an appropriate distance/similarity metric, and then, at
#the second step, use existing clustering techniques, such as k-means, hierarchical clustering, 
#density-based clustering or subspace clustering, to find clustering structures.

# Measures of distance/dissimilarity
# Euclidean distance
# Manhattan distance
# Maximum norm
# Hamming distance
# The angle between two vectors (inner product)
# Dynamic Time Warping (DTW) distance

# Get all four indices in a data file 
head(dta_2) # nasdaq, nyse and sp_500

#add hang seng
data_stockindices <- dta_2 %>% 
  cbind(data_hseng$Hang_Seng)
  
colnames(data_stockindices)[4] <- c("hseng")
str(data_stockindices)
head(data_stockindices)

plot.ts(data_stockindices, main = "Plot of Four (4) Stock Indices")

# Hierarchical Clustering with Dynamic Time Warping (DTW) Distance

# Compute DTW Distances
# DTW finds optimal alignment between two time series, and DTW distance
# is used as a distance metric.

library(dtw)
distMatrix_dtw <- dist(t(data_stockindices), method="DTW")
distMatrix_dtw

observedLabels <- c("nasdaq", "nyse", "sp500", "hseng")

hc <- hclust(distMatrix_dtw, method="average")
#plot(hc, labels = observedLabels, main = "")
plot(hc,main="")
# since we have only 4 maximum clusters, get 3 clusters
memb <- cutree(hc, k = 3)
#str(memb)
table(observedLabels, memb)

# Hierarchical clustering with Euclidean distance

distMatrix_euc <- dist(t(data_stockindices)) # method="Euclidean" ommitted because it is the default, but you can add it
distMatrix_euc
hc <- hclust(distMatrix_euc, method = "ave")
#plot(hc, labels = observedLabels, main = "")
plot(hc, main = "")
# Since we have only 4 maximum clusters, get all 3 clusters
memb_euc <- cutree(hc, k = 3)
table(observedLabels, memb_euc)

# From the dendogram plots and tables, both DTW and EUCLIDIAN give the same clustering results

# K-Means Clustering
ds <- t(data_stockindices)
set.seed(123)
km_clus_3 <- kmeans(ds, centers=3)
sort(km_clus_3$cluster)

# Results from K-Means Clustering also align with those in the two types of Hierarchical clustering 


#Sources Cited

# 1) Hyndman, Rob J., and George Athanasopoulos. "Forecasting: Principles and Practice" Otexts. N.p., May 2012. Web.

# 2) Schmidt, Drew. Autoplot: Graphical Methods with ggplot2 Wrathematics, my stack runneth over. June, 2012. Web.
















