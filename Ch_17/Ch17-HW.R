# Clear the environment
rm(list = ls())

# Set the working directory
setwd("~/Documents/5504 Spring 2024/Ch_17")

# Q1 (17-7) ----

# part a (Manual Calculations)

# build data frame
Q1 <- data.frame(Week = c(1:12),
                  Sales = c(17, 21, 19, 23, 18, 16, 20, 18, 22, 20, 15, 22))

# Technical Trading Rules Package
library(TTR)

# Moving Average 4 weeks, Round to 2 decimals
round(
  SMA(Q1$Sales, n = 4)
  , digits = 2)
# Last entry is the forecast for period 13. 

# We need entries 4:11 to be inserted in rows 5:12 of a new column: SMA_4
Q1$SMA_4[5:12] <- round(
                        SMA(Q1$Sales, n = 4),
                        digits = 2)[4:11]

# Squared Errors
Q1$ErrorSq_4[5:12] <- round(
                      (Q1$Sales[5:12]-Q1$SMA_4[5:12])^2,
                      digits = 2)

# Get the sum of all columns (if needed)
colSums(Q1) # We get NA since we have missing values

# na.rm = TRUE removes any missing values 
colSums(Q1, na.rm = TRUE) 

# Alt method for Total Error Sq
sum(Q1$ErrorSq_4, na.rm = TRUE)

# MSE 4 weeks
mean(Q1$ErrorSq_4, na.rm = TRUE) # part b

# Moving Average 5 weeks, Round to 2 decimals
round(
  SMA(Q1$Sales, n = 5)
  , digits = 2)
# Last entry is the forecast for period 13. 

# We need entries 5:11 to be inserted in rows 6:12 of a new column: SMA_5
Q1$SMA_5[6:12] <- round(
                        SMA(Q1$Sales, n = 5),
                        digits = 2)[5:11]

# Squared Errors
Q1$ErrorSq_5[6:12] <- round(
  (Q1$Sales[6:12]-Q1$SMA_5[6:12])^2,
  digits = 2)

# Get the sum of all columns 
colSums(Q1, na.rm = TRUE) # na.rm = TRUE removes any missing values 

# Alt method for Total Error Sq
sum(Q1$ErrorSq_5, na.rm = TRUE)

# MSE 5 weeks
mean(Q1$ErrorSq_5, na.rm = TRUE) # part b

# part c: 5 week moving average provides the smallest MSE.

# Optimized method using forecast package (no manual calculations)
library(forecast)

# Given data
Sales <- c(17, 21, 19, 23, 18, 16, 20, 18, 22, 20, 15, 22)

# Create a time series object
sales_ts <- ts(Sales)
sales_ts

# Compute four-week moving average
four_week_ma <- ma(sales_ts, order = 4)
four_week_ma

# Compute five-week moving average
five_week_ma <- ma(sales_ts, order = 5)
five_week_ma

# Compute accuracy measures for four-week moving average
accuracy(four_week_ma, x = sales_ts)

# Compute accuracy measures for five-week moving average
accuracy(five_week_ma, x = sales_ts)

# part c: 4 week moving average provides the smallest MSE.

# The discrepancy in MSE is from the different approaches used in the calculations.
# ma function calculates the ma using centered moving average, by default. 
# In centered ma, average for each observation is based on a symmetric window of observations centered around it.
# e.g. centered 3 period ma is calculated as the average of the current observation and the two adjacent observations on either side.

# Q2 (17-9)----

# build the data frame
Q2 <- data.frame(Week = c(1:12),
                 Sales = c(17, 21, 19, 23, 18, 16, 20, 18, 22, 20, 15, 22))

# Part a
# Exp Smoothing Alpha 0.1
Q2$EMA1 <- EMA(Q2$Sales, n = 1, ratio = .1)

# Adjust location of forecast values
Q2$EMA_1[2:12] <- round(
                  EMA(Q2$Sales, n = 1, ratio = .1),
                  digits = 2)[1:11]
# The last entry is the forecast for period 13.

# Errors
Q2$ErrorSq_1 <- round(
                      (Q2$Sales-Q2$EMA_1)^2,
                      digits = 2)

# MSE Alpha = 0.1
mean(Q2$ErrorSq_1, na.rm = TRUE) 

# Exp Smoothing Alpha 0.2
Q2$EMA2 <- EMA(Q2$Sales, n = 1, ratio = .2)

# Adjust location of forecast values
Q2$EMA_2[2:12] <- round(
                        EMA(Q2$Sales, n = 1, ratio = .2),
                        digits = 2)[1:11]
# The last entry is the forecast for period 13.

# Errors
Q2$ErrorSq_2 <- round(
                      (Q2$Sales-Q2$EMA_2)^2,
                      digits = 2)

# MSE Alpha = 0.2
mean(Q2$ErrorSq_2, na.rm = TRUE)

# Alpha = 0.2 smoothing would be preferred based on MSE.

# Part b
# Calculating MAE for Alpha 0.1
Q2$ErrorAbs_1 <- round(
                      abs(Q2$Sales - Q2$EMA_1), 
                      digits = 2)
mean(Q2$ErrorAbs_1, na.rm = TRUE)

# Calculating MAE for Alpha 0.2
Q2$ErrorAbs_2 <- round(
                      abs(Q2$Sales - Q2$EMA_2),
                      digits = 2)

mean(Q2$ErrorAbs_2, na.rm = TRUE)
#The Alpha = 0.1 smoothing would be preferred based upon MAE.

# Part c
# Calculating MAPE for alpha = 0.1
Q2$ErrorPer_1 <- round(
                      (abs(Q2$Sales-Q2$EMA_1)/Q2$Sales), digits = 4)

round(
      mean(Q2$ErrorPer_1, na.rm = TRUE),
      digits = 4)

# Calculating MAPE for alpha = 0.2
Q2$ErrorPer_2 <- round(
                     (abs(Q2$Sales-Q2$EMA_2)/Q2$Sales), digits = 4)

round(
  mean(Q2$ErrorPer_2, na.rm = TRUE),
  digits = 4)

# Alpha = 0.1 smoothing would be preferred based upon MAPE.

# Optimized method using forecast package
# Load the forecast package
library(forecast)

# Given data
Sales <- c(17, 21, 19, 23, 18, 16, 20, 18, 22, 20, 15, 22)

# Apply exponential smoothing with alpha = 0.1
fit_01 <- ses(Sales, alpha = 0.1)

# Apply exponential smoothing with alpha = 0.2
fit_02 <- ses(Sales, alpha = 0.2)

# Calculate Mean Squared Error (MSE) for alpha = 0.1
accuracy(fit_01)

# Calculate Mean Squared Error (MSE) for alpha = 0.2
accuracy(fit_02)

# Model fit_01 appears to perform slightly better than model fit_02.

# Q3 (17-11) ----

# part a

# For TS plot, it's a good idea to build an actual time series
TS3 <- ts(c(80, 82, 84, 83, 83, 84, 85, 84, 82, 83, 84, 83),
          start=c(2020,1), frequency = 12) # monthly
print(TS3)
plot(TS3)

# alt method for a better looking plot
library(ggfortify)
# extension of ggplot2 with enhanced visualization functions for time series

# Autoplot for better visualization
autoplot(TS3) + xlab("Month") + ylab("% On time") # horizontal with shift at the beginning

# part b (manual)
# for manual calculations it's better to have a data frame
Q3 <- data.frame(Month = c(1:12),
                 Percentage = c(80, 82, 84, 83, 83, 84, 85, 84, 82, 83, 84, 83))


#Technical Trading Rules Package
library(TTR)

# part b
# Simple Moving Average 3 months
SMA(Q3$Percentage, n = 3) 
# first value is F4, and last value is F13

Q3$SMA_3[4:12] <- round(SMA(Q3$Percentage, n = 3), digits = 2)[3:11]

# plot them together

library(tidyverse)
Q3 %>% 
  ggplot(aes(Month, Percentage)) +
                geom_point() +
                geom_line() +
                geom_point(aes(Month, SMA_3)) +
                geom_line(aes(Month, SMA_3), color = "tomato")

# Part b
# MSE for 3 Month SMA
Q3$ErrorSq_3 <- round(
                  (Q3$Percentage-Q3$SMA_3)^2, digits = 2)

mean(Q3$ErrorSq_3, na.rm = TRUE)

# Exponential Smoothing alpha 0.2
Q3$EMA2 <- EMA(Q3$Percentage, n = 1, ratio = .2)
# Fist entry is F2 and last entry is the F13.

Q3$EMA_2[2:12] <- EMA(Q3$Percentage, n = 1, ratio = .2)[1:11]

Q3$ErrorSq_2 <- (Q3$Percentage - Q3$EMA_2)^2
mean((Q3$ErrorSq_2), na.rm = TRUE)

# 3 Month MA provides the most accurate forecast using MSE.

# part c
# Forecast for next month

SMA(Q3$Percentage, n = 3)[12]
# Last entry is the forecast for period 13 (Forecast done in Dec. for Jan. next year)

# Optimized method using the forecast package:
# Load the forecast package
library(forecast)

# Given data
shipments <- c(80, 82, 84, 83, 83, 84, 85, 84, 82, 83, 84, 83)

# Convert to time series object
shipments_ts <- ts(shipments)

# a. Construct a time series plot
plot(shipments_ts, main = "Monthly Percentage of On-Time Shipments",
     xlab = "Month", ylab = "Percentage", col = "blue", lwd = 2)

# b. Compare the 3-month moving average and exponential smoothing with alpha = 0.2
# Three-month moving average
ma3 <- ma(shipments_ts, order = 3)

# Exponential smoothing with alpha = 0.2
es <- ses(shipments_ts, alpha = 0.2)

# Calculate MSE for both approaches
accuracy(ma3, x = shipments_ts) # accuracy against actual values (shipments_ts)
accuracy(es)

# c. Forecast for next month using exponential smoothing
forecast(es, h = 1)

# Q4 (17-15) ----

#Technical Trading Rules Package
library(TTR)

TS4 <- ts(c(7.35, 7.40, 7.55, 7.56, 7.60, 7.52, 7.52, 7.70, 7.62, 7.55),
         start=c(2021,1,1), 
         frequency = 52) # weekly data
print(TS4)
plot(TS4) # Data pattern is horizontal, seems like trend b/c of Y scale
plot(TS4, ylim = c(0,10))

# Alt method for better visualization (Autoplot)
autoplot(TS4,
ylim = c(0, 10)) +
xlab("Week") +
ylab("Index") # horizontal 

# part b (Manual)
# for manual calculations it's better to have a data frame
Q4 <- data.frame(Week = c(1:10),
                 Index = c(7.35, 7.40, 7.55, 7.56, 7.60, 7.52, 7.52, 7.70, 7.62, 7.55))

# Exponential Smoothing alpha 0.2
EMA2 <- EMA(Q4$Index, n = 1, ratio = .2)
EMA2
# Fist entry is F2 and last entry is the F11.

Q4$EMA_2[2:10] <- EMA(Q4$Index, n = 1, ratio = .2)[1:9]

# Errors
Q4$ErrorSq_2 <- (Q4$Index-Q4$EMA_2)^2
mean(Q4$ErrorSq_2, na.rm = TRUE) # MSE Alpha 0.2

# Exponential Smoothing alpha 0.3
EMA3 <- EMA(Q4$Index, n = 1, ratio = .3)
EMA3
# Fist entry is F2 and last entry is the F11.

Q4$EMA_3[2:10] <- EMA(Q4$Index, n = 1, ratio = .3)[1:9]

# Errors
Q4$ErrorSq_3 <- (Q4$Index-Q4$EMA_3)^2
mean(Q4$ErrorSq_3, na.rm = TRUE) # MSE Alpha 0.3

# Alpha 0.3 is a better model
EMA(Q4$Index, n = 1, ratio = .3)[10] # last value is the forecast for week 11

# Optimized Method using forecast package
# Load the forecast package
library(forecast)

# Given data
futures_index <- c(7.35, 7.40, 7.55, 7.56, 7.60, 7.52, 7.52, 7.70, 7.62, 7.55)

# Convert to time series object
futures_index_ts <- ts(futures_index)

# a. Construct a time series plot
plot(futures_index_ts, main = "Commodity Futures Index",
     xlab = "Week", ylab = "Index Value", col = "blue", lwd = 2)

# b. Compute exponential smoothing forecasts for alpha = 0.2
es_02 <- ses(futures_index_ts, alpha = 0.2)

# c. Compute exponential smoothing forecasts for alpha = 0.3
es_03 <- ses(futures_index_ts, alpha = 0.3)

# d. Compare the MSE for both forecasts and forecast for week 11
# Calculate MSE for alpha = 0.2
accuracy(es_02)

# Calculate MSE for alpha = 0.3
accuracy(es_03)

# alpha = 0.3 has a lower MSE compared to alpha = 0.2  the model 

# Forecast for week 11 using exponential smoothing with alpha = 0.3
forecast(es_03, h = 1)

# Q5 (17-19) ----

# part a
TS_5 <- ts( c(120, 110, 100, 96, 94, 92, 88))

print(TS_5)
plot(TS_5) # Data pattern is linear

# Better visualization (Autoplot)
autoplot(TS_5) +
  xlab("t") +
  ylab("Yt") # linear 

# part b, Linear Regression
trend_model <- lm(TS_5 ~ time(TS_5))
summary(trend_model)

# Alt method for Linear Regression
Q5 <- data.frame(t = c(1:7), 
                 Yt = c(120, 110, 100, 96, 94, 92, 88))
Q5_slr <- lm(Yt ~ t, data = Q5)
summary(Q5_slr)

# part c forecast for t = 8
predict(Q5_slr, list(t=8))

# note about MSE
anova(Q5_slr)
anova(Q5_slr)[2,3] # this MSE is SSE/df (df = n-k-1) different from SSE/n

# Q6 (17-25) ----

# part a
TS6 <- ts(c(33.27, 44.35, 57.39, 74.76, 93.80, 117.58),
         start=c(2012), 
         frequency = 1) # annual
print(TS6)
plot(TS6)
autoplot(TS6) # Data pattern is upward

# par b, Linear Regression
Q6 <- data.frame(year = c(1:6),
                 sub = c(33.27, 44.35, 57.39, 74.76, 93.80, 117.58))

Q6_model_1 <- lm(sub ~ year, data = Q6)
summary(Q6_model_1)

# part c, Quadratic Regression
Q6_model_2 <- lm(sub ~ poly(year, degree = 2, raw = T), data = Q6)
summary(Q6_model_2)

# part d, MSE Calculation

# Linear Model
anova(Q6_model_1)
anova(Q6_model_1)[2,3] # this MSE is SSE/df (df = n-k-1) different from SSE/n
anova(Q6_model_1)[2,2]/nrow(Q6) # This is SSE/n

# Quadratic Model
anova(Q6_model_2)
anova(Q6_model_2)[2,3] # this MSE is SSE/df (df = n-k-1) different from SSE/n
anova(Q6_model_2)[2,2]/nrow(Q6) # This is SSE/n

# Quadratic model appears better according to MSE.

# Part e, Forecast
predict(Q6_model_1, list(year=7))
predict(Q6_model_2, list(year=7))

# part f. quadratic model is more aggressive 
# quadratic model could be questioned for projections more than one year in the future.
# For one period ahead, the quadratic model is preferred because of its lower MSE.

# Q7 (17-28) ----

# part a, Visualization
TS7 <- ts(c(71, 49, 58, 75, 68, 41, 60, 84, 62, 51, 53, 72),
         start=c(2020,1), 
         frequency = 4) # quarterly
print(TS7)
autoplot(TS7) # Horizontal Pattern

# Part b, Regression
Q7 <- data.frame(Quarter=c(1:4), TS7) # 1:4 repeats itself

# Convert Quarter to Factor
Q7$Quarter <- as.factor(Q7$Quarter)
levels(Q7$Quarter)

# Simple Linear Regression without dummy variables
Q7_slr_1 <- lm(TS7 ~ Quarter, data = Q7)
summary(Q7_slr_1)
predict(Q7_slr_1)[1:4]

# Simple Linear Regression with dummy variables
Q7_dummy <- model.matrix(~ Quarter-1, Q7) # -1: exclude the intercept
Q7 <- cbind(Q7_dummy, Q7)

# Simple Linear Regression
Q7_slr <- lm(TS7 ~ Quarter1 + Quarter2 + Quarter3, data = Q7)
summary(Q7_slr) 
predict(Q7_slr)
predict(Q7_slr)[1:4]

# Q8 (17-32) ----

# part a

# build data frame
Q8 <-   c(20,  100, 175, 13,
          37,  136, 245, 26,
          75,  155, 326, 48,
          92,  202, 384, 82,
          176, 282, 445, 181)

# convert it into a time series
TS8 <- ts(Q8, start=2019, frequency=4) # quarterly

# Data is in a time series format
print(TS8)

# time series plot
plot(TS8)

# alt method using autoplot from ggfortify
library(ggfortify) # Load the ggfortify package

autoplot(TS8) + xlab("Year") + ylab("Revenue (1000)") # Upward Linear trend + Seasonal

# add a column for quarters 1:4
Q8 <- data.frame(Quarter= c(1:4), Revenue = Q8)

# convert this variable to a factor
Q8$Quarter <- as.factor(Q8$Quarter)
levels(Q8$Quarter)

# Simple Linear Regression without dummy variables
Q8_slr_1 <- lm(Revenue ~ Quarter, data = Q8)
summary(Q8_slr_1)
predict(Q8_slr_1)
predict(Q8_slr_1)[1:4]

# create dummy variable matrix
Q8_dummy <- model.matrix(~ Quarter -1, Q8) # -1: exclude the intercept
Q8 <- cbind(Q8_dummy, Q8)

# Simple Linear Regression
Q8_slr_2 <- lm(Revenue ~ Quarter1 + Quarter2 + Quarter3, data = Q8)
summary(Q8_slr_2)
predict(Q8_slr_2)
predict(Q8_slr_2)[1:4]

# part c
# adding Period
Q8$Period <- c(1:20)

# Simple Linear Regression with trend
Q8_slr_2 <- lm(Revenue ~ Quarter1 + Quarter2 + Quarter3 + Period, data = Q8)

# coefficients
summary(Q8_slr_2)

# forecasts
predict(Q8_slr_2, data.frame(Quarter1=1, Quarter2=0, Quarter3=0, Period=21))
predict(Q8_slr_2, data.frame(Quarter1=0, Quarter2=1, Quarter3=0, Period=22))
predict(Q8_slr_2, data.frame(Quarter1=0, Quarter2=0, Quarter3=1, Period=23))
predict(Q8_slr_2, data.frame(Quarter1=0, Quarter2=0, Quarter3=0, Period=24))

# Q9 (17-37) ----

# part a

TS9 <- ts(c(1690, 940, 2625, 2500,
            1800, 900, 2900, 2360,
            1850, 1100, 2930, 2615),
         start=2020, 
         frequency=4)

print(TS9)
autoplot(TS9) # Linear Trend with Seasonal Pattern

# part b (optional)
library(fpp2) # Forecasting: Principles and Practice Ver.2

# Moving Average 4 quarters (not centered)
TS9_MA_4 <- ma(TS9, order = 4, centre = F)
print(TS9_MA_4) # The last entry is the forecast for period 13.

# Centered Moving Average 4 quarters
TS9_MA_4_C <- ma(TS9, order = 4, centre = T)
print(TS9_MA_4_C)

# part c
# Seasonal Indexes (Decomposition)

# Manual:
## Step 1: TS values / centered MA = un-adjusted seasonal index for each period
## Step 2: Average un-adjusted seasonal indices for each period
## Step 3: Multiplicative model requires that sum of seasonal indices come to 4.00.
## Step 4: Adjustment: multiply indexes by the # of seasons divide by sum of the un-adjusted indexes.

# R gives you adjusted seasonal indexes.

TS9_Decomp <- decompose(TS9, type = "multiplicative" )
print(TS9_Decomp)
autoplot(TS9_Decomp)

# adjusted seasonal indexes.
print(TS9_Decomp$seasonal)

# part d
# Largest seasonal index is 3rd Quarter (July, Aug, Sep) back-to-school

# part e
# Calculating De-Seasonalized Time Series
TS9_des <- TS9/TS9_Decomp$seasonal
print(TS9_des)

# Part f
# Using the De-seasonalized Time Series to Identify Trend
# Simple Linear Regression
Q9 <- data.frame(Period=(1:12), TS9_des)
Q9_slr <- lm(TS9_des ~ Period, data = Q9)
summary(Q9_slr)

# Forecast Year 4
Q9_forecast <- predict(Q9_slr, list(Period=c(13:16)))
Q9_forecast

# Part g
# Adjusted linear trend forecasts by multiplying in the adjusted seasonal indexes
Adj_Q9_forecast <- Q9_forecast * TS9_Decomp$seasonal[1:4]
print(Adj_Q9_forecast)

# Q10 (17-43) ----

# part a
TS_10 <- ts(c(2750, 3100, 3250, 2800, 2900, 3050, 3300, 3100, 2950, 3000, 3200, 3150),
         start=c(2020,1,1),
         frequency = 52) # weekly

print(TS_10)
autoplot(TS_10) # Linear Trend?
autoplot(TS_10, ylim = c(0, 4000)) # No Linear Trend: Horizontal Pattern

# part b
# Exponential smoothing Alpha=0.4
EMA(TS_10, n = 1, ratio = .4) # The last entry is the forecast for period 13
EMA(TS_10, n = 1, ratio = .4)[12]

# Q11 (17-54) ----

TS11 <- ts(c(6,  15, 10, 4,
             10, 18, 15, 7,
             14, 26, 23, 12,
             19, 28, 25, 18,
             22, 34, 28, 21,
             24, 36, 30, 20,
             28, 40, 35, 27), start=2011, frequency=4)
print(TS11)
autoplot(TS11)

# part a
library(fpp2) # Forecasting: Principles and Practice Ver.2

# Centered Moving Average 4 quarters
TS11_CMA <- ma(TS11, order = 4, centre = T)
print(TS11_CMA)

# part b
# ts.plot plots several time series on a common plot.
ts.plot(TS11_CMA, TS11) 
# CMA values smooths data by removing seasonal effects and shows the trend.

# part c
# Seasonal Indexes (Decomposition)
Decomp_TS11 <- decompose(TS11, type = "multiplicative" )
autoplot(Decomp_TS11)
print(Decomp_TS11$seasonal)

# largest seasonal index in Q2 (Apr-Jun) (peak summer boating season)
# smallest seasonal index in Q4 (Oct- Dec) (Decreased boating in the fall and winter)

# Case Study (Forecasting Lost Sales  Carlson Department Store) ----

## Q1  ----
# An estimate of sales for Carlson Department Store had there been no hurricane.

library(readxl)
carlson <- read_excel("carlsonsales.xlsx")
glimpse(carlson)
head(carlson)

# Building time series
# Start 9th month (September 2015)
carl_ts <- ts(carlson$Sales, start= c(2015, 9), frequency = 12)
print(carl_ts)
autoplot(carl_ts)

# Decomposition
carl_decomp <- decompose(carl_ts, type = "multiplicative" )
autoplot(carl_decomp)
print(carl_decomp)

# Calculating De-seasonalized Sales
carl_des_sales <- carl_ts/carl_decomp$seasonal
carlson <- cbind(carlson, carl_des_sales)

# Using the Deseasonalized Time Series to Identify Trend
# Simple Linear Regression
carl_slr <- lm(carl_des_sales ~ Period, data = carlson)
summary(carl_slr)

# Forecasting without irregularity or hurricane (4 month closed, Sep - Dec) 
carl_des_forecast <- predict(carl_slr, list(Period=c(49:52)))
carl_des_forecast

# Now we add the seasonal effect
carl_forecast <- carl_des_forecast * carl_decomp$seasonal[1:4]
print(carl_forecast)


## Q2  ----
# An estimate of countywide department store sales had there been no hurricane.
library(readxl)
county_df_full <- read_excel("countysales.xlsx")
glimpse(county_df_full)
head(county_df_full)
# Note: County Excel File has 52 month with Actual values of Sep - Dec of yr 5)

county_df <- county_df_full[c(1:48),] # we need a comparable data set (first 48 rows)

# Building time series
# Start 9th month (Sep) of 2015
county_ts <- ts(county_df$Sales, start= c(2015, 9), frequency = 12)
print(county_ts)
autoplot(county_ts)

# Decomposition
county_decomp <- decompose(county_ts, type = "multiplicative" )
autoplot(county_decomp)
print(county_decomp)

# Calculating De-seasonalized Sales
county_des_sales <- county_ts/county_decomp$seasonal
county_df <- cbind(county_df, county_des_sales)
county_df$Period <- c(1:48)

# Using  Deseasonalized Time Series to Identify Trend
# Simple Linear Regression
county_slr <- lm(county_des_sales ~ Period, data = county_df)
summary(county_slr)

# Forecasting based on the slr (4 month closed, Sep - Dec)
county_des_forecast <- predict(county_slr, list(Period=c(49:52)))
county_des_forecast

# Now we add the seasonal effect
county_forecast <- county_des_forecast * county_decomp$seasonal[1:4]
print(county_forecast)


## Q3  -----
# An estimate of lost sales for Carlson from September to December.

# Comparison: county forecast vs. county actual sales
# Actual Values (4 month closed, Sep - Dec)
county_actual <- county_df_full$Sales[c(49:52)]
print(county_actual)

# Let's put county forecast and county actual sales in a data frame
Lost_Sales <- data.frame(county_actual, county_forecast)

# Then let's calculate the ratio of these two values (lift factor)
Lost_Sales$Ratio_A_F <- Lost_Sales$county_actual/Lost_Sales$county_forecast

# For the four-month total, we calculate the average lift factor 
mean(Lost_Sales$Ratio_A_F)
# For the 4-month total, actual sales exceeded the forecast by around 30%.
# Explanation: people had to replace personal property damaged by the storm.

# Now, let's put the forecast for Carlson in this data frame
Lost_Sales$Carl_F <- carl_forecast

# Then we multiply the forecast by corresponding lift factor
Lost_Sales$Carl_LS <- Lost_Sales$Carl_F * Lost_Sales$Ratio_A_F

# By adding them together we find the total lost sales
sum(Lost_Sales$Carl_LS)
# Carlson suffered a business interruption claim of $15,864,850.

