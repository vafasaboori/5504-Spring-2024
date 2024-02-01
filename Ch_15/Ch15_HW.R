# Clear the environment
rm(list = ls())

# Set the working directory
setwd("~/Documents/5504 Spring 2024/Ch_15")

# Q1 (15-5) ----
# read excel file
library(readxl)
Q1 <- read_excel("showtime.xlsx")
head(Q1)
summary(Q1)

# change column names
colnames(Q1) <- c( "revenue", "tv", "news")

# Part a, simple regression
slr1a <- lm(revenue ~ tv, data = Q1)
summary(slr1a)
coef(slr1a)

# Part b, multiple regression
slr1b <- lm(revenue ~ tv + news, data = Q1)
summary(slr1b)
coef(slr1b)

# part c, coefficients for tv are not the same in part a and b
  # part a: tv coef. is change in revenue due to a unit change in tv ads
  # part b: tv coef. is change in revenue due to a unit change in tv ads with news ads held constant.

# predict tv = 3600 and news = 1600
round(
  predict(slr1b, data.frame(tv=3.6, news = 1.6))
  *1000,
  0)

# Q2 (15-8) ----
# read excel file
library(readxl)
Q2 <- read_excel("Ships.xlsx")
head(Q2)

# change column names
colnames(Q2) <- c( "ship", "overall", "shore", "food")

# Part a, simple regression
slr2a <- lm(overall ~ shore, data = Q2)
summary(slr2a)
coef(slr2a)

# Part b, multiple regression
slr2b <- lm(overall ~ shore + food, data = Q2)
summary(slr2b)
coef(slr2b)

# predict shore 80 and food = 90
round(
  predict(slr2b, data.frame(shore=80, food = 90)),
  2)

# Q3 (15-12) ----
# read excel file
library(readxl)
Q3 <- read_excel("Exer2.xlsx")
head(Q3)

# multiple regression
slr3 <- lm(Y ~ X1 + X2, data = Q3)
slr3$coefficients # coefficients

# part a, b R2 and Adj. R2
summary(slr3) 
summary(slr3)$r.squared # R2
summary(slr3)$adj.r.squared # Adjusted R2

# part c, R2> 0.7 regression equation explain a large amount of the variability in the data

# Q4 (15-18) ----
library(readxl)
Q4 <- read_excel("PitchingMLB.xlsx")
head(Q4)

# change some column names
colnames(Q4)[6] <- "SO_IP"
colnames(Q4)[7] <- "HR_IP"
colnames(Q4)[8] <- "R_IP"

head(Q4)

# part a: multiple regression
slr4a <- lm(R_IP ~ SO_IP + HR_IP, data = Q4)
slr4a$coefficients # coefficients

# part b: R2 and Adj. R2
summary(slr4a) 
summary(slr4a)$r.squared # R2
summary(slr4a)$adj.r.squared # Adjusted R2

# The fit is not bad, because it explains around 50% of the variability

# part c: multiple regression
slr4c <- lm(ERA ~ SO_IP + HR_IP, data = Q4)
slr4c$coefficients # coefficients

# part a: R2 and Adj. R2
summary(slr4c) 
summary(slr4c)$r.squared # R2
summary(slr4c)$adj.r.squared # Adjusted R2

# The fit is not bad, because it explains 58.00% of the variability

# Q5 (15-23) ----
# read excel file
library(readxl)
Q5 <- read_excel("showtime.xlsx")
head(Q5)

# change column names
colnames(Q5) <- c( "revenue", "tv", "news")

# Part a, F test in multiple regression 
slr5 <- lm(revenue ~ tv + news, data = Q5)
summary(slr5)

# Extract F-statistic
summary(slr5)$fstatistic
summary(slr5)$fstatistic[1] # F-statistic

# Extract F test p-value
# pf() calculates the cumulative distribution function of the F-distribution
pf(summary(slr5)$fstatistic[1], 
   summary(slr5)$fstatistic[2], summary(slr5)$fstatistic[3], 
   lower.tail = FALSE)

# The overall model is  significant

# Part b, c: t tests in multiple regression 
summary(slr5)
summary(slr5)$coefficients[ , 3] # Returning t-value
summary(slr5)$coefficients[ , 4] # Returning p-value
# both "tv" and "news" are significant, should not be dropped

# Q6 (15-26) ----
library(readxl)
Q6 <- read_excel("PitchingMLB.xlsx")
head(Q6)

# change some column names
colnames(Q6)[6] <- "SO_IP"
colnames(Q6)[7] <- "HR_IP"
colnames(Q6)[8] <- "R_IP"

head(Q6)

# multiple regression
slr6 <- lm(R_IP ~ SO_IP + HR_IP, data = Q6)
slr6$coefficients # coefficients

# part a, b: F and t Test
summary(slr6) 

# There is a significant overall relationship.
# bot IVs are also significant.

# Q7 (15-29) extra ----
# read excel file
library(readxl)
Q7 <- read_excel("showtime.xlsx")
head(Q7)

# change column names
colnames(Q7) <- c( "revenue", "tv", "news")
head(Q7)

# Part a: prediction
slr7 <- lm(revenue ~ tv + news, data = Q7)
summary(slr7)

predict(slr7, data.frame(tv = 3.5, news = 2.3))

# Part b, confidence interval
predict(slr7, data.frame( tv = 3.5, news = 2.3), interval = "confidence", level = 0.95)

# Part c, prediction interval
predict(slr7, data.frame( tv = 3.5, news = 2.3), interval = "prediction", level = 0.95)

# Q8 (15-31) ----
library(readxl)
Q8 <- read_excel("autoresale.xlsx")
head(Q8)

# Part a: prediction
slr8 <- lm(Price ~ Mileage + Age, data = Q8)
summary(slr8)

predict(slr8, data.frame(Mileage = 40000, Age = 4))

# Part b, confidence and prediction interval
predict(slr8, data.frame(Mileage = 40000, Age = 4), interval = "confidence", level = 0.95)
predict(slr8, data.frame(Mileage = 40000, Age = 4), interval = "prediction", level = 0.95)

# Q9 (15-36) ----
# Read Excel file in R
library(readxl)
repair <- read_excel("repair.xlsx")
head(repair)

#change column names
colnames(repair) <- c("time", "months", "type", "person")

# Re-level categorical data
repair$type <- relevel(factor(repair$type), ref = "mechanical")
repair$person <- relevel(factor(repair$person), ref = "Bob Jones") # not necessary, bob is already 0

# Regression
repair_model <- lm(time ~ months + type + person, data = repair)

#coefficients, statistics, and sig levels
summary(repair_model) 

# The addition of Person is not statistically significant.
# Person is highly correlated with months since last service 
cor(repair$months, as.numeric(repair$person))
cor.test(repair$months, as.numeric(repair$person))

# Q10 (15-38) ----
# Read Excel file in R
library(readxl)
stroke <- read_excel("Stroke.xlsx")
head(stroke)

# part a and b Regression
stroke_model <- lm(Risk ~ Age + Pressure + Smoker, data = stroke)
summary(stroke_model)
# p-value for smoking is .010 < α = .05, smoking is a significant factor.

# part c predict
predict(stroke_model, data.frame(Age = 68, Pressure = 175, Smoker = "Yes"))

# The physician would recommend quit smoking (b3) and begin bp treatment (b2)

# Q11 (15-41) ----
# read excel file
library(readxl)
Q11 <- read_excel("showtime.xlsx")
head(Q11)

# change column names
colnames(Q11) <- c( "revenue", "tv", "news")
head(Q11)

# Part a: regression
slr11 <- lm(revenue ~ tv + news, data = Q11)
summary(slr11)
coefficients(slr11)

# part b std residual plot
# residual analysis
plot(slr11, which = 3) # no outliers
plot(slr11, which = 1) 

plot(slr11, which = 4) # D1 > 1, first observation is influential
plot(slr11, which = 5) # D1 > 1, first observation is influential

# 1 = residuals versus fitted
# 2 = QQ plot
# 3 = Scale-Location
# 4 = Cook's Distance
# 5 = Williams-like Graph

# alt method
# Residual analysis 
library(tidyverse)
Q11$predicted <- fitted(slr11)
Q11$residuals <- residuals(slr11)
Q11$std_residuals <- rstandard(slr11)

Q11 %>% ggplot(aes(x = predicted, y = std_residuals)) + 
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")  # we observe no pattern

# few observations, difficult to determine error assumptions violated.
# For instance, we can say there does not appear to be any pattern in the plot
# alternatively we can say there is a curvilinear pattern in the plot.

# none of the standard residuals are less than -2 or greater than 2 (no outlier)

# Q12 (15-42)----
# Read Excel file in R
library(readxl)
auto <- read_excel("Auto2.xlsx")
head(auto)

# Curb weight: weight including a full tank of fuel and all standard equipment.

# change col names
colnames(auto) <- c( "car", "price", "weight", "hp", "speed")

# part a Regression
auto_model <- lm(speed ~ price + hp, data = auto)
summary(auto_model)
coefficients(auto_model)

# part b residual analysis
plot(auto_model, which = 3) # part c. no outliers
plot(auto_model, which = 4) # part d.	observation 2 as an influential observation.
plot(auto_model, which = 5)

# 1 = residuals versus fitted
# 2 = QQ plot
# 3 = Scale-Location
# 4 = Cook's Distance
# 5 = Williams-like Graph

# alt method
# Residual analysis 
auto$predicted <- fitted(auto_model)
auto$residuals <- residuals(auto_model)
auto$std_residuals <- rstandard(auto_model)

auto %>% ggplot(aes(x = predicted, y = std_residuals)) + 
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")  

# There appears to be a highly unusual trend (megaphone) in the standardized residuals.
# residual plot does not support the assumption about error
# There are no std residuals above +2 or below -2


# Q13 (15-47)----

# Part a) Regression Equation
# P: probability of return for the sophomore year
# logit(P/1-P) = b0 + b1(GPA) + b2(Program) 

# Part b) What is the interpretation of E(y) when x2 = 0?
# For a given GPA, it's the prob. that a student who not attended orientation will return for sophomore year.

# Part c) Use both IVs to compute the estimated logit.
# Estimated Logit is the reg. equation 15.37 [logit(P/1-P) = b0 + b1(GPA) + b2(Program)]

library(readxl)
lakeland <- read_excel("Lakeland.xlsx")
head(lakeland)

# logistic regression
lakeland_model <- glm(Return ~ Program + GPA, data = lakeland, family = 'binomial')
coefficients(lakeland_model)

# Coefficients represent changes in log-odds ln(odds) of DV (hard to interpret)
# We need to convert them to Odds Ratio using exp() function 
exp(lakeland_model$coefficients[-1]) # we don't need intercept

# odds-ratios interpretation for IVs
# categorical IV (e.g. program): the odds of program attendees returning vs non-attendees
# program = 4.76, odds of an attendee returning are 4.76x a non-attendee

# continuous IV (e.g. GPA): the odds of returning increased for every additional unit of GPA
# GPA = 12.66, for every unit increase in GPA, odds of returning as a sophomore is 12.67x higher

# Part d) Overall Significance
summary(lakeland_model) # we don't get the overall significance
# R does not report overall model significance (beyond scope)

# The overall significance of model is tested by "Analysis of Deviance"
# It could be obtained by a Chi-Squared test (likelihood ratio test, LRT)
library(lmtest)
lrtest(lakeland_model) # this gives us the deviance table

# Part e) Each IV Significance
summary(lakeland_model)

# We can also conduct Chi-Squared test for each IV
library(car)
Anova(lakeland_model, test = "LR") # likelihood ratio test

# Part f) predict
predict(lakeland_model, data.frame(GPA = 2.5, Program = 0), type = "response")
predict(lakeland_model, data.frame(GPA = 2.5, Program = 1), type = "response")

# Part g) Interpretation of odds ratio for the orientation program?
lakeland_model$coefficients
exp(lakeland_model$coefficients[-1])
# odds of continuing for students who attended orient 4.7624 x greater those not.

# Part h) Recommendation
# We recommend making the orientation program required. 
# Odds of continuing are much higher for students who have attended the orientation program.

# case (NASCAR) ----
library(readxl)
nascar <- read_excel("nascar.xlsx")
head(nascar)

# Part 1
round(cor(nascar[,-1]),2) # correlation matrix
# The variable most highly correlated with Winnings ($) is the number of top-ten finishes.

# alt method
library(PerformanceAnalytics)
chart.Correlation(nascar[,-1],
                  histogram = TRUE)

# simple regression winnings and top10

# change col names
colnames(nascar)[5] = "Top5"
colnames(nascar)[6] = "Top10"
colnames(nascar)[7] = "Winnings"

nascar_model_1<- lm(Winnings ~ Top10, data = nascar)
summary(nascar_model_1)

# Part 2
nascar_model_2 <- lm(Winnings ~ Poles + Wins + Top5 + Top10, data = nascar)
summary(nascar_model_2)

# t-test: The only significant variable is Top 10, with a p-value of .0015.
# R2 = 0.82, whereas the model with only Top 10 had an R2 = .806. 
# Adding more IVs added little to the model’s ability to explain variation in Winnings.

# Part 3, Create two new independent variables: Top 2–5 and Top 6–10. Top 2–5 
# Top 2-5
nascar$Top2_5 <- nascar$Top5 - nascar$Wins

# Top 6-10
nascar$Top6_10 <- nascar$Top10 - nascar$Top5

# regression Winnings ($) ~ Wins, Top 2–5, and Top 6–10.
nascar_model_3 <- lm(Winnings ~ Poles + Wins + Top2_5 + Top6_10, data = nascar)
summary(nascar_model_3)

# T test: only IV not significant is Poles, with a p-value of .9047.
# We get better results because we greatly reduced the multicollinearity
# Multicollinearity is reduced by replacing Top 5 with Top 2–5 and Top 10 with Top 6–10.

# Correlation Matrix below provides evidence of this (lower correlations)
round(cor(nascar[, -c(1,5,6)]),2) # excluding driver, top5, and top10

# alt method
library(PerformanceAnalytics)
chart.Correlation(nascar[, -c(1,5,6)],
                  histogram = TRUE)

# part 4, What regression equation you recommend to predict Winnings ($)?

# Keep Wins, Top2-5, and Top6-10
nascar_model_4 <- lm(Winnings ~ Wins + Top2_5 + Top6_10, data = nascar)
summary(nascar_model_4)

# Wins: one unit increase in wins, other IV constant, increases winnings by $204,735.
# Top 2–5: one unit increase in Top 2–5, other IV constant, increases winnings by $186,778.
# Top 6–10: one unit increase in Top 6–10, other IV constant, increases winnings by $116,189.

# In Class Practice (15-48) ----
library(readxl)
TireRatings <- read_excel("TireRatings.xlsx")
head(TireRatings)

#Rename column with space
colnames(TireRatings)[4] <- "BuyAgain"

# Make sure your variables are numeric
TireRatings$Wet <- as.numeric(TireRatings$Wet)
TireRatings$Noise <- as.numeric(TireRatings$Noise)
TireRatings$Buy_Again <- as.numeric(TireRatings$BuyAgain)

# Create the logistic regression model
tire_model <- glm(Purchase ~ Wet + Noise, data = TireRatings, family = "binomial")

# Print the summary to get the estimated coefficients
summary(tire_model)

# (a) Logistic regression equation
# The logistic regression equation is given by:
# logit(Purchase) = b0 + b1 * Wet + b2 * Noise
# where b0 is the intercept, b1 is the coefficient for Wet, and b2 is the coefficient for Noise

# (b) Estimated logit

# Coefficients represent changes in log-odds (ln(odds) of DV (hard to interpret)
# We need to convert them to Odds Ratio using exp() function 
exp(tire_model$coefficients[-1]) # we don't need intercept

# for a 1-unit increase in Wet rating, odds of purchasing are multiplied by  29.2. 
# For a 1-unit increase in Noise rating, odds of purchasing are multiplied by 6.15.
# In general odds ratio >1 indicates a positive association between IV and DV
# while an odds ratio <1 indicates a negative association between IV and DV

# (c) Estimate probability for Wet rating of 8 and Noise rating of 8
predict(tire_model, data.frame(Wet = 8, Noise = 8), type = "response")

# (d) Estimate probability for Wet rating of 7 and Noise rating of 7
predict(tire_model, data.frame(Wet = 7, Noise = 7), type = "response")

# (e) Interpretation
# Wet and Noise ratings of 7 are both considered excellent performance ratings
# Nonetheless, the probability of repurchasing such tire is extremely low (0.0406)
# But a one-point increase in both ratings increases the probability to 0.8837
# So achieving the highest possible levels of performance is essential for manufacturer
