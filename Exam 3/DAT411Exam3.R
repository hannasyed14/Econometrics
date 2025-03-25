# Hanna Syed Exam 3

rm(list = ls())
library("dplyr")
library("ggplot2")
library(tidyverse)
library(lmtest)
library("sandwich")
library("tseries")
library("forecast")
library(fUnitRoots)
library("ARDL")
library(purrr)
library(lubridate)
library(zoo)
library(githubinstall)
library('MASS')
library(oosanalysis)
gh_install_packages("franz-maikaefer/oosanalysis-R-library", ref = "9b4251b")
#install.packages("margins")
library(margins)


######## SECTION 2 #########

### Import Data ###

macro <- read.csv("/Users/hannasyed/Desktop/Macro.Fall24.csv")
hpi <- read.csv("/Users/hannasyed/Desktop/HPI.Fall24.csv")

# Subset hpi data 
# I accidentaly looked at the wrong row and did this for DC. My apologies! 
HPI_DC <- hpi[hpi$cbsa == 47894,]

# Make sure data set match 
macro$DATE <- as.Date(macro$DATE, format ="%m/%d/%Y")
macro$yr <- year(macro$DATE)
macro$month <- month(macro$DATE)
macro$qtr <- (macro$month - 1) %/% 3 + 1



### Merge datasets ###
full_data_set <- merge(HPI_DC, macro, by=c("yr", "qtr"))
full_data_set$MarketIndex <- as.numeric(full_data_set$MarketIndex)


### Stationary ADF ###

colnames(full_data_set)
variables <- c("index_nsa", "index_sa", "CPI","RGDPPC", "MORTGAGE30US", "BAA", "M2S", "T10Y3M", "NASDAQ")
results_S <- list()
for (i in variables) {
  ts_data <- ts(full_data_set[[i]])
  result <- adfTest(ts_data, type = "c")
  results_S[[i]] <- result@test[["p.value"]][[1]]
}

ADFT <- do.call(rbind,results_S)
ADFT
#index_nsa    0.9900000
#index_sa     0.9900000
#CPI          0.9900000
#RGDPPC       0.9607195
#MORTGAGE30US 0.2026731
#BAA          0.2246189
#M2S          0.9900000
#T10Y3M       0.1267494
#NASDAQ       0.9900000
#We fail to reject the null hypothesis of unit root(non-stationarity) for all constants

# Percent Change 
full_data_set <- full_data_set %>%
  mutate(
    HPR = ((index_nsa / dplyr::lag(index_nsa) - 1) * 100),
    Inflation = ((CPI / dplyr::lag(CPI) - 1) * 100),
    GDP = ((RGDPPC / dplyr::lag(RGDPPC) - 1) * 100),
    Mortgage = ((MORTGAGE30US / dplyr::lag(MORTGAGE30US) - 1) * 100),
    Bond = ((BAA / dplyr::lag(BAA) - 1) * 100),
    M2 = ((M2S / dplyr::lag(M2S) - 1) * 100),
    Treasury = ((T10Y3M / dplyr::lag(T10Y3M) - 1) * 100),
    Stock = ((NASDAQ / dplyr::lag(NASDAQ) - 1) * 100)
  )

# Retest for Stationarity 
variables <- c("HPR", "Inflation", "GDP", "Mortgage", "Bond", "M2", "Treasury", "Stock")
results_S <- list()
for (i in variables) {
  ts_data <- ts(full_data_set[[i]])
  result <- adfTest(ts_data, type = "c")
  results_S[[i]] <- result@test[["p.value"]][[1]]
}

ADFT <- do.call(rbind,results_S)
ADFT
#HPR       0.01
#Inflation 0.01
#GDP       0.01
#Mortgage  0.01
#Bond      0.01
#M2        0.01
#Treasury  0.01
#Stock     0.01
# We reject the null hypothesis of unit root (nonstatioanarity) at the 1% level for all constants> 
# We find statistical evidence in favor of stationarity for all constants. 

full_data_set$lag_HPR = dplyr::lag(full_data_set$HPR, n = 1L)
full_data_set$lag_Inflation = dplyr::lag(full_data_set$Inflation, n = 1L)

not_delete <- which(!is.na(full_data_set$lag_HPR))
full_data_set<- full_data_set[not_delete,]

regression1 <- lm(HPR ~ lag_HPR + lag_Inflation, data = full_data_set)



### Homoscedasticity ###

#BP Test
bptest(regression1)
# From the BP test we fail to reject the null hypothesis of homoscedasticity. 

#White Test 
full_data_set$residuals_sq <- residuals(regression1)^2
white_model <- lm(residuals_sq ~ lag_HPR + lag_Inflation + I(lag_HPR^2) + I(lag_Inflation^2) +
                    I(lag_HPR * lag_Inflation), data = full_data_set)

wtest <- lrtest(regression1, white_model)
print(wtest)
#We reject the null hypothesis of homoscedasticity at the 1% level, in favor of heteroscedasticity 
# Correct

regression2 <- coeftest(regression1, vcov. = vcovHC(regression1, type = "HC1"))
summary(regression1)
regression2
# lag_HPR: After correcting for heteroskedasticity, the standard error increased, which made the t-value smaller, but still significant at the 1% level
# lag_Inflation: After correcting for heteroskedasticity, the standard error increased, which made the t-value smaller. 
#In regression1, lag_Inflation was significant at the 5% level (p-value = 0.03888), but in regression2, the p-value increased to 0.119
#lag_Inflation is now insignificant at the 5% level. 
#after adjusting for heteroskedasticity, lag_Inflation loses statistical significance
# we fail to reject the null hypothesis for lag_Inflation in regression2

### Serial Correlation ###
bgtest(regression1)
# We fail to reject the null hypothesis of no serial correlation at the 10% level. We find significant statistical evidence of no serial correlation  
#Newey-West
regression3 <- coeftest(regression1, vcov = NeweyWest(regression1))
regression1
regression2
regression3
#After accounting for both heteroskedasticity and serial correlation, lag_HPR is statistically significant at the 1% level and lag_Inflation is statistically significant at the 5% level.
#This result contrasts with regression2. Lag_Inflation was not significant after adjusting for heteroskedasticity alone but now is after adjusting for both 

acf(full_data_set$HPR)
pacf(full_data_set$HPR)

#I created the baseline model to have one lag because for baseline models you want the simplest model to be like a bench mark
#the simplest model is a random walk where the lag is 1
baseline_model <- lm(HPR ~ dplyr::lag(HPR, n = 1L), data = full_data_set)
model1 <- lm(HPR ~ dplyr::lag(HPR, n = 3L), data = full_data_set ) 
model2 <- lm(HPR ~ dplyr::lag(HPR, n = 3L) + dplyr::lag(HPR, n = 4L), data = full_data_set ) 
model3 <- lm(HPR ~ dplyr::lag(HPR, n = 3L) + dplyr::lag(HPR, n = 4L)+ dplyr::lag(HPR, n = 5L), data = full_data_set ) 
model4 <- lm(HPR ~ dplyr::lag(HPR, n = 3L) + dplyr::lag(HPR, n = 4L)+ dplyr::lag(HPR, n = 5L)+ dplyr::lag(HPR, n = 6L), data = full_data_set ) 
model5 <- lm(HPR ~ dplyr::lag(HPR, n = 3L) + dplyr::lag(HPR, n = 4L)+ dplyr::lag(HPR, n = 5L)+ dplyr::lag(HPR, n = 6L)+ dplyr::lag(HPR, n = 7L), data = full_data_set ) 
model6 <- lm(HPR ~ dplyr::lag(HPR, n = 3L) + dplyr::lag(HPR, n = 4L)+ dplyr::lag(HPR, n = 5L)+ dplyr::lag(HPR, n = 6L)+ dplyr::lag(HPR, n = 7L)+ dplyr::lag(HPR, n = 8L), data = full_data_set ) 
model7 <- lm(HPR ~ dplyr::lag(HPR, n = 3L) + dplyr::lag(HPR, n = 4L)+ dplyr::lag(HPR, n = 5L)+ dplyr::lag(HPR, n = 6L)+ dplyr::lag(HPR, n = 7L)+ dplyr::lag(HPR, n = 8L)+ dplyr::lag(HPR, n = 9L), data = full_data_set ) 
model8 <- lm(HPR ~ dplyr::lag(HPR, n = 3L) + dplyr::lag(HPR, n = 4L)+ dplyr::lag(HPR, n = 5L)+ dplyr::lag(HPR, n = 6L)+ dplyr::lag(HPR, n = 7L)+ dplyr::lag(HPR, n = 8L)+ dplyr::lag(HPR, n = 9L)+ dplyr::lag(HPR, n = 10L), data = full_data_set ) 

models <- list(baseline_model, model1, model2, model3, model4, model5, model6, model7, model8)
aic_values <- sapply(models, AIC)
bic_values <- sapply(models, BIC)
#AIC is the lowest for model4
#BIC is the lowest for model4
# Based off both AIC and BIC the best fit for the data is model4
#The above I had the baseline model and 1-4
#I decided to add more models to test more lags 
#AIC seemingly keeps decreasing as we add more lags and create more models
#BIC is lowest at model 6 
# in order to "KISS" I will use model 6 
# AR(8) model 



### OOS Forecasting ### 

#same thing happened as in the video so had to do this 
full_data_set$lag3_HPR = dplyr::lag(full_data_set$HPR, n = 3L)
full_data_set$lag4_HPR = dplyr::lag(full_data_set$HPR, n = 4L)
full_data_set$lag5_HPR = dplyr::lag(full_data_set$HPR, n = 5L)
full_data_set$lag6_HPR = dplyr::lag(full_data_set$HPR, n = 6L)
full_data_set$lag7_HPR = dplyr::lag(full_data_set$HPR, n = 7L)
full_data_set$lag8_HPR = dplyr::lag(full_data_set$HPR, n = 8L)

not_delete <- which(!is.na(full_data_set$lag8_HPR))
full_data_set<- full_data_set[not_delete,]


init_train_size <- floor(0.50 * nrow(full_data_set))
model0 <- function(data) lm(HPR ~ 1, data = full_data_set)
model1a <- function(data) lm(HPR ~ lag3_HPR + lag4_HPR + lag5_HPR + lag6_HPR + lag7_HPR + lag8_HPR, data = full_data_set)
model2a <- function(data) lm(HPR ~lag3_HPR + lag4_HPR + lag5_HPR + lag6_HPR + lag7_HPR + lag8_HPR + lag_Inflation, data = full_data_set)

mean_Forecast <- recursive_forecasts(model0, full_data_set, init_train_size, "rolling")
AR8_Forecast <- recursive_forecasts(model1a, full_data_set, init_train_size, "rolling")
ARDL8_model_Forecast <- recursive_forecasts(model2a, full_data_set, init_train_size, "rolling")

Results1 <- clarkwest(model0,model1a,full_data_set,init_train_size, window = "rolling")
# We reject the null at the 1% level, we find statistical evidence that the model1a is more accurate than to the model of the mean (model0) 
Results2 <- clarkwest(model1a,model2a,full_data_set,init_train_size, window = "rolling")
# We fail to reject, we do not find statistical evidence that model2a is more accurate than model1a. 
Results3 <- clarkwest(model0,model2a,full_data_set,init_train_size, window = "rolling")
## We reject the null at the 1% level, we find statistical evidence that the model2a is more accurate than to the model of the mean (model0) 



### R squared OOS ###

#combine forecasts 
combined_forecasts <- data.frame(Mean = mean_Forecast, AR8 = AR8_Forecast, ARDL8 = ARDL8_model_Forecast)

#add HPR
#I made the start the values after the training set we made earlier with init_train_size so I added 1 to go to the next row 
#I made the end the total number of rows in combined plus the init_train_size to match the number of rows in combined_forecasts.
combined_forecasts$HPR <- full_data_set$HPR[(init_train_size + 1):(init_train_size + nrow(combined_forecasts))]

#then take the actual value that existed that is connected with your forecast and subtract your forecast from it, square that value
combined_forecasts$SEmean <- (combined_forecasts$HPR - combined_forecasts$Mean)^2
combined_forecasts$SEAR8 <- (combined_forecasts$HPR - combined_forecasts$AR8)^2
combined_forecasts$SEARDL8 <- (combined_forecasts$HPR - combined_forecasts$ARDL8)^2

#then take the mean of that overall for all three rows
m_mean <- mean(combined_forecasts$SEmean)
m_AR8 <- mean(combined_forecasts$SEAR8)
m_ARDL8 <- mean(combined_forecasts$SEARDL8)

#then you are going to take that mean saured forecasting error and you are going to divide AR8_Forecast by mean_forecast and ARDL8_model_Forecast by mean_forecast and that will be your out of sample r squared 
# (MSFE(model)/MSFE(mean))
Rsquared_AR8 <- (m_AR8 / m_mean)
Rsquared_ARDL8 <- (m_ARDL8 / m_mean)


######## SECTION 3 #########

game <- read.csv("/Users/hannasyed/Desktop/Game Data - Copy.csv")

### Create Variable Win ####
game$win <- ifelse(game$TEAM.1.OUTCOME == "W",1,0) 

### Subset the data by your CONFERENCE1 ### MAAC
game_new <- game[which(game$TEAM.1.CONFERENCE == "MAAC"),]

LPM <- lm(game_new$win ~ game_new$TEAM.1.TURNOVER.. + game_new$TEMPO + game_new$TEAM.1.POINTS.PER.100.POSSESSION)
summary(LPM)
# R^2: Our model can explain 29.31% of the variance in the dependent variable, win.
# Adjusted R^2: Accounting for the adjusted loss of degrees of freedom, our model can explain 29.26% of the variance in the dependent variable, win.
# Intercept: turnover, tempo, and points per 100 possesions equal 0, it is associated with the probability of winning being -1.4271 
#The probabilility of winning being negative is not realistic but is like a base value for if/when all the independent variables are 0
# Turnover: Holding all else comstant, For every one unit increase in turnovers, it is associated with an increase in the probability of winning by 0.0081 units.
# TEMPO: Holding all else comstant, For every one unit increase in tempo, it is associated with a decrease in the probability of winning by 0.0038 units.
# PP100P:  Holding all else comstant, For every one unit increase in points per 100 possessions, it is associated with an increase in the probability of winning by 0.0199 units.

### Hypothesis Testing ###
#Ho: β = 0 (The coefficient has no impact on the probability of winning)
#Ha: β != 0 (The coefficient has an impact on the probability of winning)
# Turnover: We reject the null hypothesis at the 1% level. We find statistically significant evidence that turnovers have an effect on the probability of winning
# Tempo: We reject the null hypothesis at the 1% level. We find statistically significant evidence that tempo has an effect on the probability of winning
# PP100P : We reject the null hypothesis at the 1% level. We find statistically significant evidence that points per 100 possesions have an effect on the probability of winning
# Overall Significance: We reject the null hypothesis at the 1% level, and we find statistical evidence of overall significance that turnover, tempo, and points per 100 possessions are jointly not equal to 0 and are associated with the probability of winning.


### Logit ###
logit_model <- glm(win ~ TEAM.1.TURNOVER.. + TEMPO + TEAM.1.POINTS.PER.100.POSSESSION, family = binomial(), data = game_new)
marginal_effects_log <- margins(logit_model)
summary(marginal_effects_log)
# PP100P: Holding all else contstant, for every one unit of points per 100 possesions we would expect the probability of the team winning to increase by 2.02%
# Turnover: Holding all else contstant, for each one unit increase in turnovers,we would expect the probability of the teams winning to increase by 0.81%
# Tempo: Tempo: Holding all else constant, for every one-unit increase in tempo, we would expect the probability of the team winning to decrease by 0.39%.

### Hypothesis Testing ###
#Ho: β = 0 (The average marginal effect of the coefficient is equal to 0)
#Ha: β != 0 (The average marginal effect of the coefficient is not equal to 0)
# PP100P: We reject the null hypothesis at the 1% level that the true value of this coefficient's marginal effect is equal to 0. There is statistically significant evidence that points per 100 possessions positively affect the probability of winning.
# Turnover: We reject the null hypothesis at the 1% level that the true value of this coefficient's marginal effect is equal to 0. There is statistically significant evidence that turnovers positively affect the probability of winning.
# Tempo: We reject the null hypothesis at the 1% level that the true value of this coefficient's marginal effect is equal to 0. There is statistically significant evidence that tempo negatively affects the probability of winning.


## Probit ### 
probit_model <- glm(win ~ TEAM.1.TURNOVER.. + TEMPO + TEAM.1.POINTS.PER.100.POSSESSION, family = binomial(link = "probit"), data = game_new)
marginal_effects_pro <- margins(probit_model)
summary(marginal_effects_pro)
# Intercept: Holding all else constant, when turnover, tempo, and points per 100 possesions are zero, the z-score for the probability of the team winning is -6.62
# Turnover: Holding all else constant, for every one-unit increase in turnovers, we would expect the z-score for the probability of the team winning to increase by 0.02766
# Tempo: Holding all else constant, for every one-unit increase in tempo, we would expect the z-score for the probability of the team winning to decrease by 0.01367
# PP100P: Holding all else constant, for every one-unit increase in points per 100 possessions, we would expect the z-score for the probability of the team winning to increase by 0.06890.
# PP100P: We reject the null hypothesis at the 1% level that the true value of this coefficient's marginal effect is equal to 0. There is statistically significant evidence that points per 100 possessions positively affect the probability of winning.
# Turnover: We reject the null hypothesis at the 1% level that the true value of this coefficient's marginal effect is equal to 0. There is statistically significant evidence that turnovers positively affect the probability of winning.
# Tempo: We reject the null hypothesis at the 1% level that the true value of this coefficient's marginal effect  is equal to 0. There is statistically significant evidence that tempo negatively affects the probability of winning.


### Overall ###
#PP100P has the  most significant positive impact on the probability of winning
#This suggests the more points the team scores the more likely they are to win
#Turnover has a smaller positive impact on the probability of winning compared to PP100P
#This is not expected as you would logically think the more turnovers would indicate more losses
#temp negatively affects the probability of winning
#This could suggest that playing the game at a higher tempo or speed is not good for winnning




