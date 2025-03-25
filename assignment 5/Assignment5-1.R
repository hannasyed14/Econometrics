# Hanna Syed and Plu Reh Assignment 5 
rm(list = ls())
library("dplyr")
library("ggplot2")
library(lmtest)
library("sandwich")
library("tseries")
library("forecast")
library(fUnitRoots)
library("ARDL")
library(tidyverse)
library('purrr')
#library('githubinstall')
#install.packages('oosanalysis')
#gh_install_packages("franz-maikaefer/oosanalysis-R-library", ref = "9b4251b")
library(oosanalysis)
library('MASS')
library(oosanalysis)
#install.packages("zoo")
library(zoo)

# read data in 
Canada_Q <- read.csv("Quarterly.csv")
Canada_A <- read.csv("Annual.csv")
str(Canada_Q)
str(Canada_A)

# change data column names
colnames(Canada_Q) <- c("Date", "GDP", "ER", "CPI", "M2", "IR")
colnames(Canada_A) <- c("Date", "Pop", "Infl")

# change annual data into quarterly and extract year and quarter
quarters <- c("Q1", "Q2", "Q3", "Q4")
expanded_annual_data <- merge(expand.grid(Year = Canada_A$Date, Quarter = quarters), Canada_A, by.x = "Year", by.y = "Date")
expanded_annual_data$Year <- as.Date(expanded_annual_data$Year, format = "%Y-%m-%d")
expanded_annual_data$Year <- format(expanded_annual_data$Year, "%Y")
expanded_annual_data$Year <- as.numeric(expanded_annual_data$Year)

# extract year and quarter of quarterly data
Canada_Q$Date <- trimws(Canada_Q$Date)
Canada_Q$Date <- as.Date(Canada_Q$Date, format = "%Y-%m-%d")
Canada_Q$Year <- as.numeric(format(Canada_Q$Date, "%Y"))
Canada_Q$Quarter <- quarters(as.yearqtr(Canada_Q$Date))

# combine data to one dataset
Canada <- merge(Canada_Q, expanded_annual_data, by = c("Year", "Quarter"), all.x = TRUE)
str(Canada)

# get rid of rows with NA's 
Canada$GDP[Canada$GDP == "."] <- NA
Canada$M2[Canada$M2 == "."] <- NA
Canada$ER[Canada$ER == "."] <- NA
Canada$CPI[Canada$CPI == "."] <- NA
Canada$IR[Canada$IR == "."] <- NA

# fixing empty values in data 
#I tried to fix this in Exel but I kept messing it up, 
# so I replaced NA values with previous values to help fix the most
# current previous years. It did change the early years around 1915 
# because half were NA, but I figured this would be the best option that 
# I can think of as we only need the recent years for prediction. 
# Sorry if I messed it up a bit!
Canada$Pop[is.na(Canada$Pop)] <- 40097761
Canada$GDP[is.na(Canada$GDP)]<- 588322.8125
Canada$Infl[is.na(Canada$Infl)] <- 3.8790016
Canada$M2[is.na(Canada$M2)] <- 1512836666666.666667


#Get Real GDP per Capita 
Canada$GDP <- as.numeric(Canada$GDP)
Canada$RGDPPC <- Canada$GDP * 1000000 / Canada$Pop

colnames(Canada)

# Ensure variables are numeric
Canada$CPI <- as.numeric(Canada$CPI)
Canada$M2 <- as.numeric(Canada$M2)
Canada$Pop <- as.numeric(Canada$Pop)
Canada$Infl <- as.numeric(Canada$Infl)
Canada$RGDPPC <- as.numeric(Canada$RGDPPC)
str(Canada)

# test for Stationarity for all variables 
variables <- c("CPI", "M2", "Pop", "Infl", "RGDPPC")
results_S <- list()
for (i in variables) {
  ts_data <- ts(Canada[[i]])
  result <- adfTest(ts_data, type = "nc")
  results_S[[i]] <- result@test[["p.value"]][[1]]
}
ADFT <- do.call(rbind,results_S)
ADFT

# We reject the null hypothesis of CPI at the 5% level, we find statistical evidence that 
# the data provided is stationary
# We fail to reject the null hypothesis for M2 because the data provided indicated
# to be not non-stationary
# We fail to reject the null hypothesis for Population because the data provided indicated
# to be not non-stationary
# We fail to reject the null hypothesis for Inflation because the data provided indicated
# to be not non-stationary
# We fail to reject the null hypothesis for RGDPPC because the data provided indicated
# to be not non-stationary

### Granger Causality Test ###
#GCCPII <- grangertest(RGDPPCGR ~ CPI, order = 2, data = Canada)
#GCRGD <- grangertest(RGDPPCGR ~ M2, order = 2, data = Canada)

Canada <- Canada %>%
mutate(CPIGR = ((CPI / dplyr::lag(CPI) - 1) * 100),
       #M2GR = ((M2 / dplyr::lag(M2) - 1) * 100),
       #POPGR = ((Pop / dplyr::lag(Pop) - 1) * 100),
       #InflGR = ((Infl / dplyr::lag(Infl) - 1) * 100),
       RGDPPCGR = ((RGDPPC / dplyr::lag(RGDPPC) - 1) * 100))
not_delete <- which(!is.na(Canada$RGDPPCGR))
Canada <- Canada[not_delete,]
# 
### Finding the best forecasting model
acf(Canada$RGDPPCGR)
# NEEDS FIXING

### Check out AIC & BIC ###
Canada$RGDPPCGR_lag1 <- dplyr::lag(Canada$RGDPPCGR,1)
Canada$RGDPPCGR_lag2 <- dplyr::lag(Canada$RGDPPCGR,2)

init_train_size <- floor(0.50 * nrow(Canada))

model0 <- function(data) lm(RGDPPCGR ~ 1, data = Canada)
model11 <- function(data) lm(RGDPPCGR ~ RGDPPCGR_lag1 + RGDPPCGR_lag2, data = Canada)

mean_model_Forecast <- recursive_forecasts(model0, Canada, init_train_size, "rolling")
ARDL2_model_Forecast <- recursive_forecasts(model11, Canada, init_train_size, "rolling")
Results             <- clarkwest(model0, model11,Canada, init_train_size, window = "rolling")
# 

### Forecast into the future ###

Canada2 <- Canada
Canada2[nrow(Canada2)+1,] <- c(2025, "Q1", NA)
model01 <- function(data) lm(RGPPCGR ~ 1, data = Canada2)
mean_model_Forecast2 <- recursive_forecasts(model0, Canada2, init_train_size, "rolling")


Canada2$X <- 0
model0 <- lm(RGDPPCGR ~ X, data = Canada2)
new_data <- data.frame(X = 0)
forecasted_values1 <- forecast(model0, new_data, h = 1)

model11 <- lm(RGDPPCGR ~ RGDPPCGR_lag1 + RGDPPCGR_lag2, data = Canada)
new_data <- tail(Canada, 1) %>%
  mutate(RGDPPCGR_lag2 = RGDPPCGR_lag1,
         RGDPPCGR_lag1 = RGDPPCGR)
forecasted_values2 <- predict(model11, newdata = new_data)
# Q1,2025 for Canada, should have a growth rate of about 0.36%



