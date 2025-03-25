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
library('githubinstall')
install.packages('oosanalysis')
gh_install_packages("franz-maikaefer/oosanalysis-R-library", ref = "9b4251b")
library(oosanalysis)
library('MASS')
library(oosanalysis)
install.packages("zoo")
library(zoo)

# read data in 
Canada_Q <- read.csv("/Users/hannasyed/Desktop/Quarterly.csv")
Canada_A <- read.csv("/Users/hannasyed/Desktop/Annual.csv")
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
#I tried to fix this in Exel but I kept messing it up, so I replaced NA values with previous values to help fix the most current previous years. It did change the early years around 1915 because half were NA, but I figured this would be the best option that I can think of as we only need the recent years for prediction. Sorry if I messed it up a bi!
Canada$Pop[is.na(Canada$Pop)] <- 40097761
Canada$GDP[is.na(Canada$GDP)]<- 588322.8125
Canada$Infl[is.na(Canada$Infl)] <- 3.8790016
Canada$M2[is.na(Canada$M2)] <- 1512836666666.666667


#Get Real GDP per Capita 
Canada$GDP <- as.numeric(Canada$GDP)
Canada$RGDPPC <- Canada$GDP * 1000000 / Canada$Pop


# test for Stationarity for all variables 








