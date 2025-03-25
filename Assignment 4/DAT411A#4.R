#Hanna Syed and Plu Reh
#Assignment 4 

rm(list = ls())
library("dplyr")
library("ggplot2")
library(lmtest)
library("sandwich")
library('tseries')
library("forecast")
library(fUnitRoots)

# Data is based on rent of primary residence in U.S City Average
Data <- read.csv("Annual(in).csv")
View(Data)

# Rename columns
Data <- Data %>%
  rename(inflation = M2SL, m2g = GDPC1)

Data$inflation <- as.numeric(Data$inflation)
Data$m2g <- as.numeric(Data$m2g)
Data$DATE <- as.Date(Data$DATE, format = "%m/%d/%Y")
Data <- Data %>% filter(!is.na(inflation) & !is.na(m2g))

# Calculate inflation and M2 growth 
Data <- Data %>%
  filter(inflation > 0 & m2g > 0) %>%  
  mutate(
    inflation = c(NA, diff(log(inflation))),  
    m2g = c(NA, diff(log(m2g))),  
    RGDPG = c(NA, diff(log(m2g)))  
  ) %>%
  filter(!is.na(inflation) & !is.na(m2g) & !is.na(RGDPG))

# ADF tests to check stationarity
variables <- c("inflation", "m2g")
results_C <- list()

for (i in variables) {
  ts_data <- ts(Data[[i]])  
  result <- adfTest(ts_data, type = "c") 
  results_C[[i]] <- result@test[["p.value"]][[1]]  
}

#results of the ADF test for stationarity
print(results_C)
#Inflation: We fail to reject the null hypothesis, we do not have sufficient evidence to conclude that inflation is not non stationary
#m2g: We reject the null, we have suffiecient evidence to prove m2g is not non-stationary


regression  <- summary(lm(inflation ~ m2g, data = Data))
regression1 <- lm(inflation ~ m2g, data = Data)
# Intercept: If m2g equals 0, it is associated with a decrease of 4.953e+03 units.
# For every one unit increase in m2g, it is associated with a price increase of 8.967e-01 units.
# The p-value is < 2.2e-16, suggesting that it has a positive effect of inflation on rent.
# The fit of the model is strong, shown by an R-Squared value 0.8618. 


bgtest(regression1, order = 1)  # for first-order autocorrelation
# Breusch-Godfrey test for autocorrelation. 
# LM test of 59.689, indicating that there is a strong serial correlation being shown
# With a p-value of 1.111e-14, we reject the null hypothesis on the 5% level,
# because we find enough statistical evidence of serial correlation.

# Include lags for solution
coeftest(regression1, vcov = NeweyWest(regression1))
# After running the NeweyWest test, we find that if m2g equals 0,
# it is associated now with a decrease of 4952.87451 units. 
# For every one unit increase in m2g, it is associated with an increase of inflation by 8.967e-01 units.
# With a change in p-value to 0.2434, we would fail to reject the null hypothesis
# on the 5% level, because we do not have sufficient evidence to indicate that 
# m2g influences inflation.

result <- grangertest(inflation ~ m2g, order = 3, data = Data)
# Model 1 includes the past 3 lags of inflation and m2g
# Model 2 includes the past 3 lags of inflation only
# With a p-value of 0.03235, it can indicate that the two models are 
# are statistically different at the 5% level, usggesting that the lagged
# values of m2g improved the predication of inflation.

acf(Data$inflation, na.action = na.pass)
#4 lags
pacf(Data$inflation, na.action = na.pass)
#1 lag



