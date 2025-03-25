# Hanna Syed Exam 2 DAT 411 

rm(list = ls())
library("dplyr")
library("ggplot2")
library(lmtest)
library("sandwich")
library("tseries")
library("forecast")
library(fUnitRoots)
library("ARDL")

data <- read.csv("/Users/hannasyed/Desktop/ECO 455 - Exam 1 - Spring 2024.csv")
data <- na.omit(data)
str(data)
data$EMR <- as.numeric(data$EMR)
data$SMB <- as.numeric(data$SMB)
data$HML <- as.numeric(data$HML)
data$Other <- as.numeric(data$Other)



 ### Run the Regression ### 

regressionA <- lm(data$Other ~ data$EMR + data$SMB + data$HML)
regression1 <- summary(regressionA)



 ### Interpret ###

#R²: Our model can explain 91.5% of the variance in the dependent variable, Other.
#Adjusted R²: Accounting for the adjusted loss of degrees of freedom, our model can explain 91.48% of the variance in the dependent variable, Other. 
#Intercept: If the dependent variable equals 0, it is associated with the EMR, SMB, and HML factors increase of 0.06486 units.
#EMR: For every one unit increase in EMR, it is associated with the dependent variable increase of 1.04743 units.
#SMB: For every one unit increase in SMB, it is associated with the dependent variable increase of 0.06021 units.
#HML: For every one unit increase in HML, it is associated with the dependent variable increase of 0.38020 units.



 ### Hypothesis Test ##

#Ho: β = 0 (Coefficient has no impact on Others return)
#Ha: β != 0 (Does not equal 0) (Coefficient has an impact on Others return)

# EMR
#We reject the null hypothesis at the 1 percent level, and we find statistical evidence that changes in Excessive Market Returns (EMR) are associated with Others return.
# SMB
#We reject the null hypothesis at the 1 percent level, and we find statistical evidence that changes in Small Minus Big (SMB) are associated with Others return.
# HML 
#We reject the null hypothesis at the 1 percent level, and we find statistical evidence that changes in High Minus Low (HML) are associated with Others return.
# Overall Significance of Model Overall 
#We reject the null hypothesis at the 1 percent level, and we find statistical evidence of overall significnace that EMR,SMB,HML are jointly equal to 0 are associated with Others return. 



 ### Testing Joint Significance ###

regressionB <- lm(data$Other ~ data$EMR)
summary(regressionB)

anova(regressionA,regressionB)
#We reject the null at the one percent level, we find stat evidence that the coefficients on SMB and HMl are not jointly equal to 0.  
#We find statistical evidence that changes in Small Minus Big SMB and HML are associated with Others return. Our model of regression A that includes SMB and HMl is superior to regression B which only includes EMR



 ### Heteroscedasticity ###

#BP Test 
bptest(regressionA)
#We reject the null hypothesis of homoscedasticity at the 1 percent level and find evidence of heteroscedasticity. As such, we utilize heteroscedasticity corrected standard errors.

#White Test

data$residuals_sq <- residuals(regressionA)^2

white_model <- lm(residuals_sq ~ EMR + SMB + HML + I(EMR^2) + I(SMB^2) + I(HML^2) +  
                    I(EMR*SMB) + I(SMB*HML), data = data)

wtest <- lrtest(regressionA, white_model)
print(wtest)
#We reject the null hypothesis of homoscedasticity at the 1 percent level and find evidence of heteroscedasticity. As such, we utilize heteroscedasticity corrected standard errors.

regression2 <- coeftest(regressionA, vcov. = vcovHC(regressionA, type = "HC1"))
regression2
summary(regressionA)
#EMR: After correcting for heteroskedasticity, the standard error increased and made the t value smaller, but EMR is stil significant at the 1% level 
#SMB: After correcting for heteroskedasticity, the higher standard error increased and made the t value smaller and a larger p-value. Now we reject the null for SMB at the 10 percent level. SMB becomes less statistically significant
#HML: After correcting for heteroskedasticity, the standard error increased and made the t value smaller, but HML is stil significant at the 1% level 
#The main change with all variables is in the Std. Error increased after correction, which makes lower t values



 ### Serial Correlation ###

bgtest(regressionA)
# We reject the null hypothesis at the 1% level we find statistical evidence of serial correlation.

regression3 <- coeftest(regressionA, vcov = NeweyWest(regressionA))

regression1
regression2
regression3
#When accounting for both heteroscedasticity and serial correlation we fail to reject the null hypothesis for SMB in regression3, meaing that SMB is not statistically significant and that SMB has no effect on Others returns



 ### Stationary ### 

# ADF Test
variables <- c("EMR", "SMB", "HML", "Other")
results_C <- list()
for (i in variables) {
  ts_data <- ts(data[[i]])
  result <- adfTest(ts_data, type = "c")
  results_C[[i]] <- result@test[["p.value"]][[1]]
}
# We reject the null hypothesis at the 1% level, we find statisictal evidence that the all the constant variables are all stationary 



 ### Granger ###

acf(data$EMR, na.action = na.pass)
pacf(data$EMR, na.action = na.pass)
GrangerEMR <- grangertest(Other ~ EMR, order = 5, data = data)
# We fail to reject the null hypothesis, we find statistical evidence that EMR does not Granger cause Other's returns at 5 lags

acf(data$SMB, na.action = na.pass)
pacf(data$SMB, na.action = na.pass)
GrangerSMB <- grangertest(Other ~ SMB, order = 3, data = data)
#We fail to reject the null hypothesis, we find statistical evidence that SMB does not Granger cause Other's returns at 3 lags.

acf(data$HML, na.action = na.pass)
pacf(data$HML, na.action = na.pass)
GrangerHML <- grangertest(Other ~ HML, order = 1, data = data)
#We reject the null hypothesis at the 10% significance level, and we find statistical evidence that HML Granger-causes Other's returns at 1 lag.



 ### Times Series Model ###

acf(data$Other, na.action = na.pass)
pacf(data$Other, na.action = na.pass)

acf(data$HML, na.action = na.pass)
pacf(data$HML, na.action = na.pass)

#AR
model1 <- Arima(data$Other, order=c(5,0,0))
#ARMA
model2 <- Arima(data$Other, order=c(3,0,0))

#ARDL 
ardl_model <- ardl(Other ~ HML, order = c(3,1),
 start = 1, end =nrow(data),
 data = data)

ardl_model2 <- ardl(Other ~ HML, order = c(5,1),
                   start = 1, end =nrow(data),
                   data = data)

ardl_model3 <- ardl(Other ~ HML, order = c(5,2),
                   start = 1, end =nrow(data),
                   data = data)

models <- list(model1, model2,ardl_model)
aic_values <- sapply(models, AIC)
bic_values <- sapply(models, BIC)
# The ARDL model is the best because both the AIC and BIC is the smallest 

models <- list(ardl_model, ardl_model2, ardl_model3)
aic_values <- sapply(models, AIC)
bic_values <- sapply(models, BIC)
# Of the ARDL models, Model 2 is the best as it has the lowest AIC and BIC 
# ardl_model2 includes 5 lags of the Other return (dependent variable) and 1 lag of HML (independent varivle)





