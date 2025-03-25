#Hanna Syed and Plu Reh Assignment 2 Part 1

#(1)-----------------------------------------------------------------
# The issue with soley relying on r^2 is that you can not compare r^2 against eachother unless it is between the same dependent variable. 
# Another issue is that r^2 can be driven by the variance of the error term. 
# Furthermore by adding more independent variables will typically increase r^2, so it is not an accurate depicition of if the model is more accurate or not for the real world. 
# By adding more variables it creates so much noise to skew the r^2

# I will be using the data set called UsedCar under the data tab in D2L to demonstrate
data <- read.csv("/Users/hannasyed/Desktop/UsedCar.csv")
str(data)

#Creating a regression model to get the relationship between Length and Width of a used car
# Length being the dependent variable and Width being the independant 
model1 <- lm(data$Length~data$Width)
summary(model1)
# r^2 = 0.5047
# Adding Horsepower as another independent variable to create noise 
model2 <- lm(data$Length~data$Width + data$Horsepower)
summary(model2)
#r^2 = 0.5049
# Horsepower is an irrelevant variable to the regression model however it increased the r^2
# This shows the problem with r^2 by showing that adding more independent variables, even if the variable does not make sense for the regression, influences r^2 with no true meaning 

#(2)-----------------------------------------------------------------

#1 Linear in Coefficients - the relationship between indep. and dep. variables are linear and if not then regression will be inaccurate 
summary(model1)
#The p value is < 2.2e-16 which is less than the significance value of 0.05
#Lower p value indicates linearity is violated
# y = e^B0 x^B1 e^Ei.  (not linear)
# take log to make linear----> lny = B0 + Bln(x) + E


#2 Random Sample - every element of population has an equal probability of being selected to avoid bias 
# The data includes 157 obs which is > 30. If the data set of 157 was chosen out of all Used Cars to exist with equal probability then the data is randomly sampled


#3 No Perfect Collinearity - variable should not be perfectly explained by another set of variables 
data$Collinear <- 2 * data$Length #making a column of data that is twice the length so that is a perfect linear function of Length
modelcollinearity <- lm(data$Length ~ data$Width + data$Collinear)
summary(modelcollinearity)
#The warning message in the summary indicates that collinearity is present
#It cant distinguish between two variables


#4 Homoscedasticity - Variances of error term given all other x's equal to variance of error term 
model1 <- lm(data$Length~data$Width)
plot(model1$fitted.values, residuals(model1),
     xlab = "Fitted Value",  
     ylab = "Residulal",  
     main = "")  
# By making a residual plot for model1 we can determine if it is Homoscedasticity or Heteroscedasticity
#Since there is no pattern in the residuals the data displays homoscedasticity 


#5 No Serial Correlation - Current value of error term is not a function of past error term
#For the Used Car Scenario, horsepower could affect the price of the car today
#but also can effect the price of another car later which creates a pattern in the error term which leads to serial correlation 


#6 No Relationship between Regressors and Error Term - E[E|X] = 0 Don't miss important variables correlated to dependent variable
# if we dont include the horsepower of the car in predicting a Used Car's price, the residuals and model could be correlated to the other varibles included 
# This would lead to innaccurate results as the regressors would be connected to the error term 





