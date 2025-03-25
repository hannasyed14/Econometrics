#Hanna Syed DAT 411 Exam 1
library("ggplot2")
library("stargazer")

#Import the data  
data <- read.csv("/Users/hannasyed/Desktop/DAT 411 – Exam 1- Fall 2024.csv")
str(data)



#Get summary statistics of ER and ERM and discuss them. 
short <- data[,c("Games","ERM")]
stargazer(short, type = "text")
#On average, the excess return of games will differ from the mean value of 1.129 by 8.917.
#On average, the excess return measure will differ from the mean value of 0.685 by 5.332.
#The number of excess return of games has a larger standard deviation, indicating that this data is more widespread.
#The ERM has a smaller standard deviation, indicating that this data is more narrowly spread about the mean.
# The variation in excess return of games is larger than ERM


#Conduct a hypothesis test of the population mean for ER and ERM 
#𝐻0:𝜇=0,𝐻𝑎:𝜇≠0
test1 <- t.test(short$Games, mu = 0, alternative = "two.sided")
#We reject the null hypothesis at 5% because we have statistical evidence to suggest that the true mean number of excess returns of games is not equal to 0 
test1ERM <- t.test(short$ERM, mu = 0, alternative = "two.sided")
#We reject the null hypothesis at 5% because we have statistical evidence to suggest that the true mean number of ERM is not equal to 0 

#𝐻0: 𝜇≥0,𝐻𝑎:𝜇<0
test2 <- t.test(short$Games, mu = 0, alternative = "less")
# We fail to reject the null hypothesis because we have statistical evidence to suggest that the true mean number of excess returns of games is less than 0
test2ERM <- t.test(short$ERM, mu = 0, alternative = "less")
# We fail to reject the null hypothesis because we have statistical evidence to suggest that the true mean number of ERM is less than 0

#𝐻0: 𝜇≤0,𝐻𝑎:𝜇>0
test3 <- t.test(short$Games, mu = 0, alternative = "greater")
#We reject the null hypothesis because we have statistical evidence to suggest the true mean number of excess returns of games is greater than 0
test3ERM <- t.test(short$ERM, mu = 0, alternative = "greater")
#We reject the null hypothesis because we have statistical evidence to suggest the true mean number of ERM is greater than 0



#Run the following Regression (CAPM) E Rport ,i=β0 + β1 ER M i +ε
reg <- lm(data$Games ~ data$ERM)
summary(reg)



#Discuss R-squared, and Adjusted R-squared 
#Our model can explain 68.66% of the variance of Games excess returns. 
#Accounting for the adjusted loss of degree of freedom our model can explain 68.64% of variance of Games excess returns 



#Interpreted the coefficient
#Intercept: If ERM equals 0, it is associated with an expected number of Games excess returns being approximately 0.1789.
#For every one unit increase in ERM, the number of Games excess returns is expected to increase by 1.3858



#conduct the following individual hypothesis test: 𝐻0: 𝛽1=0,𝐻𝑎:𝛽1≠0
#t = 1.3858/0.0273 = 50.76 
#We reject the null hypothesis at 5% because we find statistical evidence that a change in ERM is associated with a change in the excess returns of games.
#This means we have evidence that an increase in ERM is associated with playing more games.



#Save the predicted values. Create a scatter plot of the predicted values and ERM and a line graph of predicted values and ERM in the same graph. Discuss this graph.   
data$predicted <- predict(reg, data)
ggplot(data, aes(x = ERM)) + 
  geom_point(aes(y = Games), color = 'blue') +  # Actual values
  geom_line(aes(y = predicted), color = 'pink') +  # Predicted values
  labs(title = "Actual vs. Predicted Values",
       x = "ERM",
       y = "Games") + 
  theme_minimal() + 
  theme(legend.position = "bottom")
#The points tend to follow the trend of the line indicating the model being a good fit meaning that ERM is a good predictor of Games
# Many of the game values are clustered together suggesting that that relationship between ERM and Games is consisstent but maybe only works well within that range of the cluster



#Run the following Regression (FF3):  
#𝐸𝑅𝑝𝑜𝑟𝑡,𝑖=𝛽0+𝛽1𝐸𝑅𝑀𝑖+𝛽2𝑆𝑀𝐵𝑖+𝛽3𝐻𝑀𝐿𝑖+𝜖𝑖
reg2 <- lm(data$Games ~ data$ERM + data$SMB + data$HML)
summary(reg2)



#Compare Rsquared and Adj Rsquared with the previous model.
#Our model can explain 70.68% of the variance of Games excess returns. 
#Accounting for the adjusted loss of degree of freedom our model can explain 70.60% of variance of Games excess returns 
#The seccond model has a higher r^2 an adjusted r^2 suggesting that the additional variables SMB and HML explain more of the variablility in Games
#The addition of the additional independent variables slighlty improves the model by roughly 2%
#While the first model does capture the variance, the addition variables imrpoves the model overall


#Interpreted the coefficient
#Intercept: If ERM, SMB, and HML all equal 0, it is associated with an expected number of Games excess returns being approximately 0.12987
#ERM: For every one unit increase in ERM, the number of Games excess returns is expected to increase by 1.29416, holding all else constant.
#SMB: For every one unit increase in SMB, the number of Games excess returns is expected to increase by 0.38611, holding all else constant.
#HML: For every one unit increase in HML, the number of Games excess returns is expected to increase by 0.12672, holding all else constant.



#conduct the following individual hypothesis test: 
#𝐻0: 𝛽1=0,𝐻𝑎:𝛽1≠0
#t = 1.29416/0.02842 = 45.535
#We reject the null hypothesis at the 5% significance level because we have statistical evidence that a change in ERM is associated with a change in the number of excess return of games.

#𝐻0: 𝛽2=0,𝐻𝑎:𝛽2≠0
#t = 0.38611/0.04681 = 8.249
#We reject the null hypothesis at the 5% significance level because we have statistical evidence that a change in SMB is associated with a change in the number of excess return of games.

#𝐻0: 𝛽3=0,𝐻𝑎:𝛽3≠0
#t = 0.12672/0.04062 = 3.119
#We reject the null hypothesis at the 5% significance level because we have statistical evidence that a change in HML is associated with a change in the number of excess return of games.

#All three variables are statistically significant in explaining the variance in excess return of games



