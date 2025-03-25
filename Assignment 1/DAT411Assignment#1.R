#Hanna Syed and Plu Reh

rm(list = ls())

data <- read.csv("/Users/hannasyed/Desktop/HeightWeight.csv")
str(data)

# HEIGHT SUMMARY----------------------
meanH <- mean(data$Height)
sdH <- sd(data$Height)
minH <- min(data$Height)
maxH <- max(data$Height)
rangeH <- maxH - minH

# WEIGHT SUMMARY-----------------------
meanW <- mean(data$Weight)
sdW <- sd(data$Weight)
minW <- min(data$Weight)
maxW <- max(data$Weight)
rangeW <- maxW - minW

#On average, the height will differ from the mean value of 67.99 by 1.90 
#On average, the weight will differ from the mean value of 127.08 by 11.66
#Weight has a larger standard deviation indicating the data is more widespread.
#Height has a smaller standard deviation indicating the data is more narrowly spread about the mean. 
#The variation in weight is greater than height. 


#CORRELATION---------------------------------
correlation <- cor(data$Height, data$Weight)

#The correlation coefficient of 0.50 indicates a moderately strong positive correlation between height and weight.

#HYPOTHESIS TESTING-----------------------------
#a.Height 
#i. H0 : μ=77 , Ha : μ ≠77
t.test(data$Height, mu = 77, alternative = "two.sided")
#Reject null hypothesis because p-value < 2.2e-16
#ii. H0 : μ ≤77 , Ha : μ>77
t.test(data$Height, mu = 77, alternative = "greater")
#Fail to reject null hypothesis because p-value = 1
#iii. H0 : μ ≥77 , Ha : μ<77
t.test(data$Height, mu = 77, alternative = "less")
#Reject null hypothesis because p-value < 2.2e-16

#b. Weight 
#i.H0 : μ=215 , Ha : μ ≠215
t.test(data$Weight, mu = 215, alternative = "two.sided")
#Reject null hypothesis because p-value < 2.2e-16
#ii. H0 : μ ≤215 , Ha : μ>215
t.test(data$Weight, mu = 215, alternative = "greater")
#Fail to reject null hypothesis because p-value = 1
#iii. H0 : μ ≥215 , Ha : μ<215
t.test(data$Weight, mu = 215, alternative = "less")
#Reject null hypothesis because p-value < 2.2e-16

#REGRESSION----------------------------------
Regression <- lm(data$Height ~ data$Weight)
summary(Regression)

#Height = 57.57 + 0.082(weight) + e
#A one unit increase in weight is associated with a 0.082 unit increase in height
#57.57 is the expected height if weight was 0. The y intercept has no pratical meaning in this scenario as the height being 0 is not in our real life range of height 
#Our model can explain 0.2529 value amount of variance of the weight
#Accounting for the adjusted loss of degree of freedom our model can explain 0.2528 amount of variance of weight

