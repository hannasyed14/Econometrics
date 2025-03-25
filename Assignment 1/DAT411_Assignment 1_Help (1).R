rm(list=ls(all=TRUE))
library(stargazer)

setwd(paste0("C:/Users/",Sys.info()['user'],"/Dropbox/BAN 609/Data/"))

### Import data ####
data <- read.csv("NBADataSet.csv")
data_small <- data.frame(data$height, data$weight)
### Summary Statistics ###
colnames(data_small) <- c("height","weight")

summary <- stargazer(data_small, 
          title="Summary Statistics", 
          label="tab:summary", 
          table.placement = "ht", 
          type = "text",
          digits=2)


#Summary Statistics
#=======================================
#  Statistic    N   Mean  St. Dev. Min Max
#---------------------------------------
#  data.height 382 197.85   8.55   173 215
#  data.weight 382 99.67   12.00   72  130
#---------------------------------------

cor <- cor(data_small)

##             data.height data.weight
## data.height   1.0000000   0.8345153
## data.weight   0.8345153   1.0000000




ttest_twotailed_h <- t.test(data_small$height, mu = 195, alternative = "two.sided")
ttest_righttailed_h <- t.test(data_small$height, mu = 195, alternative = "greater")
ttest_lefttailed_h <- t.test(data_small$height, mu = 195, alternative = "less")


ttest_twotailed_w <- t.test(data_small$weight, mu = 97, alternative = "two.sided")
ttest_righttailed_w <- t.test(data_small$weight, mu = 97, alternative = "greater")
ttest_lefttailed_w <- t.test(data_small$weight, mu = 97, alternative = "less")


#### Part 2 ####

### Create Matrix ####
one <- matrix(nrow = 382, ncol = 1, 1)
weight <- as.matrix(data$weight)
height <- as.matrix(data$height)
y <- weight
x <- cbind(one,height)

### Solve OLS ####

beta_hat <- solve(t(x) %*% x) %*% t(x) %*% y

print(beta_hat)


### Easy Way ####

result <- lm(weight ~ height, data = data)


### Wow they match #####



