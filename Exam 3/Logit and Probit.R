rm(list=ls(all=TRUE))
library(dplyr)
library(ggplot2)
#install.packages(c("MASS","nnet"))
library(MASS)
library(nnet)
setwd("C:/Users/mcgurkz/Dropbox/BAN 609/Data/")
options(max.print = 10000) 

### Import Data ###

NCAA <- read.csv("GameData.csv")


NCAA$CONFERENCE1 <-gsub("P10","P12",NCAA$CONFERENCE1)
NCAA$CONFERENCE2 <-gsub("P10","P12",NCAA$CONFERENCE2)

NCAA$CONFERENCE1 <-gsub("Ind","ind",NCAA$CONFERENCE1)
NCAA$CONFERENCE2 <-gsub("Ind","ind",NCAA$CONFERENCE2)


#### Create Variables ###
#### Win/Loss ####

NCAA$W <- ifelse(NCAA$OUTCOME1 == "W",1,0) 

### Creating Dummies ###
Location_dummies <- model.matrix(~ LOCATION1 - 1, data = NCAA)
Conference_dummies1 <- model.matrix(~ CONFERENCE1 - 1, data = NCAA)
Conference_dummies2 <- model.matrix(~ CONFERENCE2 - 1, data = NCAA)
Team_dummies <- model.matrix(~ TEAM1 - 1, data = NCAA)

NCAA <- cbind(NCAA,Location_dummies,Conference_dummies1,Team_dummies)

### Interaction Between Conferences ####
for (i in 1:ncol(Conference_dummies1)) {
  for (j in i:ncol(Conference_dummies2)) {
    if (i != j) {  # Ensure we don't create interaction terms for the same conference
      # Generate interaction term name
      interaction_name = paste("conf", gsub("CONFERENCE1", "", colnames(Conference_dummies1)[i]), 
                               gsub("CONFERENCE2", "", colnames(Conference_dummies2)[j]), sep = "_")
      
      # Create the interaction term
      NCAA[[interaction_name]] = Conference_dummies1[, i] * Conference_dummies2[, j]
    }
  }
}


### Check if We got it correct #####
print(mean(NCAA$W)) ### We know what the correct answer should be.... ####


### Model ####
# Find interaction term names
interaction_terms <- grep("conf", names(NCAA), value = TRUE)
interaction_string <- paste(interaction_terms, collapse = " + ")
# Full formula string
formula_string <- paste("W ~ SCORE1 + BAD1 + TURNOVER1 + FTR + OffReb +", interaction_string)
options(expressions = 50000)
model_formula <- as.formula(formula_string)

### Linear Probability Model ###

LPM <- lm(model_formula, data = NCAA)
predicted_probs_LPM <- data.frame(predict(LPM))
team_game <- NCAA[,c("TEAM1","TEAM2","OUTCOME1")]
team_game <- cbind(team_game,predicted_probs_LPM)
result_LPM <- summary(LPM)


### Logit ###
logit_model <- glm(model_formula, data = NCAA, family = binomial())
predicted_probs_logit <- predict(logit_model, type = "response")
team_game <- cbind(team_game,predicted_probs_logit)
Logit <- summary(logit_model)

exp(4.249e-02) ### SCORE 
exp(-1.442e-01) ## TO 
exp(-7.677e-01) ## MAAC to ACC 



## Probit ### 
probit_model <- glm(model_formula, data = NCAA, family = binomial(link = "probit"))
predicted_probs_probit <- predict(probit_model, type = "response")




