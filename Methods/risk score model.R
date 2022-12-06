

# This will be a code to create a risk score model for food borne illness
# This script works in conjucntion with other scripts from our project; 
# all of which are publicly avilable on our github: 
# https://github.com/timhedspeth/PHP-2550-Final-Project 


# To implement this we will first: 
  # Read in the data and load packages 
  # Use the add_outbreak() function 


rm(list=ls())

#~~~~~~~~~~~~~~~~~~~~~#
## Packages and Data ## 
#~~~~~~~~~~~~~~~~~~~~~#

library(kableExtra)
library(tidyverse)
library(ggpubr)
library(mice) 
library(glmnet)  
library(bestglm)
library(pROC)
library(DescTools)
library(caret)
library(kableExtra)
library(ggpubr)


setwd("~/Desktop/Semester_3/Practical/Final") # Where all our data is 
source("Outbreak_function.R") # The function we will use 

# Read in the data sets 
ecoli <- read.csv("final_ecoli.csv")
campylobacter  <- read.csv("final_campylobacter.csv")
salmonella <- read.csv("final_salmonella.csv")


#~~~~~~~~~~~~#
## Cleaning ##
#~~~~~~~~~~~~#

# Restrict to complete cases
ecoli <- ecoli %>% filter(!is.na(month) & !is.na(year) & !is.na(Min.same)  & !is.na(Strain)) 


campylobacter <- campylobacter %>% filter(!is.na(month) & !is.na(year) & 
                                          !is.na(Min.same)  & 
                                          !is.na(SNP.cluster) & !is.na(Isolation.type)  
                                          & !is.na(Assembly)& !is.na(AMR.genotypes))  %>% 
                                   select(-c(Min.diff))


#apply(campylobacter, 2, function(x){return(sum(is.na(x))/length(x))}) #All clean


#apply(salmonella, 2, function(x){return(sum(is.na(x))/length(x))}) #All clean


salmonella <- salmonella %>% filter(!is.na(month) & !is.na(year) & 
                                      !is.na(Min.same)  & !is.na(Serovar) &  
                                      !is.na(Isolation.type)  & !is.na(AMR.genotypes) & 
                                      !is.na(Computed.types)) %>% 
                                select(-c(Min.diff))

## Add outbreak ## 

ecoli <-  add_outbreak(ecoli)
campylobacter <- add_outbreak(campylobacter)
salmonella <- add_outbreak(salmonella)

## Delete the variables that are used to determine outbreak ## 

ecoli <- ecoli %>% select(-c(year, Location, Min.same, day, year, 
                             month_year, region))
campylobacter <- campylobacter %>% select(-c(year, Location, Min.same, 
                                             month_year, region, Assembly))

salmonella <- salmonella %>% select(-c(year, Location, Min.same, 
                                             month_year, region))


#~~~~~~~~~~~~~~~~~~~~#
## Lasso Regression ##
#~~~~~~~~~~~~~~~~~~~~#



ecoli[] <- lapply(ecoli, function(x){return(as.factor(x))})
campylobacter[] <- lapply(campylobacter, function(x){return(as.factor(x))})
salmonella[] <- lapply(salmonella, function(x){return(as.factor(x))})


#apply(ecoli, 2, function(x){return(sum(is.na(x))/length(x))})  # all complete
                                                                # uncomment to validate



# Make the risk score models ## 

# E coli 
ecoli_coef <- lasso(ecoli, 10)
ecoli_coef <- as.data.frame(as.matrix(ecoli_coef))
eco_coef <- as.vector(ecoli_coef$s1)
ecoli_coefs <- round(ecoli_coef$s1/median(eco_coef[eco_coef != 0]))  
ecoli_coef$s1 <- ecoli_coefs

# Campylobacter
campylobacter_coef <- lasso(campylobacter,10) 
campylobacter_coef <- as.data.frame(as.matrix(campylobacter_coef))
camp_coef <- as.vector(campylobacter_coef$s1)
campylobacter_coefs <- round(campylobacter_coef$s1/median(camp_coef[camp_coef != 0]))
campylobacter_coef$s1 <- campylobacter_coefs

# Salmonella  
salmonella_coef <- lasso(salmonella, 10)
salmonella_coef <- as.data.frame(as.matrix(salmonella_coef))
sal_coef <- as.vector(salmonella_coef$s1)
salmonella_coefs  <- round(salmonella_coef$s1/median(sal_coef[sal_coef != 0]))
salmonella_coef$s1 <- salmonella_coefs



## Evaluate the model ##

variables1 <- model.matrix(outbreak~., ecoli)
variables2 <- model.matrix(outbreak~., campylobacter)
variables3 <- model.matrix(outbreak~., salmonella)


## Append the scores ## 

ecoli$score <- variables1 %*% ecoli_coefs
campylobacter$score <- variables2 %*% campylobacter_coefs
salmonella$score <- variables3 %*% salmonella_coefs


# How does the score do? # 

mod_ecoli <- glm(outbreak ~ score, data=ecoli, family = quasibinomial()) 
mod_campylobacter <- glm(outbreak ~ score, data=campylobacter, family = quasibinomial()) 
mod_salmonella <- glm(outbreak ~ score, data=salmonella, family = quasibinomial())



# 

threshhold <- .5 

ecoli$precited <- predict(mod_ecoli, type = "response")
salmonella$predicted <- predict(mod_salmonella, type = "response") 

# Ecoli 
risk_perfomance_ecoli <- roc(outbreak ~ precited, data=ecoli)
auc(risk_perfomance_ecoli)
ggroc(risk_perfomance_ecoli)

# Salmonella
risk_perfomance_salmonella <- roc(outbreak ~ predicted, data=salmonella)
auc(risk_perfomance_salmonella)
ggroc(risk_perfomance_salmonella) +
  theme_minimal() + 
  geom_abline(intercept = 1, slope = 1,
              color = "darkgrey", linetype = "dashed") +
  labs(title = "ROC curve") #+ 
  #scale_color_discrete(name = "Model", 
     #                  labels = c("Lasso, Clinical RS", 
       #                           "Ridge, Clinical RS"))

confusionMatrix(as.factor(ifelse(salmonella$predicted > threshhold, 
                                 1,0)), salmonella$outbreak)
BrierScore(mod_salmonella)



## For the poster we will only present on the salmonella

# We will vary the input parameters by \pm 5% to see how this impacts the


# Lower by 10% 
salmonella_lower <- read.csv("final_salmonella.csv")
salmonella_lower <- salmonella_lower %>% filter(!is.na(month) & !is.na(year) & 
                                      !is.na(Min.same)  & !is.na(Serovar) &  
                                      !is.na(Isolation.type)  & !is.na(AMR.genotypes) & 
                                      !is.na(Computed.types)) %>% 
                              select(-c(Min.diff))
salmonella_lower <- add_outbreak(salmonella_lower, 18, 6.3)
salmonella_lower <- salmonella_lower %>% select(-c(year, Location, Min.same, 
                                       month_year, region))
salmonella_lower[] <- lapply(salmonella_lower, function(x){return(as.factor(x))})

salmonella_lower_coef <- lasso(salmonella_lower, 10)
salmonella_lower_coef <- as.data.frame(as.matrix(salmonella_lower_coef))
salmonella_lower_coefs  <- round(salmonella_lower_coef$s1/median(salmonella_lower_coef$s1 != 0 ))
salmonella_lower_coef$s1 <- salmonella_lower_coefs



salmonella_lower$score <- model.matrix(outbreak~., salmonella_lower) %*% salmonella_lower_coefs
mod_salmonella_lower <- glm(outbreak ~ score, data=salmonella_lower, 
                            family = quasibinomial())

salmonella_lower$predicted <- predict(mod_salmonella_lower, type="response")
risk_perfomance_salmonella_lower <- roc(outbreak ~ predicted, data=salmonella_lower)
auc(risk_perfomance_salmonella_lower)


# Raise by 10% 

salmonella_upper <- read.csv("final_salmonella.csv")
salmonella_upper <- salmonella_upper %>% filter(!is.na(month) & !is.na(year) & 
                                                  !is.na(Min.same)  & !is.na(Serovar) &  
                                                  !is.na(Isolation.type)  & !is.na(AMR.genotypes) & 
                                                  !is.na(Computed.types)) %>% 
  select(-c(Min.diff))
salmonella_upper <- add_outbreak(salmonella_upper, 22, 7.7)
salmonella_upper <- salmonella_upper %>% select(-c(year, Location, Min.same, 
                                                   month_year, region))
salmonella_upper[] <- lapply(salmonella_upper, function(x){return(as.factor(x))})

salmonella_upper_coef <- lasso(salmonella_upper, 10)
salmonella_upper_coef <- as.data.frame(as.matrix(salmonella_upper_coef))
salmonella_upper_coefs  <- round(salmonella_upper_coef$s1/median(salmonella_upper_coef$s1 != 0 ))
salmonella_upper_coef$s1 <- salmonella_upper_coefs



salmonella_upper$score <- model.matrix(outbreak~., salmonella_upper) %*% salmonella_upper_coefs
mod_salmonella_upper <- glm(outbreak ~ score, data=salmonella_upper, 
                            family = quasibinomial())

salmonella_upper$predicted <- predict(mod_salmonella_upper, type="response")
risk_perfomance_salmonella_upper <- roc(outbreak ~ predicted, data=salmonella_upper)
auc(risk_perfomance_salmonella_upper)











ggroc(risk_perfomance_salmonella_lower) +
  theme_minimal() + 
  geom_abline(intercept = 1, slope = 1,
              color = "darkgrey", linetype = "dashed") +
  labs(title = "ROC curve")



# Raise by 10% 
salmonella <- read.csv("final_salmonella.csv")
salmonella <- salmonella %>% filter(!is.na(month) & !is.na(year) & 
                                      !is.na(Min.same)  & !is.na(Serovar) &  
                                      !is.na(Isolation.type)  & !is.na(AMR.genotypes) & 
                                      !is.na(Computed.types)) %>% 
                             select(-c(Min.diff))
salmonella <- add_outbreak(salmonella, 22, 7.7)



