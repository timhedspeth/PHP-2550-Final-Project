

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



#~~~~~~~~~~~~~~~~~~~~#
## Lasso Regression ##
#~~~~~~~~~~~~~~~~~~~~#




# Since the outbreak is a function of region and month year we will delete it 
# The outbreak function requires Min.Same 

ecoli <- ecoli %>% filter(!is.na(month) & !is.na(year) & !is.na(Min.same)) 


# Add the outbreak to all of the data sets, using 
# the function with the parameters specified by Dr. Julian 
  
ecoli <-  add_outbreak(ecoli)

# We will drop the day and go to complete cases for strain 
ecoli <- ecoli %>% select(-c(day, month_year, Location, region, Min.same)) %>% filter(!is.na(Strain))


ecoli[] <- lapply(ecoli, function(x){return(as.factor(x))})

#apply(ecoli, 2, function(x){return(sum(is.na(x))/length(x))})  # all complete
                                                                # uncomment to validate



# Make the risk score model 
ecoli_coef <- lasso(ecoli, 10)
ecoli_coef <- as.data.frame(as.matrix(ecoli_coef))

ecoli_coefs <- round(ecoli_coef$s1/median(ecoli_coef$s1 != 0))  

ecoli_coef$s1 <- ecoli_coefs


## Evaluate the model ##

variables1 <- model.matrix(outbreak~., ecoli)

ecoli$score <- variables1 %*% ecoli_coefs

# How does the score do?

mod <- glm(outbreak ~ score, data=ecoli, family = quasibinomial()) 

ecoli$precited <- predict(mod, type = "response")
threshold <- .5 

clinical_perfomance <- roc(outbreak ~ precited, data=ecoli)
auc(clinical_perfomance)

ggroc(clinical_perfomance)




