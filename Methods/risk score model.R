

# This will be a code to create a risk score model for food borne illness
# This script works in conjucntion with other scripts from our project; 
# all of which are publicly avilable on our github: 
# https://github.com/timhedspeth/PHP-2550-Final-Project 


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


# Add the outbreak to all of the data sets, using 
# the function with the parameters specified by Dr. Julian 

ecoli <-  add_outbreak(ecoli)

# Remove variables that could causes issues 
ecoli1  <- ecoli[,-c(1,3,4,5,6,7,8,9,13,14,15,16,17,18,23)]
ecoli1[] <- lapply(ecoli1, function(x){return(as.factor(x))})

apply(eco, 2, function(x){return(sum(is.na(x))/length(x))})

ecoli1 <- ecoli1 %>% filter(!is.na(Min.same))

eco <- ecoli1 %>% filter(!is.na(month) & !is.na(Strain))

lasso(eco, 6)
