
# This will be a walk through of the salmonella risk score model code # 


#~~~~~~~~~~~~~~~~~~~~~#
## Data and packages ##
#~~~~~~~~~~~~~~~~~~~~~#


rm(list=ls())

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
salmonella <- read.csv("final_salmonella.csv")

#~~~~~~~~~~~~~~~~~~~~~~~~#
## Define the functions ## 
#~~~~~~~~~~~~~~~~~~~~~~~~#

add_outbreak <- function(dataset, numbercases = 20, similarity = 7){
  #' This function will create a binary variable to identify if a case was part of a user defined outbreak
  #' 
  #'@param data, the data frame of interest, must contain 
  #'@param numbercases, the number of cases in your outbreak 
  #'@param similarity, how closely related do you want you want the strains to be 
  
  dataset <- as.data.frame(dataset)
  
  
  # Try to catch an inappropriate data frame 
  if(!("month" %in% names(dataset) | "year" %in% names(dataset) | 
       "region" %in% names(dataset) | "Min.same" %in% names(dataset))){
    warning("It appears you don't have the proper columns, please make sure you
            have month, year, region, and Min.same as columns")
  }
  
  # Find the number of cases in an outbreak   
  data_subset <- dataset %>%
    filter(!is.na(month) & (region  != "USA, General")) %>% 
    group_by(year, month, SNP.cluster) %>% 
    summarize(#mean1 = round(mean(Min.same, na.rm = T),2), 
      #sd1 = round(sd(Min.same, na.rm = T),2),
      num = n())  %>% 
    ungroup() %>% 
    # Substantial number of cases that are very closely related 
    filter(num > numbercases)  %>% 
    mutate(outbreak = 1)
  #pivot_wider(names_from = region, values_from = c(mean1, sd1))
  
  # Join inoutbreak information
  dataset <- left_join(dataset, data_subset, by= c("year", "month", "SNP.cluster")) %>%
    select(-c(num))
  dataset$outbreak[is.na(dataset$outbreak)] <- 0
  
  # What do we return? 
  if(sum(dataset$outbreak ==1) == 0){
    return("There appear to be no outbreaks in this data based on your paramters")
  } else{
    return(dataset)
  }
  
  
}


#~~~~~~~~~~~~~~~~~~#
## Lasso Function ## 
#~~~~~~~~~~~~~~~~~~# 

# This function was adapted from a code provided by Alice Paul, PhD, the 
# instructor for  this course 

lasso <- function(df,numtimes, prob = .3) { 
  #' Runs 10-fold CV for lasso and returns corresponding coefficients 
  #' @param df, data set
  #' @return coef, coefficients for minimum cv error
  
  # Matrix form for ordered variables 
  x.ord <- model.matrix(outbreak~., data = df)[,-1] 
  y.ord <- df$outbreak

  # Generate folds
  k <- numtimes 
  #set.seed(1) # consistent seeds between imputed data sets
  folds <- as.numeric(salmonella$month)
  
  
  #  This function will auto do weighting 

  prop0 <-sum(df$outbreak == 0)/nrow(df)
  prop1 <- sum(df$outbreak == 1)/nrow(df)
  modweight <- ifelse(df$outbreak == 0, 
                      1/prop0, 
                      1/prop1)
  
  # Lasso model
  #if(prop0 > prob & prop1 > prob){
  #lasso_mod <- cv.glmnet(x.ord, y.ord, nfolds = numtimes, foldid = folds, 
  #                      alpha = 1, family = "binomial") 
  #} else{
  lasso_mod <- cv.glmnet(x.ord, y.ord, nfolds = 12, foldid = folds, 
                         alpha = 1, family = "binomial", weights = modweight) 
  #}
  
  # Get coefficients 
  coef <- coef(lasso_mod, lambda = lasso_mod$lambda.min) 
  return(coef) 
} 



## We will do a complete case analysis ##

salmonella <- salmonella %>% filter(!is.na(month) & !is.na(year) & 
                                      !is.na(Min.same)  & !is.na(Serovar) &  
                                      !is.na(Isolation.type)  & !is.na(AMR.genotypes) & 
                                      !is.na(Computed.types) & !is.na(SNP.cluster)) %>% 
                              select(-c(Min.diff, Location))

# We will add an outbreak to the model 
salmonella <- add_outbreak(salmonella)
lapply(salmonella, function(x) sum(is.na(x))/length(x))

#salmonella <- add_outbreak(salmonella)

# Reclean the data 

SNP_levels_df <- as.data.frame(table(as.factor(salmonella$SNP.cluster)))
SNP_included <- SNP_levels_df  %>% filter(Freq > 300) # Remove
levels_SNP <- as.vector(SNP_included$Var1) 
salmonella$SNP.cluster <- case_when(salmonella$SNP.cluster %in% levels_SNP ~ salmonella$SNP.cluster, 
                                    is.na(salmonella$SNP.cluster) ~ salmonella$SNP.cluster,
                                    !(salmonella$SNP.cluster %in% levels_SNP) ~ "Other")

AMR_levels_df <- as.data.frame(table(as.factor(salmonella$AMR.genotypes)))
AMR_included <- AMR_levels_df  %>% filter(Freq > 200) # Remove
levels_AMR <- as.vector(AMR_included$Var1) 
salmonella$AMR.genotypes <- case_when(salmonella$AMR %in% levels_AMR ~ salmonella$AMR, 
                                      is.na(salmonella$AMR) ~ salmonella$AMR,
                                      !(salmonella$AMR %in% levels_AMR) ~ "Other")



salmonella <- salmonella %>% select(-c(year,  Computed.types))

salmonella[] <- lapply(salmonella, function(x){return(as.factor(x))})

salmonella_coef <- lasso(salmonella, 10)
salmonella_coef <- as.data.frame(as.matrix(salmonella_coef))
sal_coef <- as.vector(salmonella_coef$s1)
salmonella_coefs  <- round(salmonella_coef$s1/median(sal_coef[sal_coef != 0]))
salmonella_coef$s1 <- salmonella_coefs


variables3 <- model.matrix(outbreak~., salmonella)
salmonella$score <- variables3 %*% salmonella_coefs

mod_salmonella <- glm(outbreak ~ score, data=salmonella, family = quasibinomial())

threshhold <- .5  # For confusion matrices  
salmonella$predicted <- predict(mod_salmonella, type = "response") 

risk_perfomance_salmonella <- roc(outbreak ~ predicted, data=salmonella)
auc(risk_perfomance_salmonella)
BrierScore(mod_salmonella)

ggroc(risk_perfomance_salmonella) +
  theme_minimal() + 
  geom_abline(intercept = 1, slope = 1,
              color = "darkgrey", linetype = "dashed")



## Normal logistic regression 
salmonella <- salmonella[,-c(10:11)]
mod <- glm(outbreak ~ ., data=salmonella, family=quasibinomial())
salmonella$predicted <- predict(mod, type ="response")

risk_perfomance_salmonella <- roc(outbreak ~ predicted, data=salmonella)
auc(risk_perfomance_salmonella)
BrierScore(mod_salmonella)

ggroc(risk_perfomance_salmonella) +
  theme_minimal() + 
  geom_abline(intercept = 1, slope = 1,
              color = "darkgrey", linetype = "dashed")




## Exploration of data ## 


table(salmonella$outbreak)
167/(14882+167)

ggplot(data=salmonella, aes(x=as.factor(month), y  = outbreak)) + 
  geom_point()

# 3 outbreaks, 3 different months 

salmonella  %>% group_by(Isolation.source.category, outbreak) %>% summarize(num =n()) 
# outbreaks

salmonella  %>% group_by(month, outbreak) %>% summarize(num =n()) 

salmonella  %>% group_by(region, outbreak) %>% summarize(num =n()) 

salmonella  %>% group_by(SNP.cluster, outbreak) %>% summarize(num =n()) 

salmonella  %>% group_by(AMR.genotypes, outbreak) %>% summarize(num =n()) 

salmonella  %>% group_by(Serovar, outbreak) %>% summarize(num =n()) 

salmonella  %>% group_by(Isolation.type, outbreak) %>% summarize(num =n()) 

salmonella  %>% group_by(Computed.types, outbreak) %>% summarize(num =n()) 


