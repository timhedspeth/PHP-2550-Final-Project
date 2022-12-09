
## ECOLI ## 


# This will be a walk through of the ecoli risk score model code # 


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
ecoli <- read.csv("final_ecoli.csv")

#~~~~~~~~~~~~~~~~~~~~~~~~#
## Define the functions ## 
#~~~~~~~~~~~~~~~~~~~~~~~~#

add_outbreak <- function(dataset, numbercases = 10, similarity = 7){
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
  folds <- as.numeric(df$month)
  
  
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

ecoli <- ecoli %>% filter(!is.na(month) & !is.na(year) & 
                          !is.na(SNP.cluster)  & !is.na(Strain)) %>%
                   select(-c(day, Min.same))



# We will add an outbreak to the model 
ecoli <- add_outbreak(ecoli, 15)
#lapply(ecoli, function(x) sum(is.na(x))/length(x))

#ecoli <- add_outbreak(ecoli)

# Reclean the data 

SNP_df <- as.data.frame(table(as.factor(ecoli$SNP.cluster)))
snp_included <- SNP_df %>% filter(Freq > 100)
levels_snp <- as.vector(snp_included$Var1)
ecoli$SNP.cluster <- case_when(ecoli$SNP.cluster %in% levels_snp ~ ecoli$SNP.cluster, 
                               is.na(ecoli$SNP.cluster) ~ ecoli$SNP.cluster,
                               !(ecoli$SNP.cluster %in% levels_snp) ~ "Other")


AMR_df <- as.data.frame(table(as.factor(ecoli$AMR.genotypes)))
AMR_included <- AMR_df %>% filter(Freq > 300)
levels_AMR <- as.vector(AMR_included$Var1)
ecoli$AMR.genotypes <- case_when(ecoli$AMR.genotypes %in% levels_AMR ~ ecoli$AMR.genotypes, 
                                 is.na(ecoli$AMR.genotypes) ~ ecoli$AMR.genotypes,
                                 !(ecoli$AMR.genotypes %in% levels_AMR) ~ "Other")



ecoli <- ecoli %>% select(-c(Location))

ecoli_train <- ecoli %>% filter(year != 2021 & year !=  2019)
ecoli_test <- ecoli  %>%  filter(year == 2021 | year ==  2019 )
table(ecoli_test$outbreak)

ecoli_train <- ecoli_train %>% select(-c(year))
ecoli_test <- ecoli_test %>% select(-c(year))


ecoli_train[] <- lapply(ecoli_train, function(x){return(as.factor(x))})
ecoli_test[] <- lapply(ecoli_test, function(x){return(as.factor(x))})


ecoli_coef <- lasso(ecoli_train, 12)
ecoli_coef <- as.data.frame(as.matrix(ecoli_coef))
sal_coef <- as.vector(ecoli_coef$s1)
ecoli_coefs  <- round(ecoli_coef$s1/median(sal_coef[sal_coef != 0]))
ecoli_coef$s1 <- ecoli_coefs


ecoli_test$Strain <- factor(ecoli_test$Strain, 
                                      levels = c("Ecoli",
                                                 "O-:H28",
                                                 "O145:H28",
                                                 "O157:H7",
                                                 "Other", 
                                                 "ST131"))
ecoli_test$Strain <- relevel(ecoli_test$Strain, "Ecoli")

ecoli_test$SNP.cluster <- factor(ecoli_test$SNP.cluster, 
                                 levels = c("Other", 
                                            "PDS000053707.4",
                                            "PDS000066932.4",
                                            "PDS000117186.31"))



dim(model.matrix(outbreak~., ecoli_test))
dim(model.matrix(outbreak~., ecoli_train))

variables3 <- model.matrix(outbreak~., ecoli_test)


# We need to add some levels to the factor
#  Serovar same 
#  Isolation.type =  same
#



ecoli_test$score <- variables3 %*% ecoli_coefs

mod_ecoli <- glm(outbreak ~ score, data=ecoli_test, family = quasibinomial())

# For confusion matrices  
ecoli_test$predicted <- predict(mod_ecoli, type = "response") 

risk_perfomance_ecoli <- roc(outbreak ~ predicted, data=ecoli_test)
auc(risk_perfomance_ecoli)
BrierScore(mod_ecoli)

ggroc(risk_perfomance_ecoli) +
  theme_minimal() + 
  geom_abline(intercept = 1, slope = 1,
              color = "darkgrey", linetype = "dashed")
threshhold <- .5
confusionMatrix(as.factor(ifelse(ecoli_test$predicted > threshhold, 
                                 1,0)), ecoli_test$outbreak)



# Training 
variables4 <- model.matrix(outbreak~., ecoli_train)
ecoli_train$score <- variables4[,-1] %*% ecoli_coefs[-1]

mod_ecolit <- glm(outbreak ~ score, data=ecoli_train, family = quasibinomial())

threshhold <- .5  # For confusion matrices  
ecoli_train$predicted <- predict(mod_ecolit, type = "response") 

risk_perfomance_ecoli <- roc(outbreak ~ predicted, data=ecoli_train)
auc(risk_perfomance_ecoli)
BrierScore(mod_ecoli)

ggroc(risk_perfomance_ecoli) +
  theme_minimal() + 
  geom_abline(intercept = 1, slope = 1,
              color = "darkgrey", linetype = "dashed")

