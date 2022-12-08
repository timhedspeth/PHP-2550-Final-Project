

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
ecoli <- ecoli %>% filter(!is.na(month) & !is.na(year) & !is.na(Min.same)  & !is.na(Strain), 
                          !is.na(SNP.cluster)) 


campylobacter <- campylobacter %>% filter(!is.na(month) & !is.na(year) & 
                                          !is.na(Min.same)  & 
                                          !is.na(SNP.cluster) & !is.na(Isolation.type)  
                                          & !is.na(Assembly)& !is.na(AMR.genotypes) 
                                          )  %>% 
                                   select(-c(Min.diff))


#apply(campylobacter, 2, function(x){return(sum(is.na(x))/length(x))}) #All clean


#apply(salmonella, 2, function(x){return(sum(is.na(x))/length(x))}) #All clean


salmonella <- salmonella %>% filter(!is.na(month) & !is.na(year) & 
                                      !is.na(Min.same)  & !is.na(Serovar) &  
                                      !is.na(Isolation.type)  & !is.na(AMR.genotypes) & 
                                      !is.na(Computed.types) & !is.na(SNP.cluster)) %>% 
                                select(-c(Min.diff))

## Add outbreak ## 

ecoli <-  add_outbreak(ecoli)
campylobacter <- add_outbreak(campylobacter)
salmonella <- add_outbreak(salmonella)


## Reclassify the ##

# SNP Cluster 
SNP_df <- as.data.frame(table(as.factor(ecoli$SNP.cluster)))
snp_included <- SNP_df %>% filter(Freq > 100)
levels_snp <- as.vector(snp_included$Var1)
ecoli$SNP.cluster <- case_when(ecoli$SNP.cluster %in% levels_snp ~ ecoli$SNP.cluster, 
                               is.na(ecoli$SNP.cluster) ~ ecoli$SNP.cluster,
                               !(ecoli$SNP.cluster %in% levels_snp) ~ "Other")

SNP_levels_df <- as.data.frame(table(as.factor(campylobacter$SNP.cluster)))
SNP_included <-  SNP_levels_df %>% filter(Freq > 250)
levels_SNP <- as.vector(SNP_included$Var1) 
campylobacter$SNP.cluster <- case_when(campylobacter$SNP.cluster %in% levels_SNP ~ campylobacter$SNP.cluster, 
                                       is.na(campylobacter$SNP.cluster) ~ campylobacter$SNP.cluster,
                                       !(campylobacter$SNP.cluster %in% levels_SNP) ~ "Other")

SNP_levels_df <- as.data.frame(table(as.factor(salmonella$SNP.cluster)))
SNP_included <- SNP_levels_df  %>% filter(Freq > 1000) # Remove
levels_SNP <- as.vector(SNP_included$Var1) 
salmonella$SNP.cluster <- case_when(salmonella$SNP.cluster %in% levels_SNP ~ salmonella$SNP.cluster, 
                                    is.na(salmonella$SNP.cluster) ~ salmonella$SNP.cluster,
                                    !(salmonella$SNP.cluster %in% levels_SNP) ~ "Other")

# AMR
AMR_df <- as.data.frame(table(as.factor(ecoli$AMR.genotypes)))
AMR_included <- AMR_df %>% filter(Freq > 300)
levels_AMR <- as.vector(AMR_included$Var1)
ecoli$AMR.genotypes <- case_when(ecoli$AMR.genotypes %in% levels_AMR ~ ecoli$AMR.genotypes, 
                                 is.na(ecoli$AMR.genotypes) ~ ecoli$AMR.genotypes,
                                 !(ecoli$AMR.genotypes %in% levels_AMR) ~ "Other")

AMR_levels_df <- as.data.frame(table(as.factor(campylobacter$AMR.genotypes)))
AMR_levels_df  %>% filter(Freq > 1000) # Keep it this way 
AMR_included <-  AMR_levels_df %>% filter(Freq > 1000)
levels_AMR <- as.vector(AMR_included$Var1) 
campylobacter$AMR.genotypes <- case_when(campylobacter$AMR.genotypes %in% levels_AMR ~ campylobacter$AMR.genotypes, 
                                         is.na(campylobacter$AMR.genotypes) ~ campylobacter$AMR.genotypes,
                                         !(campylobacter$AMR.genotypes %in% levels_AMR) ~ "Other")

AMR_levels_df <- as.data.frame(table(as.factor(salmonella$AMR.genotypes)))
AMR_included <- AMR_levels_df  %>% filter(Freq > 1000) # Remove
levels_AMR <- as.vector(AMR_included$Var1) 
salmonella$AMR.genotypes <- case_when(salmonella$AMR %in% levels_AMR ~ salmonella$AMR, 
                                      is.na(salmonella$AMR) ~ salmonella$AMR,
                                      !(salmonella$AMR %in% levels_AMR) ~ "Other")





## Delete the variables that are used to determine outbreak ## 

ecoli <- ecoli %>% select(-c(year, Location, Min.same, day, 
                             month_year, month))
campylobacter <- campylobacter %>% select(-c(year, Location, Min.same, 
                                             month_year, Assembly, month 
                                             ))

salmonella <- salmonella %>% select(-c(year, Location, Min.same, 
                                             month_year, month 
                                      ))


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


#~~~~~~~~~~~~~~~~~~~~~~~~#
## Performance Measures ##
#~~~~~~~~~~~~~~~~~~~~~~~~#


threshhold <- .5  # For confusion matrices  


ecoli$predicted <- predict(mod_ecoli, type = "response")
salmonella$predicted <- predict(mod_salmonella, type = "response") 
campylobacter$predicted <- predict(mod_campylobacter, type = "response")


# Ecoli 
risk_perfomance_ecoli <- roc(outbreak ~ predicted, data=ecoli)
measures <- data.frame()
measures[1,1] <- "E. Coli"
measures[1,2] <- auc(risk_perfomance_ecoli)
measures[1,3] <- BrierScore(mod_ecoli)
measures[1,4] <- 0.8172
measures[1,5] <- 0.8708
measures[1,6] <- 0.6747

  
ggroc(risk_perfomance_ecoli) +
  theme_minimal() + 
  geom_abline(intercept = 1, slope = 1,
              color = "darkgrey", linetype = "dashed") +
  labs(title = "E. Coli ROC curve")  +
  annotate("text", x =.95,  y = .95, label="AUC = .88", 
           size = 5, 
           color = "red") +
  annotate("text", x =.905,  y = .9, label="Brier Score = .12", 
           size = 5, 
           color = "red")

confusionMatrix(as.factor(ifelse(ecoli$predicted > threshhold, 
                                 1,0)), ecoli$outbreak)

# Salmonella
risk_perfomance_salmonella <- roc(outbreak ~ predicted, data=salmonella)
measures[2,1] <- "Salmonella"
measures[2,2] <- auc(risk_perfomance_salmonella)
measures[2,3] <- BrierScore(mod_salmonella)
measures[2,4] <- .817
measures[2,5] <- .96
measures[2,6] <- .24 


ggroc(risk_perfomance_salmonella) +
  theme_minimal() + 
  geom_abline(intercept = 1, slope = 1,
              color = "darkgrey", linetype = "dashed") +
  labs(title = "Salmonella ROC curve") +
  annotate("text", x =.95,  y = .95, label="AUC = .87", 
           size = 5, 
           color = "red") +
  annotate("text", x =.905,  y = .9, label="Brier Score = .11", 
           size = 5, 
           color = "red")



#+ 
#scale_color_discrete(name = "Model", 
     #                  labels = c("Lasso, Clinical RS", 
       #                           "Ridge, Clinical RS"))

confusionMatrix(as.factor(ifelse(salmonella$predicted > threshhold, 
                                 1,0)), salmonella$outbreak)


## camplyobcter ##
risk_perfomance_campylobacter <- roc(outbreak ~ predicted, data=campylobacter)
measures[3,1] <- "campylobacter"
measures[3,2] <- auc(risk_perfomance_campylobacter)
measures[3,3] <- BrierScore(mod_campylobacter)
measures[3,4] <- .9946
measures[3,5] <- .999
measures[3,6] <- .6308


ggroc(risk_perfomance_campylobacter) +
  theme_minimal() + 
  geom_abline(intercept = 1, slope = 1,
              color = "darkgrey", linetype = "dashed") +
  labs(title = "Campylobacter ROC curve") +
  annotate("text", x =.03,  y = .75, label="AUC = .99", 
           size = 5, 
           color = "red") +
  annotate("text", x =.09,  y = .7, label="Brier Score = .0003", 
           size = 5, 
           color = "red")



confusionMatrix(as.factor(ifelse(campylobacter$predicted > threshhold, 
                                 1,0)), campylobacter$outbreak)






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
## Plot with all 3 illnesses ## 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


ggroc(list(Ecoli = risk_perfomance_ecoli, 
           Salmonela = risk_perfomance_salmonella,
           Camplyobacter =risk_perfomance_campylobacter)) + 
    #scale_color_manual(values = c("red", "green", "blue")) +
        theme_minimal() + 
    geom_abline(intercept = 1, slope = 1,
              color = "darkgrey", linetype = "dashed") +
  labs(title = "ROC curves, Risk Score Models") +
  annotate("text", x =.04,  y = .75, label="AUC = .88", 
           size = 4, 
           color = "red") +
  annotate("text", x =.075,  y = .7, label="Brier Score = .12", 
           size = 4, 
           color = "red") +
  annotate("text", x =.04,  y = .65, label="AUC = .87", 
           size = 4, 
           color = "green") +
  annotate("text", x =.075,  y = .6, label="Brier Score = .11", 
           size = 4, 
           color = "green") +
  annotate("text", x =.04,  y = .55, label="AUC = .99", 
           size = 4, 
           color = "blue") +
  annotate("text", x =.085,  y = .5, label="Brier Score = .0003", 
           size = 4, 
           color = "blue") +
  scale_color_discrete(name = "Model", 
                       labels = c("E. Coli", 
                             "Salmonela", 
                             "Campylobacter")) +
  scale_color_manual(values = c("red", "green", "blue"), 
                     labels = c("E. Coli", 
                                "Salmonela", 
                                "Campylobacter"), 
                     name = "Illness:") +
   theme(legend.position = "bottom")




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



