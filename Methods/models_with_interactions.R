# This code will be the primary script in terms of modeling for our
# project, this file contains the model fitting for our 4 models of interest 
# risk score models and classification trees for Salmonella and E. Coli 


##~~~~~~~~~~~~~~~~~~~~~~~##
#### Data and packages ####
##~~~~~~~~~~~~~~~~~~~~~~~##


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
library(rpart)
library(rpart.plot)

setwd("~/Desktop/Semester_3/Practical/Final") # Where all our data is 
salmonella <- read.csv("final_salmonella.csv")
source("functions_outbreaks_and_lasso.R")
source("compress_levels.R")





## We will do a complete case analysis ##

salmonella <- salmonella %>% filter(!is.na(month) & !is.na(year) & 
                                      !is.na(Min.same)  & !is.na(Serovar) &  
                                      !is.na(Isolation.type)  & !is.na(AMR.genotypes) & 
                                      !is.na(Computed.types) & !is.na(SNP.cluster)) %>% 
                              dplyr::select(-c(Min.diff, Location))

# We will add an outbreak to the model 
salmonella <- add_outbreak(salmonella, 8)


# Compress the levels of SNP clusters to be the top overall
salmonella$SNP.cluster <- compress_levels_factors(salmonella, 'SNP.cluster')


## Create a test and train split
salmonella_train <- salmonella %>% filter(year != 2021 & year !=  2019 & 
                                          year != 2020 & year !=  2022)
salmonella_test <- salmonella  %>%  filter(year == 2021 | year ==  2019 )
table(salmonella_test$outbreak)

# Get rid of variables that we won't use in analysis 
salmonella_train <- salmonella_train %>% dplyr::select(-c(year, Min.same))
salmonella_test <- salmonella_test %>% dplyr::select(-c(year, Min.same))

# Make the variables factors
salmonella_train[] <- lapply(salmonella_train, function(x){return(as.factor(x))})
salmonella_test[] <- lapply(salmonella_test, function(x){return(as.factor(x))})

## Consider Interactions, based on our understanding of the data ##

# It seems natural that the SNP cluster would be related to most other variables 
# like time of the year (month), Isolation etc. 

salmonella_train$snp_month <- interaction(salmonella_train$SNP.cluster, salmonella_train$month)
salmonella_train$snp_source <- interaction(salmonella_train$SNP.cluster, salmonella_train$Isolation.source.category)
salmonella_train$month_source <- interaction(salmonella_train$month, salmonella_train$Isolation.source.category)

# Add to test set 
salmonella_test$snp_month <- interaction(salmonella_test$SNP.cluster, salmonella_test$month)
salmonella_test$snp_source <- interaction(salmonella_test$SNP.cluster, salmonella_test$Isolation.source.category)
salmonella_test$month_source <- interaction(salmonella_test$month, salmonella_test$Isolation.source.category)



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#### Salmonella, Risk Score ####
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

# Perform Lasso
salmonella_coef <- lasso(salmonella_train, 12)
salmonella_coef <- as.data.frame(as.matrix(salmonella_coef))
sal_coef <- as.vector(salmonella_coef$s1)

# Divide the Lasso
salmonella_coefs  <- round(salmonella_coef$s1/mean(sal_coef[sal_coef != 0]))
salmonella_coef$s1 <- salmonella_coefs


## Some coefficients that we trained the data on will not be observed in the 
## test set so we will need to add these to be able to assess how the 
## model perform on testing data 


# We note that the factors relating to SNP and AMR are the issue as 
# some of the levels are not observed, so we will add them now so the 
# score can be calculated

salmonella_test$SNP.cluster <- factor(salmonella_test$SNP.cluster, 
                                      levels = c("Other", 
                                                 "PDS000004723.723",
                                                 "PDS000029659.612",
                                                 "PDS000030237.975",
                                                 "PDS000032668.817",
                                                 "PDS000083226.277",
                                                 "PDS000089910.245",
                                                 "PDS000120941.3"))
salmonella_test$SNP.cluster <- relevel(salmonella_test$SNP.cluster, "Other")
salmonella_test$AMR.genotypes <- factor(salmonella_test$AMR.genotypes, 
                                        levels = c("aadA1=COMPLETE,gyrA_D87Y=POINT,mdsA=COMPLETE,mdsB=COMPLETE,sul1=COMPLETE,tet(A)=COMPLETE",
                                                   "aph(3'')-Ib=COMPLETE,aph(6)-Id=COMPLETE,mdsA=COMPLETE,mdsB=COMPLETE,tet(B)=COMPLETE",     
                                                   "fosA7=COMPLETE,mdsA=COMPLETE,mdsB=COMPLETE" ,                                             
                                                   "fosA7=PARTIAL,mdsA=COMPLETE,mdsB=COMPLETE" ,                                              
                                                   "gyrA_D87Y=POINT,mdsA=COMPLETE,mdsB=COMPLETE" ,                                            
                                                   "mdsA=COMPLETE,mdsB=COMPLETE"       ,                                                      
                                                   "mdsA=COMPLETE,mdsB=COMPLETE,sul2=COMPLETE,tet(A)=COMPLETE",                               
                                                   "Other"))
salmonella_test$AMR.genotypes <- relevel(salmonella_test$AMR.genotypes, "aadA1=COMPLETE,gyrA_D87Y=POINT,mdsA=COMPLETE,mdsB=COMPLETE,sul1=COMPLETE,tet(A)=COMPLETE")

snp_month_levels_train <- as.list(levels(salmonella_train$snp_month))
salmonella_test$snp_month <- factor(salmonella_test$snp_month, levels = snp_month_levels_train)
snp_source_levels_train <- as.list(levels(salmonella_train$snp_source))
salmonella_test$snp_source <- factor(salmonella_test$snp_source, levels = snp_source_levels_train)



# We need to make sure that all levels of the factor are accounted for 
dim(model.matrix(outbreak~., salmonella_test))
dim(model.matrix(outbreak~., salmonella_train))


# Get the risk scores in the testing data 
variables3 <- model.matrix(outbreak~., salmonella_test)

salmonella_test$score <- variables3[,-1] %*% salmonella_coefs[-1]

mod_salmonella <- glm(outbreak ~ score, data=salmonella_test, family = quasibinomial())

# For confusion matrices  
salmonella_test$predicted <- predict(mod_salmonella, type = "response") 

risk_perfomance_salmonella <- roc(outbreak ~ predicted, data=salmonella_test)
auc(risk_perfomance_salmonella)
BrierScore(mod_salmonella)

ggroc(risk_perfomance_salmonella) +
  theme_minimal() + 
  geom_abline(intercept = 1, slope = 1,
              color = "darkgrey", linetype = "dashed")
threshhold <- .5
confusionMatrix(as.factor(ifelse(salmonella_test$predicted > threshhold, 
                                 1,0)), salmonella_test$outbreak)

salmonella_test$score <- as.numeric(salmonella_test[,13])
salmonella_test$outbreak_1 <- case_when(salmonella_test$outbreak == "0" ~ 0, 
                                        salmonella_test$outbreak == "1" ~ 1)
salmonella_test  %>% group_by(score) %>% dplyr::summarize(out = sum(outbreak_1), 
                                                          cases =n())

variables4 <- model.matrix(outbreak~., salmonella_train)
salmonella_train$scores <- variables4[,-1] %*% salmonella_coefs[-1]
salmonella_train$score <- as.numeric(salmonella_train[,13])
salmonella_train$outbreak_1 <- case_when(salmonella_train$outbreak == "0" ~ 0, 
                                         salmonella_train$outbreak == "1" ~ 1)

salmonella_train  %>% group_by(score) %>% dplyr::summarize(out = sum(outbreak_1), 
                                                           num = n())

##~~~~~~~~~~~~~~~~~~~~~##
#### Salmonella Tree #### 
##~~~~~~~~~~~~~~~~~~~~~##

# Remove the variables from past training predictions 
salmonella_train <- salmonella_train[,-c(13:15)]
tree_salmonella <- rpart(outbreak ~ ., data = salmonella_train, 
                         method = "class", 
                         control=rpart.control(minsplit = 20, 
                                               minbucket = 30,cp =.01)) 

# Plot the tree 
prp(tree_salmonella)


plotcp(tree_salmonella, sub = "Figure 3. How x error changes based on complexity") 
printcp(tree_salmonella) 

preds = predict(tree_salmonella, salmonella_train, type = "class")
sensitivity(preds, salmonella_train$outbreak)
specificity(preds, salmonella_train$outbreak)

# Predict with Tree
predict_salmonella_tree <- as.data.frame(predict(tree_salmonella, salmonella_train))
names(predict_salmonella_tree) <- c("prob_tree")
salmonella_append <- cbind(salmonella_train, predict_salmonella_tree)

# ROC Analysis
salmonella_acc <- roc(outbreak ~ prob_tree, data =salmonella_append)
#AUC
salmonella_AUC = round(auc(salmonella_acc), 2)

# Brier Score
salmonella_Brscr = round(sum((predict_salmonella_tree[,2] - (as.numeric(salmonella_train$outbreak) - 1))^2)/length(salmonella_train$outbreak),2)

ggroc(salmonella_acc) + ggtitle('E. Coli Classification Tree ROC Curve') + 
  annotate("text", x = .91, y = .95, label = paste0('AUC = ', salmonella_AUC), color = 'red') +
  annotate("text", x = .85, y = .9, label = paste0('Brier Score = ', salmonella_Brscr), color = 'red')


# Predict on Test Set
pred_test = predict(tree_salmonella, newdata = salmonella_test, type = "class")
sensitivity(pred_test, salmonella_test$outbreak)
specificity(pred_test, salmonella_test$outbreak)

# Predict with Tree
predict_test_tree <- as.data.frame(predict(tree_salmonella, salmonella_test))
names(predict_test_tree) <- c("prob_tree")
salmonella_test_append <- cbind(salmonella_test, predict_test_tree)

# ROC Analysis
salmonella_test_acc <- roc(outbreak ~ prob_tree, data =salmonella_test_append)
#AUC
salmonella_test_AUC = round(auc(salmonella_test_acc), 2)

# Brier Score
salmonella_test_Brscr = round(sum((predict_test_tree[,2] - (as.numeric(salmonella_test$outbreak) - 1))^2)/length(salmonella_test$outbreak),2)

ggroc(salmonella_test_acc) + ggtitle('Salmonella Classification Tree ROC Curve') + 
  annotate("text", x = .91, y = .95, label = paste0('AUC = ', salmonella_test_AUC), color = 'red') +
  annotate("text", x = .85, y = .9, label = paste0('Brier Score = ', salmonella_test_Brscr), color = 'red')


##~~~~~~~~~~~~~~~~~~~~~~~~~##
#### E. Coli Risk Scores ####
##~~~~~~~~~~~~~~~~~~~~~~~~~##

# Read in the ecoli data 
ecoli <- read.csv("final_ecoli.csv")



## We will do a complete case analysis ##
ecoli <- ecoli %>% filter(!is.na(month) & !is.na(year) & 
                          !is.na(SNP.cluster)  & !is.na(Strain) & 
                          !is.na(AMR.genotypes) & !is.na(Location)& 
                          !is.na(Isolation.type) & !is.na(region) & 
                          !is.na(Isolation.source.category)) %>%
                   dplyr::select(-c(day, Min.same, Location))



# We will add an outbreak to the model 
ecoli <- add_outbreak(ecoli, 10)


# Compress the SNP Cluster levels 
ecoli$SNP.cluster <- compress_levels_factors(ecoli, 'SNP.cluster')

## Get test and train splits ##
ecoli_train <- ecoli %>% filter(year != 2021 & year !=  2019 & 
                                year != 2020 & year !=  2022)
ecoli_test <- ecoli  %>%  filter(year == 2021 | year ==  2019 )
table(ecoli_test$outbreak)

ecoli_train <- ecoli_train %>% dplyr::select(-c(year))
ecoli_test <- ecoli_test %>% dplyr::select(-c(year))


ecoli_train[] <- lapply(ecoli_train, function(x){return(as.factor(x))})
ecoli_test[] <- lapply(ecoli_test, function(x){return(as.factor(x))})


# It seems natural that the SNP cluster would be related to most other variables 
# like time of the year (month), Isolation etc. 

ecoli_train$snp_month <- interaction(ecoli_train$SNP.cluster, ecoli_train$month)
ecoli_train$snp_source <- interaction(ecoli_train$SNP.cluster, ecoli_train$Isolation.source.category)
ecoli_train$month_source <- interaction(ecoli_train$month, ecoli_train$Isolation.source.category)

# Add to test set 
ecoli_test$snp_month <- interaction(ecoli_test$SNP.cluster, ecoli_test$month)
ecoli_test$snp_source <- interaction(ecoli_test$SNP.cluster, ecoli_test$Isolation.source.category)
ecoli_test$month_source <- interaction(ecoli_test$month, ecoli_test$Isolation.source.category)



# Lasso 
ecoli_coef <- lasso(ecoli_train, 12)
ecoli_coef <- as.data.frame(as.matrix(ecoli_coef))
eco_coef <- as.vector(ecoli_coef$s1)
ecoli_coefs  <- round(ecoli_coef$s1/median(eco_coef[eco_coef != 0]))
ecoli_coef$s1 <- ecoli_coefs

# Need  to check strain, SNP cluster, 


# Make sure all the factors are interpe 

#levels(ecoli_test$month_source) == levels(ecoli_train$month_source)
strain_month_levels_train <- as.list(levels(ecoli_train$Strain))
ecoli_test$Strain <- factor(ecoli_test$Strain, 
                            levels = strain_month_levels_train)
snp_month_levels_train <- as.list(levels(ecoli_train$SNP.cluster))
ecoli_test$SNP.cluster <- factor(ecoli_test$SNP.cluster, 
                                 levels = snp_month_levels_train)

snp_month_levels_train <- as.list(levels(ecoli_train$snp_month))
ecoli_test$snp_month <- factor(ecoli_test$snp_month, levels = snp_month_levels_train)
snp_source_levels_train <- as.list(levels(ecoli_train$snp_source))
ecoli_test$snp_source <- factor(ecoli_test$snp_source, levels = snp_source_levels_train)


# Make sure all 
dim(model.matrix(outbreak~., ecoli_test))
dim(model.matrix(outbreak~., ecoli_train))

variables3 <- model.matrix(outbreak~., ecoli_test)


ecoli_test$score <- variables3[,-1] %*% ecoli_coefs[-1]

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


##  Test 
ecoli_test$score <- as.numeric(ecoli_test[,12])
ecoli_test$outbreak_1 <- case_when(ecoli_test$outbreak == "0" ~ 0, 
                                   ecoli_test$outbreak == "1" ~ 1)
ecoli_test  %>% group_by(score) %>% dplyr::summarize(out = sum(outbreak_1), 
                                                     cases =n())

variables4 <- model.matrix(outbreak~., ecoli_train)
ecoli_train$scores <- variables4[,-1] %*% ecoli_coefs[-1]
ecoli_train$score <- as.numeric(ecoli_train[,12])
ecoli_train$outbreak_1 <- case_when(ecoli_train$outbreak == "0" ~ 0, 
                                    ecoli_train$outbreak == "1" ~ 1)

ecoli_train  %>% group_by(score) %>% dplyr::summarize(out = sum(outbreak_1), 
                                                      num = n())





#### Tree  Ecoli ####

ecoli_train  <- ecoli_train[,-c(12:14)]
tree_ecoli <- rpart(outbreak ~ ., data = ecoli_train, method = "class", control=rpart.control(minsplit = 20, minbucket = 30,cp =.01)) 

plot(tree_ecoli,  
     main = "Intial tree model",  
     sub = "Figure 2. Intial tree model, prior to pruning")  
text(tree_ecoli) 
prp(tree_ecoli)


plotcp(tree_ecoli, sub = "Figure 3. How x error changes based on complexity") 
printcp(tree_ecoli) 

preds = predict(tree_ecoli, ecoli_train, type = "class")
sensitivity(preds, ecoli_train$outbreak)
specificity(preds, ecoli_train$outbreak)

# Predict with Tree
predict_ecoli_tree <- as.data.frame(predict(tree_ecoli, ecoli_train))
names(predict_ecoli_tree) <- c("prob_tree")
ecoli_append <- cbind(ecoli_train, predict_ecoli_tree)

# ROC Analysis
ecoli_acc <- roc(outbreak ~ prob_tree, data =ecoli_append)
#AUC
ecoli_AUC = round(auc(ecoli_acc), 2)

# Brier Score
ecoli_Brscr = round(sum((predict_ecoli_tree[,2] - (as.numeric(ecoli_train$outbreak) - 1))^2)/length(ecoli_train$outbreak),2)

ggroc(ecoli_acc) + ggtitle('E. Coli Classification Tree ROC Curve') + 
  annotate("text", x = .91, y = .95, label = paste0('AUC = ', ecoli_AUC), color = 'red') +
  annotate("text", x = .85, y = .9, label = paste0('Brier Score = ', ecoli_Brscr), color = 'red')


# Predict on Test Set
pred_test = predict(tree_ecoli, newdata = ecoli_test, type = "class")
sensitivity(pred_test, ecoli_test$outbreak)
specificity(pred_test, ecoli_test$outbreak)

# Predict with Tree
predict_test_tree <- as.data.frame(predict(tree_ecoli, ecoli_test))
names(predict_test_tree) <- c("prob_tree")
ecoli_test_append <- cbind(ecoli_test, predict_test_tree)

# ROC Analysis
ecoli_test_acc <- roc(outbreak ~ prob_tree, data =ecoli_test_append)
#AUC
ecoli_test_AUC = round(auc(ecoli_test_acc), 2)

# Brier Score
ecoli_test_Brscr = round(sum((predict_test_tree[,2] - (as.numeric(ecoli_test$outbreak) - 1))^2)/length(ecoli_test$outbreak),2)

ggroc(ecoli_test_acc) + ggtitle('E. Coli Classification Tree ROC Curve') + 
  annotate("text", x = .91, y = .95, label = paste0('AUC = ', ecoli_test_AUC), color = 'red') +
  annotate("text", x = .85, y = .9, label = paste0('Brier Score = ', ecoli_test_Brscr), color = 'red')







#### Plot the actual ROC Curve we will use #### 


ggroc(list(Ecoli_risk = risk_perfomance_ecoli, 
           Salmonela_risk = risk_perfomance_salmonella,
           Ecoli_tree= ecoli_test_acc, 
           Salmonella_tree = salmonella_test_acc), size =1) + 
  scale_size_manual(values = c(1, 1,1,1)) +
  theme_minimal() + 
  geom_abline(intercept = 1, slope = 1,
              color = "darkgrey", linetype = "dashed") +
  labs(title = "ROC curves, Risk Score and Tree Models") +
  annotate("text", x =.04,  y = .75, label="AUC = .54", 
           size = 4.5, 
           color = "red") +
  annotate("text", x =.075,  y = .7, label="Brier Score = .06", 
           size = 4.5, 
           color = "red") +
  annotate("text", x =.04,  y = .65, label="AUC = .57", 
           size = 4.5, 
           color = "blue2") +
  annotate("text", x =.075,  y = .6, label="Brier Score = .18", 
           size = 4.5, 
           color = "blue2") +
  annotate("text", x =.04,  y = .55, label="AUC = .57", 
           size = 4.5, 
           color = "darkgreen") +
  annotate("text", x =.07,  y = .5, label="Brier Score = .03", 
           size = 4.5, 
           color = "darkgreen") +
  annotate("text", x =.04,  y = .45, label="AUC = .53", 
           size = 4.5, 
           color = "chocolate4") +
  annotate("text", x =.07,  y = .4, label="Brier Score = .25", 
           size = 4.5, 
           color = "chocolate4") +
  scale_color_manual(values = c("red", "blue2", "darkgreen", "chocolate4"), 
                     labels = c("E.coli, Risk score", 
                                "Salmonella, Risk Score", 
                                "E.coli, Tree model", 
                                "Salmonella, Tree model"), 
                     name = "Model:") +
  theme(legend.position = "bottom") 



