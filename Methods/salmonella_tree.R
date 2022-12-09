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

salmonella <- read.csv("~/Brown University/PHP_2550/Data/final_salmonella.csv")

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

salmonella <- salmonella %>% filter(!is.na(month) & !is.na(year) & 
                                      !is.na(Min.same)  & !is.na(Serovar) &  
                                      !is.na(Isolation.type)  & !is.na(AMR.genotypes) & 
                                      !is.na(Computed.types) & !is.na(SNP.cluster)) %>% 
  select(-c(Min.diff, Location))

# We will add an outbreak to the model 
salmonella <- add_outbreak(salmonella, 10)
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



salmonella <- salmonella %>% select(-c(Computed.types))

salmonella_train <- salmonella %>% filter(year != 2022 & year !=  2019)
salmonella_test <- salmonella  %>%  filter(year == 2022 | year ==  2019 )
table(salmonella_test$outbreak)

salmonella_train <- salmonella_train %>% select(-c(year, Min.same))
salmonella_test <- salmonella_test %>% select(-c(year, Min.same))


salmonella_train[] <- lapply(salmonella_train, function(x){return(as.factor(x))})
salmonella_test[] <- lapply(salmonella_test, function(x){return(as.factor(x))})



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
                                        levels = c("aph(3'')-Ib=COMPLETE,aph(6)-Id=COMPLETE,blaTEM-1=COMPLETE,mdsA=COMPLETE,mdsB=COMPLETE,sul2=COMPLETE,tet(B)=COMPLETE", 
                                                   "blaTEM-116=COMPLETE,mdsA=COMPLETE,mdsB=COMPLETE", 
                                                   "fosA7=COMPLETE,mdsA=COMPLETE,mdsB=COMPLETE", 
                                                   "gyrA_D87N=POINT,mdsA=COMPLETE,mdsB=COMPLETE", 
                                                   "mdsA=COMPLETE,mdsB=COMPLETE", 
                                                   "Other"))
salmonella_test$AMR.genotypes <- relevel(salmonella_test$AMR.genotypes, "aph(3'')-Ib=COMPLETE,aph(6)-Id=COMPLETE,blaTEM-1=COMPLETE,mdsA=COMPLETE,mdsB=COMPLETE,sul2=COMPLETE,tet(B)=COMPLETE")

tree_salmonella <- rpart(outbreak ~ ., data = salmonella_train, method = "class", control=rpart.control(minsplit = 20, minbucket = 30,cp =.01)) 

plot(tree_salmonella,  
     main = "Intial tree model",  
     sub = "Figure 2. Intial tree model, prior to pruning")  
text(tree_salmonella) 

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

ggroc(salmonella_acc, size=0.8) + ggtitle('E. Coli Classification Tree ROC Curve') + 
  annotate("text", x = .91, y = .95, label = paste0('AUC = ', salmonella_AUC), color = '#A03E3F') +
  annotate("text", x = .85, y = .9, label = paste0('Brier Score = ', salmonella_Brscr), color = '#5566AB')+
  theme_minimal()


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

ggroc(salmonella_test_acc, size=0.8) + ggtitle('Salmonella Classification Tree ROC Curve') + 
  annotate("text", x = .91, y = .95, label = paste0('AUC = ', salmonella_test_AUC), color = '#A03E3F') +
  annotate("text", x = .85, y = .9, label = paste0('Brier Score = ', salmonella_test_Brscr), color = '#5566AB')+
  theme_minimal()

