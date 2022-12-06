
# For the purposes of our PHP 2550 final project we are interested in outbreaks 
# in the data collected from the NCBI database. We will define outbreaks based
# on the attributes:  region of the united states, month, number of cases, and 
# the Min.same 

# In order to make this as reproducible as possible we have created a function
# that classifies outbreaks 

library(tidyverse)


#~~~~~~~~~~~~~~~~~~~~~#
## Outbreak function ##
#~~~~~~~~~~~~~~~~~~~~~#

 

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
                 filter(!is.na(month) & region  != "USA, General") %>% 
                 group_by(year, month, region) %>% 
                 summarize(mean1 = round(mean(Min.same, na.rm = T),2), 
                           sd1 = round(sd(Min.same, na.rm = T),2),
                           num = n())  %>% 
                 ungroup() %>% 
                 # Substantial number of cases that are very closely related 
                 filter(mean1 < similarity & num > numbercases) #%>% 
                 #pivot_wider(names_from = region, values_from = c(mean1, sd1))
  
  
  
  # Define a new outbreak variable 
  dataset$month_year <- paste0(dataset$year,dataset$month,dataset$region)
  data_subset$month_year <- paste0(data_subset$year, data_subset$month, data_subset$region)
  
  # Put the outbreaks in the data frame 
  dataset$outbreak <- ifelse(dataset$month_year %in% as.vector(data_subset$month_year),
                             1, 0)
  

  # What do we return? 
  if(sum(dataset$outbreak ==1) == 0){
    return("There appear to be no outbreaks in this data based on your paramters")
  } else{
    return(as.data.frame(dataset))
  }
  
  
}


#~~~~~~~~~~~~~~~~~~#
## Lasso Function ## 
#~~~~~~~~~~~~~~~~~~# 

# This function was adapted from a code provided by Alice Paul, PhD, the 
# instructor for  this course 

lasso <- function(df,numtimes) { 
  #' Runs 10-fold CV for lasso and returns corresponding coefficients 
  #' @param df, data set
  #' @return coef, coefficients for minimum cv error
  
  # Matrix form for ordered variables 
  x.ord <- model.matrix(as.factor(outbreak)~., data = df)[,-1] 
  y.ord <- as.factor(df$outbreak)
  
  # Generate folds
  k <- numtimes 
  set.seed(1) # consistent seeds between imputed data sets
  folds <- sample(1:k, nrow(df), replace=TRUE)
  
  
  #  This function will auto do weighting if one of the proportions is 
  #  below 25% observed weighting will occur 
  
  table_outcomes <- as.data.frame(table(df$outbreak))
  prop0 <- table_outcomes[1,2]/(sum(table_outcomes[,2]))
  prop1 <- table_outcomes[2,2]/(sum(table_outcomes[,2]))
  modweight <- ifelse(df$outbreak == 0, 
                      1/prop0, 
                      1/prop1)
  
  # Lasso model
  if(prop0 > .3 & prop1 > .3){
  lasso_mod <- cv.glmnet(x.ord, y.ord, nfolds = numtimes, foldid = folds, 
                         alpha = 1, family = "binomial") 
  } else{
  lasso_mod <- cv.glmnet(x.ord, y.ord, nfolds = numtimes, foldid = folds, 
                         alpha = 1, family = "binomial", weights = modweight) 
  }
  
  # Get coefficients 
  coef <- coef(lasso_mod, lambda = lasso_mod$lambda.min) 
  return(coef) 
} 





