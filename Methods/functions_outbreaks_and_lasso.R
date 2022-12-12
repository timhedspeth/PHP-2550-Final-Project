
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

# This function will group cases that are similar and add them to an outbreak variable 
# this follows the definition from NCBI, that an outbreak is one of many cases in an 
# area 

add_outbreak_simple <- function(dataset, numbercases = 20, similarity = 7){
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
                 group_by(year, month, Location) %>% # Location 
                 summarize(num = n())  %>% 
                 ungroup() %>% 
                 # Substantial number of cases that are very closely related 
                 filter(num > numbercases) #%>% 
                 #pivot_wider(names_from = region, values_from = c(mean1, sd1))
  
  #mean1 < similarity 
  
  # Define a new outbreak variable 
  dataset$month_year <- paste0(dataset$year,dataset$month,dataset$region, dataset$SNP.cluster,dataset$Isolation.source.category)
  data_subset$month_year <- paste0(data_subset$year, data_subset$month, data_subset$region, data_subset$SNP.cluster,data_subset$Isolation.source.category)
  
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


# We also consider cases where there are a large number of closely related cases 
# based on SNP. cluster 

add_outbreak <- function(dataset, numbercases = 20){
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
    summarize(num = n())  %>% 
    ungroup() %>% 
    # Substantial number of cases that are very closely related 
    filter(num > numbercases)  %>% 
    mutate(outbreak = 1)
  
  # Join in outbreak information
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

# This function was adapted and modified from a code provided
# by Alice Paul, PhD, the instructor for this course 


lasso <- function(df,numtimes) { 
  #' Runs 10-fold CV for lasso and returns corresponding coefficients 
  #' @param df, data set
  #' @return coef, coefficients for minimum cv error
  
  # Matrix form for ordered variables 
  x.ord <- model.matrix(outbreak~., data = df)[,-1] 
  y.ord <- df$outbreak
  
  # Generate folds
  k <- numtimes 
  folds <- as.numeric(df$month) # so that we spit on month which allows for 
                                # the test and train split does not contain 
                                # the same outbreaks 
  
  
  #  This function will auto do weighting as we expect the outcome to be 
  #  rare in this case 
  
  prop0 <-sum(df$outbreak == 0)/nrow(df)
  prop1 <- sum(df$outbreak == 1)/nrow(df)
  modweight <- ifelse(df$outbreak == 0, 
                      1/prop0, 
                      1/prop1)
  
  # Lasso
  lasso_mod <- cv.glmnet(x.ord, y.ord, nfolds = 12, foldid = folds, 
                         alpha = 1, family = "binomial", weights = modweight) 
  

  # Get coefficients that minimize the lambda 
  coef <- coef(lasso_mod, lambda = lasso_mod$lambda.min) 
  return(coef) 
  
} 

