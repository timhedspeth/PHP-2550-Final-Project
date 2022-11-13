
# Please note this is an initial sanity check, so we do not do all illnesses 

#~~~~~~~~~~~~#
## Packages ## 
#~~~~~~~~~~~~#

library(lubridate) # working with dates 
library(tidyverse) # working with data 
library(kableExtra) # Make nicer plots 
library(ggpubr) # Arrange GGplots 
library(pROC) 
library(olsrr) 
library(rpart.plot) 
library(tsModel)
library(caret)


#~~~~~~~~#
## Data ## 
#~~~~~~~~#

# Set the working directory and read in the preprocessed data 
setwd("~/Desktop/Semester_3/Practical/Final")
ecoli <- read.csv("ecoli_clean_final.csv")



#~~~~~~~~~~~~~~~~~~~~~~~#
## Harmonic Regression ##
#~~~~~~~~~~~~~~~~~~~~~~~#

# We will attempt to forecast the number of cases, but as we observed in the EDA
# there are clear trends based on seasonality (e.g. summer is more likely to 
# have more infections), thus to build a model we will do a simple example


# We need our data cleaned first, the number of cases per month in each year

ecoli_model_cases <- ecoli %>% 
  filter(!is.na(month)) %>% 
  group_by(year, month) %>% 
  summarize(cases = n())

# What covariates will we adjust for in this model?
# Since this is aggregate data we cannot look at individual level covariates
# but we can look at counts of categorical covariates 


ecoli_counts <- ecoli %>% 
  filter(!is.na(month)) %>% 
  group_by(year, month) %>%
  count(region) %>% 
  pivot_wider(names_from = region, values_from = n)

miss <-  apply(ecoli_counts,2,is.na)
miss <- as.data.frame(miss)
miss1 <- as.data.frame(apply(miss,2,sum)/nrow(miss))



# Join the data sets  
ecoli_test <- left_join(ecoli_model_cases, ecoli_counts, by=c("year"="year", "month"="month"))

model1 <- glm(cases ~  West + Midwest + South +Northeast + 
                harmonic(month,2,12), 
              family=quasipoisson, 
              data = ecoli_test)

summary(model1)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
## Classification of outbreaks ## 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


# We want to quantify variables that could be important in predicting an outbreak 
# which we will do by 

# We want to extract when a strain was part of an outbreak, by our definition 
ecoli$id <- seq(1,nrow(ecoli))


eco_subset <- ecoli %>% filter(!is.na(month)) %>% 
  group_by(year, month, region) %>% 
  summarize(mean1 = round(mean(Min.same, na.rm = T),2), 
            sd1 = round(sd(Min.same, na.rm = T),2),
            num = n())  %>% 
  ungroup() %>% 
  # Substantial number of cases that are very closely related 
  filter(mean1 < 3 & num > 100) %>% 
  pivot_wider(names_from = region, values_from = c(mean1, sd1))

# Define a new outbreak variable 
ecoli$month_year <- paste0(ecoli$year,ecoli$month)
eco_subset$month_year <- paste0(eco_subset$year, eco_subset$month)

ecoli$outbreak <- ifelse(ecoli$month_year %in% as.vector(eco_subset$month_year),
                         1, 0)

# Now we can feed data into a regression model to predict the outbreaks 

# We want to look at seasonality 

ecoli <- ecoli %>% 
  mutate(season =case_when(month ==1 ~ "Winter", 
                           month ==2 ~ "Winter",
                           month ==3 ~ "Spring",
                           month ==4 ~ "Spring",
                           month ==5 ~ "Spring",
                           month ==6 ~ "Summer",
                           month ==7 ~ "Summer",
                           month ==8 ~ "Summer",
                           month ==9 ~ "Fall",
                           month ==10 ~ "Fall",
                           month ==11 ~ "Fall",
                           month ==12 ~ "Winter"))


#length(which(ecoli$outbreak == 1))/nrow(ecoli)
# There is only 6% in the outbreak so we will need to weight this regressionn 

## Outbreak 

# Weight the models
proportion0_ecoli <- 1/(sum(ecoli$outbreak == 0)/nrow(ecoli)) 
proportio1_ecoli <- 1/(sum(ecoli$outbreak == 1)/nrow(ecoli)) 

modweights <- ifelse(ecoli$outbreak == 0, 
                     proportion0_ecoli, 
                     proportio1_ecoli) 

ecoli$outbreak <- as.integer(ecoli$outbreak) 

# Fit the model 
logistic_model <- glm(outbreak ~ season + Isolation.source.category,
                      data = ecoli, 
                      weights = modweights, 
                      family=quasibinomial())

summary(logistic_model)


ecoli$prob <- predict(logistic_model, ecoli, type="response")

# Get the AUC 
acc <- roc(outbreak ~ prob, data =ecoli)
auc(acc)

# Confusion matrix 
ecoli$predicted_response_log <- ifelse(ecoli$prob > .5, 1,0)
confusionMatrix(as.factor(ecoli$predicted_response_log), as.factor(ecoli$outbreak))




## Tree based model ## 

tree <- rpart(outbreak~ season + Isolation.source.category, data = ecoli, weights = modweights, 
              control=rpart.control(minsplit = 20, minbucket = 20/3,cp =.001)) 

# Not as pretty tree plot 
plot(tree,  
     main = "Intial tree model",  
     sub = "Figure 2. Intial tree model, prior to pruning")  
text(tree) 

# Pretty tree based model 
prp(tree)

# Assess the complexity of tree 
plotcp(tree,  
       sub = "Figure 3. How x error changes based on complexity") 
printcp(tree) 

# Assess how the model is performing 
predicted_tree <- as.data.frame(predict(tree, ecoli))
names(predicted_tree) <- c("prob_tree")
ecoli <- cbind(ecoli, predicted_tree)

acc1 <- roc(outbreak ~ prob_tree, data =ecoli)

auc(acc1)

# Confusion matrix 
ecoli$predicted_response_tree <- ifelse(ecoli$prob_tree > .5, 1,0)
confusionMatrix(as.factor(ecoli$predicted_response_tree), as.factor(ecoli$outbreak))

