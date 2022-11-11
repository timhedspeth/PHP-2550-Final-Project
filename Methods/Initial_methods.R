
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



# There is missingness in the outcome so we will weight the outcome 


ecoli_test <- left_join(ecoli_model_cases, ecoli_counts, by=c("year"="year", "month"="month"))

model1 <- glm(cases ~ year + West + Midwest + South +Northeast + 
                harmonic(month,1,12), 
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

## Outbrekak 

proportion0_ecoli <- 1/(sum(ecoli$outbreak == 0)/nrow(ecoli)) 
proportio1_ecoli <- 1/(sum(ecoli$outbreak == 1)/nrow(ecoli)) 

modweights <- ifelse(ecoli$outbreak == 0, 
                     proportion0_ecoli, 
                     proportio1_ecoli) 

ecoli$outbreak <- as.integer(ecoli$outbreak) 

logistic_model <- glm(outbreak ~ season + Isolation.source.category,
                      data = ecoli, 
                      weights = modweights, 
                      family=quasibinomial())

summary(logistic_model)

library(pROC)

ecoli$prob <- predict(logistic_model, ecoli, type="response")

acc <- roc(outbreak ~ prob, data =ecoli)

auc(acc)


## Tree based model ## 

tree <- rpart(outbreak~ season + Isolation.source.category, data = ecoli, weights = modweights, 
              control=rpart.control(minsplit = 20, minbucket = 20/3,cp =.001)) 


plot(tree,  
     main = "Intial tree model",  
     sub = "Figure 2. Intial tree model, prior to pruning")  
text(tree) 

plotcp(tree,  
       sub = "Figure 3. How x error changes based on complexity") 
#printcp(tree) 

predicted_tree <- as.data.frame(predict(tree, ecoli))
names(predicted_tree) <- c("prob_tree")
ecoli <- cbind(ecoli, predicted_tree)

acc1 <- roc(outbreak ~ prob_tree, data =ecoli)

auc(acc1)

