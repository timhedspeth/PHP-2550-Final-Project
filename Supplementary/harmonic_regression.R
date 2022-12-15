#~~~~~~~~~~~~#
## Packages ## 
#~~~~~~~~~~~~#

library(lubridate) # working with dates 
library(tidyverse) # working with data 
library(tsModel)
library(caret)

#read data
ecoli <- read.csv("final_ecoli.csv")

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
ecoli_test_full <- na.omit(ecoli_test)

model1 <- glm(cases ~  West + Midwest + South +Northeast + 
                harmonic(month,2,12), 
              family=quasipoisson, 
              data = ecoli_test_full)
summary(model1)

#fit pred values
pred1 <- predict(model1, type="response", ecoli_test_full)
#calculate MSE
mean((ecoli_test_full$cases - pred1)^2) #2736.313

#plot actual vs pred
#create ecoli data 
ecoli_mod <- data.frame(Predicted= pred1,  
                        Observed = ecoli_test_full$cases)

ggplot(ecoli_mod,                                     
       aes(x = Predicted,
           y = Observed)) +
  geom_point() +
  geom_abline(intercept = 0,
              slope = 1,
              color = "#5566AB",
              size = 0.5)+
  theme(panel.background = element_rect(fill="transparent"),
        axis.line.x.bottom=element_line(size=0.5),
        axis.line.y.left=element_line(size=0.5))


#cross validation
set.seed(123)
train_control <- trainControl(method = "cv", number = 10, 
                              classProbs = FALSE)

mod_cv <- train(cases~ West + Midwest + South +Northeast + 
                  harmonic(month,2,12),
                data = ecoli_test_full,
                trControl = train_control,
                method = "glm",
                family=quasipoisson)
summary(mod_cv)

#fit pred values

x_vars<- model.matrix(cases ~ West + Midwest + South +Northeast+
                        month+harmonic(month,2,12),
                      ecoli_test_full)

pred_cv <- mod_cv %>% predict(x_vars)

#check R2/RMSE
mod_cv$results

#plot actual vs pred
#create ecoli data 
ecoli_mod_cv <- data.frame(Predicted= pred_cv,  
                           Observed = ecoli_test_full$cases)

ggplot(ecoli_mod_cv,                                     
       aes(x = Predicted,
           y = Observed)) +
  geom_point() +
  geom_abline(intercept = 0,
              slope = 1,
              color = "#5566AB",
              size = 0.5)+
  theme(panel.background = element_rect(fill="transparent"),
        axis.line.x.bottom=element_line(size=0.5),
        axis.line.y.left=element_line(size=0.5))








