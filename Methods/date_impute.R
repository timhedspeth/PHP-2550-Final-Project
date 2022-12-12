
# An exaplanation and demonstrations as to why imputation of dates is
# not a fesiable consideration in our analysis 

# We wanted to impute the dates that were missing from 
# collection date variable (the variable of interest) as there 
# is a high degree of missingness at the date level


#~~~~~~~~~~~~~~~~~~~~~#
## Packages and data ##
#~~~~~~~~~~~~~~~~~~~~~#

library(tidyverse)
library(kableExtra)
library(anytime)
library(lubridate)

# Data sets 
setwd("~/Desktop/Semester_3/Practical/Final") 
ecoli <- read.csv("final_ecoli_date.csv")
campylobacter  <- read.csv("final_campylobacter_date.csv")
salmonella <- read.csv("final_salmonella_date.csv")


#~~~~~~~~~~~~~~~~~~~~~~~~#
## Imputation of dates? ## 
#~~~~~~~~~~~~~~~~~~~~~~~~#

# The way that the create date is created is as a character, all of which are 
# 20, which we will assume implies that there is a uniformity between the dates
# that we are 

#x <- lapply(as.vector(ecoli$Create.date), nchar)
#unique(x)

# There is excess information so we will only select the information 
# that is pertinent to the date 

ecoli <- ecoli %>% filter(!is.na(Collection.date) & !is.na(day) & !is.na(month))

ecoli$Create.date <- substr(ecoli$Create.date, 1,10) 


# Now check the difference in complete dates
ecoli$Collection.date <- as.Date(ecoli$Collection.date)
ecoli$Create.date <- as.Date(ecoli$Create.date)

diff_in_days_ecoli <- as.data.frame(as.numeric(ecoli$Create.date-ecoli$Collection.date))
summary(diff_in_days_ecoli)
sd(diff_in_days_ecoli)


# Now lets repeat the process for salmonella 
salmonella <- salmonella %>% filter(!is.na(year) & !is.na(day) & !is.na(month))
salmonella$Create.date  <- substr(salmonella$Create.date,1,10)
salmonella$Create.date <- as.Date(salmonella$Create.date)
salmonella$Collection.date <- as.Date(salmonella$Collection.date)

diff_in_days_sal <- as.data.frame(as.numeric(salmonella$Create.date-salmonella$Collection.date))
summary(diff_in_days_sal)
sd(diff_in_days_sal)

# Repeat for camplyobacter
campylobacter <- campylobacter %>% filter(!is.na(year) & !is.na(day) & !is.na(month))
campylobacter$Create.date  <- substr(campylobacter$Create.date,1,10)
campylobacter$Create.date <- as.Date(campylobacter$Create.date)
campylobacter$Collection.date <- as.Date(campylobacter$Collection.date)

diff_in_days_camp <- as.data.frame(as.numeric(campylobacter$Create.date-campylobacter$Collection.date))
summary(diff_in_days_camp)
sd(diff_in_days_camp)


# We will additionally create density plots to view the distributions along with 
# the summary statistics that were created to show our concern about imputation 

names(diff_in_days_camp) <- c("diff_camp")
names(diff_in_days_ecoli) <- c("diff_ecoli")
names(diff_in_days_sal) <- c("diff_sal")

# Plot the densities 
ggplot(data = diff_in_days_camp, aes(x=diff_camp, color="red")) +
  geom_density() + 
  geom_density(data = diff_in_days_ecoli, aes(x = diff_ecoli, color ="blue")) + 
  geom_density(data = diff_in_days_sal, aes(x = diff_sal, color="green")) + 
  labs(title = "Densities of differences in days", 
       x ="Difference in days") + 
  scale_color_manual(name = "Pathogen", 
                       labels = c("Camplyobacter", 
                                  "E. Coli", 
                                  "Salmonella"), 
                       values = c("red", "blue", "green"))




