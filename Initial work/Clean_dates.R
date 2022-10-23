## This is a code for PHP 2550 at Brown University SPH, the purpose of which 
## is to clean dates that have varying levels of information e.g. some are 
## complete, some are not, to account for this heterogenity we preprocess 
## the data here so that computation time for the main file is not hindered 
## by this computation 

# We have to consider the date, given that the Colelction date is very bad we will
# have to clean it ourself 


#~~~~~~~~~~~~~~~~~~~~~~#
## Libraries and Data ##
#~~~~~~~~~~~~~~~~~~~~~~# 

library(anytime)
library(lubridate)
library(tidyverse)

setwd("~/Desktop/Semester_3/Practical/Final")
salmonella <- read.csv("final_salmonella.csv")
ecoli <- read.csv("final_ecoli.csv")
campylobacter <- read.csv("final_Campylobacter.csv")

#~~~~~~~~~~~~~~#
## Salmonella ##
#~~~~~~~~~~~~~~# 

# Start with dates that have complete information 
sal_complete_date <- salmonella %>% filter(nchar(Collection.date) > 7)
sal_complete_date$Collection.date <- as.Date(sal_complete_date$Collection.date)
sal_complete_date$year <- year(sal_complete_date$Collection.date)
sal_complete_date$month <- month(sal_complete_date$Collection.date)
sal_complete_date$day <- day(sal_complete_date$Collection.date)

# Some dates have year and month lets get this info 
sal_only_month <- salmonella %>% filter(nchar(Collection.date) == 7)
sal_only_month$Collection.date <- anydate(sal_only_month$Collection.date)
sal_only_month$year <- year(sal_only_month$Collection.date)
sal_only_month$month <- month(sal_only_month$Collection.date)
sal_only_month$day <- NA

# Some dates have only the year, so we must account for this  
sal_only_year <- salmonella %>% filter(nchar(Collection.date) < 7)
sal_only_year$Collection.date <- anydate(sal_only_year$Collection.date)
sal_only_year$year <- year(sal_only_year$Collection.date)
sal_only_year$month <- NA
sal_only_year$day <- NA

salmonella <- rbind(sal_complete_date, sal_only_month, sal_only_year)
write.csv(salmonella, "final_salmonella_date.csv", row.names = FALSE)



#~~~~~~~~~#
## Ecoli ## 
#~~~~~~~~~#

# Start with dates that have complete information 
ecoli_complete_date <- ecoli %>% filter(nchar(Collection.date) > 7)
ecoli_complete_date$Collection.date <- as.Date(ecoli_complete_date$Collection.date)
ecoli_complete_date$year <- year(ecoli_complete_date$Collection.date)
ecoli_complete_date$month <- month(ecoli_complete_date$Collection.date)
ecoli_complete_date$day <- day(ecoli_complete_date$Collection.date)

# Some dates have year and month lets get this info 
ecoli_only_month <- ecoli %>% filter(nchar(Collection.date) == 7)
ecoli_only_month$Collection.date <- anydate(ecoli_only_month$Collection.date)
ecoli_only_month$year <- year(ecoli_only_month$Collection.date)
ecoli_only_month$month <- month(ecoli_only_month$Collection.date)
ecoli_only_month$day <- NA

# Some dates have only the year, so we must account for this  
ecoli_only_year <- ecoli %>% filter(nchar(Collection.date) < 7)
ecoli_only_year$Collection.date <- anydate(ecoli_only_year$Collection.date)
ecoli_only_year$year <- year(ecoli_only_year$Collection.date)
ecoli_only_year$month <- NA
ecoli_only_year$day <- NA

ecoli <- rbind(ecoli_complete_date, ecoli_only_month, ecoli_only_year)
write.csv(ecoli, "final_ecoli_date.csv", row.names = FALSE)



#~~~~~~~~~~~~~~~~~#
## Camploybacter ##
#~~~~~~~~~~~~~~~~~#


# Start with dates that have complete information 
camp_complete_date <- campylobacter %>% filter(nchar(Collection.date) > 7)
camp_complete_date$Collection.date <- as.Date(camp_complete_date$Collection.date)
camp_complete_date$year <- year(camp_complete_date$Collection.date)
camp_complete_date$month <- month(camp_complete_date$Collection.date)
camp_complete_date$day <- day(camp_complete_date$Collection.date)

# Some dates have year and month lets get this info 
camp_only_month <- campylobacter %>% filter(nchar(Collection.date) == 7)
camp_only_month$Collection.date <- anydate(camp_only_month$Collection.date)
camp_only_month$year <- year(camp_only_month$Collection.date)
camp_only_month$month <- month(camp_only_month$Collection.date)
camp_only_month$day <- NA

# Some dates have only the year, so we must account for this  
camp_only_year <- campylobacter %>% filter(nchar(Collection.date) < 7)
camp_only_year$Collection.date <- anydate(camp_only_year$Collection.date)
camp_only_year$year <- year(camp_only_year$Collection.date)
camp_only_year$month <- NA
camp_only_year$day <- NA

campylobacter <- rbind(camp_complete_date, camp_only_month, camp_only_year)
write.csv(campylobacter, "final_campylobacter_date.csv", row.names = FALSE)

