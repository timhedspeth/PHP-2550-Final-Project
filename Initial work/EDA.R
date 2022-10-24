
## This is a file for the EDA for the final Project for PHP 2550 at Brown Univeristy, this 
## code is also avilable in the Rmarkdown file Final_project-EDA_markdown.RMD also on 
## this github 


#~~~~~~~~~~~~#
## Packages ## 
#~~~~~~~~~~~~#

library(lubridate) # working with dates 
library(tidyverse) # working with data 
library(kableExtra) # Make nicer plots 
library(ggpubr) # Arrange GGplots 

#~~~~~~~~#
## Data ## 
#~~~~~~~~#

# Set the working directory and read in the preprocessed data 
setwd("~/Desktop/Semester_3/Practical/Final")
salmonella <- read.csv("final_salmonella_date.csv")
ecoli <- read.csv("final_ecoli_date.csv")
campylobacter <- read.csv("final_Campylobacter_date.csv")

# Suppress summaries info when knitting to a pdf 
options(dplyr.summarise.inform = FALSE)
library(dplyr, warn.conflicts = FALSE)
options(tidyverse.quiet = TRUE)



#~~~~~~~~~~~~~~~~#
## Missing Data ## 
#~~~~~~~~~~~~~~~~#

# The first objective of this EDA is to explore the missing patterns in our 
# data at the surface level 


# Replace Missing strings with NA 
salmonella[salmonella == ""] <- NA 
ecoli[ecoli == ""] <- NA
campylobacter[campylobacter == ""] <- NA 


## Missingness by column ## 

# Salmonella
na_by_cols_sal <- as.data.frame(apply(salmonella,2, is.na))
sum_na_by_col_sal <- apply(na_by_cols_sal,2,sum)/nrow(na_by_cols_sal)
sum_na_by_col_sal <- as.data.frame(round(sum_na_by_col_sal,2))

# E. Coli 
na_by_cols_ecoli <- as.data.frame(apply(ecoli,2, is.na))
sum_na_by_col_ecoli <- apply(na_by_cols_ecoli,2,sum)/nrow(na_by_cols_ecoli)
sum_na_by_col_ecoli <- as.data.frame(round(sum_na_by_col_ecoli,2))

# Camplyobaceter 
na_by_cols_camp <- as.data.frame(apply(campylobacter,2, is.na))
sum_na_by_col_camp <- apply(na_by_cols_camp,2,sum)/nrow(na_by_cols_camp)
sum_na_by_col_camp <- as.data.frame(round(sum_na_by_col_camp,2))

# Get the data together 
Missing_by_col <- cbind(sum_na_by_col_sal, sum_na_by_col_ecoli, sum_na_by_col_camp)
names(Missing_by_col) <- c("x", "y", "z") # Create a dummy variable for filtering 
Missing_by_col <- Missing_by_col %>% filter(x != 0 & 
                                              y != 0 & 
                                              z != 0   ) # Filter out all variables with no missing data 
names(Missing_by_col) <- c("Salmonella", "E. Coli", "Campylobacter")

# Print the data frame 
Missing_by_col  %>% 
  kbl(caption = "Percent missing for each illness dataset", booktabs=T,    escape=F, align = "c") %>%
  kable_styling(full_width = FALSE, latex_options = c('hold_position'))



## Missingness by row

# Using the commented out code below we check to see if any of the data is 
# complete for a given observation 

#sum(complete.cases(salmonella))
#sum(complete.cases(ecoli))
#sum(complete.cases(campylobacter))

# There are no complete cases in the data set 

# Salmonella 
na_by_row_sal <- as.data.frame(apply(salmonella,1, is.na))
sum_na_by_row_sal <- apply(na_by_row_sal,2,sum)/nrow(na_by_row_sal)
salmonella_missing <- as.data.frame(sum_na_by_row_sal)

# E. Coli 
na_by_row_ecoli <- as.data.frame(apply(ecoli,1, is.na))
sum_na_by_row_ecoli <- apply(na_by_row_ecoli,2,sum)/nrow(na_by_row_ecoli)
ecoli_missing <- as.data.frame(sum_na_by_row_ecoli)

# Campylobacter
na_by_row_camp <- as.data.frame(apply(campylobacter,1, is.na))
sum_na_by_row_camp <- apply(na_by_row_camp,2,sum)/nrow(na_by_row_camp)
campylobacter_missing <- as.data.frame(sum_na_by_row_camp)

# Make a plot of missingness by observation, This did not make it into the final EDA 
ggplot(data = salmonella_missing, aes(x = sum_na_by_row_sal, color = "sum_na_by_row_sal")) + 
  geom_density(alpha = .9) + 
  geom_density(data = ecoli_missing, aes(x = sum_na_by_row_ecoli,
                                         color = "sum_na_by_row_ecoli"), alpha = .9) + 
  geom_density(data = campylobacter_missing, aes(x = sum_na_by_row_camp, 
                                                 color = "sum_na_by_row_camp"), alpha = .9)


## Delete variables with most of the values missing 

salmonella <- salmonella[,-which(names(salmonella) %in% c("Serovar", "Host.Disease", "Lat.Lon", "Source.type", "Outbreak"))]
ecoli <- ecoli[,-which(names(ecoli) %in% c("Serovar", "Host.Disease", "Lat.Lon", "Source.type", "Outbreak"))]
campylobacter <- campylobacter[,-which(names(campylobacter) %in% c("Serovar", "Host.Disease", "Lat.Lon", "Source.type", "Outbreak"))]



# Let us consider data by year in general 



# How many Samonella cases per year? 
Num_per_year_salmonella <- salmonella %>% 
  group_by(year) %>% 
  summarize(num = n())

# How many ecoli cases per year? 
Num_per_year_ecoli <- ecoli %>% 
  group_by(year) %>% 
  summarize(num = n())

# How many ecoli cases per year? 
Num_per_year_campylobacter <- campylobacter %>% 
  group_by(year) %>% 
  summarize(num = n())

# Lets look at the trend over time, and 
figure1 <- ggplot(data = Num_per_year_salmonella, aes(x = year, y = num, color = "Salmonella")) +
  geom_point() + 
  geom_line() +
  # E coli 
  geom_point(data = Num_per_year_ecoli, aes(x = year, y = num, color = "E coli")) + 
  geom_line(data = Num_per_year_ecoli, aes(x = year, y = num, color = "E coli")) + 
  # campylobacter
  geom_point(data = Num_per_year_campylobacter, aes(x = year, y = num, color = "Campylobacter")) + 
  geom_line(data = Num_per_year_campylobacter, aes(x = year, y = num, color = "Campylobacter")) + 
  theme_minimal() + 
  labs(title= "Number of Food-Borne Illness cases by year", 
       x = "Year", 
       y = "Number of cases")  + 
  scale_color_discrete(name = "Illness")

# Print the figure out 
annotate_figure(figure1, 
                bottom = text_grob("Figure 1: Number of cases per year, all illnesses", color = "black",   hjust = 1, x = 1, face = "italic", size = 10),)



# We can also look to trends over regions in the US 


## Define the regions in all the data sets 

salmonella <- salmonella %>% mutate(region = case_when(Location %in% c("ME", "VT", "NH", 
                                                                       "MA", "CT", "RI", 
                                                                       "NY", "NJ", "PA", 
                                                                       "Northeast") ~ "Northeast", 
                                                       Location %in% c("DE", "MD", "DC", 
                                                                       "WV", "VA", "NC", 
                                                                       "SC", "GA", "FL", 
                                                                       "AL", "MS", "LA", 
                                                                       "TX", "OK", "AR", 
                                                                       "TN", "KY", 
                                                                       "South") ~ "South", 
                                                       Location %in% c("OH", "MI", "IN", 
                                                                       "IL", "WI", "MN", 
                                                                       "IA", "MO", "KS", 
                                                                       "NE", "SD", "ND", 
                                                                       "Midwest") ~ "Midwest", 
                                                       Location %in% c("NM", "CO", "WY", 
                                                                       "MT", "ID", "UT", 
                                                                       "AZ", "NV", "WA", 
                                                                       "OR", "CA", "HI", 
                                                                       "AK", "West", 
                                                                       "Western Region") ~ "West", 
                                                       Location %in% c("USA", "PR", "GU") ~ "USA, General"))


ecoli <- ecoli %>% mutate(region = case_when(Location %in% c("ME", "VT", "NH", 
                                                             "MA", "CT", "RI", 
                                                             "NY", "NJ", "PA", 
                                                             "Northeast") ~ "Northeast", 
                                             Location %in% c("DE", "MD", "DC", 
                                                             "WV", "VA", "NC", 
                                                             "SC", "GA", "FL", 
                                                             "AL", "MS", "LA", 
                                                             "TX", "OK", "AR", 
                                                             "TN", "KY", 
                                                             "South") ~ "South", 
                                             Location %in% c("OH", "MI", "IN", 
                                                             "IL", "WI", "MN", 
                                                             "IA", "MO", "KS", 
                                                             "NE", "SD", "ND", 
                                                             "Midwest") ~ "Midwest", 
                                             Location %in% c("NM", "CO", "WY", 
                                                             "MT", "ID", "UT", 
                                                             "AZ", "NV", "WA", 
                                                             "OR", "CA", "HI", 
                                                             "AK", "West", 
                                                             "Western Region") ~ "West", 
                                             Location %in% c("USA", "PR", "GU") ~ "USA, General"))

campylobacter <- campylobacter %>% mutate(region = case_when(Location %in% c("ME", "VT", "NH", 
                                                                             "MA", "CT", "RI", 
                                                                             "NY", "NJ", "PA", 
                                                                             "Northeast") ~ "Northeast", 
                                                             Location %in% c("DE", "MD", "DC", 
                                                                             "WV", "VA", "NC", 
                                                                             "SC", "GA", "FL", 
                                                                             "AL", "MS", "LA", 
                                                                             "TX", "OK", "AR", 
                                                                             "TN", "KY", 
                                                                             "South") ~ "South", 
                                                             Location %in% c("OH", "MI", "IN", 
                                                                             "IL", "WI", "MN", 
                                                                             "IA", "MO", "KS", 
                                                                             "NE", "SD", "ND", 
                                                                             "Midwest") ~ "Midwest", 
                                                             Location %in% c("NM", "CO", "WY", 
                                                                             "MT", "ID", "UT", 
                                                                             "AZ", "NV", "WA", 
                                                                             "OR", "CA", "HI", 
                                                                             "AK", "West", 
                                                                             "Western Region") ~ "West", 
                                                             Location %in% c("USA", "PR", "GU") ~ "USA, General"))

# Now we will try to look at the proportion of cases in each region in each  year

# Salmonella
cases_by_region_sal <- salmonella %>% filter(!is.na(year)) %>% 
  group_by(region, year) %>% 
  summarize(num = n()) %>% 
  ungroup() %>%  
  group_by(year) %>%
  summarize(region = region, percent = round(num/sum(num),2)) %>%
  pivot_wider(names_from = region, values_from = percent) 



# E. Coli

cases_by_region_ecoli <- ecoli %>% filter(!is.na(year)) %>% 
  group_by(region, year) %>% 
  summarize(num = n()) %>% 
  ungroup() %>%  
  group_by(year) %>% 
  summarize(region = region, percent = round(num/sum(num),2)) %>% 
  pivot_wider(names_from = region, values_from = percent) 



# campylobacter

cases_by_region_camp <- campylobacter %>% filter(!is.na(year)) %>% 
  group_by(region, year) %>% 
  summarize(num = n()) %>% 
  ungroup() %>%  
  group_by(year) %>% 
  summarize(region = region, percent = round(num/sum(num),2)) %>%                                                              pivot_wider(names_from = region, values_from = percent) 

## Get the data into a table 
cases_region <- as.data.frame(rbind(cases_by_region_sal, cases_by_region_ecoli, cases_by_region_camp))
cases_region[31:33, ] <- ""
cases_region <- apply(cases_region, 2, as.character)
cases_region[31, 1] <- "Salomella"
cases_region[32, 1] <- "E. Coli"
cases_region[33, 1] <- "Campylobacter"

# Rearrange the data frame 
cases_region <- cases_region[c(31,1:10,32,11:20,33,21:30), c(1,5,2,3,4,6)]

# Print the data to a kable table 
cases_region %>% 
  kbl(caption = "Percent of each years total cases by region of US by Illness", booktabs=T,    escape=F, align = "c") %>%
  kable_styling(full_width = FALSE, latex_options = c('hold_position'))



## We now want to look at our Isolation sources,  for each of the Illnesses by year 

# Salmonella # 

# Get the data for the data 
plotdata_sal <- salmonella %>% group_by(year, Isolation.source.category) %>% 
  summarize(num = n())

# Create a plot 
plot5 <- ggplot(data = plotdata_sal, aes(x = year, y = num, color = Isolation.source.category)) + 
  geom_line() + 
  geom_point() + 
  theme(legend.position = "none") + 
  labs(title= "Salmonella", 
       x = "", 
       y = "Number of cases")


# E. Coli # 

# Get the data 
plotdata_ecoli <- ecoli %>% group_by(year, Isolation.source.category) %>% 
  summarize(num = n())

# Plot the data 
plot6 <- ggplot(data = plotdata_ecoli, aes(x = year, y = num, color = Isolation.source.category)) + 
  geom_line() + 
  geom_point() + 
  theme(legend.position = "none") +
  labs(title= "E. Coli", 
       x = "", 
       y = "Number of cases")  + 
  scale_color_discrete(name = "Illness source")


# Campylobacter #

# Get the data for the plot 
plotdata_camp <- campylobacter %>% group_by(year, Isolation.source.category) %>%
  summarize(num = n())

# Create the plot 
plot7 <- ggplot(data = plotdata_camp, aes(x = year, y = num, color = Isolation.source.category)) + 
  geom_line() + 
  geom_point() +
  labs(title= "Campylobacter", 
       x = "Year", 
       y = "Number of cases") + 
  scale_color_discrete(name = "Isolation Source")

# Arrange and annotate the plots 
figure3 <- ggarrange(plot5, plot6, nrow =2)
figure3 
annotate_figure(plot7, 
                bottom = text_grob("Figure 3: Number of cases by Isolation Source", color = "black",   hjust = 1, x = 1, face = "italic", size = 10),)

# Lets look at Min same as a metric for similarity and defining an outbreak 
# Generally all of the codes below are to find the percent of missing data, % which 
# are closely related (within 7 SNPs) and those that are distant 


##  Salmonella ## 

# Get percents for each of the 3 categories, missing, closely related, not closely related 
Missing_min_same <- length(which(is.na(salmonella$Min.same)))/nrow(salmonella)
close_min_same <- length(which(salmonella$Min.same <= 7))/nrow(salmonella)
far_min_same <- length(which(salmonella$Min.same > 7))/nrow(salmonella)

# Make the data frame and reaarange  
min_same_salmonella <- data.frame()
min_same_salmonella[1,1] <- "Missing"
min_same_salmonella[1,2] <- round(Missing_min_same,2)
min_same_salmonella[2,1] <- "Closely related (within 7 SNPs)"
min_same_salmonella[2,2] <- round(close_min_same,2)
min_same_salmonella[3,1] <- "Not closely related"
min_same_salmonella[3,2] <- round(far_min_same,2)
names(min_same_salmonella) <- c("Minimum Same", "Percent")


# E. Coli ## 

# Get percents for each of the 3 categories, missing, closely related, not closely related 
Missing_min_same <- length(which(is.na(ecoli$Min.same)))/nrow(ecoli)
close_min_same <- length(which(ecoli$Min.same <= 7))/nrow(ecoli)
far_min_same <- length(which(ecoli$Min.same > 7))/nrow(ecoli)

# Make the data frame and reaarange  
min_same_ecoli <- data.frame()
min_same_ecoli[1,1] <- "Missing"
min_same_ecoli[1,2] <- round(Missing_min_same,2)
min_same_ecoli[2,1] <- "Closely related (within 7 SNPs)"
min_same_ecoli[2,2] <- round(close_min_same,2)
min_same_ecoli[3,1] <- "Not closely related"
min_same_ecoli[3,2] <- round(far_min_same,2)
names(min_same_ecoli) <- c("Minimum Same", "Percent")



## Campylobacter ## 

# Get percents for each of the 3 categories, missing, closely related, not closely related 
Missing_min_same <- length(which(is.na(campylobacter$Min.same)))/nrow(campylobacter)
close_min_same <- length(which(campylobacter$Min.same <= 7))/nrow(campylobacter)
far_min_same <- length(which(campylobacter$Min.same > 7))/nrow(campylobacter)

# Make the data frame and reaarange  
min_same_campylobacter <- data.frame()
min_same_campylobacter[1,1] <- "Missing"
min_same_campylobacter[1,2] <- round(Missing_min_same,2)
min_same_campylobacter[2,1] <- "Closely related (within 7 SNPs)"
min_same_campylobacter[2,2] <- round(close_min_same,2)
min_same_campylobacter[3,1] <- "Not closely related"
min_same_campylobacter[3,2] <- round(far_min_same,2)
names(min_same_campylobacter) <- c("Minimum Same", "Percent")

## Create a data frame with all the data in a nice format, which requires rearranging 

Minimum_same_illnesses <- as.data.frame(rbind(min_same_salmonella, min_same_ecoli, min_same_campylobacter))
Minimum_same_illnesses[10:12,] <- ""
Minimum_same_illnesses <- apply(Minimum_same_illnesses, 2, as.character)
Minimum_same_illnesses[10,1] <- "Salmonella"
Minimum_same_illnesses[11,1] <- "Ecoli"
Minimum_same_illnesses[12,1] <- "Campylobacter"
Minimum_same_illnesses <- Minimum_same_illnesses[c(10,1:3,11,4:6,12,7:9),]

# print  to a data frame 
Minimum_same_illnesses %>%
  kbl(caption = "Percentanges regarding Closely related SNPs by Illness", booktabs=T,    escape=F, align = "c") %>%
  kable_styling(full_width = FALSE, latex_options = c('hold_position'))





## We can now look at what we define to be outbreaks, in which we find there are more than 100 people
## in a given month that have a closely related strain of an illness 

# Get the month name into a readable form, we only need this for Salmonella and E. Coli as the 
# results are not worthy of a table for Campylobacter 

# Change the months #
salmonella <- salmonella %>% mutate(month1 = case_when(month ==1 ~ "Jan", 
                                                       month ==2 ~ "Feb",
                                                       month ==3 ~ "Mar",
                                                       month ==4 ~ "Apr",
                                                       month ==5 ~ "May",
                                                       month ==6 ~ "Jun",
                                                       month ==7 ~ "Jul",
                                                       month ==8 ~ "Aug",
                                                       month ==9 ~ "Sep",
                                                       month ==10 ~ "Oct",
                                                       month ==11 ~ "Nov",
                                                       month ==12 ~ "Dec"))

ecoli <- ecoli %>% mutate(month1 = case_when(month ==1 ~ "Jan", 
                                             month ==2 ~ "Feb",
                                             month ==3 ~ "Mar",
                                             month ==4 ~ "Apr",
                                             month ==5 ~ "May",
                                             month ==6 ~ "Jun",
                                             month ==7 ~ "Jul",
                                             month ==8 ~ "Aug",
                                             month ==9 ~ "Sep",
                                             month ==10 ~ "Oct",
                                             month ==11 ~ "Nov",
                                             month ==12 ~ "Dec"))



# Salmonella "outbreaks" #

# Look at the number of cases in a region in a given month/year and find how closely related they 
# are on average 
sal_subset <- salmonella %>%
  filter(!is.na(month1)) %>% 
  group_by(year, month1, region) %>% 
  summarize(mean1 = round(mean(Min.same, na.rm = T),2), 
            sd1 = round(sd(Min.same, na.rm = T),2),
            num = n())  %>% 
  ungroup() %>% 
  # Substantial number of cases that are very closely related 
  filter(mean1 < 3 & num > 100) %>% 
  pivot_wider(names_from = region, values_from = c(mean1, sd1))

# Get the data into a data frame, copy and paste the mean and sd in  one column
sal_subset <- as.data.frame(sal_subset)
sal_subset[,4] <- paste(sal_subset[,4], "(", sal_subset[,8], ")")
sal_subset[,5] <- paste(sal_subset[,5], "(", sal_subset[,9], ")")
sal_subset[,6] <- paste(sal_subset[,6], "(", sal_subset[,10], ")")
sal_subset[,7] <- paste(sal_subset[,7], "(", sal_subset[,11], ")")
sal_subset <- sal_subset[,-c(8:11)]
sal_subset <- apply(sal_subset,2,as.character)
sal_subset <- as.data.frame(sal_subset)

# Get rid of NA  (NA) from cells in the data frame 
sal_subset$mean1_Northeast <- ifelse(nchar(sal_subset$mean1_Northeast) == 9, 
                                     mean1_Northeast <- "", 
                                     mean1_Northeast <- sal_subset$mean1_Northeast)
sal_subset$mean1_West <- ifelse(nchar(sal_subset$mean1_West) == 9, 
                                mean1_West <- "", 
                                mean1_West <- sal_subset$mean1_West)
sal_subset$mean1_South <- ifelse(nchar(sal_subset$mean1_South) == 9, 
                                 mean1_South <- "", 
                                 mean1_South <- sal_subset$mean1_South)
sal_subset$mean1_Midwest <- ifelse(nchar(sal_subset$mean1_Midwest) == 9, 
                                   mean1_Midwest <- "", 
                                   mean1_Midwest <- sal_subset$mean1_Midwest)

names(sal_subset) <- c("Year", "Month", "Number of cases", "Midwest", "South", "West", "Northeast")

# Print to a pretty data frame 
sal_subset %>%
  kbl(caption = "Average (Standard Deviation) Min.same for closely related strains when more than 100 cases occur in a region, Salmonella", booktabs=T,    escape=F, align = "c") %>%
  kable_styling(full_width = FALSE, latex_options = c('hold_position'))





# Ecoli  "outbreaks" 

# Look at the number of cases in a region in a given month/year and find how closely related they 
# are on average 
eco_subset <- ecoli %>% filter(!is.na(month1)) %>% 
  group_by(year, month1, region) %>% 
  summarize(mean1 = round(mean(Min.same, na.rm = T),2), 
            sd1 = round(sd(Min.same, na.rm = T),2),
            num = n())  %>% 
  ungroup() %>% 
  # Substantial number of cases that are very closely related 
  filter(mean1 < 3 & num > 100) %>% 
  pivot_wider(names_from = region, values_from = c(mean1, sd1))

# Get the data into a data frame, copy and paste the mean and sd in  one column
eco_subset <- as.data.frame(eco_subset)
eco_subset[,4] <- paste(eco_subset[,4], "(", eco_subset[,8], ")")
eco_subset[,5] <- paste(eco_subset[,5], "(", eco_subset[,9], ")")
eco_subset[,6] <- paste(eco_subset[,6], "(", eco_subset[,10], ")")
eco_subset[,7] <- paste(eco_subset[,7], "(", eco_subset[,11], ")")
eco_subset <- eco_subset[,-c(8:11)]
eco_subset <- apply(eco_subset,2,as.character)

eco_subset <- as.data.frame(eco_subset)
names(eco_subset) <- c("Year", "Month", "Number of cases", "West", "Midwest", "South", "USA")


# Get rid of NA  (NA) from cells in the data frame 
eco_subset$USA <- ifelse(nchar(eco_subset$USA) == 9, 
                         USA <- "", 
                         USA <- eco_subset$USA)
eco_subset$West <- ifelse(nchar(eco_subset$West) == 9, 
                          West <- "", 
                          West <- eco_subset$West)
eco_subset$South <- ifelse(nchar(eco_subset$South) == 9, 
                           South <- "", 
                           South <- eco_subset$South)
eco_subset$Midwest <- ifelse(nchar(eco_subset$Midwest) == 9, 
                             Midwest <- "", 
                             Midwest <- eco_subset$Midwest)

# Print to a pretty data frame 
eco_subset %>%
  kbl(caption = "Average (Standard Deviation) Min.same for closely related strains when more than 100 cases occur in a region, E. Coli", booktabs=T,    escape=F, align = "c") %>%
  kable_styling(full_width = FALSE, latex_options = c('hold_position'))


# Campylobacter 

# Look at the number of cases in a region in a given month/year and find how closely related they 
# are on average 
camp_subset <- campylobacter %>% 
  filter(!is.na(month)) %>% 
  group_by(year, month, region) %>% 
  summarize(mean1 = round(mean(Min.same, na.rm = T),2), 
            sd1 = round(sd(Min.same, na.rm = T),2),
            num = n())  %>% 
  ungroup() %>% 
  # Substantial number of cases that are very closely related 
  filter(mean1 < 3 & num == 43) %>% 
  pivot_wider(names_from = region, values_from = c(mean1, sd1))

## Nothing by our threshold, just the max that we have changed in the summarization above  


