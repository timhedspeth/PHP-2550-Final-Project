# This is an updated preprocessing script for our data of interest, where
# we add a region variable to the data set for analysis 



#~~~~~~~~~~~~~~~~~~~~~#
## Packages and data ## 
#~~~~~~~~~~~~~~~~~~~~~#

library(tidyverse)
library(kableExtra)
library(anytime)
library(lubridate)
library(rlang)
source("compress_levels.R")
source("functions_outbreaks_and_lasso.R")
# Data sets 
setwd("~/Desktop/Semester_3/Practical/Final") 
ecoli <- read.csv("final_ecoli_date.csv")
campylobacter  <- read.csv("final_campylobacter_date.csv")
salmonella <- read.csv("final_salmonella_date.csv")




#~~~~~~~~~~~~~~~~~~~~~~#
## Dropping variables ## 
#~~~~~~~~~~~~~~~~~~~~~~#

# We want to drop variables that have a very high degree of 
# missingness in all 

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
  kbl(caption = "Percent missing for each illness dataset", booktabs=T, escape=F, align = "c") %>%
  kable_styling(full_width = FALSE, latex_options = c('hold_position'))


# Variables that we will drop and why:
 #  Host  disease: large degree of missingness in all! 
 #  Isolation source: we have a new variable for this! 
 #  Lat.long: least degree of missingness is .81!
 #  Source type: all missing! 
 #  Outbreak: This is all missing! 
 
ecoli <- ecoli %>% dplyr::select(-c(Host.disease, Lat.Lon, Source.type, Outbreak))
campylobacter <- campylobacter %>% dplyr::select(-c(Host.disease, Lat.Lon, Source.type, Outbreak))
salmonella <- salmonella %>% dplyr::select(-c(Host.disease, Lat.Lon, Source.type, Outbreak))


#~~~~~~~~~~~#
## Regions ## 
#~~~~~~~~~~~#


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


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
## Which variables will be dropped from each ## 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


# We know that X.organism sole provides the name of what we are examining
# Create date and Collection date can be dropped as  we already have date info
# Isolation source: we have our own 
# Latitude and longitude won't be helpful for analysis 
# So this will be dropped from all  



ecoli <- ecoli %>% dplyr::select(-c(X.Organism.group, Create.date, Collection.date, 
                             Isolation.source))
campylobacter <- campylobacter %>% dplyr::select(-c(X.Organism.group, Create.date,
                                             Collection.date, Isolation.source))
salmonella <- salmonella %>% dplyr::select(-c(X.Organism.group, Create.date, 
                                       Collection.date, Isolation.source))



## E. coli ## 

apply(ecoli, 2, function(x){return(sum(is.na(x))/length(x))})


# Serovar = 87% missing 
# Host disease = 89% missing
# Isolation type = 68 % missing
# Source.type = 99% missing 
# Min.diff = 85% missing 
# outbreak = 99% missing 
# Computed types = 100% missing 

ecoli <- ecoli %>% dplyr::select(-c(Serovar,Min.diff, Computed.types))

apply(ecoli, 2, function(x){return(sum(is.na(x))/length(x))})



# We now want to drop variables that do not have enough 
# unique levels to reclassify and reclassify the levels of 
# those that do have enough info so that the analysis is not 
# overwhelmed 


## E. coli ## 

# Strain 

ecoli$Strain[ecoli$Strain == "E. coli"] <- "Ecoli"
ecoli$Strain <- as.vector(compress_levels_factors(ecoli, 'Strain'))

# AMR genotype
ecoli$AMR.genotypes <- compress_levels_factors(ecoli, 'AMR.genotypes') # Enough granularity 

# Isolate Identifiers 
compress_levels_factors(ecoli, 'Isolate.identifiers') # Not enough granularity


# Isolate 
compress_levels_factors(ecoli, 'Isolate') # Not enough granularity


# Bio sample 
compress_levels_factors(ecoli, 'BioSample') # Not enough granularity


# Assembly 
compress_levels_factors(ecoli, 'Assembly') # Not enough granularity


# Remove these variables 
ecoli <- ecoli %>% dplyr::select(-c(Isolate.identifiers, Isolate, BioSample, Assembly))


## Campylobacter ##

apply(campylobacter, 2, function(x){return(sum(is.na(x))/length(x))})


# Serovar = 99% missing 
# Host.disease = 99% missing  
# Source.type = 100% missing 
# Outbreak = 100% missing
# Computed.types = 100% missing
# day = 98%  missing 

campylobacter <- campylobacter %>% dplyr::select(-c(Serovar, Computed.types))


# Strain 
compress_levels_factors(campylobacter, 'Strain') # Not enough granularity  

# Isolate identifiers 
compress_levels_factors(campylobacter, 'Isolate.identifiers') # Not enough granularity  

# Isolate 
compress_levels_factors(campylobacter, 'Isolate') # Not enough granularity  

# Isolation
isolation_levels_df <- as.data.frame(table(as.factor(campylobacter$Isolation.type)))
#isolation_levels_df  %>% filter(Freq > 1) # Keep it this way only 2 levels 

#  Biosample
compress_levels_factors(campylobacter, 'BioSample') # Not enough granularity  

# AMR.genotypes
campylobacter$AMR.genotypes <- compress_levels_factors(campylobacter, 'AMR.genotypes')  

# Remove the variables that don't have enough data 
campylobacter <- campylobacter %>% dplyr::select(-c(Strain, Isolate.identifiers, Isolate, BioSample,day))



## Salmonella ## 

apply(salmonella, 2, function(x){return(sum(is.na(x))/length(x))})


# Host.Disease: 99% missing 
# Source.type: 99%
# Outbreak: 99%
# day: 85% 

salmonella <- salmonella %>% dplyr::select(-c(day))

# Strain 
compress_levels_factors(salmonella, 'Strain') # Delete 

# Isolate.identifiers
compress_levels_factors(salmonella, 'Isolate.identifiers') # Delete 

# Serovar 
salmonella$Serovar <- compress_levels_factors(salmonella, 'Serovar') 

#  Isolates  
compress_levels_factors(salmonella, 'Isolate') # Delete 

# Isolation type 
isolationtype_levels_df <- as.data.frame(table(as.factor(salmonella$Isolation.type)))
#isolationtype_levels_df  %>% filter(Freq > 1) # Keep

# BioSample
compress_levels_factors(salmonella, 'BioSample') # Delete 

# Assembly 
compress_levels_factors(salmonella, 'Assembly') # Delete 

# Computed Types 
salmonella$Computed.types <- compress_levels_factors(salmonella, 'Computed.types') # Delete 

# AMR genotypes 
salmonella$AMR.genotypes <- compress_levels_factors(salmonella, 'AMR.genotypes') # Delete 


# Remove some variables 
salmonella <- salmonella %>% dplyr::select(-c(Strain, Isolate.identifiers, Isolate,
                                       BioSample, Assembly))





#~~~~~~~~~~~~~~~~~~~~~#
## Read the data out ##
#~~~~~~~~~~~~~~~~~~~~~#


write.csv(ecoli, "final_ecoli.csv", row.names = FALSE)
write.csv(campylobacter, "final_campylobacter.csv", row.names = FALSE)
write.csv(salmonella, "final_salmonella.csv", row.names = FALSE)


