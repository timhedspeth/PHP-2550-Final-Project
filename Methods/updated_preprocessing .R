# This is an updated preprocessing script for our data of interest, where
# we add a region variable to the data set for analysis 



#~~~~~~~~~~~~~~~~~~~~~#
## Packages and data ## 
#~~~~~~~~~~~~~~~~~~~~~#

library(tidyverse)
library(kableExtra)

# Data sets 
setwd("~/Desktop/Semester_3/Practical/Final") 
ecoli <- read.csv("final_ecoli_date.csv")
campylobacter  <- read.csv("final_campylobacter_date.csv")
salmonella <- read.csv("final_salmonella_date.csv")


#~~~~~~~~~~~~~~~~~~~~~~#
## Dropping variables ## 
#~~~~~~~~~~~~~~~~~~~~~~#

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


# Variables that we will drop and why:
 #  Host  disease: large degree of missingness in all! 
 #  Isolation source: we have a new variable for this! 
 #  Lat.long: least degree of missingness is .81!
 #  Source type: all missing! 
 #  Outbreak: This is all missing! 
 
#ecoli <- ecoli %>% select(-c(Host.disease, Lat.Lon, Source.type, Outbreak))
#campylobacter <- campylobacter %>% select(-c(Host.disease, Lat.Lon, Source.type, Outbreak))
#salmonella <- salmonella %>% select(-c(Host.disease, Lat.Lon, Source.type, Outbreak))


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

ecoli <- ecoli %>% select(-c(X.Organism.group, Create.date, Collection.date, Isolation.source, Lat.Lon))
campylobacter <- campylobacter %>% select(-c(X.Organism.group, Create.date, Collection.date, Isolation.source, Lat.Lon))
salmonella <- salmonella %>% select(-c(X.Organism.group, Create.date, Collection.date, Isolation.source, Lat.Lon))



## E. coli ## 

apply(ecoli, 2, function(x){return(sum(is.na(x))/length(x))})


# Serovar = 87% missing 
# Host disease = 89% missing
# Isolation type = 68 % missing
# Source.type = 99% missing 
# Min.diff = 85% missing 
# outbreak = 99% missing 
# Computed types = 100% missing 

ecoli <- ecoli %>% select(-c(Serovar, Host.disease, Isolation.type, Source.type,
                             Min.diff, Outbreak, Computed.types))

apply(ecoli, 2, function(x){return(sum(is.na(x))/length(x))})

# Strains 
strain_levels_df <- as.data.frame(table(as.factor(ecoli$Strain)))
strain_included <-  strain_levels_df %>% filter(Freq > 15)
levels_strain <- as.vector(strain_included$Var1) 
ecoli$Strain <- case_when(ecoli$Strain %in% levels_strain ~ ecoli$Strain, 
                          is.na(ecoli$Strain) ~ ecoli$Strain,
                          !(ecoli$Strain %in% levels_strain) ~ "Other")
ecoli$Strain[ecoli$Strain == "E. coli"] <- "Ecoli"

# Isolate Identifiers 
identifiers_levels_df <- as.data.frame(table(as.factor(ecoli$Isolate.identifiers)))
#identifiers_levels_df %>% filter(Freq == 2)

# There are only a few that have 2 observations, none more than that, we drop it 

# Isolate 
isolate_levels_df <- as.data.frame(table(as.factor(ecoli$Isolate)))
#isolate_levels_df %>% filter(Freq > 1)

# No levels are unique, delete 

# SNP Cluster 
SNP_df <- as.data.frame(table(as.factor(ecoli$SNP.cluster)))
snp_included <- SNP_df %>% filter(Freq > 100)
levels_snp <- as.vector(snp_included$Var1)
ecoli$SNP.cluster <- case_when(ecoli$SNP.cluster %in% levels_snp ~ ecoli$SNP.cluster, 
                          is.na(ecoli$SNP.cluster) ~ ecoli$SNP.cluster,
                          !(ecoli$SNP.cluster %in% levels_snp) ~ "Other")

# Biosample 
sample_df <- as.data.frame(table(as.factor(ecoli$BioSample)))
#sample_df %>%  filter(Freq > 2)

# Bio sample only has 26 with Freq = 2, we will delete it 

assembly_df <- as.data.frame(table(as.factor(ecoli$Assembly)))
#assembly_df %>% filter(Freq == 1)

#  Assembly is unique to each, delete 

# AMR
AMR_df <- as.data.frame(table(as.factor(ecoli$AMR.genotypes)))
AMR_included <- AMR_df %>% filter(Freq > 300)
levels_AMR <- as.vector(AMR_included$Var1)
ecoli$AMR.genotypes <- case_when(ecoli$AMR.genotypes %in% levels_AMR ~ ecoli$AMR.genotypes, 
                               is.na(ecoli$AMR.genotypes) ~ ecoli$AMR.genotypes,
                               !(ecoli$AMR.genotypes %in% levels_AMR) ~ "Other")




ecoli <- ecoli %>% select(-c(Isolate.identifiers, Isolate, BioSample, Assembly))



#~~~~~~~~~~~~~~~~~~~~~#
## Read the data out ##
#~~~~~~~~~~~~~~~~~~~~~#


write.csv(ecoli, "final_ecoli.csv", row.names = FALSE)
write.csv(campylobacter, "final_campylobacter.csv", row.names = FALSE)
write.csv(salmonella, "final_salmonella.csv", row.names = FALSE)


