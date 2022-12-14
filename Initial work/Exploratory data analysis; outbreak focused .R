

# It is of interest for us to detect and predict outbreaks and create 
# tools for public health officials to determine if a case is part 
# of an outbreak 


#~~~~~~~~~~~~~~~~~~~~~#
## Data and packages ##
#~~~~~~~~~~~~~~~~~~~~~#


rm(list=ls())

library(kableExtra)
library(tidyverse)
library(ggpubr)
library(mice) 
library(glmnet)  
library(bestglm)
library(pROC)
library(DescTools)
library(caret)
library(kableExtra)
library(ggpubr)
library(rpart)
library(rpart.plot)

setwd("~/Desktop/Semester_3/Practical/Final") # Where all our data is 
salmonella <- read.csv("final_salmonella.csv")
ecoli <- read.csv("final_ecoli.csv")
source("functions_outbreaks_and_lasso.R")
source("compress_levels.R")


#~~~~~~~~~~~~~~~~~#
## Add outbreaks ## 
#~~~~~~~~~~~~~~~~~#

# We want to add the outbreaks to our data and 

ecoli <- add_outbreak(ecoli, 10)
salmonella <- add_outbreak(salmonella,10)

# We note that this plot is not cleaned as 
# in depth as others as it is a supplementary figure  


# Proportion of cases in an outbreak 
prportion_outbreaks_ecoli <- ecoli %>% dplyr::filter(!is.na(year)) %>% 
                                       dplyr::group_by(as.factor(year)) %>% 
                                       dplyr::summarize(num= round(sum(outbreak)/n(),3))

prportion_outbreaks_salm <- salmonella %>% dplyr::filter(!is.na(year)) %>% 
                                           dplyr::group_by(as.factor(year)) %>% 
                                           dplyr::summarize(num= round(sum(outbreak)/n(),3))

props_outbreaks <- left_join(prportion_outbreaks_ecoli, prportion_outbreaks_salm, by = c("as.factor(year)"))


names(props_outbreaks) <- c("Year", "E. Coli", "Salmonella")

props_outbreaks %>% kableExtra::kbl(caption = "Proportion of outbreaks in a year  by pathogen",
                    booktabs=T, escape=T, align = "c") %>%
  kableExtra::kable_styling(full_width = FALSE, latex_options = c('hold_position'))



# We are interested in the proportion of cases in an outbreak in each 
ecoli <- ecoli %>% filter(!is.na(month)  & !is.na(year))
breaks <- ecoli %>% group_by(month, year) %>% dplyr::summarize(total =sum(outbreak)) %>% ungroup()

ggplot(data = breaks, aes(x = as.numeric(month), y =total)) +
  geom_point(aes(color = as.factor(year))) +
  geom_smooth() + 
  scale_x_continuous(breaks=seq(1,12,1), labels=c("Jan", "Feb", 
                                                  "Mar", "Apr", "May", 
                                                  "Jun", "Jul", "Aug", 
                                                  "Sep", "Oct", 
                                                  "Nov", "Dec")) +
  theme(panel.background = element_rect(fill="transparent"),
        axis.line=element_line(size=0.2)) +
  geom_smooth() + 
  theme_minimal() +
  labs(title= "Number of Monthly Cases in an outbreak", 
       x = "", 
       y = "Number of outbreak related cases")  + 
  scale_color_discrete(name = "Year") +
  theme(axis.line=element_line(size=0.3),
        legend.position = "bottom")


# Salmonella 
salmonella <- salmonella %>% filter(!is.na(month)  & !is.na(year))
breaks_sal <- salmonella %>% group_by(month, year) %>% dplyr::summarize(total =sum(outbreak)) %>% ungroup()

ggplot(data = breaks_sal, aes(x = as.numeric(month), y =total)) +
  geom_point(aes(color = as.factor(year))) +
  geom_smooth() + 
  scale_x_continuous(breaks=seq(1,12,1), labels=c("Jan", "Feb", 
                                                  "Mar", "Apr", "May", 
                                                  "Jun", "Jul", "Aug", 
                                                  "Sep", "Oct", 
                                                  "Nov", "Dec")) +
  theme(panel.background = element_rect(fill="transparent"),
        axis.line=element_line(size=0.2)) +
  geom_smooth() + 
  theme_minimal() +
  labs(title= "Number of Monthly Cases in an outbreak", 
       x = "", 
       y = "Number of outbreak related cases")  + 
  scale_color_discrete(name = "Year") +
  theme(axis.line=element_line(size=0.3),
        legend.position = "bottom")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
## Frequency of SNP clusters over time ## 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


# Get the top 7 SNPs and their frequency over time 
SNP_sal <- salmonella %>% 
           filter(!is.na(SNP.cluster)) %>%
           group_by(SNP.cluster) %>% 
           dplyr::summarize(num = n())  %>% 
           slice_max(num, n  = 7) 
top_snps_sal <- unlist(SNP_sal)
top_snps_sal <- as.data.frame(top_snps_sal)
top_sal <-  as.vector(top_snps_sal[1:7,])

# Extract the number of cases yearly 
sal_sub_snp <- salmonella %>% 
               filter(SNP.cluster %in% top_sal) %>% 
               group_by(SNP.cluster, year) %>% 
               dplyr::summarize(num = n())


# Repeat for E. Coli, get top cases 
SNP_eco <- ecoli %>%
           filter(!is.na(SNP.cluster)) %>% 
           group_by(SNP.cluster) %>% 
           dplyr::summarize(num = n())  %>% 
           slice_max(num, n  = 7) 
top_snps_eco <- unlist(SNP_eco)
top_snps_eco <- as.data.frame(top_snps_eco)
top_eco <-  as.vector(top_snps_eco[1:7,])

# Extract the number of cases yearly 
eco_sub_snp <- ecoli %>% 
               filter(SNP.cluster %in% top_eco) %>% 
               group_by(SNP.cluster, year) %>% 
               dplyr::summarize(num = n())


# Get the data together for a plot 
sal_sub_snp <- as.data.frame(sal_sub_snp)
sal_sub_snp$pathogen <- "Salmonella"
eco_sub_snp <- as.data.frame(eco_sub_snp)
eco_sub_snp$pathogen <- "E. coli"

snp_over_time <- rbind(sal_sub_snp, eco_sub_snp)

# Now we can make the plot based on SNP cluster 

plot <- ggplot(data=snp_over_time, aes(x= year, y = num, color = SNP.cluster))+
         facet_grid(rows=vars(pathogen))  + 
         geom_point() +
         geom_line() +
         labs(title= "Number of Monthly Cases in Each Year; SNP cluters", 
              x = "", 
              y = "Number of cases")  + 
         scale_color_discrete(name = "Year") +
         theme(axis.line=element_line(size=0.3),
               legend.position = "bottom")

annotate_figure(plot, 
                bottom = text_grob("Figure 3: Number of cases from each of the top 
                                   7 SNP clusters over our time frame", 
                                   color = "black",   hjust = 1, x = 1, 
                                   face = "italic", size = 10),)

