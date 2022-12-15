# PHP 2550 Final Project
## Food Borne Pathogens 

Hi! Welcome to our project page! We are a group of graduate students in the department of Biostatistics at Brown University School of Public Health. We are working on a project regarding the outbreak of foodborne illnesses in the United States. In short we will attempt to apply classification methods to predict if a case is part of an outbreak, and derive models that are accessible and understandable to public health officals who may have access to and understand biological information that is avilable, regardless of their statistics background. 

As of .  At this point in time our analysis is focused on exploring the data, regarding Salmonella, E. Coli, and Campylobacter. In our EDA working with the [pathogens data from NCBI](https://www.ncbi.nlm.nih.gov/pathogens/), we explored both spatial (regions of the United States) and temporal (2013-2022) trends in the number of cases of each illness. This will lay the ground work for us to apply machine learning time series or other statistical methodlogies to the data to answer our question of interest. Thus we will be continually updating this repository, we currently have a folder for our initial analysis, that will serve as a building block for the rest of this project. The code and associated data used for the exploratory analysis is avilable in the [Initial Work](https://github.com/timhedspeth/PHP-2550-Final-Project/tree/main/Initial%20work) folder. We have advanced to the modeling phase of our work, in which we attempt to look at the seasonality of the viruses. where we now are attempting We have last updated this repository (12/15/22). 

## Guide to repositories and code  

### Initital Work 

This project uses the [R programing language, version 4.1.3](https://www.r-project.org/) while also requiring the use of [Rstudio](https://www.rstudio.com/) with typesetting done in [LaTeX via overleaf](https://www.overleaf.com/login). 

The first scripts used are: [Salomella_LocationClean.R](https://github.com/timhedspeth/PHP-2550-Final-Project/blob/main/Initial%20work/Salmonella_LocationClean.R), [campyobacter_LocationClean.R](https://github.com/timhedspeth/PHP-2550-Final-Project/blob/main/Initial%20work/campylobacter_LocationClean.R), and [ecoli_LocationClean.R](https://github.com/timhedspeth/PHP-2550-Final-Project/blob/main/Initial%20work/ecoli_LocationClean.R) which are used to reduce our data to observations that are from the United States. We next found it pertient to reduce the levels of the *Isolation_source* variable to fewer levels so that they can be used in analysis, this is achieved with the [source_groups.R](https://github.com/timhedspeth/PHP-2550-Final-Project/blob/main/source_groups.R) script. We lastly found issues regarding the *Colection.date* variable, which required cleaning for use in analysis, which is done in the [Clean_dates.R](https://github.com/timhedspeth/PHP-2550-Final-Project/blob/main/Initial%20work/Clean_dates.R) file. 

With preprocessing completed we next moved to the exploratory phase of our analysis. The script file used for this analysis is [EDA.R](
https://github.com/timhedspeth/PHP-2550-Final-Project/blob/main/Initial%20work/EDA.R). 

### Methods 

We have moved into the modeling phase of this project, where we hope to identify trends in seasonality, and identify outbreaks with harmonic regression, logistic regression and Tree based classification models. This code is avilable in [Initial_methods.R](https://github.com/timhedspeth/PHP-2550-Final-Project/blob/main/Methods/Initial_methods.R) file under the methods file. We also decided to to create a function that can define an outbreak for data that has been proocessed in a similar way to ours in an effort to have a function that allows us to varry the outbreak measure we are interested in, which is found in [Outbreak.R](https://github.com/timhedspeth/PHP-2550-Final-Project/blob/main/Methods/Outbreak_function.R).




If you have any questions please feel to reach out:

anthony_girard1@brown.edu 

timothy_hedspeth@brown.edu

yutong_li1@brown.edu 
