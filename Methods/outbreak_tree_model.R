library(rpart)
library(pROC)

ecoli <- read.csv("final_ecoli.csv")

source("Outbreak_function.R")

ecoli <- ecoli %>% filter(!is.na(month) & !is.na(year) & !is.na(Min.same)) 
ecoli <- add_outbreak(ecoli)

ecoli <- ecoli %>% select(-c(day, month_year, Location, region, Min.same, year)) %>% filter(!is.na(Strain))
ecoli[] <- lapply(ecoli, function(x){return(as.factor(x))})

tree <- rpart(outbreak ~ ., data = ecoli, control=rpart.control(minsplit = 20, minbucket = 20,cp =.001)) 

plot(tree, main = "Outbreak Classification Tree")
text(tree) 

plotcp(tree, sub = "Change in x error with increasing complexity") 
printcp(tree) 

predicted_tree <- as.data.frame(predict(tree, ecoli))
names(predicted_tree) <- c("prob_tree")
ecoli_append <- cbind(ecoli, predicted_tree)

acc1 <- roc(outbreak ~ prob_tree, data =ecoli_append)
plot(acc1, main = "Outbreak Classification Tree ROC")
auc(acc1)

