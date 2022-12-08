library(rpart)
library(pROC)
library(DescTools)

source("Outbreak_function.R") # The function we will use 

# Load data and add outbreak variable
ecoli <- read.csv("final_ecoli.csv")
ecoli <- ecoli %>% filter(!is.na(month) & !is.na(year) & !is.na(Min.same)) 
ecoli <- add_outbreak(ecoli)

# Clean SNP Cluster 
SNP_df <- as.data.frame(table(as.factor(ecoli$SNP.cluster)))
snp_included <- SNP_df %>% filter(Freq > 100)
levels_snp <- as.vector(snp_included$Var1)
ecoli$SNP.cluster <- case_when(ecoli$SNP.cluster %in% levels_snp ~ ecoli$SNP.cluster, 
                               is.na(ecoli$SNP.cluster) ~ ecoli$SNP.cluster,
                               !(ecoli$SNP.cluster %in% levels_snp) ~ "Other")


# Clean AMR
AMR_df <- as.data.frame(table(as.factor(ecoli$AMR.genotypes)))
AMR_included <- AMR_df %>% filter(Freq > 300)
levels_AMR <- as.vector(AMR_included$Var1)
ecoli$AMR.genotypes <- case_when(ecoli$AMR.genotypes %in% levels_AMR ~ ecoli$AMR.genotypes, 
                                 is.na(ecoli$AMR.genotypes) ~ ecoli$AMR.genotypes,
                                 !(ecoli$AMR.genotypes %in% levels_AMR) ~ "Other")

# Drop unused variables
ecoli <- ecoli %>% select(-c(day, month_year, Location, region, Min.same, year)) %>% filter(!is.na(Strain))
ecoli[] <- lapply(ecoli, function(x){return(as.factor(x))})

# Classification Tree
tree_ecoli <- rpart(outbreak ~ ., data = ecoli, control=rpart.control(minsplit = 20, minbucket = 10,cp =.001)) 

plot(tree_ecoli,  
     main = "Intial tree model",  
     sub = "Figure 2. Intial tree model, prior to pruning")  
text(tree_ecoli) 

plotcp(tree_ecoli, sub = "Figure 3. How x error changes based on complexity") 
printcp(tree_ecoli) 

# Predict with Tree
predict_ecoli_tree <- as.data.frame(predict(tree_ecoli, ecoli))
names(predict_ecoli_tree) <- c("prob_tree")
ecoli_append <- cbind(ecoli, predict_ecoli_tree)

# ROC Analysis
ecoli_acc <- roc(outbreak ~ prob_tree, data =ecoli_append)
#AUC
ecoli_AUC = round(auc(ecoli_acc), 2)

# Brier Score
ecoli_Brscr = round(sum((predict_ecoli_tree[,2] - (as.numeric(ecoli$outbreak)-1))^2)/length(ecoli$outbreak),2)

# ROC Plot
ggroc(ecoli_acc) + ggtitle('E. Coli Classification Tree ROC Curve') + 
  annotate("text", x = .91, y = .95, label = paste0('AUC = ', ecoli_AUC), color = 'red') +
  annotate("text", x = .85, y = .9, label = paste0('Brier Score = ', ecoli_Brscr), color = 'red')
