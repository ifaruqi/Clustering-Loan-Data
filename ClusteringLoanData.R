## ----setup, include=FALSE------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----warning=FALSE, include=FALSE----------------------------------------------------------------------------------------------------------------------

# Install and load required packages

# install.packages("factoextra")
# install.packages("cluster")
# install.packages("dplyr")
# install.packages("psych")
# install.packages("psychTools")
# install.packages("readxl")
# install.packages("tidyverse")
# install.packages("devtools")
#install_github("kbroman/broman")
library(devtools)
library(factoextra)
library(cluster)
library(tidyverse)
library(dplyr)
library(psych)
library(psychTools)
library(readxl)
library(splitstackshape)
library(ROSE)
library(plyr)
library(corrplot)
library(ggpubr)
library(gridExtra)
library(cluster)
library(dbscan)
library(stats)
library(arsenal)
library(pvclust)
library(plotly)
library(cluster)
library(scatterplot3d)




## ----warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------
# Load the dataset
df <- read_excel("loan_data_ADA_assignment.xlsx")
describe(df)
summary(df)
str(df)


## ----message=FALSE, warning=FALSE----------------------------------------------------------------------------------------------------------------------
# Get the clear idea what variables contain the most NA value
datatype = summarise(df, lapply(df, class))
na <- as.data.frame(t(summarise_all(df, ~sum(is.na(.x)))))
na <- cbind(VariableName = rownames(na), na)
na <- cbind(dataType = datatype, na)
rownames(na) <- NULL
colnames(na)[3] <- "NAValues"
colnames(na)[1] <- "DataType"
na <- na[order(-na$NAValues),]

na$DataType <- as.character(na$DataType)


## ----warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------
#picked the relevant variables
fdf <- df %>% select (annual_inc,dti,emp_length,installment,int_rate,loan_amnt,term,total_acc,total_pymnt,grade,home_ownership,purpose,loan_is_bad) 



## ----warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------
set.seed(10)

#sampling 500 observations from the data
sample <- fdf[sample(nrow(fdf), 500, replace = FALSE), ]
describe(sample)
headTail(sample)
summary(sample)
str(sample)

#changing the emp_length from NAS to zero
sample$emp_length[is.na(sample$emp_length)] <-0 
summarise_all(sample,~sum(is.na(.x)))

#selecting only integer variables
sample_only_int <- sample[,-c(10,11,12,13)]



## ----warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------

#normalise data using custom function
minMax <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
 
#scaling
sample_only_int <- as.data.frame(lapply(sample_only_int, minMax))
head(sample_only_int)




## ----warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------
( sampleMatrix<-round(cor(sample_only_int),2) )

lowerCor(sample_only_int)

KMO(sample_only_int)
#overall MSA is 0.51, so there is multicollinearity in this data, as confirmed by Correlation matrix

cortest.bartlett(sample_only_int)



## ----warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------

pcs_in_sample = sample_only_int %>% select(installment,int_rate,loan_amnt,term,total_acc,total_pymnt)   #or sample_only_int

sample_non_pcs <- sample_only_int %>% select(annual_inc,dti,emp_length)  #total_rec_late_fee and recoveries contains mostly zeroes



## ----warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------

#correlation between variables selected for PC
( sampleMatrix<-round(cor(pcs_in_sample),2) )

lowerCor(pcs_in_sample)

#checking KMO for variables selected for PC
KMO(pcs_in_sample)
options(scipen = 9999)
cortest.bartlett(pcs_in_sample)


## ----warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------
#Finding Mahalanobis distance
Maha <- mahalanobis(sample_only_int,colMeans(sample_only_int),cov(sample_only_int))

MahaPvalue <-pchisq(Maha,df=8,lower.tail = FALSE)
print(sum(MahaPvalue<0.01))



## ----warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------
#first we will do PCA on selected columns sample, then proceed on doing it on sample_outp (sample without the outliers)
pca <- prcomp(pcs_in_sample, center = TRUE, scale. =TRUE)
pca


## ----warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------
#Getting Eigen values for all PCs
eig.val <- get_eigenvalue(pca)
eig.val


## ----warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------
#Scree plot
fviz_eig(pca, addlabels = TRUE, ylim=c(0,80))



## ----warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------
pca_vars = get_pca_var(pca)
print(pca_vars)
pca_vars$coord

corrplot(pca_vars$cos2, is.corr=FALSE)



## ----warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------
#plotting the top10 rows which contributed to the PC
fviz_contrib(pca,choice="ind",axes=1:2,top=10)



## ----warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------
sampleMaha<-cbind(sample_only_int, Maha, MahaPvalue)

#sample after removing outliers
sample_outp = subset(sampleMaha,MahaPvalue>0.01)

sample_outp <- sample_outp[,-c(1,2,3,10,11)]

pca_outp <- prcomp(sample_outp, center = TRUE, scale. =TRUE)
pca_outp

#Eigen value of PCs after removing outliers
eig.val.outp <- get_eigenvalue(pca_outp)
eig.val.outp



## ----warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------
#visulaization of amount of variance explained by the dimensions (not cumulative) for sample after removing outliers
fviz_eig(pca_outp, addlabels = TRUE, ylim=c(0,70))


#visulaization of amount of variance explained by the dimensions (not cumulative) for sample before removing outliers
fviz_eig(pca, addlabels = TRUE, ylim=c(0,70))




## ----warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------
#using principal() to find PC weights and scores for each row
pcModel<-principal(pcs_in_sample, 6, rotate="none", weights=TRUE, scores=TRUE)

print.psych(pcModel, cut=0.3, sort = TRUE)
plot(pcModel$values, type = "b")



## ----warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------
#Oblimin rotation
pcModel3o<-principal(pcs_in_sample, 3, rotate="oblimin")
print.psych(pcModel3o, cut=0.3, sort=TRUE)

pcModel4o<-principal(pcs_in_sample, 4, rotate="oblimin")
print.psych(pcModel4o, cut=0.3, sort=TRUE)

#Varimax rotation
pcModel3v<-principal(pcs_in_sample, 3, rotate="varimax")
print.psych(pcModel3v, cut=0.3, sort=TRUE)

pcModel4v<-principal(pcs_in_sample, 4, rotate="varimax")
print.psych(pcModel4v, cut=0.3, sort=TRUE)

#Quartimax rotation
pcModel3q<-principal(pcs_in_sample, 3, rotate="quartimax")
print.psych(pcModel3o, cut=0.3, sort=TRUE)

pcModel4q<-principal(pcs_in_sample, 4, rotate="quartimax")
print.psych(pcModel4o, cut=0.3,sort=TRUE)




## ----warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------
#3 factor
faModel_ML <- (fa(pcs_in_sample, 3, n.obs=500, rotate="none", fm="ml"))
print(faModel_ML, cut=0.3,sort="TRUE")
fa.diagram(faModel_ML)

# 4 factors
faModel_ML <- (fa(pcs_in_sample, 4, n.obs=500, rotate="none", fm="ml"))
print(faModel_ML, cut=0.3,sort="TRUE")
fa.diagram(faModel_ML)


#3 factor oblimin
fa3o<-(fa(pcs_in_sample,3, n.obs=500, rotate="oblimin", fm="ml"))
print.psych(fa3o, cut=0.3,sort="TRUE")
fa.diagram(fa3o)

#4 factor oblimin
fa4o<-(fa(pcs_in_sample,4, n.obs=500, rotate="oblimin", fm="ml"))
print.psych(fa4o, cut=0.3,sort="TRUE")
fa.diagram(fa4o)

#3 factor varimax
fa3v<-(fa(pcs_in_sample,3, n.obs=500, rotate="varimax", fm="ml"))
print.psych(fa3v, cut=0.3,sort="TRUE")
fa.diagram(fa3v)

#4 factor varimax
fa4v<-(fa(pcs_in_sample,4, n.obs=500, rotate="varimax", fm="ml"))
print.psych(fa4v, cut=0.3,sort="TRUE")
fa.diagram(fa4v)

#3 factor quartimax
fa3q<-(fa(pcs_in_sample,3, n.obs=500, rotate="quartimax", fm="ml"))
print.psych(fa3q, cut=0.3,sort="TRUE")
fa.diagram(fa3q)

#4 factor quartimax
fa4q<-(fa(pcs_in_sample,4, n.obs=500, rotate="quartimax", fm="ml"))
print.psych(fa4q, cut=0.3,sort="TRUE")
fa.diagram(fa4q)




## ----warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------
#PC model scores
print.psych(pcModel, cut=0.3, sort=TRUE)


## ----warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------
pcModel$weights


## ------------------------------------------------------------------------------------------------------------------------------------------------------
head(pcModel$scores, 10)


## ----warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------

#binding the PC scores for each row with corresponding nonPc variables.
sample_withPC <- cbind(sample_non_pcs, pcModel$scores)

#removing last 3 columns because we are selecting only the first 3 Principal components which explains 89.4% of the variance.
sample_withPC <- sample_withPC[,-c(7,8,9)]
head(sample_withPC)


## ----warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------
#computing gapstat tpo find the optimum number of clusters
gap_stat <- clusGap(sample_withPC, FUN = hcut, nstart = 25, K.max = 10, B = 50)  #just PCs is giving no of clusters as 1.; sample and sample with PCs gives 3.
fviz_gap_stat(gap_stat)


## ----warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------
#confirming the optimum number of clusters with elbow method
fviz_nbclust(sample_withPC, kmeans, method = "wss")



## ----warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------
set.seed(123)

km <- kmeans(sample_withPC, centers = 4, nstart = 25, iter.max=100000)
km



## ----warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------
#loading cluster labels to a vector "clus"
clus <- factor(km$cluster)

#binding the categorical variables back to the sample
sample_new = cbind(sample_withPC,sample[,c(10,11,12,13)])

#adding cluster labels to the sample
sample_new = cbind(sample_new,clus)
head(sample_new)



## ----warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------
library(cluster)

# Compute the silhouette measure for each data point
sil_scores <- silhouette(km$cluster, dist(sample_new))

# Compute the average silhouette score for all data points
avg_sil_score <- mean(sil_scores[, 3])

# Print the average silhouette score
cat("Average Silhouette Score:", round(avg_sil_score, 2))



## ----warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------
library(openxlsx)

# Write the dataframe to an Excel file
write.xlsx(sample_new, file = "sample_after_clustering.xlsx", sheetName = "Sheet1")



## ----warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------

#getting the mean values for each column of the sample
hcentres<-aggregate(x=sample, by=list(cluster=clus), FUN="mean")
print(hcentres)



## ----warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------

ggplot(sample, aes(installment, loan_amnt, color = clus)) + 
  geom_point() + 
  theme_bw()



## ----warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------

ggplot(sample, aes(grade, int_rate, color = clus)) + 
  geom_jitter() + 
  theme_bw()



## ----warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------

ggplot(sample, aes(annual_inc, grade, color = clus)) + 
  geom_point() + xlim(10000,250000) +
  theme_bw()



## ----warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------

ggplot(sample, aes(term,int_rate, color = clus)) + 
  geom_point() + 
  theme_bw()



## ----warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------
ggplot(sample, aes(loan_amnt,int_rate, color = clus)) + 
  geom_point() + xlim(1000,50000) +                         
  theme_bw()


## ----warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------
ggplot(sample_new, aes(x = PC1, y = PC2, color = as.factor(km$cluster))) +
  geom_point() +
  labs(title = "K-Means Clustering with 4 Centers", x = "Principal Component 1", y = "Principal Component 2")



## ----warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------
#Representation of the same in 3-d including the 3rd Principal component.
library(plotly)

plot_ly(sample_new, x = ~PC1, y = ~PC2, z = ~PC3, color = ~as.factor(km$cluster), type = "scatter3d", mode = "markers") %>%
  layout(scene = list(xaxis = list(title = "Principal Component 1"),
                      yaxis = list(title = "Principal Component 2"),
                      zaxis = list(title = "Principal Component 3"),
                      color = list(title = "Cluster")))



## ----warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------
library(rgl)
with(sample_new, plot3d(PC1, PC2, PC3, col = km$cluster, type = 's', size = 2))



## ----warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------

# Create a function to calculate the frequency table and plot the distribution for each categorical variable
freq_plot <- function(df, col) {
  freq_tbl <- df %>% group_by(clus, !!sym(col)) %>% summarise(count = n())
  
  ggplot(freq_tbl, aes(x = !!sym(col), y = count, fill = clus)) + 
    geom_bar(stat = "identity", position = "dodge") + 
    ggtitle(paste("Distribution of", col)) + 
    xlab(col) + ylab("Frequency") +
    theme(plot.title = element_text(hjust = 0.5))
}

#we will Call the function for each categorical variable later





## ----warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------
table(sample$grade,sample_new$clus)

freq_plot(sample_new, "grade")

ggplot(sample_new, aes(x = grade, fill = clus)) +
  geom_bar() +
  labs(title = "Distribution of Grade", x = "Grade", y = "Frequency")




## ----warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------
table(sample$home_ownership,sample_new$clus)

freq_plot(sample_new, "home_ownership")

ggplot(sample_new, aes(x = home_ownership, fill = clus)) +
  geom_bar() +
  labs(title = "Distribution of Home Ownership", x = "Home-Ownership", y = "Frequency")




## ----warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------
table(sample$purpose,sample_new$clus)

ggplot(sample_new, aes(x = purpose, fill = clus)) +
  geom_bar() +
  labs(title = "Distribution of Purpose", x = "Purpose", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))




## ----warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------
ggplot(sample_new, aes(x = purpose, fill = clus)) +
  geom_bar() +
  labs(title = "Distribution of Purpose", y = "Frequency") +
  scale_x_discrete(limits = c("car", "home_improvement", "house","major_purchase","medical","moving","other","renewable_energy","small_business","vacation","wedding")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))




## ----warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------
table(sample$loan_is_bad,sample_new$clus)

freq_plot(sample_new, "loan_is_bad")

ggplot(sample_new, aes(x = loan_is_bad, fill = clus)) +
  geom_bar() +
  labs(title = "Distribution of loan_is_bad", x = "Loan is Bad", y = "Frequency")





## ----warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------

(1-mean(df$loan_is_bad))*100

loan_is_bad_table = as.data.frame(as.matrix(table(sample$loan_is_bad,sample_new$clus)))
loan_is_bad_table <- loan_is_bad_table %>% pivot_wider(names_from = Var2,values_from = c(Freq))

colnames(loan_is_bad_table) <- c("Loan is Bad","Cluster 1","Cluster 2", "Cluster 3", "Cluster 4")

# Create a new row with the percentage values
percentages <- c("Percentage", rep(0, ncol(loan_is_bad_table) - 1))
for (i in 2:ncol(loan_is_bad_table)) {
  percentages[i] <- (loan_is_bad_table[1, i] / (loan_is_bad_table[1, i] + loan_is_bad_table[2, i]))*100
}

# Combine the original dataframe with the new row
loan_is_bad_table <- rbind(loan_is_bad_table, percentages)

loan_is_bad_table




## ----warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------

library(broom)
library(dplyr)

# Conduct t-tests
ttest_1_2 <- t.test(annual_inc + dti + emp_length + PC1 + PC2 + PC3 ~ clus, data = sample_withPC, subset = (clus %in% c(1,2)))
ttest_2_3 <- t.test(annual_inc + dti + emp_length + PC1 + PC2 + PC3 ~ clus, data = sample_withPC, subset = (clus %in% c(2,3)))
ttest_3_4 <- t.test(annual_inc + dti + emp_length + PC1 + PC2 + PC3 ~ clus, data = sample_withPC, subset = (clus %in% c(3,4)))
ttest_1_4 <- t.test(annual_inc + dti + emp_length + PC1 + PC2 + PC3 ~ clus, data = sample_withPC, subset = (clus %in% c(1,4)))


# Store results in dataframe
ttest_results <- bind_rows(
  glance(ttest_1_2),
  glance(ttest_2_3),
  glance(ttest_3_4),
  glance(ttest_1_4)
) %>%
  select(estimate, statistic, p.value)

# View dataframe
ttest_results



## ----warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------
set.seed(123)


#creating a sample of 100 observation for validation from our sample of 500 observation.
validation_sample <- sample_withPC[sample(nrow(sample_withPC),100,replace=FALSE),]

#doing K-Means on those 100 observation
km_val <- kmeans(validation_sample, centers = 4, nstart = 25, iter.max=100000)
km_val

validation_sample <- cbind(validation_sample,km_val$cluster)


clus_val <- as.data.frame(km_val$cluster)

#finding index of all those 100 observations from our sample!
index_val <- row.names(clus_val)


val_analysis <- sample[index_val, , drop = FALSE]

cluster_orginal <- sample_new[index_val, , drop = FALSE]$clus

sample_withPC_100 = sample_withPC[index_val,]



#sample of 100 observations with cluster labels
sample_withPC_100 <- cbind(sample_withPC_100,km_val$cluster,cluster_orginal)

sample_withPC_100$`km_val$cluster`[sample_withPC_100$`km_val$cluster`==1] <- 9
sample_withPC_100$`km_val$cluster`[sample_withPC_100$`km_val$cluster`==2] <- 1
sample_withPC_100$`km_val$cluster`[sample_withPC_100$`km_val$cluster`==9] <- 2


sample_withPC_100$`km_val$cluster` == sample_withPC_100$cluster_orginal

count(sample_withPC_100$`km_val$cluster` == sample_withPC_100$cluster_orginal)




## ----warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------
hcentres<-aggregate(x=val_analysis, by=list(cluster=km_val$cluster), FUN="mean")
print(hcentres)

