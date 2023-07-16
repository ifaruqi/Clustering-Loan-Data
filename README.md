# Clustering-Loan-Data

## Data
The loan data contains loans issued through 2012-2013, including the current loan status (Current, Late, Fully Paid, etc.) and latest payment information. 
Features (variables) include credit scores, number of finance inquiries, address including zip codes and state, and collections among others. 
Collections indicates whether the customer has missed one or more payments and the team is trying to recover their money.

## Cluster Analysis
The analysis of loan applicant data resulted in identifying four distinct clusters based on loan amounts, income, and instalment payments. 
Cluster 1 comprises young individuals with low income who took small loans with lower instalment values. 
Cluster 2 consists of high- income applicants who took larger loans at higher interest rates for longer periods. 
Medium- income earners are divided into Cluster 3 and 4, with the prior includes those who took larger loans and paid higher instalments, and the latter comprises applicants who took comparatively lower loans and paid lower instalments and total payment.

## Validation
In this study, I conducted in-sample validation by randomly selecting 100 subsamples from sample dataset of 500 observations. I then clustered the subsamples and compared the result with the clustering solution from the sample dataset to determine the accuracy of our clustering solution. 
I performed 5-fold cross-validation and obtained the result of 94.6% accuracy. The result shows that our clustering solution has a good generalizability and can be used for further analysis.

## Non Performing Loan
The distribution of the bad loans in each of the four clusters was analysed. Cluster 1 has the
largest number of good and bad loans due to the high number of observations in this cluster.
However, when calculating percentages, the proportion of bad loans in this cluster (and cluster
4) are similar to the population mean (15%). Cluster 2 has the highest percentage of bad loans
(17%), indicating that most of the bad loans would be classified to this cluster. Finally, cluster
3 has the smallest number and percentage of bad loans (7%), indicating that most of the
observations in this cluster were good loans. Thus, the bank should be more confident on giving
loan to borrowers from cluster 3.
