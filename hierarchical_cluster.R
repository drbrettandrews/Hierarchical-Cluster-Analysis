#install packages
install.packages("tidyverse")
install.packages("cluster")
install.packages("fpc")
install.packages("factoextra")
install.packages("janitor")

#load libraries
library(tidyverse)
library(cluster)
library(fpc)
library(factoextra)
library(janitor)

#set working directory (change this for your computer)
setwd("#####")

#read in datafile
magdf<-read.csv("young_professional_magazine.csv")
View(magdf)

#Create a data frame with only binary variables - Gender, 
#Real.Estate.Purchases, Graduate.Degree, Have.Children - 
#by removing columns 1, 4, 5, and 7
bindf<-magdf[c(-1,-4,-5,-7)]
View(bindf)

#calculate distance between each pair of observations using the dist function 
#and manhattan distance
match_dist<-dist(bindf, method="manhattan")

#run hierarchical clustering with the hclust function and group average linkage
cl_match_avg<-hclust(match_dist, method="average")

#plot the dendrogram
plot(cl_match_avg)

#Create 4 clusters using the cutree function
cl_match_avg_4<-cutree(cl_match_avg, k=4)

#display vector of cluster assignments for each observation
cl_match_avg_4

#visualize clusters on the dendrogram
rect.hclust(cl_match_avg, k=4, border=2:4)

#link cluster assignments to original categorical data frame
hcl4df<-cbind(bindf, clusterID=cl_match_avg_4)

#write data frame to CSV file to analyze in Excel
write.csv(hcl4df, "magazine_hier4_clusters.csv")

#display number of observations in each cluster
hcl4df %>%
  group_by(clusterID) %>%
  summarize(n())

#attach value labels to binary variables
hcl4df$Female<-factor(hcl4df$Female,levels=c(0,1),labels=c("no","yes"))
hcl4df$Real.Estate.Purchases<-factor(hcl4df$Real.Estate.Purchases,levels=c(0,1),labels=c("No","Yes"))
hcl4df$Graduate.Degree<-factor(hcl4df$Graduate.Degree,levels=c(0,1),labels=c("No","Yes"))
hcl4df$Have.Children<-factor(hcl4df$Have.Children,levels=c(0,1),labels=c("No","Yes"))

#Create frequency tables for each variable overall
tabyl(hcl4df$Female)
tabyl(hcl4df$Real.Estate.Purchases)
tabyl(hcl4df$Graduate.Degree)
tabyl(hcl4df$Have.Children)

#Create frequency tables for each variable by cluster
tabyl(hcl4df,Female,clusterID) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits=2) %>%
  adorn_ns()

tabyl(hcl4df,Real.Estate.Purchases,clusterID) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits=2) %>%
  adorn_ns()

tabyl(hcl4df,Graduate.Degree,clusterID) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits=2) %>%
  adorn_ns()

tabyl(hcl4df,Have.Children,clusterID) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits=2) %>%
  adorn_ns()



