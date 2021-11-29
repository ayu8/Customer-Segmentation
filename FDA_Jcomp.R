rm(list = ls())       #clear the environment window

setwd("E:/B. Tech/5th Sem/CSE3505 - Data Analytics/Project")
myData = read.csv("Mall_Customers.csv")

is.null(myData)         #checking null values
dim(myData)             #checking the dimensions of our dataset
head(myData)            #first 6 observations
summary(myData)
str(myData)
colnames(myData)
colnames(myData)[4] <- "AnnualIncome"
colnames(myData)[5] <- "SpendingScore"
colnames(myData)


# Analysing 'Age' attribute
unique(myData$Age)
length(unique(myData$Age))
summary(myData$Age)
sd(myData$Age)
hist(myData$Age,
     main = "Distribution of age of all customers",
     xlab = "Age",
     ylab = "Count or Frequency",
     labels = TRUE,
     col = "red")
boxplot(myData$Age,
        col="#ff0066",
        main="Boxplot of Age",
        ylab = "Age")


# Analysing Gender column
unique(myData$Gender)
length(unique(myData$Gender))
#summary(myData$Gender)
p = table(myData$Gender)
p
barplot(p,
        main = "Barplot for Gender distribution",
        ylab = "Count",
        xlab = "Gender",
        col = rainbow(2),
        legend = names(p))
p/sum(p)
pie_lbls = paste(names(p), "->", p/sum(p)*100, "%", sep = " ")
#install.packages("plotrix")
library(plotrix)
pie3D(p, labels = pie_lbls, main="pie chart for gender distribution")


# Analysing Annual Income column
unique(myData$AnnualIncome)
length(unique(myData$AnnualIncome))
summary(myData$AnnualIncome)
sd(myData$AnnualIncome)
hist(myData$AnnualIncome,
     col="#660033",
     main="Histogram for Annual Income",
     xlab="Annual Income  (in k$)",
     ylab="Frequency or Count",
     labels=TRUE)
q = density(myData$AnnualIncome)
plot(q, 
     main = "Density curve for Annual Income",
     xlab = "Annual Income (in k$)",
     ylab = "Density",
     col = "blue",
     type = "l")
polygon(q, col="pink")


# Analysing Spending Score column
unique(myData$SpendingScore)
length(unique(myData$SpendingScore))
summary(myData$SpendingScore)
sd(myData$SpendingScore)

boxplot(myData$SpendingScore,
        horizontal=TRUE,
        col="pink",
        main="BoxPlot for Spending Score")

hist(myData$SpendingScore,
     col="#f696ae",
     main="Histogram of Spending Score",
     xlab="Spending Score",
     ylab="Frequency or Count",
     labels=TRUE)

library(ggplot2)

ggplot(myData) + geom_point(aes(x = Age, y = AnnualIncome, col = Gender))
ggplot(myData) + geom_point(aes(x = Age, y = SpendingScore, col = Gender))
ggplot(myData) + geom_point(aes(x = AnnualIncome, y = SpendingScore, col = Gender))


#K-means clustering algo
df1 = myData[,4:5]
library(ClusterR)
opt <- Optimal_Clusters_KMeans(df1, max_clusters = 10, plot_clusters = T)       #for elbow method
opt <- Optimal_Clusters_KMeans(df1, max_clusters = 10, plot_clusters = T, criterion = 'silhouette')

set.seed(1)
km = kmeans(df1, centers = 5, iter.max = 100, nstart = 25)
km
#library(factoextra)
#library(cluster)

df1 = cbind(df1, km$cluster)
colnames(df1)[3] = "ClusterNumber"
ggplot(df1) + geom_point(aes(x = AnnualIncome, y = SpendingScore, col = as.factor(ClusterNumber))) + scale_color_discrete(name="Cluster Number")


#heirarchial clustering algorithm
df2 = myData[,4:5]
distance_matrix <- dist(df2)     # Euclidean Distance
hc_obj <- hclust(distance_matrix, method = 'complete')
#install.packages("dendextend")
library(dendextend)
clusters_of_customers <- cutree(hc_obj, k = 5)
dendogram_of_customers <- as.dendrogram(hc_obj)
colorful_dend <- color_branches(dendogram_of_customers, k = 5)
plot(colorful_dend)

library(dplyr)
df2 = mutate(df2, cluster=clusters_of_customers)
ggplot(df2, aes(x = AnnualIncome, y = SpendingScore, color = factor(cluster))) + geom_point() + scale_color_discrete(name = 'Cluster number')
