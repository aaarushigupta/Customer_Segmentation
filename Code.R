library(ggplot2)
library(RColorBrewer)
library(purrr)
library(cluster) 
library(gridExtra)
library(grid)
library(NbClust)
library(factoextra)

customer_data <- read.csv("Mall_Customers.csv");

str(customer_data)
names(customer_data)
head(customer_data)

barplot(table(customer_data$Gender), main = "Gender Comparision", 
        xlab = "Gender", 
        ylab = "Number of people", 
        ylim = c(0,120),
        col = brewer.pal(3, "Set2"),
        border = "white")

hist(customer_data$Age, main = "Age Distribution",
     xlab = "Age",
     ylab = "Frequency",
     ylim = c(0,40),
     col = brewer.pal(8, "Set3"),
     border = "white",
     labels = T)

hist(customer_data$Annual.Income..k..,
     col=brewer.pal(8,"Accent"),
     main="Histogram for Annual Income",
     xlab="Annual Income Class",
     ylab="Frequency",
     border = "white",
     labels=TRUE)

plot(density(customer_data$Annual.Income..k..),
     col="yellow",
     main="Density Plot for Annual Income",
     xlab="Annual Income Class",
     ylab="Density")
polygon(density(customer_data$Annual.Income..k..),
        col=brewer.pal(3,"Dark2"))

hist(customer_data$Spending.Score..1.100.,
     main="HistoGram for Spending Score",
     xlab="Spending Score Class",
     ylab="Frequency",
     col=brewer.pal(11,"Set3"),
     border = "white",
     labels=TRUE)

fviz_nbclust(customer_data[,3:5], kmeans, method = "wss", k.max = 24) + theme_minimal() + ggtitle("the Elbow Method") 

fviz_nbclust(customer_data[,3:5], kmeans, method = "silhouette", k.max = 24) + theme_minimal() + ggtitle("The Silhouette Plot")

gap_stat <- clusGap(customer_data[,3:5], FUN = kmeans, nstart = 30, K.max = 24, B = 50)
fviz_gap_stat(gap_stat) + theme_minimal() + ggtitle("Gap Statistic Plot")

k5<-kmeans(customer_data[,3:5],5,iter.max=100,nstart=50,algorithm="Lloyd")
k5

pcclust=prcomp(customer_data[,3:5],scale=FALSE)
summary(pcclust)
pcclust$rotation[,1:3]

set.seed(1)
ggplot(customer_data, aes(x =Annual.Income..k.., y = Spending.Score..1.100.)) + 
      geom_point(stat = "identity", aes(color = as.factor(k5$cluster))) +
      scale_color_discrete(name=" ",
                           breaks=c("1", "2", "3", "4", "5"),
                           labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5")) +
      ggtitle("Segments of Mall Customers", subtitle = "Using K-means Clustering") + labs(x = "Annual Income", y = "Spending Score")
