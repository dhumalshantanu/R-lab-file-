##Practical 10 

#Load libraries 
install.packages("factoextra")
library(factoextra)

#Perform K-means clustering with K = 4 on the iris dataset and compare with species labels.
iris_data <- iris[, -5]
set.seed(123)
kmeans_iris <- kmeans(iris_data, centers = 4, nstart = 20)
table(Cluster = kmeans_iris$cluster, Species = iris$Species)

#Apply clustering on mtcars using mpg, hp, and wt. Interpret clusters.
mtcars_data <- mtcars[, c("mpg", "hp", "wt")]
set.seed(123)
kmeans_mtcars <- kmeans(scale(mtcars_data), centers = 3, nstart = 20)
kmeans_mtcars$centers
mtcars_clustered <- cbind(mtcars, Cluster = kmeans_mtcars$cluster)
head(mtcars_clustered)

#Use the elbow method to find optimal number of clusters for iris.
wss <- numeric(10)
for (k in 1:10) {
  km <- kmeans(iris_data, centers = k, nstart = 20)
  wss[k] <- km$tot.withinss
}

plot(1:10, wss, type = "b", pch = 19,
     xlab = "Number of Clusters (K)",
     ylab = "Total Within-Cluster Sum of Squares",
     main = "Elbow Method for Optimal K - Iris")

#Visualize clusters using PCA-based scatterplot in factoextra.
fviz_cluster(kmeans_iris, data = iris_data,
             geom = "point",
             ellipse.type = "norm",
             main = "K-means Clustering on Iris (K=4)")

#Create a subset of iris with only Sepal.Length and Sepal.Width and cluster it.
iris_subset <- iris[, c("Sepal.Length", "Sepal.Width")]

set.seed(123)
kmeans_subset <- kmeans(iris_subset, centers = 3, nstart = 20)

fviz_cluster(kmeans_subset, data = iris_subset,
             geom = "point",
             main = "Clustering using Sepal.Length & Sepal.Width")
