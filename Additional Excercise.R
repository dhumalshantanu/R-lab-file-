#1. Linear Regression

#Manual Implementation
data <- mtcars
x <- data$wt
y <- data$mpg
x_mean <- mean(x)
y_mean <- mean(y)
b1 <- sum((x - x_mean) * (y - y_mean)) / sum((x - x_mean)^2)
b0 <- y_mean - b1 * x_mean
y_pred <- b0 + b1 * x
cat("Manual Linear Regression:\n")
cat("Intercept =", b0, "  Slope =", b1, "\n")
# Model accuracy
SSE <- sum((y - y_pred)^2)
SSR <- sum((y_pred - y_mean)^2)
R2 <- SSR / (SSE + SSR)
cat("R-squared =", R2, "\n")

#Library Implementation
model <- lm(mpg ~ wt, data = mtcars)
summary(model)
plot(mtcars$wt, mtcars$mpg,
     main = "Linear Regression (mpg vs wt)",
     xlab = "Weight (1000 lbs)",
     ylab = "Miles per Gallon",
     pch = 19,
     col = "blue")
abline(model, col = "red", lwd = 2)
print(model)


#2.Logistic Regression

#Manual 
sigmoid <- function(z) {
  1 / (1 + exp(-z))
}
x <- c(1, 2, 3, 4, 5)
y <- c(0, 0, 0, 1, 1)
b0 <- 0
b1 <- 0
lr <- 0.1
epochs <- 1000
for (i in 1:epochs) {
  z <- b0 + b1 * x
  y_pred <- sigmoid(z)
  
  
  db0 <- sum(y_pred - y) / length(y)
  db1 <- sum((y_pred - y) * x) / length(y)
  
  
  b0 <- b0 - lr * db0
  b1 <- b1 - lr * db1
}

cat("Intercept:", b0, "Slope:", b1, "\n")
y_pred_class <- ifelse(sigmoid(b0 + b1 * x) > 0.5, 1, 0)
cat("Predictions:", y_pred_class, "\n")

#Library Implementation
data <- data.frame(x, y)
model <- glm(y ~ x, data = data, family = binomial)
summary(model)

pred <- predict(model, type = "response")
pred_class <- ifelse(pred > 0.5, 1, 0)
cat("Predicted Classes:", pred_class, "\n")


#3. k-nearest neighbours

#Manual
euclidean_distance <- function(a, b) {
  sqrt(sum((a - b)^2))
}

knn_manual <- function(train_data, train_labels, test_point, k) {
  distances <- apply(train_data, 1, function(x) euclidean_distance(x, test_point))
  neighbors <- order(distances)[1:k]
  majority_label <- names(sort(table(train_labels[neighbors]), decreasing = TRUE))[1]
  return(majority_label)
}
train_data <- matrix(c(1,2,2,3,3,4,5,6), ncol=2)
train_labels <- c('A','A','B','B')
test_point <- c(3,3)

predicted <- knn_manual(train_data, train_labels, test_point, k=3)
cat("Predicted Class:", predicted, "\n")

#Library Implementation
library(class)

train_data <- matrix(c(1,2,2,3,3,4,5,6), ncol=2)
train_labels <- c('A','A','B','B')
test_data <- matrix(c(3,3), ncol=2)

pred <- knn(train_data, test_data, train_labels, k=3)
cat("Predicted Class (Library):", pred, "\n")


#4. Decision Tree

#Manual
entropy <- function(y) {
  p <- table(y) / length(y)
  -sum(p * log2(p))
}

information_gain <- function(x, y, split_val) {
  left <- y[x <= split_val]
  right <- y[x > split_val]
  
  total_entropy <- entropy(y)
  weighted_entropy <- (length(left)/length(y))*entropy(left) + 
    (length(right)/length(y))*entropy(right)
  total_entropy - weighted_entropy
}
x <- c(2.7, 1.5, 3.6, 4.2, 3.9, 6.5)
y <- c('A', 'A', 'B', 'B', 'B', 'B')

splits <- sort(unique(x))
gains <- sapply(splits, function(s) information_gain(x, y, s))
best_split <- splits[which.max(gains)]
cat("Best Split Value:", best_split, "\n")

#Library Implementation
library(rpart)
library(rpart.plot)

data <- data.frame(x, y)
model <- rpart(y ~ x, data = data, method = "class")
rpart.plot(model)


#5. k means clustering 

#Manual 
set.seed(1)
data <- matrix(rnorm(20), ncol = 2)
k <- 2

centroids <- data[sample(1:nrow(data), k), ]
for (i in 1:10) {
  distances <- as.matrix(dist(rbind(centroids, data)))[1:k, (k+1):(k+nrow(data))]
  clusters <- apply(distances, 2, which.min)
  
  for (j in 1:k) {
    centroids[j, ] <- colMeans(data[clusters == j, , drop = FALSE])
  }
}

cat("Final Centroids:\n")
print(centroids)

#Library Implementation
set.seed(1)
data <- matrix(rnorm(20), ncol = 2)
kmeans_result <- kmeans(data, centers = 2)
print(kmeans_result$centers)

̥