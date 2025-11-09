##Practical 9 

#Load library
install.packages("rpart")
install.packages("rpart.plot")
install.packages("pROC")
library(rpart)
library(rpart.plot)
library(pROC)

#Build a logistic regression model predicting am (automatic/manual) in mtcars using hp and wt.
model_log <- glm(am ~ hp + wt, data = mtcars, family = binomial)
summary(model_log)

#Construct a decision tree to classify Species in the full iris dataset.
tree_model <- rpart(Species ~ ., data = iris, method = "class")
rpart.plot(tree_model)

#Calculate confusion matrix and accuracy for Q1.
pred <- predict(tree_model, iris, type = "class")
conf_matrix <- table(Predicted = pred, Actual = iris$Species)
conf_matrix
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
accuracy

#Plot the ROC curve for the logistic regression model in Q1 (mtcars).
prob <- predict(model_log, type = "response")
roc_curve <- roc(mtcars$am, prob)
plot(roc_curve, col = "blue", main = "ROC Curve - Logistic Regression")
auc(roc_curve)

#Compare logistic regression and decision tree performance on the iris dataset.
iris_binary <- subset(iris, Species != "virginica")
iris_binary$Species <- factor(iris_binary$Species)
log_model_iris <- glm(Species ~ ., data = iris_binary, family = binomial)
pred_log <- ifelse(predict(log_model_iris, type = "response") > 0.5, "versicolor", "setosa")
conf_log <- table(Predicted = pred_log, Actual = iris_binary$Species)
acc_log <- sum(diag(conf_log)) / sum(conf_log)
acc_tree <- accuracy
cat("Logistic Regression Accuracy:", acc_log, "\nDecision Tree Accuracy:", acc_tree)
̥