# Practical 5

#Load necessary dataset
data("iris")
data("mtcars")

#Perform binning on Sepal.Length from iris into categories: Short, Medium, Long 
iris$Sepal.Length.Category <- cut(iris$Sepal.Length,
                                  breaks = 3,
                                  labels = c("Short","Medium","Long"))
table(iris$Sepal.Length.Category)

#Convert Species column in iris into dummy variables
iris_dummies <- cbind(iris, model.matrix(~Species - 1, data = iris))
head(iris_dummies[c("Species","Speciessetosa","Speciesversicolor","Speciesvirginica")])

#Normalize the mpg column in mtcars dataset
mtcars$mpg_normalized <- (mtcars$mpg - min(mtcars$mpg))/
  (max(mtcars$mpg) - min(mtcars$mpg))
head(mtcars$mpg_normalized)

#Standardize the Sepal.Width column in iris
iris$Sepal.Width.Category <- scale(iris$Sepal.Width)
head(iris$Sepal.Width.Category)

#Create a new feature in mtcars: efficiency = mpg/hp
mtcars$efficiency <- mtcars$mpg / mtcars$hp
head(mtcars[c("mpg","hp","efficiency")])

