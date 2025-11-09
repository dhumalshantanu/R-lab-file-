##Practical 4

#Load necessary library
library(dplyr)
library(ggplot2)
library(modeest)

#Load necessary dataset
data("iris")
data("mtcars")
data("airquality")

#Calculate the mean,median,mode, and standard deviation of Petal.Width from iris
mean(iris$Petal.Width)
median(iris$Petal.Width)
mlv(iris$Petal.Width,method="mfv")

#Draw a histogram of mpg from the mtcars dataset 
hist(mtcars$mpg,
     main = "Histogram of mpg",
     xlab = "mpg", col = "orange",border = "white")

#Create a scatterplot of hp vs mpg from mtcars
plot(mtcars$hp, mtcars$mpg,
     main = "Scatterplot of hp vs mpg",
     xlab = "hp", ylab = "mpg",
     col = "red", pch = 20)

#Generate a boxplot for Ozone in airquality dataset
boxplot(airquality$Ozone,
        main = "Boxplot for Ozone levels",
        ylab = "Ozone",
        col = "lightgreen")

#Fill the variance of Sepal.Width in the iris dataset 
var(iris$Sepal.Width)



