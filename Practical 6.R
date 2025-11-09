##Practical 6

#Import libraries
library(dplyr)
library(ggplot2)

#Load dataset
data("mtcars")
data("airquality")

#Create a histogram of mpg from the mtcars dataset
ggplot(mtcars, aes(x=mpg))+
  geom_histogram(bins = 20, fill="pink", colour="black")+
  labs(title = "Histogram for mpg",x="mpg",y="Frequency")

#Generate a scatterplot of hp vs wt from mtcars
ggplot(mtcars, aes(x=hp,y=wt,color = "Analysis" ))+
  geom_point(size = 3)+
  labs(title = "hp vs wt")

#Draw a boxplot of Ozone values from the airquality dataset
ggplot(airquality, aes(y = Ozone)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  labs(
    title = "Boxplot of Ozone Levels",
    y = "Ozone"
  ) +
  theme_minimal()

#Find the correlation between Sepal.Length and Sepal.Width in iris
correlation <- cor(iris$Sepal.Length, iris$Sepal.Width)
correlation

#Create a pair plot of the first four columns of mtcars
pairs(mtcars[1:4],
      main = "Pair Plot of First Four Variables in mtcars",
      pch = 19,
      col = "darkgreen")

