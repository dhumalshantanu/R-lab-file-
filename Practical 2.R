##Practical 2
#Import necessary libraries 
library(dplyr)

#Import the airquality dataset and check its structure using str()
data("airquality")
str(airquality)

#Find the number of missing values in airquality
sum(is.na(airquality))

#Use summary() on the mtcars dataset and interpret the output
summary(mtcars)

#Find the mean horsepower(hp) in the mtcars dataset
mean_hp <- mean(mtcars$hp)
print(mean_hp)

#Group the iris dataset by species and calculate the average sepal width 
iris %>%
  group_by(Species) %>%
  summarise(avg_width = mean(Sepal.Width))
