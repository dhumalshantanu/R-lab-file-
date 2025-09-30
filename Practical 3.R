##Practical 3

# load necessary libraries
library(dplyr)

#Count how many missing values are present in the airquality dataset
data("airquality")
sum(is.na(airquality))

#Replace missing values in Ozone with the median instead of mean 
airquality$Ozone[is.na(airquality$Ozone)] <- median(airquality$Ozone, na.rm = TRUE)
sum(is.na(airquality$Ozone))

#Create a duplicate dataset from mtcars and remove the duplicates 
mtcars_duplicate <- mtcars 
mtcars_duplicate <- distinct(mtcars_duplicate)
nrow(distinct(mtcars_duplicate))

#Convert all car names in mtcars row names to uppercase
rownames(mtcars) <- toupper(rownames(mtcars))
head(rownames(mtcars))

#Convert the Species column in iris dataset into numeric codes(1,2,3)
iris$Species_numeric <- as.numeric(factor(iris$Species))
head(iris[, c("Species", "Species_numeric")])

