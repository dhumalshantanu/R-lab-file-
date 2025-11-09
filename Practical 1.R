##Practical 1

#Display the first 15 rows of mtcars dataset
data("mtcars")
head(mtcars, 15)

#Find the maximum and minimum value of sepal length frpm iris 
data("iris")
min_val <- min(iris$Sepal.Length)
max_val <- max(iris$Sepal.Length)
print(min_val)
print(max_val)

#Calculate the mean of the variable mpg in mtcars
mean_val <- mean(mtcars$mpg)
print(mean_val)

#Display the structure of airquality dataset
data("airquality")
str(airquality)

#Check whether the number 100 is greater than 50
print(100>50)
