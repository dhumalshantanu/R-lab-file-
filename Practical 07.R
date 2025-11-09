##Practical 7
library(dplyr)

#Load dataset
data("iris")
data("mtcars")
data("airquality")

#Perform a one-sample t-test to check if the mean mpg in mtcars differs from 20
t_test_result <- t.test(mtcars$mpg, mu = 20)
t_test_result

#Compare Petal.Width across species in iris using ANOVA
anova_model <- aov(Petal.Width ~ Species, data = iris)
summary(anova_model)

#Find the correlation between mpg and hp in mtcars
correlation <- cor(mtcars$mpg, mtcars$hp)
correlation

#Perform a paired t-test on Sepal.Length and Sepal.Width in iris 
t_test_result <- t.test(iris$Sepal.Length, iris$Sepal.Width, paired = TRUE)
t_test_result

#Check if Ozone and Temp in airquality are significantly correlated
correlation_result <- cor(airquality$Ozone, airquality$Temp)
cor_test <- cor.test(airquality$Ozone, airquality$Temp)
correlation_result
cor_test
