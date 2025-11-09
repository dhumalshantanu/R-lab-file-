##Practical 8 

#Build a regression model predicting Petal.Length from Sepal.Length in iris.
model1 <- lm(Petal.Length ~ Sepal.Length, data = iris)
summary(model1)

#Create a multiple regression model predicting mpg using wt, hp, and drat in mtcars.
model2 <- lm(mpg ~ wt + hp + drat, data = mtcars)
summary(model2)

#Check the R² and Adjusted R² of the model in Q2.
summary(model2)$r.squared        
summary(model2)$adj.r.squared   

#Plot residuals of your regression model in Q1 and interpret the result.
plot(model1$residuals, main = "Residual Plot - Q1",
     ylab = "Residuals", xlab = "Observation Index")
abline(h = 0, col = "red")

#Build a regression model predicting Ozone using Temp from the airquality dataset.
airquality <- na.omit(airquality)
model3 <- lm(Ozone ~ Temp, data = airquality)
summary(model3)


