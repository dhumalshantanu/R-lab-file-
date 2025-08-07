#Exercise questions
#Q1)Variables and Data types

#Q1)
name<- "Shantanu"
age<- 20
student<- TRUE

#Q2)
vector<- 1:10

#Q3)
var<- seq(from =5,to =50, by = 5)
print(var)

#Q4) 
fruits = c("apple","mango","strawberry","pineapple","lime")
fruits[c(2,4)]

#Q5)
random<- sample(1:100,10)
random 
maximum<- max(random)
maximum
minimum<- min(random)
minimum
mean<- mean(random)
mean

#Q6)
name<- c("Tanaya","John","Emily","Lara","Richard")
age<- c(20,21,22,34,11)
marks<-c(98,22,87,62,55)
df<- data.frame(Name = name,Age= age,Marks=marks)
print(df)

#Q7)
Marks<-df$Marks
Marks_desc<-df[order(-Marks),]
print(Marks_desc)

#Q2)Operators in R

#Q1)
10+5
10-5
10*5
10/5
10%%3
10%/%3

#Q2)
15>10
7==7

#Q3)
a<- c(2,4,6,8)
b<-c(1,3,5,7)
a+b
a-b
a*b

#Q4)
a>5
b<=4

#Q5)
5 %in% a

#Q6)
x<- c(TRUE,FALSE,TRUE,FALSE)
y<-c(TRUE,TRUE,FALSE,FALSE)
x&y
x|y
!x
!y

#Q3)Loops in R

#Q1)
for(i in 1:10)
  print (i)

#Q2)
sum_val <- 0
i <- 1
while(i<=100){
  sum_val <- sum_val + i
  i <- i + 1
}
print(sum_val)

#Q3)
for (i in 1:50){
  if(i %% 2 == 0){
    print(i)
  }
}

#Q4)
for(i in 1:10)
  print (i * 7)

#Q5)
n <- as.integer(readline(prompt = "Enter a number"))
fact <- 1
for (i in 1:n){
  fact <- fact * i
}

print(fact)

#Q6)
for(i in 1:5){
  for(i in 1:i){
    cat("*")
  }
  cat("\n")
}

#Q4)Conditionals in R

#Q1)
n <- as.integer(readline(prompt = "Enter a number :"))
if (n > 0){
  print("The number is positive")
}
if (n < 0){
  print("The number is negative")
  
}

#Q2)
n <- as.integer(readline(prompt = "Enter a number:"))
if(n %% 2 == 0){
  print("The number is even")
  
} else{
  print("The number is odd")
}

#Q3)
n <- as.integer(readline(prompt = "Enter the number:"))
if(n %% 4 == 0 && (year %% 100 != 0 || year %% 400 == 0)){
  print("It is a leap year")
  
}else{
  print("It is not a leap year")
}

#Q4) 
marks <- as.integer(readline(prompt = "Enter the marks:"))
if(marks >= 40 ){
  print("Pass")
}else{
  print("Fail")
}

#Q5)
marks <- as.integer(readline(prompt = "Enter the marks:"))
if(marks >= 90){
  print("Grade : A")
}else if (marks >= 75 && marks <= 89){
  print("Grade : B")
}else if (marks >= 60 && marks <=74){
  print("Grade : C")
}else if(marks < 60){
  print("Fail")
}

#Q5)Functions in R
#Q1)
add_numbers<- function(a, b) {
  return(a + b)
}
add_numbers(5, 7)  

#Q2)
square <- function(n) {
  return(n**2)
}
square(6)

#Q3)
factorial<- function(n) {
  if (n == 0 || n == 1) {
    return(1)
  } else {
    return(n * factorial(n - 1))
  }
}
factorial(5)

#Q4)
prime <- function(n) {
  if (n <= 1) {
    return(FALSE)
  }
  for (i in 2:sqrt(n)) {
    if (n %% i == 0) {
      return(FALSE)
    }
  }
  return(TRUE)
}
prime(7)

#Q5)
vector_stats <- function(x) {
  mean_val <- mean(x)
  median_val <- median(x)
  sd_val <- sd(x)
  
  return(list(Mean = mean_val, Median = median_val, SD = sd_val))
}

vector_stats(c(10, 20, 30, 40, 50))

#Q6)
values <- function(df, column_name) {
  sorted_values <- sort(df[[column_name]], decreasing = TRUE)
  return(head(sorted_values, 5))
}


df <- data.frame(marks = c(45, 90, 67, 88, 56, 77, 95))
values(df, "marks")

#Q6)Data Analysis - Adult Dataset 
library(dplyr)
library(ggplot2)

#Q1)
View(adult.data)

#Q2)
head(adult.data,10)

#Q3)
str(adult.data)

#Q4) 
mean(adult.data$V1,na.rm = TRUE)

#Q5) 
table(adult.data$V15)

#Q6) 
names(sort(table(adult.data$V7),decreasing = TRUE)[1])

#Q7) 
adult.data %>%
  group_by(V15) %>%
  summarise(avg_hours = mean(V5, na.rm = TRUE))

#Q8) 
ggplot(adult.data, aes(x = V4)) +
  geom_bar(fill = "skyblue")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Distribution of Education Levels", x = "Education", y = "Count")

#Q9)
adult.data %>%
  group_by(V14) %>%
  summarise(total = n(),
            high_income = sum(V15 == ">50K")) %>%
  mutate(percentage = (high_income/total) * 100) %>%
  arrange(desc(percentage)) %>%
  head(1)

#Q7)Data Analysis - IPL Data 

#Q1)
View(batting_bowling_ipl_bat)

#Q2) 
head(batting_bowling_ipl_bat,10)

#Q3) 
batting_bowling_ipl_bat %>%
  arrange(desc(Runs)) %>%
  select(Name, Runs) %>%
  head(5)

#Q4)
batting_bowling_ipl_bat %>%
  filter(!is.na(Ave)) %>%
  arrange(desc(Ave)) %>%
  select(Name, Ave) %>%
  head(1)

#Q5) 
top_10<- batting_bowling_ipl_bat %>%
  arrange(desc(SR)) %>%
  select(Name,SR) %>%
  head(10)

ggplot(top_10,aes( x = reorder(Name, -SR), y = SR)) +
  geom_bar(stat = "identity", fill = "orange") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Top 10 Players by Strike Rate", x = "Player", y = "Strike Rate")

#Q6)
cor(batting_bowling_ipl_bat$HF, batting_bowling_ipl_bat$Runs, use = "complete.obs")

