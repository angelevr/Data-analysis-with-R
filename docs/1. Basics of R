# R works as a calculator
3 + 5
4**2

# functions - commands
print("Hello, world!")

# creating objects
z <- c(1, 2, 3, 4, 5) # a vector 'z' created by concatenating 5 values

# creating a data frame with the assign operator
myDataFrame <- data.frame(a = c(1, 2, 3, 4, 5), b = c(5, 4, 3, 2, 1))

# viewing myDataFrame 
myDataFrame 
View(myDataFrame )

# errors
name

# defining the object 'name'
name <- "Tomas"
name

# setting a working directory
setwd("filepath")
# alternatively: Session -> Set Working Directory -> Choose Directory

# let'S try to analyze our own dataset
myStudents <- read.csv("student_data.csv", col_names = TRUE)

View(myStudents)
nrow(myStudents) # number of observations
ncol(myStudents) # number of variables

# descriptive statistics
# describing categorical variables with tables & barcharts
table(myStudents$field)
fieldTab <- table(myStudents$field) # creating an object
100*fieldTab/sum(fieldTab) 
barplot(fieldTab) # barplot

# mean, median, var & sd + histogram (+ line for murder rate 1.2)
mean(myStudents$age) # mean
median(myStudents$age) # median
sd(myStudents$age) # standard deviation
hist(myStudents$age) # histogram

mean(myStudents$murdratest)
median(myStudents$murdratest)
sd(myStudents$murdratest)
hist(myStudents$murdratest)
abline(v = 1.2, col = "red") # adding a horizontal line
