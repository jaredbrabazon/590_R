a <- 1
b <- 2
c <- a+b

#a and b are double precision atomic vectors. c is the operation of the addition of vectors a and b 

#set.seed() is a random number generator, this ensures that the 'random' results will be the same for all
set.seed(0)
#rnorm() makes a normalized distribution of numbers

# These are vectors with various quantities that are normalized, d+e is the first plus the first, second plus the second etc.
d <- rnorm(20)
e <- rnorm(20)
f <- d+e

#Both code snippets assign a vector to a variable or object, both are the addition of the variables, 

#How to use attributes to make data more reproducible 
#(1. attributing units to values so others can understand what units were used) 
#(2. Allows for better organization of modes)
#(3. Makes it easier to find the values (ex. not row 1 column 32, but "units between these values")


#these are common attribute functions names(x), dim(x), and class(x), not attr(x, "names"), attr(x, "dim"), and attr(x, "class") where names dim and class are not lost when a vector is changed

#Assigning a vector with length 5 having two attributes
# to check length use length(vect.5)
vect.5 <- c(1:5)
attr(vect.5, "units") <- "grams"
attr(vect.5, "sample type") <- "sand"
#use this to check attributes
str(attributes(vect.5))

#2.2.2.2 what happens to a factor when you modify its levels?
a.fact <- factor(c("a","b","c","a"))
levels(a.fact)[levels(a.fact)== "b"] <- "BEE"
levels(a.fact)
#The modified factor overwrites the previous level

#2.2.2.3 What does this code do? How do f2 and f3 differ from f1
f1 <- factor(letters)
levels(f1) <- rev(levels(f1))

f2 <- rev(factor(letters))

f3 <- factor(letters, levels = rev(letters))

#The first code in question 2 assigns a letter of the alphabet with a corresponding number a=1, b=2, etc. It then subsequently reverses the letters to z=1, y=2 etc.and overwrites the first assigned level
#The second code snippet reverses the display or ordering of the factors which in turn reorders the assigned values
#The third snippet reverses the factors but keeps the assigned numbers in their proper order resulting in z=26, y=25 etc.

#2.3.1 #as well as why does dim() return what it does
# it returns NULL when applied to a vector because it is 1D


#2.4.5 
#1.What attributes does a data frame possess?
frame1 <- data.frame(a.fact,a,d,e,f)
attributes(frame1)
# A dataframe seems to have 3 attributes. These include names, row.names, and column names

#2What does as.matrix() do when applied to a data frame with columns of different types?
as.matrix(frame1)
#it organizes and displays the vectors in the data frame in a rows and columns view

#3 Can you have a data frame with 0 rows? What about 0 columns?
#NO??? 


#SUBSETTING
nrow(mtcars)
length(mtcars)
ncol(mtcars)
#I believe they are different because one is describing the number of rows and one is describing the number of columns in the dataset
print(mtcars)

#create a vector that is the cyl column of the mtcars by [], #it seems [] prdouces a dataframe but [[]] produces a vector
cyl.vect <- mtcars[[2]]
print(cyl.vect)
str(cyl.vect)

#create a vector that is the cyl column of the mtcars by $
cyl.vect2 <- mtcars$cyl
print(cyl.vect2)

#create a dataframe that contains all the columns of mtcars, but only with cars that weigh less than 3.0 OR more than 4.0 (weight is in the wt column)
cars_frm <- data.frame(mtcars)
subset(cars_frm, wt < 3 | wt > 4)

#Create a data frame that contains all the rows of mtcars, but only the mpg and wt
cars_frm[c("mpg", "wt")]

#Which cars in the database get gas mileage (mpg) equal to the median gas mileage for the set? (Use median and which).s
md.car <- median(cars_frm$mpg)
car.names <- which(cars_frm$mpg == md.car) #this gives me the row numbers but not the names

#THIS way works, but without using which
row.names(subset(cars_frm, mpg == md.car))

#3.7.1
#mtcars[mtcars$cyl = 4, ]
mtcars[mtcars$cyl == 4, ]

#mtcars[-1:4, ] QUESTION ON THIS ONE
mtcars[-1,0:4, ] #with the negative it gives first 4 columns, without yields first 4 rows

#mtcars[mtcars$cyl <= 5]
mtcars[mtcars$cyl <= 5,]

#mtcars[mtcars$cyl == 4 | 6, ]
mtcars[mtcars$cyl == 4 | mtcars$cyl == 6, ]
subset(mtcars,cyl == 4 |cyl == 6)

##Simple Operations, read in the csv file
read.csv(file = "2016_10_11_plate_reader.csv")

plate_reader <- data.frame(read.csv(file = "2016_10_11_plate_reader.csv", skip = 33))
## skip = 33 gets rid of the first 33 rows
str(plate_reader)
# I made a dataframe, this includes vectors of factor, numeric, and int
library(tidyverse)
read_csv(file = "2016_10_11_plate_reader.csv")
tidy_frame <- data.frame(read_csv(file = "2016_10_11_plate_reader.csv"))
#It created a new column "x3" as well as put NA in empty slots
