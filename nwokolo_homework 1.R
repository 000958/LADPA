#Introduction to Data Science
#Homework 1
#Nwokolo Daniel Tochukwu

#---------------------------------------------------------------------------------------------------------
#Question 1:

#Write an R script to perform the following operations:
  #.Define a new matrix named mat with 3 row and 4 columns,filling with a sequence of values1:12 by row
  #.Add a new row to the matrix containing all 9's
  #.Add a new column to the matrix containing all 8's



mat <- matrix(1:12, nrow = 3, ncol = 4)

matr <- rbind(c(9,9,9,9), mat)

matc <- cbind(c(8,8,8,8), matr)


#----------------------------------------------------------------------------------------------------------------
#Question 2:

#Write an R script to perform the following operations:
#.Define a new list named lst with the following components: 
  #(a) a character vector containing the values "Ellen", "Catherine", and "Stephen", 
  #(b) an integer vector containing the values 90, 95, and 99, 
  #(c) a matrix containing attendance records for two class sessions where the 
  #first row represents the first session attendance for each student, and similarly for the second row.
    #.Display the names of all students.
    #.Display Stephen's grade.



char = c("Ellen", "Catherine" ,"Stephen")

int = c(90, 95, 99)

met <- matrix( c(TRUE, TRUE, TRUE, FALSE, FALSE, TRUE), nrow = 2, ncol = 3)

lst = list(char, int, met)

lst[[2]][3]



#-----------------------------------------------------------------------------------------------------------------
#Question 3:

#Write an R script to define a new character vector gender that is populated with "male" for thefirst 20 elements,
#and "female" for the next 30 elements.Use factors to quickly and efficiently 
#calculate a total number of values in each gender category. 
#[Hint: you may find the R function rep() useful for this question]


gender <- c(rep("male",20), rep("female", 30))

gender <- factor(gender)

table(gender)




#------------------------------------------------------------------------------------------------------------------
#Question 4:

#Using the airquality data set, how many missing values are in the Ozone column of this data frame? 
#[Hint: must use subsetting in your R code to solve]
#(a)43
#(b)37
#(c)9 
#(d)78


data(airquality)

subset(airquality, select = c(Ozone))

library(tidyverse)
map(airquality, ~sum(is.na(.)))


#--------------------------------------------------------------------------------------------------------------------
#Question 5:


#Extract the subset of rows of the data frame where Ozone values are above 31 and Temp values are above 90. 
#What is the mean of Solar.R in this subset? 
#[Hint: must use subsetting in your R code to solve]
#(a)334.0
#(b)185.9
#(c)205.0
#(d)212.8

subset(airquality, Ozone > 31 & Temp > 90)

mean(subset(airquality$Solar.R, airquality$Ozone > 31, airquality$Temp > 90),na.rm=TRUE)

#--------------------------------------------------------------------------------------------------------------------
#Question 6:
#Make a copy of the airquality data frame so you can add a new variable hotcold. 
#The new variable shall have two possible values: 
#"hot" if the value of the Temp variable is greater than the median value for Temp, and cold otherwise. 
#You can hand in the results of head() and tail() to show your code is working. 
#[Hint: you may find the R functions ifelse() and median() useful for this question]


airquality

AQCOPY <- data.frame(airquality)

AQCOPY$hotcold <- (mean(AQCOPY$Temp))

a = AQCOPY$hotcold
ifelse(a < AQCOPY$Temp ,"hot","cold")


#---------------------------------------------------------------------------------------------------------------------
#question 7:
#Based on a traditional English children's game, 
#write an R script that:.Prints the numbers from 1 to 100.For multiples of 3, print "Fizz" instead of the number.
#For multiples of 5, print "Buzz" instead of the number.For multiples of 3 and 5, print 
#"FizzBuzz" instead of the numberHere's an example of what the print display should look like:
#[1] 1 
#[1] 2 
#[1] "Fizz" 


for (i in 1:100)
  {
  
  if(i%%3 == 0 & i%%5 == 0) 
    {
    print('FizzBuzz')
  }
  else if(i%%3 == 0) 
      {
    print('Fizz')
  }
  else if (i%%5 == 0){
    print('Buzz')
  }
  else {
    print(i)
  }
  
}

#---------------------------------------------------------------------------------------------------------------------
#Question 8:
#Using the following matrix definition: mat1 <- matrix(rep(seq(4), 4), ncol = 4)
#use one of R's loop functions to compute the sum of each row plus 2.  
#[Hint: you'll need a user defined function (UDF) or an anonymous function to find the answer]

mat1 <- matrix(rep(seq(4), 4), ncol = 4)

apply(mat1,2,sum)+2

#---------------------------------------------------------------------------------------------------------------------
#Question 9:
#Using the built-in character vector in R that contains all the U.S. state names: state.name 
#write an R script to randomly select 10 names from the vector and create a new vector to store them. 
#For reproducibility, make sure you get the same 10 names each time you run the code. 
#Next, sort the new vector according to the state names. The resulting vector should contain 10 sorted state names. 
#[Hint: you may find the R functions sample(), order(),and set.seed(), along with the data set state.name useful for this question]

state.name

set.seed(1)
sort(sample(state.name ,10))

#had some problems getting order() to provide state names instead of integers


#---------------------------------------------------------------------------------------------------------------------
#question 10:
#Create a variable in R named xct to store the date and time of the Apollo 11 moon landing on July 20, 1969 at 20:18 UTC. 
#Calculate and display the number of years since the moon landing until today. 
#[Hint: you may find the R function Sys.time() useful for this question]

#x <- as.POSIXct("1969-07-20 20:18:00", tz = "UTC")

#y <- as.POSIXct((Sys.time()))

#years <- y -x

XCT = as.POSIXct("1969-07-20 20:18:00", tz = "UTC")
PSNT = as.POSIXct((Sys.time()))

as.numeric(difftime(PSNT, XCT, unit="weeks")) / 52.25

lubridate::time_length(difftime(PSNT, XCT), "years")




























