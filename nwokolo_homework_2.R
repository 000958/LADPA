#nwokolo daniel
#homework 2

#..1..

data("CO2")

library(sqldf)

aggregate(x=CO2[,c("uptake")], 
          by=data.frame(CO2$Type), 
          FUN=function(x) mean(x))

#     CO2.Type        x
# 1      Quebec 33.54286
# 2 Mississippi 20.88333


#..2.................................................................................


df <- data.frame(Died.At=c(22,40,72,41),
                     Writer.At=c(16, 18, 36, 36), 
                     First.Name=I(c("John", "Edgar", "Walt", "Jane")), 
                     Second.Name=I(c("Doe", "Poe", "Whitman", "Austen")),
                     Sex=I(c("MALE", "MALE", "MALE", "FEMALE")), 
                     Date.Of.Death=as.Date(c("2015-05-10", "1849-10-07", "1892-03-26","1817-07-18")))

#'data.frame':	4 obs. of  6 variables:
#$ Died.At      : num  22 40 72 41
#$ Writer.At    : num  16 18 36 36
#$ First.Name   : 'AsIs' chr  "John" "Edgar" "Walt" "Jane"
#$ Second.Name  : 'AsIs' chr  "Doe" "Poe" "Whitman" "Austen"
#$ Sex          : 'AsIs' chr  "MALE" "MALE" "MALE" "FEMALE"
#$ Date.Of.Death: Date, format: "2015-05-10" "1849-10-07" "1892-03-26" "1817-07-18"

df$Sex<-as.factor(df$Sex)

#'data.frame':	4 obs. of  6 variables:
#$ Died.At      : num  22 40 72 41
#$ Writer.At    : num  16 18 36 36
#$ First.Name   : 'AsIs' chr  "John" "Edgar" "Walt" "Jane"
#$ Second.Name  : 'AsIs' chr  "Doe" "Poe" "Whitman" "Austen"
#$ Sex          : Factor w/ 2 levels "FEMALE","MALE": 2 2 2 1
#$ Date.Of.Death: Date, format: "2015-05-10" "1849-10-07" "1892-03-26" "1817-07-18"

#age_at_death, age_as_writer, first_name, surname, gender, date_died

colnames(df) <- c("age_at_death", "age_as_writer", "first_name", "surname", "gender", "date_died")



#  age_at_death age_as_writer first_name surname gender  date_died
#1           22            16       John     Doe   MALE 2015-05-10
#2           40            18      Edgar     Poe   MALE 1849-10-07
#3           72            36       Walt Whitman   MALE 1892-03-26
#4           41            36       Jane  Austen FEMALE 1817-07-18


#Say "John Doe" died on his birthday, calculate and display the birthdate value based on the variables date_died and age_at_death

#ymd(date) - years(5)

library(lubridate)


birthday <- df[1,c(1,6)]

birthday[,2] <- ymd(birthday[, 2])

birthday[,1] <- years(birthday[, 1])

(birthday[, 2]) -(birthday[, 1])



#[1] "1993-05-10"


#..3...................................................................................................

product <- c("A", "B")
  
height <- c(10,20) 

width <- c(5,10)

weight <- c(2,NA) 

observations_wide <- data.frame(product, height, width, weight)


library(reshape2)

observations_long <- melt(data=observations_wide,
             id.vars="product", na.rm = T)


observations_long[order(observations_long$product),]

#  product variable value
#1       A   height    10
#3       A    width     5
#5       A   weight     2
#2       B   height    20
#4       B    width    10


#..4.....................................................................................................



library(datasets) 

data(mtcars) 

? mtcars

# (C) sapply(split(mtcars$mpg, mtcars$cyl), mean)

#       4        6        8 
#26.66364 19.74286 15.10000 


#..5......................................................................................................


AD<- tapply(mtcars$hp, mtcars$cyl, mean)
abs(AD[3]-AD[1])

#       8 
#126.5779 


#..6....................................................................................................


library(datasets)
data("airquality")

ozone <- subset(airquality, !is.na(Ozone), select = Ozone)
apply(ozone, 2, mean)


#Ozone 
#42.12931

# (A) 42.1


#..7...................................................................................................



temp <- subset(airquality, Month==6, select=Temp)
apply(temp, 2, mean)


#Temp 
#79.1 


# (D) 79.1

# ..8..................................................................................................

library(datasets) 

data("mtcars") 

boxplot(mpg~cyl,data=mtcars, main="Car Milage Data",
        xlab="Number of Cylinders", ylab="Miles Per Gallon")


#..9..................................................................................................

library("scatterplot3d")

scatterplot3d(mtcars$wt, mtcars$disp, mtcars$mpg, 
              main="MTCars Scatterplot",
              xlab="Weight (lbs) ", 
              ylab="Displacement (cu.in.)",
              zlab="Miles/(US)gallon ",
              grid=TRUE,
              col.axis="blue", 
              col.grid="lightblue",
              pch=ifelse(mtcars$am=="1",0,1))

#..10................................................................................................


ot <- airquality[,c('Ozone','Temp')]


ot <- airquality[,c('Ozone','Temp')]

plot(x = ot$Ozone,y = ot$Temp,
     xlab = "Ozone (ppb)",
     ylab = "Temperature (degrees F)",
     xlim = c(1,99),
     ylim = c(1,79),		 
     main = "Ozone Level vs Temp")


