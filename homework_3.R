#Question 1

install.packages("ISLR")

library(ISLR)


data(Auto)


lm_Auto <- lm(Auto$mpg ~ Auto$horsepower)

summary(lm_Auto)

#Call:
#lm(formula = Auto$mpg ~ Auto$horsepower)

#Residuals:
#  Min       1Q   Median       3Q      Max 
#-13.5710  -3.2592  -0.3435   2.7630  16.9240 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)     39.935861   0.717499   55.66   <2e-16 ***    
#  Auto$horsepower -0.157845   0.006446  -24.49   <2e-16 ***  (coefficient p-value is very low value, the relationship is negative)
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 4.906 on 390 degrees of freedom
#Multiple R-squared:  0.6059,	Adjusted R-squared:  0.6049 (approximately 60% of the response variance is explained, a strong relationship)
#F-statistic: 599.7 on 1 and 390 DF,  p-value: < 2.2e-16




#predicted mpg associated with a horsepower of 98
lm_Auto$coefficients

predict_mpg <- lm_Auto$coeff[1] + 98 * lm_Auto$coeff[2]
predict_mpg
#predict(lm_Auto, data.frame("horsepower"=98), interval="confidence")


#(Intercept) 
#24.46708 



#response and the predictor plot with abline
plot(Auto$horsepower, Auto$mpg,col="black",pch=19)
abline(lm_Auto, lwd=3, col="red")



#diagnostic plots of the fit.
par(mfrow=c(2,2))
plot(lm_Auto)



#question 2----------------------------------------------------------------------------------------


library(ISLR)


data(Auto)

attach(Auto)



#categorical variable mpg01
mpg01 <- ifelse( mpg > median(mpg), yes = 1, no = 0)
Auto <- data.frame(Auto, mpg01)




#Explore the data using EDA techniques
cor(Auto[,-9])

pairs(Auto)




#Split the data into a training set and a test set.
Auto <- data.frame(mpg01, apply(cbind(cylinders, weight, displacement, horsepower, acceleration), 
                                2, scale), year)


train <-  (year %% 2 == 0) # if the year is even (%%)
test <-  !train
Auto.train <-  Auto[train,]
Auto.test <-  Auto[test,]
mpg01.test <-  mpg01[test]




#Logistic regression
glm.fit <-  glm(mpg01 ~ cylinders + weight + displacement + horsepower,
                data = Auto,
                family = binomial,
                subset = train)
glm.probs <-  predict(glm.fit, Auto.test, type = "response")
glm.pred <-  rep(0, length(glm.probs))
glm.pred[glm.probs > 0.5] <- 1
mean(glm.pred != mpg01.test)

#[1] 0.1208791






#question 3------------------------------------------------------------------------------------



head(iris)

names(iris)

x = iris[,-3]
y = iris$Species


#Set the number of centroids to 3

names(iris)

x = iris[,-5]
y = iris$Species

kc <- kmeans(x,3)


#Review and print the cluster
kc$cluster



#Review and print the centers
kc$centers



#plot each of the resulting clusters of data points and their centers
plot(x[c("Sepal.Length", "Sepal.Width")], col=kc$cluster)
points(kc$centers[,c("Sepal.Length", "Sepal.Width")], col=1:3, pch=20, cex=3)




