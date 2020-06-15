#Final project
#Drug posession Arrests dataset from the LA databank
#It is part of a much bigger dataset ("Areest data from 2010 to present") that I intend to keep working with.
#My focus will be mostly on the age and gender of those arrests
#but I will also be looking at the entire data sets as a whole.





#download the dataset and bring it into the rstudio environment

fileUrl <- "https://data.lacity.org/api/views/isxh-ztfe/rows.csv?accessType=DOWNLOAD"

download.file(fileUrl, destfile="./datasets/LANarc.cvs")

list.files("./datasets")

LANarc <- read.table("./datasets/LANarc.cvs",
                           sep= ",",
                           header = TRUE)


#EXPLORATION




#checking NAs
colSums(is.na(LANarc))

#AGE group with most arrests
AgeRanges <- cut(LANarc$Age, 
                   breaks=seq(0,100,by=10))
table(AgeRanges, useNA="ifany")


#sex col count
library(plyr)
y <- count(LANarc$Sex.Code)
n <- sum(y$freq)
rp <- y$freq/n * 100
rp 
#to find out which sex has what number
table(LANarc$Sex.Code)
pt <- prop.table(table(LANarc$Sex.Code)) * 100 #multiply by 100 to get percentages
round(pt,digits = 1) #and round off to 1


#sex with most crime in each age group
table(LANarc$Sex.Code, cut(LANarc$Age, breaks=seq(0,100,by=10)))

#and this is a much broader view
table(LANarc$Sex.Code, LANarc$Age)

#the distribution by decent
table(LANarc$Area.Name) #plot(table(LANarc$Descent.Code))



#VISALIZATION

#freq
Area_freq <- hist(LANarc$Area.ID, freq=TRUE,  breaks=1000, col="yellow")

#density line
histObj <- hist(LANarc$Area.ID, probability=TRUE, 
                    breaks=30, col="blue")
lines(density(LANarc$Area.ID))


#Age by sex.code variable
boxplot(LANarc$Age ~ as.factor(LANarc$Sex.Code), 
        col=c("blue","orange"), varwidth=FALSE)


#age dnsity plot
dens_age <- density(LANarc$Age[which(LANarc$Sex.Code=="F")])
plot(dens_age, lwd=3, col="10")

#age denscity filtered by gender
dens_ <- density(LANarc$Age[which(LANarc$Sex.Code=="M")])
lines(dens_, lwd=2, col="12")



#ggdensity(gene_citation, x = "citation_index", y = "..count..",
#          xlab = "Number of citation",
#          ylab = "Number of genes",
#          fill = "lightgray", color = "black",
#          label = "gene", label.select = key.gns, repel = TRUE,
#          font.label = list(color= "citation_index"),
#          xticks.by = 20, # Break x ticks by 20
#          gradient.cols = c("blue", "red"),
#          legend = c(0.7, 0.6),                                 
#          legend.title = ""       # Hide legend title
#)





#corelations
cor(LANarc[,c(3,4,6,7)], method="pearson")  #(positve between area_id and reporting_dist)
  
  #                     Time       Area.ID    Reporting.District  Age
  #Time                1.00000000 -0.01331901        -0.01312015  0.02448054
  #Area.ID            -0.01331901  1.00000000         0.99921518 -0.16855671
  #Reporting.District -0.01312015  0.99921518         1.00000000 -0.16805666
  #Age                 0.02448054 -0.16855671        -0.16805666  1.00000000

pairs(LANarc[,c(4,6)])

#hex of age by area.id
library(hexbin)
attach(LANarc)
plot(hexbin(Age))

#Area.id by age (sex M is red)
plot(LANarc$Age,pch=19,col=LANarc$Sex.Code,cex=0.5)


plot(LANarc$Area.ID,
     LANarc$Age,
     pch=19,
     cex=0.5,
     xlab="Area ID",
     ylab="Age (Years)",
     col=LANarc$Sex.Code)
#3d
library(scatterplot3d)
attach(LANarc)
scatterplot3d(x=Age,
              y=Area.ID,
              z=Reporting.District,
              main="LANarc dataset",
              xlab="Area.id",
              ylab="Age(years)",
              zlab="reporting district",
              grid = TRUE,
              pch=ifelse(Sex.Code=="M","*",0))

#age levels plot
library(Hmisc)
ageGroups <- cut2(LANarc$Age,g=5)

levels(ageGroups)

plot(Area.ID,Age,pch=19,col=ageGroups,cex=1)


#MACHINE LEARNING



library(UsingR)
attach(LANarc)

plot(Area.ID,Age,pch=".",col="black")

#A linear model using basic least squares
lm1 <- lm(Age ~ Area.ID)

#A regression line through the distribution
lines(Area.ID,lm1$fitted,col="blue",lwd=0.2,)

summary(lm1)

p_age <- lm1$coeff[1] + 21 * lm1$coeff[2] #I know area id max is 21

#residuals 
par(mfrow=c(1,1))

plot(Area.ID,lm1$residuals,col="black",pch=".")
abline(c(0,0),col="blue",lwd=3)



# ---------------------------------------------------------------

n <- nrow(LANarc)
ntrain <- round(n*0.6)    # 60% for training set
set.seed(657)             # Set seed for reproducible results
tindex <- sample(n, ntrain) # Create an index

train_LANarc <- LANarc[tindex,]   # Create training set
test_LANarc <- LANarc[-tindex,]  


newcol <- data.frame(isM=(train_LANarc$Sex.Code=="M"))

train_LANarc <- cbind(train_LANarc, newcol)

glm1 <- glm(isM ~ Age, 
            data=train_LANarc, 
            family=binomial)

# Bi-modal
plot(train_LANarc$Age, train_LANarc$isM)


#making age prediction
nd <- data.frame(Age=30)
predict.glm(glm1, nd, type="response") 


formula <- isM ~Area.ID + Age

glm2 <- glm(formula, data=train_LANarc, family="binomial")

summary(glm2)

prob <- predict(glm2, nd=test_LANarc, type="response")
prob <- round(prob,3)*100   # for percentages
prob


#LA <- LANarc[,c(4,6,7)]
#LA <- na.omit(LA)
#LA <- scale(LA)
#LA <- round(LA*0.6)