## Week4RCodes.R - STAT350
## September, 2020... 

wd <-"D:/Simon Fraser University/2020 fall/STAT 350/Rfile"
setwd(wd)
##---- Install relevant libraries (Tools -> Install ...)
install.packages("MPV")
install.packages("faraway")
library(MPV) 
library(faraway)

# 3.13 from text
#Soft Drink Delivery Data
# Unit Normal Scaling with Soft Drink Delivery Data

# Begin by extracting the y's and x's from the softdrink dataframe from the MPV package
del.y<-softdrink$y
del.cases<-softdrink$x1
del.dist<-softdrink$x2
n=length(del.y)

#fit linear model to the data in the usual units
deliver.model<-lm(del.y~del.cases+del.dist)

#let's start scaling the varibales

#create the X-matrix (do not forget the column of 1's for the intercept)
delX<-cbind(rep(1,length(del.y)), del.cases,del.dist) 


#get the diagnol elements of the Hat-matrix H= X (X'X)^(-1) X'
H=delX%*%solve(t(delX)%*%delX)%*%t(delX)
hii<-diag(H) #diag returns just the diagnol terms from H

# Compute the smaple SD's for each of the x's and y's(could have just used the sd() command)
dels1<-sqrt(sum((del.cases-mean(del.cases))^2)/(n-1))
dels2<-sqrt(sum((del.dist-mean(del.dist))^2)/(n-1))
delsy<-sqrt(sum((del.y-mean(del.y))^2)/(n-1))
# or sd() or var()
sd(del.cases)
sd(del.dist)
sd(del.y)
sqrt(var(del.cases))
sqrt(var(del.dist))
sqrt(var(del.y))

# now compute the unit normal scaling   
delz1<-(del.cases-mean(del.cases))/dels1
delz2<-(del.dist-mean(del.dist))/dels2
delys<-(del.y-mean(del.y))/delsy

# "-1" to fit a model without intercept (omit intercept)
delstandreg<-lm(delys~delz1+delz2-1)
summary(delstandreg)

## Life Expectancy Data
#---- Import dataset 
table.b16<-read.table(file="table.b16.csv",header=TRUE,sep=",")
attach(table.b16)


y<-table.b16$y
X<-cbind(rep(1,length(table.b16$y)), table.b16$x1, table.b16$x2)
LifeExpModel<-lm(y~x1+x2)
LifeExpModel$coefficients

# Unit Normal Scaling
s1<-sqrt(sum((x1-mean(x1))^2)/(n-1))
s2<-sqrt(sum((x1-mean(x2))^2)/(n-1))
sy<-sqrt(sum((y-mean(y))^2)/(n-1))

z1<-(x1-mean(x1))/s1
z2<-(x2-mean(x2))/s2
ys<-(y-mean(y))/sy

LifeSRModel<-lm(ys~z1+z2-1)
LifeSRModel$coefficients

# Multicolinearity
vif(LifeExpModel)
vif(LifeSRModel)

# Model Adequacy Checking
summary(LifeExpModel)
resid(LifeExpModel)
rstandard(LifeExpModel)
rstudent(LifeExpModel)
plot(LifeExpModel)
plot(resid(LifeExpModel),x1)
plot(resid(LifeExpModel),x2)
residtable<-cbind(resid(LifeExpModel),
                  rstandard(LifeExpModel), rstudent(LifeExpModel))
summary(table.b16)

# Partial Plots 
# y, given x2
mod1 <- lm(y ~ x2 )
resid.1 <- resid(mod1)
# x1, given x2
mod2 <- lm(x1 ~ x2)
resid.2 <- resid(mod2)
# y, given x1
mod3 <- lm(y ~ x1 )
resid.3 <- resid(mod3)
# x2, given x1
mod4 <- lm(x2 ~ x1)
resid.4 <- resid(mod4)
layout(matrix(1:4, nc=2, byrow=TRUE) )
plot(x1, y, main='y ~ x1')
plot(x2, y, main='y ~ x2')
plot(resid.2, resid.1, main='y|x2 ~ x1|x2')
plot(resid.4, resid.3, main='y|x1 ~ x2|x1')