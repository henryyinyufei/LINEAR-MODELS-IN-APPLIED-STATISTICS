# install the package with the book datasets... only have to do this once
install.packages(MPV)

#load the library in R... have to do this everytime
library(MPV)

#get data (data from problem 2.13)
ozone = p2.13
names(ozone)
x=ozone$index
y=ozone$days

#plot data
plot(x,y,xlab="Index", ylab="Days",xlim=c(15,20),ylim=c(0,120))

#do a linear regression
lin.reg=lm(y~x)

#plot regression line on the scatter-plot
abline(lin.reg)

#Let's look at the regression summary
summary(lin.reg)

# 95% confidence interval of the slope and intercept
confint(lin.reg,level=0.95)

#95% confidence interval for a new value ay x= 18 
xnew=data.frame(x= c(18))
predict.lm(lin.reg,newdata=xnew,interval="confidence")

#95% prediction interval for a new value
xnew=data.frame(x= c(18))
predict.lm(lin.reg,newdata=xnew,interval="prediction")

#Plot the predcition interval for many x's to see pattern


newx = data.frame(x=seq(10,30,by = 0.05))
pred_interval <- predict(lin.reg, newdata=newx, interval="prediction",level = 0.95)

# plot the data and regression line
#stretch out the x-xis and y-axis to accomadate the predictions
plot(x,y,xlab="Index", ylab="Days",xlim=c(10,30),ylim=c(min(pred_interval)-1,max(pred_interval)+1))
abline(lin.reg)

#Plot pred intervals
matlines(newx, pred_interval[,2:3], col = "blue", lty=2)



