#Set  working directory
wd <-"D:/Simon Fraser University/2020 fall/STAT 350/Rfile"
setwd(wd)

#read in data from Center for Radiative Shock Hydrodynamics (crash)
crash=read.csv("data_computer_experiment.csv")

#attach that dataframe to use variable names in the input file
attach(crash)

#construct pairwise scatterplots
pairs(crash)

#fit linear model and summarize
crash.lm=lm(location~thickness+energy+flux+gamma+opacity)
summary(crash.lm)

#predict a future shock location
newx = data.frame(thickness=21.87,energy=3640, flux=1.658, gamma=0.711,	opacity=0.0529)
pred_interval <- predict(crash.lm, newdata=newx, interval="prediction",level = 0.95)

#residuals
plot(crash.lm)
crash.resid = rstudent(crash.lm)
plot(crash.lm$fitted,crash.resid)
title("Studentized residuals versus predicted")

# load.library("car")
library(car)
#get partial regression plots
avPlots(crash.lm)

# load.library("faraway")
library(faraway)
vif(crash.lm)




