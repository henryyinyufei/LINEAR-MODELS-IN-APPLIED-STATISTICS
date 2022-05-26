wd <- "D:/Simon Fraser University/2020 fall/STAT 350/Rfile"
setwd(wd)

#read in data
vapor=read.csv("vapor.csv")
names(vapor)

#plot the response variable versus the independent variable
attach(vapor)
plot(Temperature,Pressure)

# some other plots
plot(Temperature,log(Pressure))
plot((1/Temperature),log(Pressure))

# Let's fit a linear model anyway... you know this will not be good
vapor.lm = lm(Pressure~Temperature)
summary(vapor.lm)
plot(Temperature,Pressure)
abline(vapor.lm)

# Before doing any prediction or hypothesis tests, 
#assess the model fit and adequacy

plot(vapor.lm)

#get studentized residuals and plot them versus predicted vaues
#and the predictor
vapor.student.resid = rstudent(vapor.lm)
plot(vapor.lm$fitted.values, vapor.student.resid)
title("Studentized residuals versus fittled values")

vapor.student.resid = rstudent(vapor.lm)
plot(vapor$Temperature, vapor.student.resid)
title("Studentized residuals versus Temperature")

#probably need transformation
ln.Pressure=log(Pressure)
plot(Temperature,ln.Pressure)

#now fit a linear model to the transformaed data
vapor.ln.Pressure.lm=lm(ln.Pressure~Temperature)
summary(vapor.ln.Pressure.lm)
abline(vapor.ln.Pressure.lm)

plot(vapor.ln.Pressure.lm)

vapor.ln.Pressure.student.resid = rstudent(vapor.ln.Pressure.lm)
plot(vapor.ln.Pressure.lm$fitted.values, vapor.ln.Pressure.student.resid)
title("Studentized residuals versus fittled values")

vapor.student.resid = rstudent(vapor.lm)
plot(vapor$Temperature, vapor.ln.Pressure.student.resid)
title("Studentized residuals versus Temperature")
# could keep transforming... looking at exercise 5.2 ln(p) =constant*(-1/T)
# let's try 1/T

ln.Pressure=log(Pressure)
T.inv=1/Temperature
plot(T.inv,ln.Pressure)

#now fit a linear model to the transformaed data
vapor.ln.Pressure.lm.T.inv=lm(ln.Pressure~T.inv)
abline(vapor.ln.Pressure.lm.T.inv)

plot(vapor.ln.Pressure.lm.T.inv)
vapor.ln.Pressure.T.inv.student.resid = rstudent(vapor.ln.Pressure.lm.T.inv)
plot(vapor.ln.Pressure.lm.T.inv$fitted.values, vapor.ln.Pressure.T.inv.student.resid)
title("Studentized residuals versus fittled values")
plot(T.inv, vapor.ln.Pressure.T.inv.student.resid)
title("Studentized residuals versus 1/Temperature")


plot(vapor.ln.Pressure.lm.T.inv)

  

