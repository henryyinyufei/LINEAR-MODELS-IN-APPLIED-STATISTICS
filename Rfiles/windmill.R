wd <- "D:/Simon Fraser University/2020 fall/STAT 350/Rfile"
setwd(wd)

#read in data
wind.dat=read.csv("windmill.csv")
attach(wind.dat)

#Plot data... does a line model appear appropriate
plot(Velocity,Output)

#Linear model summry and residual plots
wind.lm = lm(Output~Velocity)
summary(wind.lm)
abline(wind.lm)
plot(wind.lm)

#Transformation x*=1/x
V1 = 1/Velocity
plot(V1,Output,xlab=c("1/Velocity"))

#Linear model summry and residual plots for transformed x
wind.lm2 = lm(Output~V1)
summary(wind.lm2)
abline(wind.lm2)
plot(wind.lm2)

#Transformation y'=ln(y)
ln.V = log(Velocity)
plot(ln.V,Output,xlab=c("ln(Velocity)"))

#Linear model summry and residual plots for transformed x
wind.ln.V = lm(Output~ln.V)
summary(wind.ln.V)
abline(wind.ln.V)
plot(wind.ln.V)


