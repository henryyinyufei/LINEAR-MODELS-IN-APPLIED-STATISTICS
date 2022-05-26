#create a random column x1, with x2 a multiple of x1
# next, generate some y's
# and extreme example
x1=rnorm(15)
x2=3*x1
y=1+2*x1+4*x2+rnorm(15,sd=1.5)

#plot data
par(mfrow=c(1,2))
plot(x1,y)
plot(x2,y)
par(mfrow=c(1,1))

#fit linear model
lin.mod=lm(y~x1+x2)
summary(lin.mod)

#############################
#Gasoline example

##---- Install relevant libraries (Tools -> Install ...)
install.packages("MPV")
install.packages("faraway")
library(MPV) 
library(faraway)

# get the textbook data for our example
gas.data = table.b3
names(gas.data)

#plot data
pairs(gas.data)

#fit liinear model
attach(gas.data)
gas.lm=lm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11)
summary(gas.lm)

#  You should do residual analysis
plot(gas.lm)

#look at pairwise correlations in the X-matrix
#
X<-cbind(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11)
X=X[c(1:22,24,26:32),] #remove NA's
cor(X)

#Variance inflation factors
vif(gas.lm)

#condition numbers
XX=t(X)%*%X
lambda = eigen(XX)$values
cond_number=max(lambda)/min(lambda) #condition mumber of matrix
indices=max(lambda)/lambda





