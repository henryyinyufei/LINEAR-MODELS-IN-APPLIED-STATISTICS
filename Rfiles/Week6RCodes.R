## Week6RCodes.R - STAT350


## Soft Drink Delivery Data... AGAIN
# Input data
del.y<-c(16.68,11.50,12.03,14.88,13.75,18.11,8,17.83,79.24,21.5,40.33,21.00,13.5,
     19.75,24,29,15.35,19,9.5,35.1,17.9,52.32,18.75,19.83,10.75)
del.cases<-c(7,3,3,4,6,7,2,7,30,5,16,10,4,6,9,10,6,7,3,17,10,26,9,8,4)
del.dist<-c(560,220,340,80,150,330,110,210,1460,605,688,215,255,462,448,776,200,132,36,770,140,810,450,635,150)

# fit model
deliver.model<-lm(del.y~del.cases+del.dist)

#compute the X matrix and then the hat-matrix... get h_ii's
delX<-cbind(rep(1,length(del.y)), del.cases,del.dist)
hii<-diag(delX%*%solve(t(delX)%*%delX)%*%t(delX))

# Identify points of high Leverage
p<-ncol(delX) # number of betas in the model (beta0,beta1,beta2)
n<-nrow(delX) # number of observations
which(hii>2*p/n) #points with high leverage

# Calculate the residuals for additional information 

resid(deliver.model)
rstandard(deliver.model)
rstudent(deliver.model)
residtable<-cbind(resid(deliver.model),
                  rstandard(deliver.model), rstudent(deliver.model))
summary(residtable)
residtable[9,] #Influential
residtable[22,]
print(influence.measures(deliver.model))

