## Week11RCodes.R - STAT350

## Soft Drink Delivery Data
# Original data (Column A of Table 11.5)
del.y<-c(16.68,11.50,12.03,14.88,13.75,18.11,8,17.83,79.24,21.5,40.33,21.00,13.5,
     19.75,24,29,15.35,19,9.5,35.1,17.9,52.32,18.75,19.83,10.75)
del.cases<-c(7,3,3,4,6,7,2,7,30,5,16,10,4,6,9,10,6,7,3,17,10,26,9,8,4)
del.dist<-c(560,220,340,80,150,330,110,210,1460,605,688,215,255,462,448,776,200,132,36,770,140,810,450,635,150)
deliver.model<-lm(del.y~del.cases+del.dist)
summary(deliver.model)

# New observations... see if coefficents make sense
new.y<-c(51,16.8,26.16,19.90,24,18.55,31.93,16.95,7,14,37.03,18.62,16.10,24.38,64.75)
new.cases<-c(22,7,15,5,6,6,10,4,1,3,12,10,7,8,32)
new.dist<-c(905,520,290,500,1000,225,775,212,144,126,655,420,150,360,1530)
new.model<-lm(new.y~new.cases+new.dist)
summary(new.model)

#predict observations using initial data set  
new.x=cbind(new.cases,new.dist)
new.x=as.data.frame((new.x))
new_names=c("del.cases","del.dist")
colnames(new.x) = new_names

preds <- predict.lm(deliver.model,new.x)

#plot predictions vs new y's
plot(new.y,preds,xlim=c(0,80),ylim=c(0,80))
abline(c(0,1))

library(caret)
R.sq = R2(preds, new.y)
RMSPE = RMSE(preds, new.y)
MAPE = MAE(preds, new.y)
print(c(R.sq,RMSPE,MAPE))


# All data (Column B of Table 11.5)
y<-c(del.y,new.y)
cases<-c(del.cases,new.cases)
dist<-c(del.dist,new.dist)
all.model<-lm(y~cases+dist)
summary(all.model)


