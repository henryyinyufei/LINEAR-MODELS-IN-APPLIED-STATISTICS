library(caret)
library(psych)
library(tidyverse)
library(car)

#regession analysis
data1 <- sat.act[complete.cases(sat.act),]

head(data1)
attach(data1)
act.lm=lm(ACT ~ gender + age + SATV + SATQ)
summary(act.lm)
plot(act.lm)


#get partial regression plots
avPlots(act.lm)
detach(data1)
# evaluate model on subsets

set.seed(71168)
nsamp=ceiling(0.8*length(data1$ACT))
training_samps=sample(c(1:length(data1$ACT)),nsamp)
training_samps=sort(training_samps)
train_data  <- data1[training_samps, ]
test_data <-   data1[-training_samps, ]


train.lm <- lm(ACT ~ gender + age + SATV + SATQ, data = train_data)
summary(train.lm)

preds <- predict(train.lm,test_data)

plot(test_data$ACT,preds, xlim=c(15,40),ylim=c(15,40))
abline(c(0,1))



R.sq = R2(preds, test_data$ACT)
RMSPE = RMSE(preds, test_data$ACT)
MAPE = MAE(preds, test_data$ACT)
print(c(R.sq,RMSPE,MAPE))


NSE=1-RMSPE^2/var(test_data$ACT) 


#do 5 times
set.seed(71168)

for(i in 1:5){

training_samps=sample(c(1:length(data1$ACT)),nsamp)
training_samps=sort(training_samps)
train_data  <- data1[training_samps, ]
test_data <-   data1[-training_samps, ]

train.lm <- lm(ACT ~ gender + age + SATV + SATQ, data = train_data)
summary(train.lm)

preds <- predict(train.lm,test_data)

plot(test_data$ACT,preds, xlim=c(15,40),ylim=c(15,40))
abline(c(0,1))

R2 = R2(preds, test_data$ACT)
RMSPE = RMSE(preds, test_data$ACT)
MAPE = MAE(preds, test_data$ACT)


print(c(i,R2,RMSPE,MAPE))

}
