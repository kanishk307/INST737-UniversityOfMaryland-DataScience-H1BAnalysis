#setting up the data
setwd("/Users/gauravhasija/Downloads")
dataset<-read.csv('onehotenc_clean.csv')
columns<-names(dataset)
attach(dataset)

#library(dplyr)
#install.packages("dplyr")

#aligning the variables
str(dataset)
dataset$CASE_STATUS_1.0 <- as.factor(dataset$CASE_STATUS_1.0)
dataset$CASE_STATUS_0.0 <- as.factor(dataset$CASE_STATUS_0.0)
dataset$AGENT_PRESENT_1.0 <- as.factor(dataset$AGENT_PRESENT_1.0)
dataset$AGENT_PRESENT_0.0 <- as.factor(dataset$AGENT_PRESENT_0.0)
dataset$OCCUPATION <- as.factor(dataset$OCCUPATION)
dataset$CASE_STATUS_0.0 <- as.factor(dataset$CASE_STATUS_0.0)
dataset$WAGE_RATE_OF_PAY_FROM_HOUR_RANGE = cut(dataset$WAGE_RATE_OF_PAY_FROM_HOUR,c(0,10,20,30,40,50,60,70,80,90,100))
dataset$DURATION_RANGE = cut(dataset$DURATION,c(0,10,20,30,40,50,60,70,80,90,100))
dataset$HOURLY_WAGE_RANGE = cut(dataset$HOURLY_WAGE,c(0,10,20,30,40,50,60,70,80,90,100))

#generation of the randon test and train samples

df<-as.data.frame(dataset)

table(train$AGENT_PRESENT_0.0)
table(test$AGENT_PRESENT_0.0)


randomsample=sample_n(df, 100000)
smp_size <- floor(0.70 * nrow(randomsample))


set.seed(123)

train_generator <- sample(seq_len(nrow(randomsample)),size=smp_size)

train <- randomsample[train_generator,]
test<- randomsample[-train_generator,]

table(train$AGENT_PRESENT_0.0)
table(test$AGENT_PRESENT_0.0)

#applying the model

modelAP<-naiveBayes(train$AGENT_PRESENT_0.0~train$OCCUPATION
                  +train$DURATION_RANGE+train$WAGE_RATE_OF_PAY_FROM_HOUR_RANGE
                  +train$HOURLY_WAGE_RANGE,data=train,laplace = 1)
modelAP


modelAP<-naiveBayes(train$AGENT_PRESENT_0.0~train$OCCUPATION
                    +train$DURATION_RANGE+train$WAGE_RATE_OF_PAY_FROM_HOUR_RANGE
                    +train$HOURLY_WAGE_RANGE,data=train,laplace = 1)
modelAP


#prediction
predmodelAP<-predict(modelAP,test)


#creation of the confusion matrix

ctable<-table(predmodelAP,test$CASE_STATUS_0.0)

ctable

fourfoldplot(ctable, color = c("#CC6666", "#99CC99"),
             conf.level = 0, margin = 1, main = "Confusion Matrix")


