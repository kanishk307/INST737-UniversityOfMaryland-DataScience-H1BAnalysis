# Loading the data
setwd("/Users/gauravhasija/Downloads")
dataset<-read.csv('onehotenc_undersamp.csv')
columns<-names(dataset)

attach(dataset)

#library(dplyr)
#install.packages("dplyr")

# Aligning the variables
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

#cutting out the train and test data 


df<-as.data.frame(dataset)


randomsample=sample_n(df, 100000)
smp_size <- floor(0.80 * nrow(randomsample))


set.seed(123)

train_generator <- sample(seq_len(nrow(randomsample)),size=smp_size)

train <- randomsample[train_generator,]
test<- randomsample[-train_generator,]


table(train$CASE_STATUS_0.0)
table(test$CASE_STATUS_0.0)

#applying the model
model<-naiveBayes(train$CASE_STATUS_0.0~train$AGENT_PRESENT_0.0+train$OCCUPATION
                  +train$DURATION_RANGE+train$WAGE_RATE_OF_PAY_FROM_HOUR_RANGE
                  +train$HOURLY_WAGE_RANGE,data=train,laplace = 1)
model


model<-naiveBayes(train$CASE_STATUS_0.0~train$AGENT_PRESENT_0.0+train$OCCUPATION,laplace = 1)
model

#performing the prediction
predmodel<-predict(model,test)




# create a table of predicted values for confusion matrix

ctable<-table(predmodel,test$CASE_STATUS_0.0)

#confusion MAtrix
ctable
#
fourfoldplot(ctable, color = c("#CC6666", "#99CC99"),
          conf.level = 0, margin = 1, main = "Confusion Matrix")






df$HOURLY_WAGE

write.csv(df,"/Users/gauravhasija/Desktop/INST737/oneHot_withCATEGORY.csv")









unique(df$HOURLY_WAGE)
df$HOURLY_WAGE_RANGE1 <- NULL

df = subset(df, select = -c(DURATION_RANGE))
