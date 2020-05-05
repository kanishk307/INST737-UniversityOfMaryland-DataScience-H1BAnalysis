#Laoding the data 
setwd("E:/INST737/Milestone3")
dataset<-read.csv('onehotenc_clean.csv')
columns<-names(dataset)
columns
df <- data.frame(dataset)


#installing packages
library(dplyr)
library(kernlab)

#Creating Train and test data randomly
randomsample=sample_n(df, 100000)
#write.csv(randomsample,"C:\\Users\\kjain307\\Documents\\GitHub\\INST737-UniversityOfMaryland-DataScience-H1BAnalysis\\Milestone2\\encoding\\sample.csv")
smp_size <- floor(0.80 * nrow(randomsample))
set.seed(123)
train_generator <- sample(seq_len(nrow(randomsample)),size=smp_size)
train <- randomsample[train_generator,]
test<- randomsample[-train_generator,]


# Aligning the variables  for required data type
train$AGENT_PRESENT_1.0<-as.factor(train$AGENT_PRESENT_1.0)
train$HOURLY_WAGE<-as.numeric(train$HOURLY_WAGE)
train$WAGE_RATE_OF_PAY_FROM_HOUR<-as.numeric(train$WAGE_RATE_OF_PAY_FROM_HOUR)
train$DURATION<-as.numeric(train$DURATION)
train$CASE_STATUS_1.0<-as.factor(train$CASE_STATUS_1.0)
test$AGENT_PRESENT_1.0<-as.factor(test$AGENT_PRESENT_1.0)
test$HOURLY_WAGE<-as.numeric(test$HOURLY_WAGE)
test$WAGE_RATE_OF_PAY_FROM_HOUR<-as.numeric(test$WAGE_RATE_OF_PAY_FROM_HOUR)
test$DURATION<-as.numeric(test$DURATION)
test$CASE_STATUS_1.0<-as.factor(test$CASE_STATUS_1.0)


#Building the model
modelBinaryNonLinearAgentPresent<- ksvm(AGENT_PRESENT_1.0~DURATION+WAGE_RATE_OF_PAY_FROM_HOUR+HOURLY_WAGE+OCCUPATION+CASE_STATUS_1.0,data=train,kernel="rbfdot")

#Getting summary of the model
summary(modelBinaryNonLinearAgentPresent)
#Prediction of test data
pred <- predict(modelBinaryNonLinearAgentPresent,newdata=test)
length(pred)
#Confusion matrix generation
tab<- table(pred,test$AGENT_PRESENT_1.0)
tab
#Accuracy check
agreement <- pred==test$AGENT_PRESENT_1.0
tabag <- table(agreement)
tabag
 ``