#Laoding the data 
setwd("E:/INST737/Milestone3")
dataset<-read.csv('onehotenc_clean.csv')
columns<-names(dataset)
#attach(dataset)

dataset$CASE_STATUS_0.0 <- NULL
dataset$AGENT_PRESENT_0.0 <- NULL

#aligning the variables
str(dataset)
dataset$CASE_STATUS_1.0 <- as.factor(dataset$CASE_STATUS_1.0)
#dataset$CASE_STATUS_0.0 <- as.factor(dataset$CASE_STATUS_0.0)
dataset$AGENT_PRESENT_1.0 <- as.factor(dataset$AGENT_PRESENT_1.0)
#dataset$AGENT_PRESENT_0.0 <- as.factor(dataset$AGENT_PRESENT_0.0)
dataset$OCCUPATION <- as.factor(dataset$OCCUPATION)
dataset$WAGE_RATE_OF_PAY_FROM_HOUR<-as.numeric(dataset$WAGE_RATE_OF_PAY_FROM_HOUR)
dataset$HOURLY_WAGE<-as.numeric(dataset$HOURLY_WAGE)
#dataset$WAGE_RATE_OF_PAY_FROM_HOUR_RANGE = cut(dataset$WAGE_RATE_OF_PAY_FROM_HOUR,c(0,10,20,30,40,50,60,70,80,90,100))
#dataset$HOURLY_WAGE_RANGE = cut(dataset$HOURLY_WAGE,c(0,10,20,30,40,50,60,70,80,90,100))
dataset$DURATION_RANGE = cut(dataset$DURATION,c(0,10,20,30,40,50,60,70,80,90,100))

# creation of test and train samples
df <- data.frame(dataset)


#install.packages("dplyr")
library(dplyr)
library(kernlab)


randomsample=sample_n(df, 100000)
#write.csv(randomsample,"C:\\Users\\kjain307\\Documents\\GitHub\\INST737-UniversityOfMaryland-DataScience-H1BAnalysis\\Milestone2\\encoding\\sample.csv")
smp_size <- floor(0.80 * nrow(randomsample))
set.seed(123)
train_generator <- sample(seq_len(nrow(randomsample)),size=smp_size)
train <- randomsample[train_generator,]
test<- randomsample[-train_generator,]




memory.limit(size = 99999999999)

#applying the model
modelMulticlassNonLinearDuration<- ksvm(DURATION_RANGE~.,data=train,kernel="rbfdot")
#Getting summary of the model
summary(modelMulticlassLinearDuration)
#Prediction of test data
pred <- predict(modelMulticlassLinearDuration,newdata=test)
length(pred)
#Confusion matrix generation
tab<- table(pred,test$DURATION_RANGE)
tab
#Accuracy check
agreement <- pred==test$DURATION_RANGE
tabag <- table(agreement)
tabag
