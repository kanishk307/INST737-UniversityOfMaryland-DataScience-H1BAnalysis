#Laoding the data 
setwd("E:/INST737/Milestone3")
dataset<-read.csv('onehotenc_clean.csv')
columns<-names(dataset)
#attach(dataset)


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

# df<-as.data.frame(dataset)


# creation of test and train samples
df <- data.frame(dataset)


#install.packages("dplyr")
library(dplyr)
library(e1071)

randomsample=sample_n(df, 100000)


#write.csv(randomsample,"C:\\Users\\kjain307\\Documents\\GitHub\\INST737-UniversityOfMaryland-DataScience-H1BAnalysis\\Milestone2\\encoding\\sample.csv")

smp_size <- floor(0.80 * nrow(randomsample))


set.seed(123)

train_generator <- sample(seq_len(nrow(randomsample)),size=smp_size)

train <- randomsample[train_generator,]
test<- randomsample[-train_generator,]

#View(df)

# table(train$AGENT_PRESENT_0.0)
# table(test$AGENT_PRESENT_0.0)


# randomsample=sample_n(df, 100000)
# smp_size <- floor(0.80 * nrow(randomsample))
# 
# 
# set.seed(123)
# 
# train_generator <- sample(seq_len(nrow(randomsample)),size=smp_size)
# 
# # train <- randomsample[train_generator,]
# # test<- randomsample[-train_generator,]
# 
# table(train$AGENT_PRESENT_0.0)
# table(test$AGENT_PRESENT_0.0)

library(kernlab)
#applying the model

memory.limit(size = 99999999999)

model2M<- ksvm(train$DURATION_RANGE~.,data=train,kernel="rbfdot")


pred2M <- predict(model2M,test)
tab2M<- table(pred2M,test$DURATION_RANGE)
tab
agreement2M <- pred2M==test$DURATION_RANGE
tabag2M <- table(agreement2M)
tabag2M
# modelSVM<-naiveBayes(train$AGENT_PRESENT_0.0~train$OCCUPATION
#                   +train$DURATION_RANGE+train$WAGE_RATE_OF_PAY_FROM_HOUR_RANGE
#                   +train$HOURLY_WAGE_RANGE,data=train,laplace = 1)
# modelAP
# 
# 
# modelAP<-naiveBayes(train$AGENT_PRESENT_0.0~train$OCCUPATION
#                     +train$DURATION_RANGE+train$WAGE_RATE_OF_PAY_FROM_HOUR_RANGE
#                     +train$HOURLY_WAGE_RANGE,data=train,laplace = 1)
# modelAP
# 
# 
# #prediction
# predmodelAP<-predict(modelAP,test)
# 
# 
# #creation of the confusion matrix
# 
# ctable<-table(predmodelAP,test$CASE_STATUS_0.0)
# 
# ctable
# 
# fourfoldplot(ctable, color = c("#CC6666", "#99CC99"),
#              conf.level = 0, margin = 1, main = "Confusion Matrix")
# 
# 
