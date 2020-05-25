#Laoding the data 
setwd("E:/INST737/Milestone3")
dataset<-read.csv('onehotenc_clean.csv')
columns<-names(dataset)
columns
df <- data.frame(dataset)

df$AGENT_PRESENT_0.0<-NULL
df$CASE_STATUS_0.0<-NULL
df$X<-NULL
col_order <- c("DURATION", "HOURLY_WAGE","WAGE_RATE_OF_PAY_FROM_HOUR", "WILLFUL_VIOLATOR","OCCUPATION","CASE_STATUS_1.0","AGENT_PRESENT_1.0")
df <- df[, col_order]
#View(df)


#installing packages
library(dplyr)
#library(kernlab)

#Creating Train and test data randomly
randomsample=sample_n(df, 10000)
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



library(caret)

#boot
train_control <- trainControl(method="boot")
#Model Logistic Regression
modelAgentPresentLogistic <- train(AGENT_PRESENT_1.0~HOURLY_WAGE+WAGE_RATE_OF_PAY_FROM_HOUR+DURATION,data=train,trControl=train_control,method="glm",family="binomial")
predAgentPresentLogistic <- predict(modelAgentPresentLogistic,test[,1:6])
length(predAgentPresentLogistic)
#Accuracy Check
confusionMatrix(predAgentPresentLogistic,test$AGENT_PRESENT_1.0)



# Confusion Matrix and Statistics
# 
# Reference
# Prediction    0    1
# 0   29   45
# 1  722 1204
# 
# Accuracy : 0.6165          
# 95% CI : (0.5948, 0.6379)
# No Information Rate : 0.6245          
# P-Value [Acc > NIR] : 0.7772          
# 
# Kappa : 0.0032          
# 
# Mcnemar's Test P-Value : <2e-16          
#                                           
#             Sensitivity : 0.03862         
#             Specificity : 0.96397         
#          Pos Pred Value : 0.39189         
#          Neg Pred Value : 0.62513         
#              Prevalence : 0.37550         
#          Detection Rate : 0.01450         
#    Detection Prevalence : 0.03700         
#       Balanced Accuracy : 0.50129         
#                                           
#        'Positive' Class : 0    

