#Laoding the data 
setwd("/Users/gauravhasija/Desktop/inst737")
dataset<-read.csv('onehotenc_clean.csv')
columns<-names(dataset)
columns
df <- data.frame(dataset)

df$AGENT_PRESENT_0.0<-NULL
df$CASE_STATUS_0.0<-NULL
df$X<-NULL
col_order <- c("DURATION", "HOURLY_WAGE","WAGE_RATE_OF_PAY_FROM_HOUR", "WILLFUL_VIOLATOR","OCCUPATION","AGENT_PRESENT_1.0","CASE_STATUS_1.0")
df <- df[, col_order]
#View(df)


#installing packages
library(dplyr)
#install.packages("kernlab")
#ibrary(kernlab)

#Creating Train and test data randomly
randomsample=sample_n(df, 10000)
#write.csv(randomsample,"C:\\Users\\kjain307\\Documents\\GitHub\\INST737-UniversityOfMaryland-DataScience-H1BAnalysis\\Milestone2\\encoding\\sample.csv")
smp_size <- floor(0.80 * nrow(randomsample))
set.seed(123)
train_generator <- sample(seq_len(nrow(randomsample)),size=smp_size)
train <- randomsample[train_generator,]
test<- randomsample[-train_generator,]

str(train)

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
# CV with K fold
train_control <- trainControl(method="cv",number=5)
modelCaseStatusLogistic <- train(CASE_STATUS_1.0~HOURLY_WAGE+WAGE_RATE_OF_PAY_FROM_HOUR+DURATION,data=train,trControl=train_control,method="glm",family="binomial")
predCaseStatusLogistic <- predict(modelCaseStatusLogistic,test[,1:6])
predCaseStatusLogistic
length(predCaseStatusLogistic)
confusionMatrix(predCaseStatusLogistic,test$CASE_STATUS_1.0)

# Confusion Matrix and Statistics
# 
# Reference
# Prediction    0    1
# 0   35    0
# 1   47 1918
# 
# Accuracy : 0.9765          
# 95% CI : (0.9689, 0.9827)
# No Information Rate : 0.959           
# P-Value [Acc > NIR] : 1.371e-05       
# 
# Kappa : 0.5882          
# 
# Mcnemar's Test P-Value : 1.949e-11       
#                                           
#             Sensitivity : 0.4268          
#             Specificity : 1.0000          
#          Pos Pred Value : 1.0000          
#          Neg Pred Value : 0.9761          
#              Prevalence : 0.0410          
#          Detection Rate : 0.0175          
#    Detection Prevalence : 0.0175          
#       Balanced Accuracy : 0.7134          
#                                           
#        'Positive' Class : 0               
#                                 
# CV with boot
train_control <- trainControl(method="boot")
modelCaseStatusLogistic <- train(CASE_STATUS_1.0~HOURLY_WAGE+WAGE_RATE_OF_PAY_FROM_HOUR+DURATION,data=train,trControl=train_control,method="glm",family="binomial")
predCaseStatusLogistic <- predict(modelCaseStatusLogistic,test[,1:6])
predCaseStatusLogistic
length(predCaseStatusLogistic)
confusionMatrix(predCaseStatusLogistic,test$CASE_STATUS_1.0)

# Confusion Matrix and Statistics
# 
# Reference
# Prediction    0    1
# 0   31    0
# 1   48 1921
# 
# Accuracy : 0.976           
# 95% CI : (0.9683, 0.9823)
# No Information Rate : 0.9605          
# P-Value [Acc > NIR] : 9.077e-05       
# 
# Kappa : 0.5537          
# 
# Mcnemar's Test P-Value : 1.170e-11       
#                                           
#             Sensitivity : 0.3924          
#             Specificity : 1.0000          
#          Pos Pred Value : 1.0000          
#          Neg Pred Value : 0.9756          
#              Prevalence : 0.0395          
#          Detection Rate : 0.0155          
#    Detection Prevalence : 0.0155          
#       Balanced Accuracy : 0.6962          
#                                           
#        'Positive' Class : 0 

# CV with LGOCV
train_control <- trainControl(method="LGOCV")
modelCaseStatusLogistic <- train(CASE_STATUS_1.0~HOURLY_WAGE+WAGE_RATE_OF_PAY_FROM_HOUR+DURATION,data=train,trControl=train_control,method="glm",family="binomial")
predCaseStatusLogistic <- predict(modelCaseStatusLogistic,test[,1:6])
predCaseStatusLogistic
length(predCaseStatusLogistic)
confusionMatrix(predCaseStatusLogistic,test$CASE_STATUS_1.0)


# Confusion Matrix and Statistics
# 
# Reference
# Prediction    0    1
# 0   31    0
# 1   48 1921
# 
# Accuracy : 0.976           
# 95% CI : (0.9683, 0.9823)
# No Information Rate : 0.9605          
# P-Value [Acc > NIR] : 9.077e-05       
# 
# Kappa : 0.5537          
# 
# Mcnemar's Test P-Value : 1.170e-11       
#                                           
#             Sensitivity : 0.3924          
#             Specificity : 1.0000          
#          Pos Pred Value : 1.0000          
#          Neg Pred Value : 0.9756          
#              Prevalence : 0.0395          
#          Detection Rate : 0.0155          
#    Detection Prevalence : 0.0155          
#       Balanced Accuracy : 0.6962          
#                                           
#        'Positive' Class : 0  
