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

train_control <- trainControl(method="cv",number=5)
modelCaseStatusSVM <- train(CASE_STATUS_1.0~HOURLY_WAGE+WAGE_RATE_OF_PAY_FROM_HOUR+DURATION,data=train,trControl=train_control,method="svmLinear", preProcess = c("center","scale"))
predCaseStatusSVM <- predict(modelCaseStatusSVM,test[,1:6])
length(predCaseStatusSVM)
confusionMatrix(predCaseStatusSVM,test$CASE_STATUS_1.0)

# Confusion Matrix and Statistics
# 
# Reference
# Prediction    0    1
# 0   50    0
# 1   45 1905
# 
# Accuracy : 0.9775        
# 95% CI : (0.97, 0.9835)
# No Information Rate : 0.9525        
# P-Value [Acc > NIR] : 4.428e-09     
# 
# Kappa : 0.6791        
# 
# Mcnemar's Test P-Value : 5.412e-11     
#                                         
#             Sensitivity : 0.5263        
#             Specificity : 1.0000        
#          Pos Pred Value : 1.0000        
#          Neg Pred Value : 0.9769        
#              Prevalence : 0.0475        
#          Detection Rate : 0.0250        
#    Detection Prevalence : 0.0250        
#       Balanced Accuracy : 0.7632        
#                                         
#        'Positive' Class : 0 
