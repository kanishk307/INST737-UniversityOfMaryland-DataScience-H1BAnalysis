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
#Model Naive Bayes
modelAgentPresentNaiveBayes <- train(AGENT_PRESENT_1.0~HOURLY_WAGE+WAGE_RATE_OF_PAY_FROM_HOUR+DURATION,data=train,trControl=train_control,method="nb")
predAgentPresentNaiveBayes <- predict(modelAgentPresentNaiveBayes,test[,1:6])
length(predAgentPresentNaiveBayes)
#Accuracy Check
confusionMatrix(predAgentPresentNaiveBayes,test$AGENT_PRESENT_1.0)


# Reference
# Prediction   0   1
# 0 344 297
# 1 462 897
# 
# Accuracy : 0.6205          
# 95% CI : (0.5988, 0.6418)
# No Information Rate : 0.597           
# P-Value [Acc > NIR] : 0.01678         
# 
# Kappa : 0.1842          
# 
# Mcnemar's Test P-Value : 2.636e-09       
#                                           
#             Sensitivity : 0.4268          
#             Specificity : 0.7513          
#          Pos Pred Value : 0.5367          
#          Neg Pred Value : 0.6600          
#              Prevalence : 0.4030          
#          Detection Rate : 0.1720          
#    Detection Prevalence : 0.3205          
#       Balanced Accuracy : 0.5890          
#                                           
#        'Positive' Class : 0     

