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




library(caret)

train_control <- trainControl(method="cv",number=5)
modelAgentPresentNaiveBayes <- train(AGENT_PRESENT_1.0~HOURLY_WAGE+WAGE_RATE_OF_PAY_FROM_HOUR+DURATION,data=train,trControl=train_control,method="nb")
predAgentPresentNaiveBayes <- predict(modelAgentPresentNaiveBayes,test[,1:6])
length(predAgentPresentNaiveBayes)
confusionMatrix(predAgentPresentNaiveBayes,test$AGENT_PRESENT_1.0)



# Confusion Matrix and Statistics
# 
# Reference
# Prediction    0    1
# 0 2886 2765
# 1 4671 9678
# 
# Accuracy : 0.6282          
# 95% CI : (0.6215, 0.6349)
# No Information Rate : 0.6222          
# P-Value [Acc > NIR] : 0.03932         
# 
# Kappa : 0.168           
# 
# Mcnemar's Test P-Value : < 2e-16         
#                                           
#             Sensitivity : 0.3819          
#             Specificity : 0.7778          
#          Pos Pred Value : 0.5107          
#          Neg Pred Value : 0.6745          
#              Prevalence : 0.3779          
#          Detection Rate : 0.1443          
#    Detection Prevalence : 0.2826          
#       Balanced Accuracy : 0.5798          
#                                           
#       'Positive' Class : 0 
