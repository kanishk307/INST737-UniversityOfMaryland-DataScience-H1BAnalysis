#Laoding the data 
setwd("E:/INST737/Milestone3")
dataset<-read.csv('onehotenc_clean.csv')
columns<-names(dataset)
columns
#attach(dataset)


df <- data.frame(dataset)




#installing packages
library(dplyr)
library(e1071)

# creation of test and train samples
#randomsample=sample_n(df1, 100000)
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





  
library(kernlab)



model<- ksvm(CASE_STATUS_1.0~DURATION+WAGE_RATE_OF_PAY_FROM_HOUR+HOURLY_WAGE+OCCUPATION,data=train,kernel="vanilladot")
summary(model)
pred <- predict(model,newdata=test)
length(pred)
tab<- table(pred,test$CASE_STATUS_1.0)
tab
agreement <- pred==test$CASE_STATUS_1.0
tabag <- table(agreement)
tabag







 

#df1 <- df
#df2 <- df

#Removing unwanted columns because predict function was getting error with selective columns. 
# df1$AGENT_PRESENT_0.0<-NULL
# df1$WILLFUL_VIOLATOR<-NULL 
# df1$AGENT_PRESENT_1.0 <-NULL
# df1$CASE_STATUS_0.0<-NULL



# 
# model2<- ksvm(train$CASE_STATUS_1.0~train$DURATION+train$WAGE_RATE_OF_PAY_FROM_HOUR+train$HOURLY_WAGE+train$OCCUPATION,data=train,kernel="rbfdot")
# summary(model2)
# pred2 <- predict(model2,test)
# tab2<- table(pred2,train$CASE_STATUS_1.0)
# agreement2 <- pred2==train$CASE_STATUS_1.0
# tabag2 <- table(agreement2)
# 









# # applying the model 
# model<- ksvm(train$CASE_STATUS_1.0~train$DURATION+train$WAGE_RATE_OF_PAY_FROM_HOUR+train$HOURLY_WAGE,data=train,kernel="vanilladot")
# model2<- ksvm(train$CASE_STATUS_1.0~train$DURATION+train$WAGE_RATE_OF_PAY_FROM_HOUR+train$HOURLY_WAGE,data=train,kernel="rbfdot")
# 
# pred <- predict(model,test)
# pred2 <- predict(model2,test)
# 
# tab<- table(pred,train$CASE_STATUS_1.0)
# agreement <- pred==train$CASE_STATUS_1.0
# tabag <- table(agreement)
# 
# tab2<- table(pred2,train$CASE_STATUS_1.0)
# agreement2 <- pred2==train$CASE_STATUS_1.0
# tabag2 <- table(agreement2)
# # summary(model)
# # 
# # pred<-predict(model, 
# #               )
# # 
# # tab<-table(Predicted=pred,Actual=train$CASE_STATUS_1.0)
# # tab
# # 
# # 
# # modelcsap<-glm(CASE_STATUS_1.0~DURATION+WAGE_RATE_OF_PAY_FROM_HOUR+HOURLY_WAGE,data=train,family="binomial")
# # summary(modelcsap)
# # 
# # #log odds and odd ratios
# # exp(coef(modelcsap)) #odds ratio
# # log(exp(coef(modelcsap))) #log odds
# # 
# # 
# # #Creating the test data frame.
# # newdata2<-with(test,data.frame(DURATION=as.numeric(test$DURATION),
# #                                WAGE_RATE_OF_PAY_FROM_HOUR=as.numeric(test$WAGE_RATE_OF_PAY_FROM_HOUR)
# #                               ,HOURLY_WAGE=as.numeric(test$HOURLY_WAGE)))
# # newdata2
# # 
# # str(newdata2)
# # #Predicting the probability based on model
# # newdata2$prob<-predict(modelcsap,newdata=newdata2,type="response")
# # 
# # newdata2
# # 
# # #Plotting the probabilities with respect to significant variables.
# # plot(newdata2$prob,newdata2$WAGE_RATE_OF_PAY_FROM_HOUR,xlab = 'Probility of case certified',ylab = 'Employee Hourly Salary' )
# # 
# # 
# # 
# # plot(newdata2$prob,newdata2$DURATION,xlab = 'Probility of case certified',ylab = 'Duration of Decision' )
# # 
# # plot(newdata2$prob,newdata2$HOURLY_WAGE,xlab = 'Probility of case certified',ylab = 'Goverment approved hourly wage' )
# # 
# 
# 
# 
# 
# 
# 
# 
