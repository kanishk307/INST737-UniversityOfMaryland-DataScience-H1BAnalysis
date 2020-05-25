# Loading the data
setwd("/Users/gauravhasija/Downloads")
dataset<-read.csv('onehotenc_clean.csv')
columns<-names(dataset)
#attach(dataset)

# creation of test and train samples
df <- data.frame(dataset)

randomsample=sample_n(df, 100000)


smp_size <- floor(0.80 * nrow(randomsample))

set.seed(123)

train_generator <- sample(seq_len(nrow(randomsample)),size=smp_size)

train <- randomsample[train_generator,]
test<- randomsample[-train_generator,]

#aligining the variables factores and numeric as required
train$AGENT_PRESENT_1.0<-as.factor(train$AGENT_PRESENT_1.0)
train$HOURLY_WAGE<-as.numeric(train$HOURLY_WAGE)
train$WAGE_RATE_OF_PAY_FROM_HOUR<-as.numeric(train$WAGE_RATE_OF_PAY_FROM_HOUR)
train$DURATION<-as.numeric(train$DURATION)
train$CASE_STATUS_1.0<-as.factor(train$CASE_STATUS_1.0)
train$OCCUPATION<-as.factor(train$OCCUPATION)

test$AGENT_PRESENT_1.0<-as.factor(test$AGENT_PRESENT_1.0)
test$HOURLY_WAGE<-as.numeric(test$HOURLY_WAGE)
test$WAGE_RATE_OF_PAY_FROM_HOUR<-as.numeric(test$WAGE_RATE_OF_PAY_FROM_HOUR)
test$DURATION<-as.numeric(test$DURATION)
test$CASE_STATUS_1.0<-as.factor(test$CASE_STATUS_1.0)
test$OCCUPATION<-as.factor(test$OCCUPATION)


#apllying the model
modelcsap<-glm(AGENT_PRESENT_1.0~DURATION+CASE_STATUS_1.0+WAGE_RATE_OF_PAY_FROM_HOUR+OCCUPATION+HOURLY_WAGE,data=train,family="binomial")
summary(modelcsap)

modelcsap<-glm(AGENT_PRESENT_1.0~DURATION+WAGE_RATE_OF_PAY_FROM_HOUR,data=train,family="binomial")
summary(modelcsap)

#log odds and odd ratios
exp(coef(modelcsap)) #odds ratio
log(exp(coef(modelcsap))) #log odds


# creating a test data frame for prediction



newdata2<-with(test,data.frame(DURATION=as.numeric(test$DURATION),
                               WAGE_RATE_OF_PAY_FROM_HOUR=as.numeric(test$WAGE_RATE_OF_PAY_FROM_HOUR)
                               ))
newdata2

# Predicting the values on model using test data
str(newdata2)

newdata2$prob<-predict(modelcsap,newdata=newdata2,type="response")

newdata2



#Ploting the probability to check the results
plot(newdata2$prob,newdata2$WAGE_RATE_OF_PAY_FROM_HOUR,xlab = 'Probility of agent present for the case',ylab = 'Employee Hourly Salary' )

plot(newdata2$prob,newdata2$DURATION,xlab = 'Probility of agent present for the case',ylab = 'Duration of Decision' )














