setwd("C:\\Users\\kjain307\\Documents\\GitHub\\INST737-UniversityOfMaryland-DataScience-H1BAnalysis\\Milestone2\\encoding")
dataset<-read.csv('onehotenc_clean.csv')
columns<-names(dataset)
#View(dataset)

df <- data.frame(dataset)


df$AGENT_PRESENT_0.0 <- as.factor(df$AGENT_PRESENT_0.0)
df$AGENT_PRESENT_1.0 <- as.factor(df$AGENT_PRESENT_1.0)
#df$AGENT_PRESENT_nan <- as.factor(df$AGENT_PRESENT_nan)
df$OCCUPATION <- as.factor(df$OCCUPATION)
df$CASE_STATUS_0.0 <- as.factor(df$CASE_STATUS_0.0)
df$CASE_STATUS_1.0 <- as.factor(df$CASE_STATUS_1.0)
#df$CASE_STATUS_nan <- as.factor(df$CASE_STATUS_nan)
#df$AGENT_PRESENT <- as.factor(df$AGENT_PRESENT)
#df$CASE_STATUS <- as.factor(df$CASE_STATUS)
#df$WILLFUL_VIOLATOR <- as.factor(df$WILLFUL_VIOLATOR)
#df$EMPLOYER_STATE <- as.factor(df$EMPLOYER_STATE)
#df$WORKSITE_STATE <- as.factor(df$WORKSITE_STATE)


#df1<-subset(df,df$HOURLY_WAGE<100 & df$WAGE_RATE_OF_PAY_FROM_HOUR<100 & df$DURATION < 90)


smp_size <- floor(0.80 * nrow(df))


set.seed(123)

train_generator <- sample(seq_len(nrow(df)),size=smp_size)

train <- df[train_generator,]
test<- df[-train_generator,]

#write.csv(train,"C:\\Users\\kjain307\\Documents\\GitHub\\INST737-UniversityOfMaryland-DataScience-H1BAnalysis\\Milestone2\\encoding\\train.csv")

#write.csv(test,"C:\\Users\\kjain307\\Documents\\GitHub\\INST737-UniversityOfMaryland-DataScience-H1BAnalysis\\Milestone2\\encoding\\test.csv")

testdata <-test

modelDU <- lm(train$HOURLY_WAGE~train$DURATION)
summary(modelDU)

modelOC <- lm(train$HOURLY_WAGE~train$OCCUPATION)
summary(modelOC)

modelAP <- lm(train$HOURLY_WAGE~train$AGENT_PRESENT_0.0)
summary(modelAP)

modelCS <- lm(train$HOURLY_WAGE~train$CASE_STATUS_0.0)
summary(modelCS)

modelWFH <- lm(train$HOURLY_WAGE~train$WAGE_RATE_OF_PAY_FROM_HOUR)
summary(modelWFH)


fit = data.frame(train,fitted.value=fitted(modelWFH),residual=resid(modelWFH))
View(fit)
pc<-predict(modelWFH,int="c",newdata=fit)
pp<-predict(modelWFH,int="p",newdata=fit)
#plot(fit$WAGE_RATE_OF_PAY_FROM_HOUR,fit$HOURLY_WAGE)
#matlines(fit,pc)
#matlines(fit,pp)




#modelMul <- lm(train$HOURLY_WAGE~train$WAGE_RATE_OF_PAY_FROM_HOUR+train$OCCUPATION+train$AGENT_PRESENT_0.0)
#summary(modelMul)

#prediction <- data.frame()

#fitMul = data.frame(train,fitted.value=fitted(modelMul),residual=resid(modelMul))
#View(fitMul)

#qqnorm(resid(modelMul))


attach(train)

modelMul2 <- lm(HOURLY_WAGE~.,data=train)
prediction <- predict(modelMul2,newdata=test)
cor(prediction,test$HOURLY_WAGE)

