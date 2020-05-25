setwd("C:\\Users\\kjain307\\Documents\\GitHub\\INST737-UniversityOfMaryland-DataScience-H1BAnalysis\\Milestone2\\encoding")
dataset<-read.csv('onehotenc_clean.csv')
columns<-names(dataset)


df <- data.frame(dataset)


df$AGENT_PRESENT_0.0 <- as.factor(df$AGENT_PRESENT_0.0)
df$AGENT_PRESENT_1.0 <- as.factor(df$AGENT_PRESENT_1.0)
df$OCCUPATION <- as.factor(df$OCCUPATION)
df$CASE_STATUS_0.0 <- as.factor(df$CASE_STATUS_0.0)
df$CASE_STATUS_1.0 <- as.factor(df$CASE_STATUS_1.0)

#Sample the database to remove outliers
#df1<-subset(df,df$HOURLY_WAGE<100 & df$WAGE_RATE_OF_PAY_FROM_HOUR<100 & df$DURATION < 90)


smp_size <- floor(0.80 * nrow(df))


set.seed(2)

train_generator <- sample(seq_len(nrow(df)),size=smp_size)

train <- df[train_generator,]
test<- df[-train_generator,]

write.csv(train,"C:\\Users\\kjain307\\Documents\\GitHub\\INST737-UniversityOfMaryland-DataScience-H1BAnalysis\\Milestone2\\encoding\\train.csv")

write.csv(test,"C:\\Users\\kjain307\\Documents\\GitHub\\INST737-UniversityOfMaryland-DataScience-H1BAnalysis\\Milestone2\\encoding\\test.csv")

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
smModelWFH <- summary(modelWFH)
mean(smModelWFH$residuals^2)


fit = data.frame(train,fitted.value=fitted(modelWFH),residual=resid(modelWFH))

pc<-predict(modelWFH,int="c",newdata=fit)
pp<-predict(modelWFH,int="p",newdata=fit)






attach(train)

modelMul2 <- lm(HOURLY_WAGE~.,data=train)
prediction <- predict(modelMul2,newdata=test)
cor(prediction,test$HOURLY_WAGE)

smModelMul2 <- summary(modelMul2)
mean(smModelMul2$residuals^2)
smModelMul2


library(glmnet)
cv.fit <- cv.glmnet(as.matrix(train[,c(-2,-3,-5,-6,-7,-8,-9,-10)]),as.vector(train[,3]),alpha=1)
plot(cv.fit)
coef(cv.fit)
cv.fit$lambda.min

prediction2 <- predict(cv.fit,newx=as.matrix(test[,c(-1,-2,-3,-5,-6,-7,-8,-9,-10)]))
cor(prediction,as.vector(test[,3]))

