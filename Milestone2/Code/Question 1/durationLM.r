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

modelAP<- lm(train$DURATION~ train$AGENT_PRESENT_0.0+train$AGENT_PRESENT_1.0,data=train)

summary(modelAP)

fit = data.frame(train,fitted.value=fitted(modelAP),residual=resid(modelAP))

View(fit)

pc <- predict(modelAP,newdata=fit)
View(pc)
pc<-predict(modelAP,int="c",newdata=fit)
pp<-predict(modelAP,int="p",newdata=fit)
plot(fit$AGENT_PRESENT_0.0,fit$DURATION)
matlines(fit,pp)
#qqnorm(resid(modelAP))
#qqline

modelHW <- lm(train$DURATION~ train$HOURLY_WAGE,data=train)

summary(modelHW)

modelWFH <- lm(train$DURATION~train$WAGE_RATE_OF_PAY_FROM_HOUR)
summary(modelWFH)

modelOC <- lm(train$DURATION~train$OCCUPATION)
summary(modelOC)

modelCS <- lm(train$DURATION~train$CASE_STATUS_0.0)
summary(modelCS)


#prediction <- predict(modelAP,test)

#output <-cbind(testdata,prediction)
