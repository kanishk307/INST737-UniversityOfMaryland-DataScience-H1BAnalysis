setwd("C:\\Users\\kjain307\\Documents\\GitHub\\INST737-UniversityOfMaryland-DataScience-H1BAnalysis\\Milestone2\\encoding")
dataset<-read.csv('clean-new3.csv')
columns<-names(dataset)
#View(dataset)

df <- data.frame(dataset)

#randomsample=sample_n(df, 100000)


#write.csv(randomsample,"C:\\Users\\kjain307\\Documents\\GitHub\\INST737-UniversityOfMaryland-DataScience-H1BAnalysis\\Milestone2\\encoding\\sample.csv")

smp_size <- floor(0.80 * nrow(df))


set.seed(123)

train_generator <- sample(seq_len(nrow(df)),size=smp_size)

train <- df[train_generator,]
test<- df[-train_generator,]

write.csv(train,"C:\\Users\\kjain307\\Documents\\GitHub\\INST737-UniversityOfMaryland-DataScience-H1BAnalysis\\Milestone2\\encoding\\train.csv")

write.csv(test,"C:\\Users\\kjain307\\Documents\\GitHub\\INST737-UniversityOfMaryland-DataScience-H1BAnalysis\\Milestone2\\encoding\\test.csv")

#plot(randomsample$DURATION,randomsample$EMPLOYER_STATE)

#modelFH=lm(train$DURATION~train$WAGE_RATE_OF_PAY_FROM_HOUR,data=train)

#summary(modelFH)

#new.data <- data.frame('train$WAGE_RATE_OF_PAY_FROM_HOUR' = c(30, 40, 50))





df$AGENT_PRESENT <- as.factor(df$AGENT_PRESENT)
df$CASE_STATUS <- as.factor(df$CASE_STATUS)
df$WILLFUL_VIOLATOR <- as.factor(df$WILLFUL_VIOLATOR)
df$EMPLOYER_STATE <- as.factor(df$EMPLOYER_STATE)
df$WORKSITE_STATE <- as.factor(df$WORKSITE_STATE)


df1<-subset(df,df$HOURLY_WAGE<100 & df$WAGE_RATE_OF_PAY_FROM_HOUR<100 & df$DURATION < 90)

randomsample=sample_n(df1, 10000)

randomsample_onehot <- one_hot(as.data.table(randomsample))

#df1<-data.frame(dataset1)

#attach(df1)
modelAP = lm(df$DURATION~df$AGENT_PRESENT)

summary(modelAP)

#write.csv(dataset1,'clean-new3.csv')
#plot(dataset1$DURATION,dataset1$AGENT_PRESENT)



#modelTest = lm(dataset$WAGE_RATE_OF_PAY_FROM_HOUR~dataset$HOURLY_WAGE,data=dataset)
#summary(modelTest)
#pred = predict(modelFH, test$WAGE_RATE_OF_PAY_FROM_HOUR)

#boxplot(dataset1$HOURLY_WAGE)




#model3 = lm(dataset$DURATION~dataset$AGENT_PRESENT+dataset$CASE_STATUS+dataset$EMPLOYER_STATE+dataset$H.1B_DEPENDENT+dataset$HOURLY_WAGE+dataset$TOTAL_WORKERS+dataset$WAGE_RATE_OF_PAY_FROM_HOUR+dataset$WILLFUL_VIOLATOR+dataset$WORKSITE_STATE)

#summary(model3)

#View(randomsample)

#model2 = lm(randomsample$DURATION~randomsample$YEARLY_WAGE)

#plot(randomsample$YEARLY_WAGE,randomsample$YEARLY_WAGE)

#corWageStat <-cor.test(,dataset$YEARLY_WAGE,dataset$CASE_STATUS,  method = "spearman")
#dataset$CASE_STATUS <- as.factor(dataset$CASE_STATUS)
#dataset$YEARLY_WAGE <- as.numeric(dataset$YEARLY_WAGE)
#corWageStat <-cor.test(,dataset$YEARLY_WAGE,dataset$CASE_STATUS,  method = "spearman")
#model1 <- lm(dataset$DURATION~dataset$YEARLY_WAGE)
#model2 <- lm(dataset$DURATION~dataset$CASE_STATUS)
#summary(model1)
#summary(model2)
