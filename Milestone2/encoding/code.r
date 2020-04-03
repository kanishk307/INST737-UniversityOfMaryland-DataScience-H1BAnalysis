setwd("C:\\Users\\kjain307\\Documents\\GitHub\\INST737-UniversityOfMaryland-DataScience-H1BAnalysis\\Milestone2\\encoding")
dataset<-read.csv('clean-new3.csv')
columns<-names(dataset)
#View(dataset)

df <- data.frame(dataset)


smp_size <- floor(0.80 * nrow(df))


set.seed(123)

train_generator <- sample(seq_len(nrow(df)),size=smp_size)

train <- df[train_generator,]
test<- df[-train_generator,]

write.csv(train,"C:\\Users\\kjain307\\Documents\\GitHub\\INST737-UniversityOfMaryland-DataScience-H1BAnalysis\\Milestone2\\encoding\\train.csv")

write.csv(test,"C:\\Users\\kjain307\\Documents\\GitHub\\INST737-UniversityOfMaryland-DataScience-H1BAnalysis\\Milestone2\\encoding\\test.csv")



df$AGENT_PRESENT <- as.factor(df$AGENT_PRESENT)
df$CASE_STATUS <- as.factor(df$CASE_STATUS)
df$WILLFUL_VIOLATOR <- as.factor(df$WILLFUL_VIOLATOR)
df$EMPLOYER_STATE <- as.factor(df$EMPLOYER_STATE)
df$WORKSITE_STATE <- as.factor(df$WORKSITE_STATE)


df1<-subset(df,df$HOURLY_WAGE<100 & df$WAGE_RATE_OF_PAY_FROM_HOUR<100 & df$DURATION < 90)

randomsample=sample_n(df1, 10000)

randomsample_onehot <- one_hot(as.data.table(randomsample))

modelAP = lm(df$DURATION~df$AGENT_PRESENT)

summary(modelAP)


