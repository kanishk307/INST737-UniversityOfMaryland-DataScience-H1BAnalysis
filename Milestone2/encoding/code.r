setwd("C:\\Users\\kjain307\\Documents\\GitHub\\INST737-UniversityOfMaryland-DataScience-H1BAnalysis\\Milestone2\\encoding")
dataset<-read.csv('2803clean-new1.csv')
columns<-names(dataset)
View(dataset)

df <- data.frame(dataset)

randomsample=sample_n(df, 10000)


View(randomsample)

model2 = lm(randomsample$DURATION~randomsample$YEARLY_WAGE)

plot(model2)

#corWageStat <-cor.test(,dataset$YEARLY_WAGE,dataset$CASE_STATUS,  method = "spearman")
#dataset$CASE_STATUS <- as.factor(dataset$CASE_STATUS)
#dataset$YEARLY_WAGE <- as.numeric(dataset$YEARLY_WAGE)
#corWageStat <-cor.test(,dataset$YEARLY_WAGE,dataset$CASE_STATUS,  method = "spearman")
#model1 <- lm(dataset$DURATION~dataset$YEARLY_WAGE)
#model2 <- lm(dataset$DURATION~dataset$CASE_STATUS)
#summary(model1)
#summary(model2)
