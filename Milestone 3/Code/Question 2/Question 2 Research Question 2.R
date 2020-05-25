#setting up the data
setwd("/Users/gauravhasija/Desktop/inst737")
dataset<-read.csv('500_undersampling.csv')
columns<-names(dataset)
#attach(dataset)

# aligning the data to required data types
#dataset$CASE_STATUS_1.0 <- as.factor(dataset$CASE_STATUS_1.0)
#dataset$AGENT_PRESENT_1.0 <- as.factor(dataset$AGENT_PRESENT_1.0)
dataset$OCCUPATION <- as.factor(dataset$OCCUPATION)
dataset$OCCUPATION_NUM <- as.integer(dataset$OCCUPATION)
dataset$X.1 <- NULL
dataset$X <- NULL
dataset$WILLFUL_VIOLATOR <- NULL
dataset$AGENT_PRESENT_0.0 <- NULL
dataset$CASE_STATUS_0.0 <- NULL
dataset$WAGE_RATE_OF_PAY_FROM_HOUR_RANGE <- NULL
dataset$DURATION_RANGE <- NULL
dataset$HOURLY_WAGE_RANGE <- NULL
dataset$OCCUPATION <- NULL



#min- max normalization
dataset$DURATION <- (dataset$DURATION - min(dataset$DURATION))/(max(dataset$DURATION) - min(dataset$DURATION))

dataset$HOURLY_WAGE <- (dataset$HOURLY_WAGE - min(dataset$HOURLY_WAGE))/(max(dataset$HOURLY_WAGE) - min(dataset$HOURLY_WAGE))

dataset$WAGE_RATE_OF_PAY_FROM_HOUR <- (dataset$WAGE_RATE_OF_PAY_FROM_HOUR - min(dataset$WAGE_RATE_OF_PAY_FROM_HOUR))/(max(dataset$WAGE_RATE_OF_PAY_FROM_HOUR) - min(dataset$WAGE_RATE_OF_PAY_FROM_HOUR))

dataset$OCCUPATION_NUM <- (dataset$OCCUPATION_NUM - min(dataset$OCCUPATION_NUM))/(max(dataset$OCCUPATION_NUM) - min(dataset$OCCUPATION_NUM))



# creation of test and train data sets

randomsample=sample_n(dataset, 5000)
smp_size <- floor(0.70 * nrow(randomsample))


set.seed(123)

train_generator <- sample(seq_len(nrow(randomsample)),size=smp_size)

train <- randomsample[train_generator,]
test<- randomsample[-train_generator,]



#install.packages("neuralnet")
#library(neuralnet)
# some data alignment
str(train)

traindf<-as.data.frame(train)
testdf<-as.data.frame(test)

head(traindf)
head(testdf)



traindf <- traindf[c("AGENT_PRESENT_1.0","HOURLY_WAGE", "DURATION", "WAGE_RATE_OF_PAY_FROM_HOUR","CASE_STATUS_1.0","OCCUPATION_NUM")]
head(traindf)

testdf <- testdf[c("AGENT_PRESENT_1.0","HOURLY_WAGE", "DURATION", "WAGE_RATE_OF_PAY_FROM_HOUR","CASE_STATUS_1.0","OCCUPATION_NUM")]
head(testdf)



# applying the neural network model multiple times by modifying the layers, 
#activation function and number of neuron to find the best model
ncs <- neuralnet(CASE_STATUS_1.0~HOURLY_WAGE+
                   DURATION+
                   WAGE_RATE_OF_PAY_FROM_HOUR+
                   AGENT_PRESENT_1.0+
                   OCCUPATION_NUM,
                 data=traindf,hidden =1,act.fct = "tanh")


plot(ncs)

outputCS <- compute(ncs, testdf[,-5])
pCS <- outputCS$net.result
predCS <- ifelse(pCS>0.5, 1, 0)
tabCS <- table(predCS, testdf$CASE_STATUS_1.0)
tabCS
1-sum(diag(tabCS))/sum(tabCS)
fourfoldplot(tabCS, color = c("#CC6666", "#99CC99"),
             conf.level = 0, margin = 1, main = "Confusion Matrix")



ncs <- neuralnet(CASE_STATUS_1.0~HOURLY_WAGE+
                   DURATION+
                   WAGE_RATE_OF_PAY_FROM_HOUR+
                   AGENT_PRESENT_1.0+
                   OCCUPATION_NUM,
                 data=traindf,hidden =2,act.fct = "tanh")


plot(ncs)

outputCS <- compute(ncs, testdf[,-5])
pCS <- outputCS$net.result
predCS <- ifelse(pCS>0.5, 1, 0)
tabCS <- table(predCS, testdf$CASE_STATUS_1.0)
tabCS
1-sum(diag(tabCS))/sum(tabCS)
fourfoldplot(tabCS, color = c("#CC6666", "#99CC99"),
             conf.level = 0, margin = 1, main = "Confusion Matrix")

ncs <- neuralnet(CASE_STATUS_1.0~HOURLY_WAGE+
                   DURATION+
                   WAGE_RATE_OF_PAY_FROM_HOUR+
                   AGENT_PRESENT_1.0+
                   OCCUPATION_NUM,
                 data=traindf,hidden =c(2,1),act.fct = "tanh")


plot(ncs)

outputCS <- compute(ncs, testdf[,-5])
pCS <- outputCS$net.result
predCS <- ifelse(pCS>0.5, 1, 0)
tabCS <- table(predCS, testdf$CASE_STATUS_1.0)
tabCS
1-sum(diag(tabCS))/sum(tabCS)
fourfoldplot(tabCS, color = c("#CC6666", "#99CC99"),
             conf.level = 0, margin = 1, main = "Confusion Matrix")

ncs <- neuralnet(CASE_STATUS_1.0~HOURLY_WAGE+
                   DURATION+
                   WAGE_RATE_OF_PAY_FROM_HOUR+
                   AGENT_PRESENT_1.0+
                   OCCUPATION_NUM,
                 data=traindf,hidden =4,act.fct = "tanh")


plot(ncs)

outputCS <- compute(ncs, testdf[,-5])
pCS <- outputCS$net.result
predCS <- ifelse(pCS>0.5, 1, 0)
tabCS <- table(predCS, testdf$CASE_STATUS_1.0)
tabCS
1-sum(diag(tabCS))/sum(tabCS)
fourfoldplot(tabCS, color = c("#CC6666", "#99CC99"),
             conf.level = 0, margin = 1, main = "Confusion Matrix")



