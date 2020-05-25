#setting up the data
setwd("/Users/gauravhasija/Desktop/inst737")
dataset<-read.csv('oneHot_withCATEGORY2MaY.csv')
columns<-names(dataset)
#attach(dataset)


#Aligning the data to required data type
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



# min - max normalization
dataset$DURATION <- (dataset$DURATION - min(dataset$DURATION))/(max(dataset$DURATION) - min(dataset$DURATION))

dataset$HOURLY_WAGE <- (dataset$HOURLY_WAGE - min(dataset$HOURLY_WAGE))/(max(dataset$HOURLY_WAGE) - min(dataset$HOURLY_WAGE))

dataset$WAGE_RATE_OF_PAY_FROM_HOUR <- (dataset$WAGE_RATE_OF_PAY_FROM_HOUR - min(dataset$WAGE_RATE_OF_PAY_FROM_HOUR))/(max(dataset$WAGE_RATE_OF_PAY_FROM_HOUR) - min(dataset$WAGE_RATE_OF_PAY_FROM_HOUR))

dataset$OCCUPATION_NUM <- (dataset$OCCUPATION_NUM - min(dataset$OCCUPATION_NUM))/(max(dataset$OCCUPATION_NUM) - min(dataset$OCCUPATION_NUM))

str(dataset)



#creation of test and train data sets
randomsample=sample_n(dataset, 5000)
smp_size <- floor(0.70 * nrow(randomsample))


set.seed(123)

train_generator <- sample(seq_len(nrow(randomsample)),size=smp_size)

train <- randomsample[train_generator,]
test<- randomsample[-train_generator,]


#More alignment of the data

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

nwr <- neuralnet(HOURLY_WAGE~WAGE_RATE_OF_PAY_FROM_HOUR+
                   DURATION+
                   AGENT_PRESENT_1.0+
                   CASE_STATUS_1.0+
                   OCCUPATION_NUM,
                 data=traindf,hidden = 3,act.fct = 'tanh')


#plot(nwr)
outputwr <- compute(nwr, testdf[,-2])
pwr <- outputwr$net.result
cor(pwr,testdf$HOURLY_WAGE)
#plot(pwr, testdf$DURATION, pch = 15, col = c("red", "blue"),xlab = "Predicted Duration",ylab= "Test Set Duration")

nwr <- neuralnet(HOURLY_WAGE~WAGE_RATE_OF_PAY_FROM_HOUR+
                   DURATION+
                   AGENT_PRESENT_1.0+
                   CASE_STATUS_1.0+
                   OCCUPATION_NUM,
                 data=traindf,hidden = 3,act.fct = 'tanh')


#plot(nwr)
outputwr <- compute(nwr, testdf[,-2])
pwr <- outputwr$net.result
cor(pwr,testdf$HOURLY_WAGE)
#plot(pwr, testdf$DURATION, pch = 15, col = c("red", "blue"),xlab = "Predicted Duration",ylab= "Test Set Duration")



nwr <- neuralnet(HOURLY_WAGE~WAGE_RATE_OF_PAY_FROM_HOUR+
                   DURATION+
                   AGENT_PRESENT_1.0+
                   CASE_STATUS_1.0+
                   OCCUPATION_NUM,
                 data=traindf,hidden = c(2,2),act.fct = 'logistic')


plot(nwr)
outputwr <- compute(nwr, testdf[,-2])
pwr <- outputwr$net.result
cor(pwr,testdf$HOURLY_WAGE)
plot(pwr, testdf$DURATION, pch = 15, col = c("red", "blue"),xlab = "Predicted Hourly Wage",ylab= "Test Set Hourly Wage")


nwr <- neuralnet(HOURLY_WAGE~WAGE_RATE_OF_PAY_FROM_HOUR+
                   DURATION+
                   AGENT_PRESENT_1.0+
                   CASE_STATUS_1.0+
                   OCCUPATION_NUM,
                 data=traindf,hidden = 5,act.fct = 'tanh')


plot(nwr)
outputwr <- compute(nwr, testdf[,-2])
pwr <- outputwr$net.result
cor(pwr,testdf$HOURLY_WAGE)
#plot(pwr, testdf$DURATION, pch = 15, col = c("red", "blue"),xlab = "Predicted Duration",ylab= "Test Set Duration")
