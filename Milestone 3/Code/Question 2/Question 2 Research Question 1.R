#setting up the data
setwd("/Users/gauravhasija/Desktop/inst737")
dataset<-read.csv('oneHot_withCATEGORY2MaY.csv')
columns<-names(dataset)
#attach(dataset)

str(dataset)
#alighning the variables for required data type
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



# min- max normalization


dataset$DURATION <- (dataset$DURATION - min(dataset$DURATION))/(max(dataset$DURATION) - min(dataset$DURATION))

dataset$HOURLY_WAGE <- (dataset$HOURLY_WAGE - min(dataset$HOURLY_WAGE))/(max(dataset$HOURLY_WAGE) - min(dataset$HOURLY_WAGE))

dataset$WAGE_RATE_OF_PAY_FROM_HOUR <- (dataset$WAGE_RATE_OF_PAY_FROM_HOUR - min(dataset$WAGE_RATE_OF_PAY_FROM_HOUR))/(max(dataset$WAGE_RATE_OF_PAY_FROM_HOUR) - min(dataset$WAGE_RATE_OF_PAY_FROM_HOUR))

dataset$OCCUPATION_NUM <- (dataset$OCCUPATION_NUM - min(dataset$OCCUPATION_NUM))/(max(dataset$OCCUPATION_NUM) - min(dataset$OCCUPATION_NUM))


#creation of train and test variables
randomsample=sample_n(dataset, 5000)
smp_size <- floor(0.70 * nrow(randomsample))


set.seed(123)

train_generator <- sample(seq_len(nrow(randomsample)),size=smp_size)

train <- randomsample[train_generator,]
test<- randomsample[-train_generator,]



install.packages("neuralnet")
library(neuralnet)
# conversion to data frame
str(train)

traindf<-as.data.frame(train)
testdf<-as.data.frame(test)

head(traindf)
head(testdf)


# some data alignment
traindf <- traindf[c("AGENT_PRESENT_1.0","HOURLY_WAGE", "DURATION", "WAGE_RATE_OF_PAY_FROM_HOUR","CASE_STATUS_1.0","OCCUPATION_NUM")]
head(traindf)

testdf <- testdf[c("AGENT_PRESENT_1.0","HOURLY_WAGE", "DURATION", "WAGE_RATE_OF_PAY_FROM_HOUR","CASE_STATUS_1.0","OCCUPATION_NUM")]
head(testdf)


# applying the neural network model multiple times by modifying the layers, 
#activation function and number of neuron to find the best modelnap <- neuralnet(AGENT_PRESENT_1.0~HOURLY_WAGE+
                 DURATION+
                 WAGE_RATE_OF_PAY_FROM_HOUR+
                 CASE_STATUS_1.0+
                 OCCUPATION_NUM,
                 data=traindf,hidden = 1)


plot(nap)
outputAP <- compute(nap, testdf[,-1])
pAP <- outputAP$net.result
predAP <- ifelse(pAP>0.5, 1, 0)
tabAP <- table(predAP, testdf$AGENT_PRESENT_1.0)
tabAP
1-sum(diag(tabAP))/sum(tabAP)

fourfoldplot(tabAP, color = c("#CC6666", "#99CC99"),
             conf.level = 0, margin = 1, main = "Confusion Matrix")

nap2 <- neuralnet(AGENT_PRESENT_1.0~HOURLY_WAGE+
                   DURATION+
                   WAGE_RATE_OF_PAY_FROM_HOUR+
                   CASE_STATUS_1.0+
                   OCCUPATION_NUM,
                 data=traindf,hidden = 2)


plot(nap2)
outputAP2 <- compute(nap2, testdf[,-1])
pAP2 <- outputAP2$net.result
predAP2 <- ifelse(pAP2>0.5, 1, 0)
tabAP2 <- table(predAP2, testdf$AGENT_PRESENT_1.0)
tabAP2
1-sum(diag(tabAP2))/sum(tabAP)

fourfoldplot(tabAP2, color = c("#CC6666", "#99CC99"),
             conf.level = 0, margin = 1, main = "Confusion Matrix")

nap3 <- neuralnet(AGENT_PRESENT_1.0~HOURLY_WAGE+
                    DURATION+
                    WAGE_RATE_OF_PAY_FROM_HOUR+
                    CASE_STATUS_1.0+
                    OCCUPATION_NUM,
                  data=traindf,hidden = c(2,1))


plot(nap3)
outputAP3 <- compute(nap3, testdf[,-1])
pAP3 <- outputAP3$net.result
predAP3 <- ifelse(pAP3>0.5, 1, 0)
tabAP3 <- table(predAP3, testdf$AGENT_PRESENT_1.0)
tabAP3
1-sum(diag(tabAP3))/sum(tabAP3)

fourfoldplot(tabAP3, color = c("#CC6666", "#99CC99"),
             conf.level = 0, margin = 1, main = "Confusion Matrix")


nap4 <- neuralnet(AGENT_PRESENT_1.0~HOURLY_WAGE+
                    DURATION+
                    WAGE_RATE_OF_PAY_FROM_HOUR+
                    CASE_STATUS_1.0+
                    OCCUPATION_NUM,
                  data=traindf,hidden = c(2,1),act.fct="tanh" )


plot(nap4)
outputAP4 <- compute(nap4, testdf[,-1])
pAP4 <- outputAP4$net.result
predAP4 <- ifelse(pAP4>0.5, 1, 0)
tabAP4 <- table(predAP4, testdf$AGENT_PRESENT_1.0)
tabAP4
1-sum(diag(tabAP4))/sum(tabAP4)

fourfoldplot(tabAP3, color = c("#CC6666", "#99CC99"),
             conf.level = 0, margin = 1, main = "Confusion Matrix")

nap5 <- neuralnet(AGENT_PRESENT_1.0~HOURLY_WAGE+
                    DURATION+
                    WAGE_RATE_OF_PAY_FROM_HOUR+
                    CASE_STATUS_1.0+
                    OCCUPATION_NUM,
                  data=traindf,hidden = 5,act.fct = 'tanh')


plot(nap5)
outputAP5 <- compute(nap5, testdf[,-1])
pAP5 <- outputAP5$net.result
predAP5 <- ifelse(pAP5>0.5, 1, 0)
tabAP5 <- table(predAP5, testdf$AGENT_PRESENT_1.0)
tabAP5
1-sum(diag(tabAP5))/sum(tabAP5)


fourfoldplot(tabAP5, color = c("#CC6666", "#99CC99"),
             conf.level = 0, margin = 1, main = "Confusion Matrix")








