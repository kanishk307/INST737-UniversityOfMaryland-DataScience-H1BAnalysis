#setting up the data
setwd("E:/INST737/Milestone3/Gaurav")
dataset<-read.csv('oneHot_withCATEGORY30APril.csv')
columns<-names(dataset)
#attach(dataset)

str(dataset)

dataset$CASE_STATUS_1.0 <- as.factor(dataset$CASE_STATUS_1.0)
dataset$AGENT_PRESENT_1.0 <- as.factor(dataset$AGENT_PRESENT_1.0)
dataset$OCCUPATION <- as.factor(dataset$OCCUPATION)
dataset$OCCUPATION_NUM <- as.numeric(dataset$OCCUPATION)
dataset$X.1 <- NULL
dataset$X <- NULL
dataset$WILLFUL_VIOLATOR <- NULL
dataset$AGENT_PRESENT_0.0 <- NULL
dataset$CASE_STATUS_0.0 <- NULL
dataset$WAGE_RATE_OF_PAY_FROM_HOUR_RANGE <- NULL
dataset$DURATION_RANGE <- NULL
dataset$HOURLY_WAGE_RANGE <- NULL
dataset$OCCUPATION <- NULL



str(dataset)


dataset$DURATION <- (dataset$DURATION - min(dataset$DURATION))/(max(dataset$DURATION) - min(dataset$DURATION))

dataset$HOURLY_WAGE <- (dataset$HOURLY_WAGE - min(dataset$HOURLY_WAGE))/(max(dataset$HOURLY_WAGE) - min(dataset$HOURLY_WAGE))

dataset$WAGE_RATE_OF_PAY_FROM_HOUR <- (dataset$WAGE_RATE_OF_PAY_FROM_HOUR - min(dataset$WAGE_RATE_OF_PAY_FROM_HOUR))/(max(dataset$WAGE_RATE_OF_PAY_FROM_HOUR) - min(dataset$WAGE_RATE_OF_PAY_FROM_HOUR))

dataset$OCCUPATION_NUM <- (dataset$OCCUPATION_NUM - min(dataset$OCCUPATION_NUM))/(max(dataset$OCCUPATION_NUM) - min(dataset$OCCUPATION_NUM))

str(dataset)

# #df<-as.data.frame(dataset)
# 
# 
# str(df)
# str(df)
# 
# #dataset$OCCUPATION_NUM<-as.factor(dataset$OCCUPATION_NUM)
# str(df)
# #install.packages("dplyr")
 library(dplyr)


randomsample=sample_n(dataset, 100000)
smp_size <- floor(0.70 * nrow(randomsample))


set.seed(123)

train_generator <- sample(seq_len(nrow(randomsample)),size=smp_size)

train <- randomsample[train_generator,]
test<- randomsample[-train_generator,]


# 
#install.packages("neuralnet")
library(neuralnet)

train_data <- train[c("DURATION","HOURLY_WAGE", "WAGE_RATE_OF_PAY_FROM_HOUR", "OCCUPATION_NUM","CASE_STATUS_1.0","AGENT_PRESENT_1.0")]
str(train_data)

# attach(train_data)

n <- neuralnet(`AGENT_PRESENT_1.0` ~ HOURLY_WAGE+
                 DURATION+
                 WAGE_RATE_OF_PAY_FROM_HOUR+
                 `CASE_STATUS_1.0`+
                 OCCUPATION_NUM,
               data=train_data, hidden = 1)

plot(n)



