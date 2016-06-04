#MOW
#Martyna Wi¹cek
#Micha³ Herman
library(lubridate)

# Wczytanie pliku
bikeData<- read.csv("train.csv")

# wyzerowanie niepotrzebnych kolumn
bikeData['casual'] <- NULL
bikeData['registered'] <- NULL

# faktoryzacja danych
bikeData$weather <- factor(bikeData$weather)
bikeData$holiday <- factor(bikeData$holiday)
bikeData$workingday <- factor(bikeData$workingday)
bikeData$season <- factor(bikeData$season)
bikeData$time <- substring(bikeData$datetime,12,20)
bikeData$time <- factor(bikeData$time)
bikeData$time <- substring(bikeData$datetime,12,20)
bikeData$time <- factor(bikeData$time)

bikeData$day <- weekdays(as.Date(bikeData$datetime))
bikeData$day <- as.factor(bikeData$day)
bikeData$weekend <- "0"
bikeData$weekend[bikeData$day == "niedziela"] <- "1"
bikeData$weekend[bikeData$day == "sobota"] <- "1"
bikeData$weekend <- as.factor(bikeData$weekend)
bikeData$hour<- as.numeric(substr(bikeData$time,1,2))
bikeData$hour <- factor(bikeData$hour)
bikeData.copy <- bikeData


# podzia³ na zbiory danych
set.seed(1235)
sam <- sample(3, nrow(bikeData), replace=TRUE, prob=c(0.6, 0.2, 0.2))
sam
trainData <- bikeData[sam==1,]
crossData <- bikeData[sam==2,]
testData  <- bikeData[sam==3,]

trainData2<-trainData
testData2<-testData
crossData2<-crossData

trainData2[,1]<- NULL
testData2[,1]<-NULL

formula <- count ~ season + holiday + workingday + weather + temp + atemp + humidity + windspeed + day + weekend + hour


#Linear Model -- Baseline
lm1 <- lm(formula, data = trainDatax)
summary(lm1)


lm1.kaggle.predict <- predict(lm1, newdata = testDatax)
str(lm1.kaggle.predict)

final.results <- data.frame(lm1.kaggle.predict, testDatax[,1])

final.results.copy <- final.results

final.results[,1] <- final.results.copy[,2]
final.results[,2] <- final.results.copy[,1]


colnames(final.results)[1] <- 'datetime'
colnames(final.results)[2] <- 'count'

write.table(final.results, file = paste0("",'results.csv'), sep = ",", row.names = FALSE, quote = FALSE)


#####tree
library('party')

#build our model
fit.ctree <- ctree(formula, data=trainData2)
#run model against test data set
predict.ctree <- predict(fit.ctree, testData2)

#build a dataframe with our results
submit.ctree <- data.frame(datetime = testData$datetime, count=predict.ctree)

#write results to .csv for submission
write.csv(submit.ctree, file="submit_ctree_v1.csv",row.names=FALSE)


########random forest
library(randomForest)

#predicting the log of registered users.
set.seed(415)
fit1 <- randomForest(formula, data=trainData,importance=TRUE, ntree=250)
importance(fit1, type=1)
plot(importance(fit1, type=1))
pred1=predict(fit1,testData2)



s<-data.frame(testData$datetime,pred1)
write.csv(s,file="submit.csv",row.names=FALSE)
