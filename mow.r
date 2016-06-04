#MOW
#Martyna Wi¹cek
#Micha³ Herman
library(lubridate)

# Wczytanie pliku
bikeData <- read.csv("train.csv")

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

testDataTimestamp <-trainData
crossDataTimestamp <- crossData
testDataTimestamp <- testData

# zachowujemy oryginaln¹ datê
trainData[,1]<- NULL
trainData[,10]<- NULL
crossData[,1]<- NULL
crossData[,10]<- NULL
testData[,1]<- NULL
testData[,10]<- NULL

# formu³a
formula <- count ~ season + holiday + workingday + weather + temp + atemp + humidity + windspeed + day + weekend + hour

# Wa¿noœæ atrybutów
library(randomForest)
set.seed(415)
fitRandomForest <- randomForest(formula, data=trainData,importance=TRUE, ntree=250)
importance(fitRandomForest, type=1)
varImpPlot(fitRandomForest)


##### model liniowy
linearModel <- lm(formula, data = trainData)
summary(linearModel)
# predykcja modelu liniowego
linearModel.predict <- predict(linearModel, newdata = testData)
str(linearModel.predict)
final.results.linearModel <- data.frame(datetime = testDataTimestamp$datetime, count = linearModel.predict)

#### drzewo regresji
library('party')
fitRegressionTree <- ctree(formula, data=trainData)
regressionTree.predict <- predict(fitRegressionTree, testData)
final.result.regressionTree <- data.frame(datetime = testDataTimestamp$datetime, count=regressionTree.predict)

#### las losowy
library(randomForest)
set.seed(415)
randomForestFit <- randomForest(formula, data=trainData, importance=TRUE, ntree=250)
randomForest.predict = predict(randomForestFit, testData)
final.result.randomForest <- data.frame(datetime = testDataTimestamp$datetime, count = randomForest.predict)
