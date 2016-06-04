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
# pogoda
bikeData$weather <- factor(bikeData$weather)
# czy dzieñ œwi¹teczny
bikeData$holiday <- factor(bikeData$holiday)
# czy dzieñ pracuj¹cy
bikeData$workingday <- factor(bikeData$workingday)
# pora roku
bikeData$season <- factor(bikeData$season)
# czas
bikeData$time <- substring(bikeData$datetime,12,20)
bikeData$time <- factor(bikeData$time)
bikeData$time <- substring(bikeData$datetime,12,20)
bikeData$time <- factor(bikeData$time)
# dzieñ miesi¹ca
bikeData$day <- weekdays(as.Date(bikeData$datetime))
bikeData$day <- as.factor(bikeData$day)
# weekend?
bikeData$weekend <- "0"
bikeData$weekend[bikeData$day == "niedziela"] <- "1"
bikeData$weekend[bikeData$day == "sobota"] <- "1"
bikeData$weekend <- as.factor(bikeData$weekend)
# miesi¹c
bikeData$month <- as.numeric(substring(bikeData$datetime,6,7))
bikeData$month <- factor(bikeData$month)
#godzina
bikeData$hour<- as.numeric(substr(bikeData$time,1,2))
# pora dnia
#wieczor (18:00 - 00:00)
bikeData$daypart <- "4"
# noc (00:00 - 06:00)
bikeData$daypart[(bikeData$hour < 6) & (bikeData$hour >= 0)] <- "1"
# rano (06:00 - 12:00)
bikeData$daypart[(bikeData$hour < 12) & (bikeData$hour >= 6)] <- "2"
# dzieñ (12:00 - 18:00)
bikeData$daypart[(bikeData$hour < 18) & (bikeData$hour >= 12)] <- "3"
bikeData$hour <- factor(bikeData$hour)
bikeData$daypart <- factor(bikeData$daypart)
bikeData.copy <- bikeData

# WYBÓR WA¯NOŒCI ATRYBUTÓW

# formu³a
formula <- count ~ season + holiday + workingday + weather + temp + atemp + humidity + windspeed + day + weekend + hour + month + daypart

# Wa¿noœæ atrybutów
library(randomForest)
set.seed(415)
fitRandomForest <- randomForest(formula, data=bikeData,importance=TRUE, ntree=250)
importance(fitRandomForest, type=1)
varImpPlot(fitRandomForest)

#%IncMSE
#season     20.90659
#holiday    18.42337
#workingday 34.73994
#weather    35.82452
#temp       31.40167
#atemp      31.94248
#humidity   49.28638
#windspeed  24.62173
#day        29.84335
#weekend    23.93734
#hour       56.61567
#month      33.98778
#daypart    25.96417


# podzia³ na zbiory danych
set.seed(1235)
sam <- sample(2, nrow(bikeData), replace=TRUE, prob=c(0.8, 0.2))
sam
trainData <- bikeData[sam==1,]
testData <- bikeData[sam==2,]

# 
#testDataTimestamp <-trainData
#crossDataTimestamp <- crossData
#testDataTimestamp <- testData

# zachowujemy oryginaln¹ datê
#trainData[,1]<- NULL
#trainData[,10]<- NULL
#crossData[,1]<- NULL
#crossData[,10]<- NULL
#testData[,1]<- NULL
#testData[,10]<- NULL

# formu³a
formulaBIKE <- count ~ season + holiday + workingday + weather + temp + atemp + humidity + windspeed + day + weekend + hour + month + daypart

##### model liniowy

# z cross
library(DAAG)
v1<-CVlm(data=trainData, form.lm=formulaBIKE, m=10,
     plotit="Observed")

# model liniowy bez cross
linearModel <- lm(formula, data = trainData)
summary(linearModel)
# predykcja modelu liniowego
linearModel.predict <- predict(linearModel, newdata = testData)
str(linearModel.predict)
final.results.linearModel <- data.frame(datetime = testData$datetime, count = linearModel.predict)

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
