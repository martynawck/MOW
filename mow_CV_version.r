setwd('C:/Users/Martyna/git/MOWMOW') 

#MOW
#Martyna Wi¹cek
#Micha³ Herman
library(lubridate)
library(hydroGOF)
library(DAAG)
library('party')
library(randomForest)
library(plyr)



# Wczytanie pliku
bikeData <- read.csv("train.csv")
# wyzerowanie niepotrzebnych kolumn
bikeData['casual'] <- NULL
bikeData['registered'] <- NULL

# iloœæ podzbiorów
k = 5

# faktoryzacja danych
# pogoda
#bikeData$weather <- factor(bikeData$weather)
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
bikeData$id <- sample(1:k, nrow(bikeData), replace = TRUE)
list <- 1:k

bikeData.copy <- bikeData

# WYBÓR WA¯NOŒCI ATRYBUTÓW

# formu³a
formula <- count ~ season + holiday + workingday + weather + temp + atemp + humidity + windspeed + day + weekend + hour + month + daypart

# Wa¿noœæ atrybutów
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

# formu³a
formulaBIKE <- count ~  workingday + weather +  temp + atemp  +humidity + day+ weekend +  hour +  month + daypart
formulaBIKE <- count ~  weather +  temp + atemp  +humidity + day+ weekend +  hour +  month + daypart
formulaBIKE <- count ~  workingday +  temp + atemp  +humidity + day+ weekend +  hour +  month + daypart
formulaBIKE <- count ~  workingday + weather  + atemp  +humidity + day+ weekend +  hour +  month + daypart
formulaBIKE <- count ~  workingday + weather +  temp   +humidity + day+ weekend +  hour +  month + daypart
formulaBIKE <- count ~  workingday + weather +  temp + atemp   + day+ weekend +  hour +  month + daypart
formulaBIKE <- count ~  workingday + weather +  temp + atemp  +humidity +  weekend +  hour +  month + daypart
formulaBIKE <- count ~  workingday + weather +  temp + atemp  +humidity + day +  hour +  month + daypart
formulaBIKE <- count ~  workingday + weather +  temp + atemp  +humidity + day+ weekend  +  month + daypart
formulaBIKE <- count ~  workingday + weather +  temp + atemp  +humidity + day+ weekend +  hour  + daypart
formulaBIKE <- count ~  workingday + weather +  temp + atemp  +humidity + day+ weekend +  hour +  month 
##### model liniowy

prediction <- data.frame()
testsetCopy <- data.frame()
dateTime <- data.frame()

for (i in 1:k){
  # wybranie zbioru trenuj¹cego oraz zbioru testuj¹cego
  trainingset <- subset(bikeData, id %in% list[-i])
  testset <- subset(bikeData, id %in% c(i))
  
  # model liniowy
  linearModel <- lm(formulaBIKE, data = trainingset)

  # wykonanie predykcji
  temp <- as.data.frame(predict(linearModel, newdata = testset))
  
  # do³¹cz predykcjê do wszystkich do tej pory otrzymanych wyników
  prediction <- rbind(prediction, temp)
  
  # dodaj dane dotycz¹ce w³aœciwej wartoœci
  testsetCopy <- rbind(testsetCopy,  as.data.frame(testset$count))
  #oraz daty
  dateTime <- rbind(dateTime, as.data.frame(testset$datetime))
  
}

# przetworzenie wyników
result <- cbind(dateTime, prediction, testsetCopy)
names(result) <- c("Time","Predicted", "Actual")
result$Difference <- abs(result$Actual - result$Predicted)
# MSE
mse(result$Predicted, result$Actual)

write.table(result, file = paste0("",'resultsNodaypartdataLM.csv'), sep = ",", row.names = FALSE, quote = FALSE)

#### drzewo regresji

formulaBIKE <- count ~  workingday + weather +  temp + atemp  +humidity + day+ weekend +  hour +  month + daypart
formulaBIKE <- count ~  weather +  temp + atemp  +humidity + day+ weekend +  hour +  month + daypart
formulaBIKE <- count ~  workingday +  temp + atemp  +humidity + day+ weekend +  hour +  month + daypart
formulaBIKE <- count ~  workingday + weather  + atemp  +humidity + day+ weekend +  hour +  month + daypart
formulaBIKE <- count ~  workingday + weather +  temp   +humidity + day+ weekend +  hour +  month + daypart
formulaBIKE <- count ~  workingday + weather +  temp + atemp   + day+ weekend +  hour +  month + daypart
formulaBIKE <- count ~  workingday + weather +  temp + atemp  +humidity +  weekend +  hour +  month + daypart
formulaBIKE <- count ~  workingday + weather +  temp + atemp  +humidity + day +  hour +  month + daypart
formulaBIKE <- count ~  workingday + weather +  temp + atemp  +humidity + day+ weekend  +  month + daypart
formulaBIKE <- count ~  workingday + weather +  temp + atemp  +humidity + day+ weekend +  hour  + daypart
formulaBIKE <- count ~  workingday + weather +  temp + atemp  +humidity + day+ weekend +  hour +  month 

prediction <- data.frame()
testsetCopy <- data.frame()
dateTime <- data.frame()

for (i in 1:k){
  # wybranie zbioru trenuj¹cego oraz zbioru testuj¹cego
  trainingset <- subset(bikeData, id %in% list[-i])
  testset <- subset(bikeData, id %in% c(i))
  
  myParam =ctree_control(maxsurrogate = 2, testtype="Bonferroni", mincriterion = 0.55, minsplit=19, maxdepth = 0)
  
  # a new decision tree
  
  # model drzewa regresji
  fitRegressionTree <- ctree(formulaBIKE, data=trainingset, controls =myParam)
  
  # wykonanie predykcji
  temp <- as.data.frame(predict(fitRegressionTree, testset))
  
  # do³¹cz predykcjê do wszystkich do tej pory otrzymanych wyników
  prediction <- rbind(prediction, temp)
  
  # dodaj dane dotycz¹ce w³aœciwej wartoœci
  testsetCopy <- rbind(testsetCopy,  as.data.frame(testset$count))
  #oraz daty
  dateTime <- rbind(dateTime, as.data.frame(testset$datetime))
  
}

# przetworzenie wyników
result <- cbind(dateTime, prediction, testsetCopy)
names(result) <- c("Time","Predicted", "Actual")
result$Difference <- abs(result$Actual - result$Predicted)
# MSE
mse(result$Predicted, result$Actual)

write.table(result, file = paste0("",'resultsNOCROSS05519REG.csv'), sep = ",", row.names = FALSE, quote = FALSE)


#### las losowy


formulaBIKE <- count ~  workingday + weather +  temp + atemp  +humidity + day+ weekend +  hour +  month + daypart
formulaBIKE <- count ~  weather +  temp + atemp  +humidity + day+ weekend +  hour +  month + daypart
formulaBIKE <- count ~  workingday +  temp + atemp  +humidity + day+ weekend +  hour +  month + daypart
formulaBIKE <- count ~  workingday + weather  + atemp  +humidity + day+ weekend +  hour +  month + daypart
formulaBIKE <- count ~  workingday + weather +  temp   +humidity + day+ weekend +  hour +  month + daypart
formulaBIKE <- count ~  workingday + weather +  temp + atemp   + day+ weekend +  hour +  month + daypart
formulaBIKE <- count ~  workingday + weather +  temp + atemp  +humidity +  weekend +  hour +  month + daypart
formulaBIKE <- count ~  workingday + weather +  temp + atemp  +humidity + day +  hour +  month + daypart
formulaBIKE <- count ~  workingday + weather +  temp + atemp  +humidity + day+ weekend  +  month + daypart
formulaBIKE <- count ~  workingday + weather +  temp + atemp  +humidity + day+ weekend +  hour  + daypart
formulaBIKE <- count ~  workingday + weather +  temp + atemp  +humidity + day+ weekend +  hour +  month 

prediction <- data.frame()
testsetCopy <- data.frame()
dateTime <- data.frame()


progress.bar <- create_progress_bar("text")
progress.bar$init(k)
for (i in 1:k){
  # wybranie zbioru trenuj¹cego oraz zbioru testuj¹cego
  trainingset <- subset(bikeData, id %in% list[-i])
  testset <- subset(bikeData, id %in% c(i))
  
  #set.seed(415)
  # las losowy
  randomForestFit <- randomForest(formulaBIKE, data=trainingset, ntree=250, mtry=4, replace=FALSE)
  
  # wykonanie predykcji
  temp <- as.data.frame(predict(randomForestFit, testset))
  
  # do³¹cz predykcjê do wszystkich do tej pory otrzymanych wyników
  prediction <- rbind(prediction, temp)
  
  # dodaj dane dotycz¹ce w³aœciwej wartoœci
  testsetCopy <- rbind(testsetCopy,  as.data.frame(testset$count))
  #oraz daty
  dateTime <- rbind(dateTime, as.data.frame(testset$datetime))
  progress.bar$step()
}

# przetworzenie wyników
result <- cbind(dateTime, prediction, testsetCopy)
names(result) <- c("Time","Predicted", "Actual")
result$Difference <- abs(result$Actual - result$Predicted)
# MSE
mse(result$Predicted, result$Actual)

write.table(result, file = paste0("",'resultsNOC4443RF.csv'), sep = ",", row.names = FALSE, quote = FALSE)


#[1] 4481.959
