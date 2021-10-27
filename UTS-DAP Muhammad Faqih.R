##Load Dataset
soccer <- read.csv('C:/Users/lenovo/Desktop/Semester 3/DAP(RZ)/UTS-DAP Muhammad Faqih/SoccerLeagues.csv', header = TRUE)
NBA <- read.csv('C:/Users/lenovo/Desktop/Semester 3/DAP(RZ)/UTS-DAP Muhammad Faqih/NBA.csv', header = TRUE)
str(soccer)

################################ Soccer ########################################

#Cleaning data
soccer$Sport <- as.factor(soccer$Sport)
soccer$Country <- as.factor(soccer$Country)
soccer$League <- as.factor(soccer$League)
soccer$Year <- as.factor(soccer$Year)
soccer$Team <- as.factor(soccer$Team)

soccer$X <-0
soccer$X <- replace(soccer$HomeRatio, soccer$HomeRatio>0, 1)
soccer$X[soccer$X != 1] <- 0
soccer$X <- as.factor(soccer$X)

soccer <- na.omit(soccer)
##Create training & testing data
str(soccer)
sample <- sample(8365, 4365)

trainset_sc <- soccer[sample, ]
testset_sc  <- soccer[-sample, ]

################################################################################
##SVM
install.packages("e1071")
library(e1071)

#svn train
str(trainset_sc)
model = svm(X~HomeWins, data = trainset_sc, kernel="radial")
summary(model)


#svm test
svm.pred = predict(model, testset_sc[, !names(testset_sc) %in% c("X")])

svm.table=table(svm.pred, testset_sc$X)
svm.table

#ConfusionMatrix
library(caret)
confusionMatrix(svm.table)

### Atau ###
model2 = svm(X~Year+Country, data = trainset_sc, kernel="radial")
summary(model2)


#svm test
svm.pred2 = predict(model, testset_sc[, !names(testset_sc) %in% c("X")])

svm.table=table(svm.pred2, testset_sc$X)
svm.table

#ConfusionMatrix
library(caret)
confusionMatrix(svm.table)
################################ NBA ###########################################

#Cleaning Data
NBA$Year <- as.factor(NBA$Year)
NBA$Team <- as.factor(NBA$Team)
NBA$Result <- 0
NBA$Result <- replace(NBA$TotalPCT, NBA$TotalPCT>0.5, 1)
NBA$Result[NBA$Result != 1] <- 0
NBA$Result <- as.factor(NBA$Result)

NBA <- na.omit(NBA)

View(NBA)
str(NBA)

##Membuat data train dan test
sample2 <- sample(1216, 516)

trainset_nba <- NBA[sample, ]
testset_nba  <- NBA[-sample, ]


##SVM Train
model3 = svm(Result~Year+Team, data = trainset_nba, kernel="radial")
summary(model3)


##svm test
svm.pred3 = predict(model3, testset_nba[, !names(testset_nba) %in% c("Result")])

svm.table=table(svm.pred3, testset_nba$Result)
svm.table

#ConfusionMatrix
library(caret)
confusionMatrix(svm.table)
