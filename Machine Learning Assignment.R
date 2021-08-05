library(readxl)
setwd("/Users/adamsarissky/Desktop/Data")
trainData <- read.csv("pml-training.csv")
classe <- as.data.frame(trainData$classe)

#Plotting the data using two random variables to see how classe is distributed
qplot(trainData$max_roll_dumbbell, trainData$max_picth_arm, col = trainData$classe)


#building prediction model using randomForest
library(caret)
library(randomForest)
#Building a model
library(rpart)
set.seed(12345)
modFit <- train(classe~., data = TrainSet, method = "rf", prox = TRUE)

setwd("/Users/adamsarissky/Desktop/Data")
trainDATA<- read.csv("pml-training.csv")
trainDATA <- subset(trainDATA, select = -X)
inTrain  <- createDataPartition(trainDATA$classe, p=0.9, list=FALSE)
TrainSet <- trainDATA[inTrain, ]
TestSet  <- trainDATA[-inTrain, ]
AllNA    <- sapply(TrainSet, function(x) mean(is.na(x))) > 0.95
TrainSet <- TrainSet[, AllNA==FALSE]
TestSet  <- TestSet[, AllNA==FALSE]

#random forest doesnt work on as large a dataset as this one
#I have to use decision tree instead
#Seemed too simple, made distinctions based on the X variable
modFit <- train(classe~., data = TrainSet, method = "rpart")

modFit$finalModel
library(rattle)
fancyRpartPlot(modFit2$finalModel)
fancyRpartPlot(modFit$finalModel)


#I removed the X variable, created a subset 
set.seed(12345)
modFit2 <- train(classe~., data = TrainSet, method = "rpart")
set.seed(12345)
modFit3 <- rpart(classe~., data = TrainSet, method = "class")
fancyRpartPlot(modFit3)
fancyRpartPlot(modFit)

#Have to remove the X in the test set as well
pred2 <- predict(modFit3, TestSet, type = "class")
#error in model anyway even with modFit3
pred <- predict(modFit2, newdata = TestSet)

NZV <- nearZeroVar(TrainSet)
TrainSet <- TrainSet[, -NZV]
TestSet  <- TestSet[, -NZV]
TrainSet <- TrainSet[, -(1:4)]
TestSet  <- TestSet[, -(1:4)]
predictionFunction(lm, modFit2, newdata = TestSet[-3])

table(pred, TestSet$classe)
TestSet$PredRight <- pred==TestSet$classe
TestSet$PredRight2 <- pred2==TestSet$classe
qplot(roll_belt, pitch_belt, color = PredRight, data = TestSet)

summary(TestSet$PredRight2)
summary(TestSet$PredRight)
qplot(roll_belt, pitch_belt, color = PredRight2, data = TestSet)
library(ggthemes)
ggplot(TestSet, aes(roll_belt, pitch_belt, color = PredRight)) + geom_point (
  )+ theme_economist()

test.data <- read.csv("pml-testing.csv")
pred2 <- predict(modFit2, newdata = test.data)
test.data$classe <- pred2
ggplot(TestSet, aes(roll_belt, pitch_belt, color = classe)) + geom_point (
)+ theme_economist()

