myPrediction <- function(trainFile,testFile){
  trainData <- read.csv(trainFile)
  classe <- trainData$classe
  trainData <- predSelect(trainData,19000)
  trainData$classe <- classe
  inTrain <- createDataPartition(trainData$classe,p=0.7,list = FALSE)
  training <- trainData[inTrain,]
  testing <- trainData[-inTrain,]
  # Applying RandomForest algorithm for Training.
  fit <- randomForest(classe~.,data = training)
  predictions <- predict(fit,newdata = testing)
  # This prints the accuracy of the Model trained 
  # for the test data split from Training dataset.
  message("Statistics on the accuracy of the Trained Model")
  print(confusionMatrix(predictions,testing$classe))
  # This part is for predicting the given TestDataset
  testData <- read.csv(testFile)
  testData <- predSelect(testData,20)
  predictions <- predict(fit,newdata = testData)
  message("Predictions obtained for pml-testing.csv Dataset using Trained Model..")
  print(predictions)
}

# This Function is for filtering out the unecessary Predictors."
predSelect <- function(x,threshold){
  x$X <- NULL
  # Filters Columns with number of NA's above Threshold.
  x <- x[,!colSums(is.na(x))>=threshold]
  # Filters out Columns which are only Numeric.
  nums <- sapply(x,is.numeric)
  x <- x[,nums]
  return(x)
}
