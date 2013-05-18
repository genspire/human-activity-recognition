
set.seed(32313)

# CHANGE TO 30 FOR FULL TEST
FOLDS <- 1


# run a hold-one-out validation holding out one subject
# parameter: a function that takes a data frame to train on and returns a predict function which can be passed a frame and returns a vector of predictions
# returns error metrics collected from the validation step
runCrossValidation <- function(modelTrainFunction){
  results <- data.frame(correct=numeric(), total=numeric())
  
  # for loop over 1-30 (once per subject)
  for(i in 1:FOLDS){
    # train model with all of X and Y (remove subject)
    predictor <- modelTrainFunction(allData[allData$subject != i, c(1:561, 563)])
    
    #pass predictor a data frame containing the hold out set and get back a vector of Y-hat
    predictions <- predictor(allData[allData$subject == i, c(1:561)]) 
    testSolutions <- allData$activity[allData$subject == i]
    
    # add row to a
    results <- rbind(results, collectValidationMetrics(testSolutions, predictions))
    print(paste("Completed validation #", i, "out of", FOLDS))

    #print(paste("SOLUTIONS",testSolutions))
    #print(paste("PREDICTIONS",predictions))
    
  }
  return(results)
}

# takes the correct answeres and the predicted answers and returns a data frame containing 
# all validation metrics collected
collectValidationMetrics <- function(testSolutions, predictions){
  solutionCount <- length(testSolutions)
  predictionCount <- length(predictions)
  
  if(predictionCount != solutionCount){
    stop(paste("The number of predictions:", predictionCount, "is not equal to the size of the test set:", solutionCount))
  } 
  
  #calculate classification accuracy
  correct <- predictions == testSolutions
  
  # ?? collect stats on the activities that were misclassified
  
  return(data.frame(correct=length(correct[correct]), total=solutionCount))
}


# what we need from each model: 
#trainFunction - a function that takes a data frame to train with and returns a predictFunction 
#predictFunction - passed a frame and returns a vector of predictions



# Tree model
library(tree)

treeTrain <- function(trainingData){
  treeModel <- tree(as.factor(activity) ~ ., data=trainingData)
  
  function(testData){
    predict(treeModel, testData, type="class")
  }
}

# Linear Model
linearTrain <- function(trainingData){
  linearModel <- lm(activity ~ ., data=trainingData)
  
  function(testData){
    # simple cutoff
    round(predict(linearModel, testData))
  }
}



# Start Tests

print("Testing tree model")
treeResults <- runCrossValidation(treeTrain)

print("Testing linear model")
linearResults <- runCrossValidation(linearTrain)




