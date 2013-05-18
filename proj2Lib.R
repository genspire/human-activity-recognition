
set.seed(32313)

#load data
load(file="./data/processedData.rda")

# CHANGE TO 30 FOR FULL TEST
FOLDS <- 30

# returns col name of the correct solution frequency column for the given class 
solClassFreqName <- function(classIndex){ paste("solClassFreq", classIndex, sep="_"); }

# returns col name containing the frequency the given class was predicted 
predClassFreqName <- function(classIndex){ paste("predlClassFreq", classIndex, sep="_"); }

# returns the col name containing the number of correct predictons for the given subject
correctSubjectName <- function(subjectIndex){ paste("corrSub", subjectIndex, sep="_"); }

# returns the col name containing the frequency the given subject was found in the data
subjectFreqName <- function(subjectIndex){ paste("subFreq", subjectIndex, sep="_"); }

# creates a matrix with the given number of rows and all the columns needed by the collectMetrics function
createResultsMatrix <- function(rowCount){
  solFreqNames <- sapply(c(1:6), solClassFreqName)
  predFreqNames <- sapply(c(1:6), predClassFreqName)
  corrSubNames <- sapply(c(1:30), correctSubjectName)
  subjectFreqName <- sapply(c(1:30), subjectFreqName)
  
  results <- matrix(nrow=rowCount, ncol=74)
  colnames(results) <- c("total", "correct", solFreqNames, predFreqNames, corrSubNames, subjectFreqName)
  return(results)
}

# run a hold-one-out validation holding out one subject
# parameter: a function that takes a data frame to train on and returns a predict function which can be passed a frame and returns a vector of predictions
# returns error metrics collected from the validation step
runHoldOut1CrossValidation <- function(modelTrainFunction){
  
  results <- createResultsMatrix(FOLDS)
  
  # for loop over 1-30 (once per subject)
  for(i in 1:FOLDS){
    # train model with all of X and Y (remove subject)
    cat("Starting fold", i, "\nSubjects passed to training function:\n", sort(unique(allData$subject[allData$subject != i])), "\n")
    
    classifier <- modelTrainFunction(allData[allData$subject != i, c(1:561, 563)])
    
    #pass classifier a data frame containing the hold out set and get back a vector of Y-hat
    predictions <- classifier(allData[allData$subject == i, c(1:561)]) 
    testSolutions <- allData$activity[allData$subject == i]
    subjects <- allData$subject[allData$subject == i]
    
    results <- collectMetrics(results, i, testSolutions, predictions, subjects)
    cat("\nCompleted validation fold #", i, "out of", FOLDS, "\n")

    #print(paste("SOLUTIONS",testSolutions))
    #print(paste("PREDICTIONS",predictions))
    
  }
  return(results)
}


# Keeps manditory subjects in the correct set (i.e. training or test)
# Runs all possible cross-validation iterations given the training set size
# Valid number of subjects to keep in hold out set must be between 4-26. 
# Note: The number of iterations is equal to choose(22, holdOutSubjectCount) for a hold out of 11 it is 705432
crossValidate <- function(modelTrainFunction, holdOutSubjectCount){
  manditoryTrain <- c(1,3,5,6)
  manditoryTest <- c(27,28,29,30)
  manditoryCount <- length(manditoryTest)
  available <- c(2,4,7,8:26)
  
  if(holdOutSubjectCount < 4 || holdOutSubjectCount > 26)
    stop("holdOutSubjectCount is invalid")
  
  iterationCount <- choose(length(available), holdOutSubjectCount-manditoryCount)
  results <- createResultsMatrix(iterationCount)
  
  rowIndex <- 1
  for(holdOut in combn(available, holdOutSubjectCount-manditoryCount, simplify=F)){
    addedTraining <- setdiff(available, holdOut)    
    results <- testModel(modelTrainFunction, c(manditoryTrain, addedTraining), c(manditoryTest, holdOut), results, rowIndex)
    rowIndex <- rowIndex +1
  }
  
  return(results)
}

# This function runs a single training/testing interation, using the training and test set passed in.
# The training and testing are vectors of subject IDs to include for that part of the evaluation.
# The modelTrainFunction is the same model function descirbed in the runHoldOut1CrossValidation function 
#  
testModel <- function(modelTrainFunction, trainingSubjects, testingSubjects, results=createResultsMatrix(1), resultsIndex=1){
  cat("Training model with subjects:\n", trainingSubjects, "\nTesting model with subjects:\n", testingSubjects, "\n")
  
  crossover <- intersect(trainingSubjects, testingSubjects)
  if( length(crossover) > 0 )
    warning("The following subjects are being used for training and test: ", crossover)
  
  if( max(trainingSubjects) > 30 || min(trainingSubjects) < 1)
    stop(paste("Training subjects invalid", trainingSubjects))
  
  if( max(testingSubjects) > 30 || min(testingSubjects) < 1)
    stop(paste("Test subjects invalid", testingSubjects))
         
  classifier <- modelTrainFunction(allData[allData$subject %in% trainingSubjects, c(1:561, 563)])
  
  #pass classifier a data frame containing the test set and get back a vector of Y-hat
  predictions <- classifier(allData[allData$subject %in% testingSubjects, c(1:561)]) 
  testSolutions <- allData$activity[allData$subject %in% testingSubjects]
  subjects <- allData$subject[allData$subject %in% testingSubjects]
  
  # evaluatate the predictions
  collectMetrics(results, resultsIndex, testSolutions, predictions, subjects)
}


# collects the frequencies of values found in the vector x and adds them to the correct
# column in the results matrix using the given colNameFun value-to-column-name mapping function.
setResultFreqs <- function(results, row, x, colNameFun){
  freqTable <- as.data.frame(table(x))
  #print("freq table:")
  #print(freqTable)
  validNames <-colnames(results)
  for(i in 1:length(freqTable$x)) {
    value <- as.character(freqTable$x[i])
    #cat("value:", value, "\nfeq", as.numeric(freqTable$Freq[i]), "\n")
    columnName <- colNameFun(value)
    #cat("colname:", columnName, "\nclass:", class(columnName))
    
    if(columnName %in% validNames){
      #cat("assiging to:", columnName, "\n")
      results[row, columnName] <- as.numeric(freqTable$Freq[i])
    }
    #else{ cat("skipped:",columnName,"\n")}
  }
  #cat("results", results, "\n")
  return(results)
}

# takes the correct answeres and the predicted answers and returns a data frame containing 
# all validation metrics collected
# Parameters:
# results - createResultsMatrix returned matrix
# resultsIndex - is the row to be filled in
# testSolutions - vector of correct answers (Y)
# predictions - vector of predicted answers (Y-hat)
# subjects - vector of subjects corresponding to solutions/predictions vectors   
collectMetrics <- function(results, resultsIndex, testSolutions, predictions, subjects){
  results[resultsIndex,] = 0
  
  solutionCount <- length(testSolutions)
  predictionCount <- length(predictions)
  
  if(predictionCount != solutionCount){
    stop(paste("The number of predictions:", predictionCount, "is not equal to the size of the test set:", solutionCount))
  } 
  
  # collect class frequencies of correct answers
  results <- setResultFreqs(results, resultsIndex, testSolutions, solClassFreqName)
  
  # collect class frequencies of predicted answers
  results <- setResultFreqs(results, resultsIndex, predictions, predClassFreqName)
  
  # collect subject frequencies in test set
  results <- setResultFreqs(results, resultsIndex, subjects, subjectFreqName)
  
  #calculate classification accuracy
  correct <- as.integer(predictions) == as.integer(testSolutions)
  
  # collect the frequencies each subject was correctly predicted
  results <- setResultFreqs(results, resultsIndex, subjects[correct], correctSubjectName)
  
  results[resultsIndex, "correct"] <- sum(correct)
  results[resultsIndex, "total"] <- solutionCount
  return(results)
}

########################################### MODELS ########################################### 

# What we need from each model to be cross validated: 
# trainFunction(trainingData) - a function that takes a data frame to train with and returns a predictFunction 
# predictFunction(testData) - passed a frame and returns a vector of predictions (i.e. integers corresponding to the activity)


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

# SVM
library(e1071)
svmTrain <- function(trainingData){
  subjectCount = length(unique(trainingData$subject))
  
  
  svmModel <- svm(as.factor(activity) ~., data=trainingData)#, kernel="sigmoid")
  
  function(testData){
    # simple cutoff
    as.numeric(predict(svmModel, testData))
  }
}


########################################### ENSEMBLE MODELS ########################################### 

# An easy way to undertake a multi-class problem is to use binarization techniques, 
# where the original problem is decomposed in several easier binary problems.
# This function employs a base learner (i.e. model as defined above) and orchestrates
# a One-vs-One decomposition strategy.
#
# This function returns a function of type trainFunction (described above).
# modelTrainFun(trainingData) - the trainFunction of the base learner to use.  Its job is to take a data frame
#   as training data and return a trained (binary) classifier that can predict between two classes.
# aggregationFun(instance, models) - the aggregation technique to use during prediction phase.
#   It is passed the instance to classify and a sequency of predictFunction functions 
#   (each being a binary classifier). Its job is to aggregate the predictions of each model and 
#   return a single prediction of the instance's class 
ovoBinaryAgg <- function(modelTrainFun, aggregationFun){
  function(trainingData){
    classes <- unique(trainingData$activity)
    models <- combn(classes, 2, function(classPair){
      
      #cat("training model for activity pair:", classPair, "\n")#, "\nNames\n") 
      
      modelTrainFun(trainingData[trainingData$activity %in% classPair,])
    }, simplify = FALSE) 
    
    cat("Trained", length(models), "binary models\n")
    
    function(testData){
      cat("Classifying", length(testData[,1]), "instances\n")
      # we need to pass the aggregationFun a data frame and this was losing the col names
      results <- c()
      for(i in 1:length(testData[,1])){
        results <- append(results, aggregationFun(testData[i,], models))
        #cat(i,", ", sep="")
      }
      return(results)
    }
  }
}

# Aggregation techniques (defined as aggregationFun above)

# Simple majority vote
voting <- function(instance, models){
  #cat("type of row:", class(instance), "\ninstance:", instance)
  #instance <- as.data.frame(row)
  #cat("using voting aggregation method with", length(models), "models\n")
  #votes <- c(rep.int(0, 6))
  
  predictions <- sapply(models, function(classify){ 
    #cat("instance type:", class(instance), "\ninstance names:", names(instance), "\n")
    classify(instance)
    #print("class is:")
    #print(c)
    #votes[c] <- votes[c] + 1
    #cat("added a vote to class: ", c, "\nnow it has",  votes[c], "votes\n")
  })
  
  #prd<-cat(predictions)
#  cat("\n\npredictions are:", predictions, "\n")
#  predictions <- as.numeric(predictions)
#  cat("after num predictions are:", predictions, "\n")
 # print(sapply(predictions, class))
  # choose the winner and break ties
 # cat("\nprediction freqs:", table(predictions), "\n")
#  cat("max freq is:", max(table(predictions)), "\n")
#  cat("To selecte from:", names(which(table(predictions)==max(table(predictions)))), "\n\n")
#  cat("\nclass of to-select-from:", class(which(predictions==max(table(predictions)),arr.ind=TRUE )),  "\n")
#  cat("\ndim of to-select-from:", dim(which(predictions==max(table(predictions)), arr.ind=TRUE)),  "\n")
  
  # randomly pick from the classifications that were the most frequent
  choices = as.numeric(names(which(table(predictions)==max(table(predictions)))))
  if(length(choices) == 1)
    winner = choices[1]
  else
    winner = sample(choices, 1)
  
  
  #winner = sample(which(predictions==max(table(predictions)), arr.ind=TRUE), 1)
  
#  cat("winning classification:", winner,"\n") 
      #"\nindexes with max votes:", seq(along=votes)[votes == max(votes)],"\n")
  #winner = sample(seq(along=votes)[votes == max(votes)], 1)
  #cat(winner, "was activitiy predicted, with", votes[winner], "votes\n")
  return(winner)
}

#ovoBinaryAgg(svmTrain, voting), ovoBinaryAgg(treeTrain, voting), ovoBinaryAgg(linearTrain, voting)
allModels <- c(linearTrain, treeTrain, svmTrain)

