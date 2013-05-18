# Run this like this: R CMD BATCH code/rawcode/finalTest.R

source("./code/rawcode/proj2Lib.R")

########################################### FINAL TEST ########################################### 
finalTestStart <-date()

trainingSubjects = c(1:26)
testingSubjects = c(27:30)

print("Final test of linear model")
linearResultsFinal <- testModel(linearTrain, trainingSubjects, testingSubjects)

print("Final test of linear model with OVO-vote")
linearVoteResultsFinal <- testModel(ovoBinaryAgg(linearTrain, voting), trainingSubjects, testingSubjects)

print("Final test of tree model")
treeResultsFinal <- testModel(treeTrain, trainingSubjects, testingSubjects)

print("Final test of tree model with OVO-vote")
treeVoteResultsFinal <- testModel( ovoBinaryAgg(treeTrain, voting), trainingSubjects, testingSubjects )

print("Final test of SVM model")
svmResultFinal <- testModel(svmTrain, trainingSubjects, testingSubjects)

print("Final test of SVM model with OVO-vote")
svmVoteResultFinal <- testModel(ovoBinaryAgg(svmTrain, voting), trainingSubjects, testingSubjects)

finalTestEnd <-date()

save(finalTestStart, finalTestEnd, linearResultsFinal, linearVoteResultsFinal, treeResultsFinal, treeVoteResultsFinal, svmResultFinal, svmVoteResultFinal, file="./data/finalTest.rda")

print("completed")