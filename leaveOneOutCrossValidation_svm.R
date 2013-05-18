# run this by: R CMD BATCH code/rawcode/leaveOneOutCrossValidation_svm.R

source("./code/rawcode/proj2Lib.R")

# leave one out svm

leaveOneOutSVMStart <-date()

print("Testing SVM model")
svmResult <- runHoldOut1CrossValidation(svmTrain)

#for this one we may have to change kernel 
print("Testing SVM model with OVO-vote")
svmVoteResult <- runHoldOut1CrossValidation(ovoBinaryAgg(svmTrain, voting))

leaveOneOutSVMEnd <-date()

save.image( file="./data/leaveOneOutCrossValidation_svm.rda")

cat("svm leave one out end:", date())