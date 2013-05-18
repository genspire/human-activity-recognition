# run this by: R CMD BATCH code/rawcode/leaveOneOutCrossValidation_tree.R

source("./code/rawcode/proj2Lib.R")

# Leave-one-(subject)-out cross-validation
leaveOneOutTreeStart <-date()

print("Testing tree model")
treeResults <- runHoldOut1CrossValidation(treeTrain)

print("Testing tree model with OVO-vote")
treeVoteResults <- runHoldOut1CrossValidation( ovoBinaryAgg(treeTrain, voting) )

leaveOneOutTreeEnd <-date()

save.image(file="./data/leaveOneOutCrossValidation_tree.rda")
