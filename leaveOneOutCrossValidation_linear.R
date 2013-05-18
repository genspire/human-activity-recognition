# run this by: R CMD BATCH code/rawcode/leaveOneOutCrossValidation_linear.R

source("./code/rawcode/proj2Lib.R")

########################################### CROSS-VALIDATION ########################################### 
# Leave-one-(subject)-out cross-validation
leaveOneOutLinearStart <-date()

print("Testing linear model")
linearResults <- runHoldOut1CrossValidation(linearTrain)

print("Testing linear model with OVO-vote")
linearVoteResults <- runHoldOut1CrossValidation(ovoBinaryAgg(linearTrain, voting))

leaveOneOutLinearEnd <-date()

save.image(file="./data/leaveOneOutCrossValidation_linear.rda")


#print("Positive definite fuzzy classifier (PDFC) with OVO-vote") 

# 3NN

# random forest

