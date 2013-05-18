# run this by: R CMD BATCH code/rawcode/crossValidation6.R

source("./code/rawcode/proj2Lib.R")

# leave one out svm
holdOutSize=6

crossValidation6Start <-date()

crossValidation6 <- lapply(allModels, crossValidate, holdOutSize)

crossValidation6End <-date()

save(crossValidation6Start, crossValidation6End, crossValidation6, file="./data/crossValidate6.rda")