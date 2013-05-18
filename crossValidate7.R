# run this by: R CMD BATCH code/rawcode/crossValidation7.R

source("./code/rawcode/proj2Lib.R")

# leave one out svm
holdOutSize=7

crossValidation7Start <-date()

crossValidation7 <- lapply(allModels, crossValidate, holdOutSize)

crossValidation7End <-date()

save(crossValidation7Start, crossValidation7End, crossValidation7, file="./data/crossValidate7.rda")