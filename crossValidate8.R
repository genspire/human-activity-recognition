# run this by: R CMD BATCH code/rawcode/crossValidation8.R

source("./code/rawcode/proj2Lib.R")

# leave one out svm
holdOutSize=8

crossValidation8Start <-date()

crossValidation8 <- lapply(allModels, crossValidate, holdOutSize)

crossValidation8End <-date()

save(crossValidation8Start, crossValidation8End, crossValidation8, file="./data/crossValidate8.rda")