# run this by: R CMD BATCH code/rawcode/crossValidation5.R

source("./code/rawcode/proj2Lib.R")

# leave one out svm
holdOutSize=5

crossValidation5Start <-date()

crossValidation5 <- lapply(allModels, crossValidate, holdOutSize)
                           
crossValidation5End <-date()

save(crossValidation5Start, crossValidation5End, crossValidation5, file="./data/crossValidate5.rda")
