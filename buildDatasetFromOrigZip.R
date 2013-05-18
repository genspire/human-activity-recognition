# This script will initialize Samsung Activity data used in testModels.R 
# http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

# Helper functions

fileCon <- function(path){  unz("./data/rawData.zip", paste("UCI HAR Dataset", path, sep="/")) }
extractTable <- function(file, col.names){ 
  con <- fileCon(file)
  table <- read.table(con, col.names=col.names)
  #close(con)
}

# Useful functions for inspecting the very large number of column names
#library(gdata) #has the startsWith function
#colContain <- function(pattern){names(allData)[grep(pattern, names(allData))] }
#colPrefix <- function(prefix){names(allData)[startsWith(names(allData), prefix,,T)] }

# Get and process data

if(! file.exists("./data", "./data/rawData.zip") ){
  warning("Downloading data (if you have already done this, check your working directory)")
  dir.create(file.path(./data), showWarnings = FALSE)
  download.file("http://archive.ics.uci.edu/ml/machine-learning-databases/00240/UCI%20HAR%20Dataset.zip", destfile="./data/rawData.zip", method="curl")
}

activityLabels <- read.table( fileCon("activity_labels.txt"), col.names=c("code", "label"))

featureLabels <- extractTable("features.txt")[,2]
# clean labels
featureLabels <- sub("()", "", featureLabels, fixed=T)

# get test data
testData <- extractTable("test/X_test.txt", col.names=featureLabels)
testData$subject <- extractTable("test/subject_test.txt")[,1]
testData$activity <- extractTable("test/y_test.txt")[,1]

# get training data
trainingData <- extractTable("train/X_train.txt", col.names=featureLabels)
trainingData$subject <- extractTable("train/subject_train.txt")[,1]
trainingData$activity <- extractTable("train/y_train.txt")[,1]

# append
allData <- rbind(testData, trainingData)

# write it out to file
save(allData, activityLabels, file="./data/processedData.rda")

# create separate frames for each subject
#subjectList <- list()
#for(i in sort(unique(allData$subject))){
#  subjectList[[i]]  = allData[allData$subject == i,]
#}



