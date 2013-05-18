source("./code/rawcode/proj2Lib.R")

load(file="./data/finalTest.rda")
load(file="./data/crossValidate5.rda")
load(file="./data/crossValidate6.rda")

#algoTypes =  factor(c("Linear", "Tree", "SVM"))

calculateAccuracy <- function(crossValidationResults){
  linear<- crossValidationResults[[1]]
  linearAccuracy <- linear[,"correct"]/linear[,"total"] * 100
  linearResults <- data.frame(cbind(algorithm="Linear", accuracy=linearAccuracy))
  
  tree<- crossValidationResults[[2]]
  treeAccuracy <- tree[,"correct"]/tree[,"total"] * 100
  treeResults <- data.frame(cbind(algorithm="Tree", accuracy=treeAccuracy))
  
  svm<- crossValidationResults[[3]]
  svmAccuracy <- svm[,"correct"]/svm[,"total"] * 100
  svmResults <- data.frame(cbind(algorithm="SVM", accuracy=svmAccuracy))

  rbind(linearResults, treeResults, svmResults)  
}

rowKappa <- function(row){
  n <- row["total"]
  h <- row["correct"]
  c <- 0
  for(i in 1:6){
    c <- row[solClassFreqName(i)] * row[predClassFreqName(i)] + c
  }  
  (n * h - c) / (n*n - c)
}

calculateKappa <- function(crossValidationResults){
  
  linear<- crossValidationResults[[1]]
  linearKappa <- apply(as.data.frame(linear), 1, rowKappa)
  linearResults <- data.frame(cbind(algorithm="Linear", kappa=linearKappa))
  
  tree<- crossValidationResults[[2]]
  treeKappa <- apply(as.data.frame(tree), 1, rowKappa)
  treeResults <- data.frame(cbind(algorithm="Tree", kappa=treeKappa))
  
  svm<- crossValidationResults[[3]]
  svmKappa <- apply(as.data.frame(svm), 1, rowKappa)
  svmResults <- data.frame(cbind(algorithm="SVM", kappa=svmKappa))
  
  rbind(linearResults, treeResults, svmResults)  
  
}



par(mfrow=c(2,3), mar=c(7, 5, 3, 1), cex.axis=1.4, mgp = c(3, .89, 0))

linAcc <- linearResultsFinal[2]/linearResultsFinal[1] *100
linVAcc<- linearVoteResultsFinal[2]/linearVoteResultsFinal[1]*100
treeAcc <- treeResultsFinal[2]/treeResultsFinal[1]*100
treeVAcc <- treeVoteResultsFinal[2]/treeVoteResultsFinal[1]*100
svmAcc <- svmResultFinal[2]/ svmResultFinal[1]*100
cross4Accuracy <- c(linAcc, linVAcc, treeAcc, treeVAcc, svmAcc)

barplot(cross4Accuracy, #names.arg=c("Linear", "Linear\nOVO", "Tree", "Tree\nOVO", "SVM")
        col=c("cyan3", "darkslategray1","green", "light green", "red"),
        ylim=c(0,100), main="Test Set Size 4",las=1) #, sub="Figure 1")

title(ylab="Accuracy", cex.lab=1.5)
text(.73,85,  round(linAcc,digits=1), cex=1.5)
#text(.73, -3,  "Linear", cex=1.3, xpd=T)

text(1.88,67,  round(linVAcc,digits=1), cex=1.5)
#text(1.88, -5.6,  "Linear\nOVO", cex=1.3, xpd=T)

text(3.1,80,  round(treeAcc,digits=1), cex=1.5)
#text(3.1, -3,  "Tree", cex=1.3, xpd=T)

text(4.3,85,  round(treeVAcc,digits=1), cex=1.5)
#text(4.3, -5.6,  "Tree\nOVO", cex=1.3, xpd=T)

text(5.5,90,  round(svmAcc,digits=1), cex=1.5)
#text(5.5, -3,  "SVM", cex=1.3, xpd=T)

mtext(c("Linear", "Tree", "SVM"), side=1, line=.6, at=c(.7,3.1,5.5), cex=.9)
mtext(c("Linear\nOVO", "Tree\nOVO"), side=1, line=1.9, at=c(1.9,4.3), cex=.9)

cross5Accuracy <- calculateAccuracy(crossValidation5)
boxplot(as.numeric(as.character(accuracy)) ~ algorithm, data=cross5Accuracy, ylim=c(78, 98),  
        las=1, col=c("cyan3", "green", "red"),  main="Test Set Size 5")#,sub="Figure 2")
title(ylab="Accuracy", cex.lab=1.5)

cross6Accuracy <- calculateAccuracy(crossValidation6)
boxplot(as.numeric(as.character(accuracy)) ~ algorithm, data=cross6Accuracy, ylim=c(78, 98),
        las=1, col=c("cyan3", "green", "red"), main="Test Set Size 6")#, sub="Figure 3")
title(ylab="Accuracy", cex.lab=1.5)



par(mgp = c(3.5, .8, 0))

linKappa <- rowKappa(linearResultsFinal[1,])
linVKappa <- rowKappa(linearVoteResultsFinal[1,])
treeKappa <- rowKappa(treeResultsFinal[1,])
treeVKappa <- rowKappa(treeVoteResultsFinal[1,])
svmKappa <- rowKappa(svmResultFinal[1,])
cross4Kappa <- c(linKappa, linVKappa, treeKappa, treeVKappa, svmKappa)

barplot(cross4Kappa, names.arg="", # names.arg=c("Linear", "Linear\nOVO", "Tree", "Tree\nOVO", "SVM"),
        col=c("cyan3", "darkslategray1","green", "light green", "red"),
        ylim=c(0,1), main="Test Set Size 4",las=1)#, sub="Figure 4"
title(ylab="Kappa", cex.lab=1.5)

mtext(c("Linear", "Tree", "SVM"), side=1, line=.6, at=c(.7,3.1,5.5), cex=.9)
mtext(c("Linear\nOVO", "Tree\nOVO"), side=1, line=1.9, at=c(1.9,4.3), cex=.9)

#text(.73, 4,  "Linear", cex=1.3, xpd=T)
#text(.73, -3,  "blash", cex=1.3, xpd=T)
#text(3,10, "here")
#text(1.88,20,  round(linVKappa,digits=1), cex=1.5)
#text(3.1,20,  round(treeKappa,digits=1), cex=1.5)
#text(4.3,87,  round(treeVKappa,digits=1), cex=1.5)
#text(5.5,92,  round(svmKappa,digits=1), cex=1.5)


cross5Kappa <- calculateKappa(crossValidation5)
boxplot(as.numeric(as.character(kappa)) ~ algorithm, data=cross5Kappa, ylim=c(.73, .97),
        las=1, col=c("cyan3", "green", "red"), main="Test Set Size 5")#,sub="Figure 5")
title(ylab="Kappa", cex.lab=1.5)

cross6Kappa <- calculateKappa(crossValidation6)
boxplot(as.numeric(as.character(kappa)) ~ algorithm, data=cross6Kappa,  ylim=c(.73, .97),
        las=1, col=c("cyan3", "green", "red"), main="Test Set Size 6")#,sub="Figure 6")
title(ylab="Kappa", cex.lab=1.5)
