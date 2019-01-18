setwd('C:/Users/Myrna/Documents/Universiteit Utrecht/Cognitive modeling/Lab 3')

library(MASS)
library(mclust)
library(gtools)

#Creating and saving the data so we don't have to do it again (part had to be done manually -_- )

# library(tidyr)
# 
# #Task 1
# bigdata <- read.table('bigdata_no_header.txt', header = FALSE, sep = ';', row.names = NULL)
# header <- read.table('Headers.txt', header = FALSE, sep = ';')
# bigdata$V31 <- NULL
# colnames(bigdata) <- as.matrix(header)[1,]
# data <- extract(bigdata,filename,c('gender','ID', 'vowel'),"([[:alnum:]]+)([[:alnum:]]+[[:alnum:]]+)([[:alnum:]]+[[:alnum:]]+)")
# write.table(data, 'finalbigdata.csv',sep = ';')

data <- read.table('finalbigdata.csv', header = TRUE, sep = ';')

plot(data$`F2 at steady state`, data$`F1 at steady state`, xlim=rev(range(700:3500)),
     ylim=rev(range(300:1300)), col="1", xlab="F2", ylab="F1", pch=1,
     main="The vowel space", cex.lab=1.3, cex.axis=1.3, cex.sub=1.3)

#Task 2
mostDistincVowels <- data[data$vowel == 'iy'|data$vowel == 'uw'|data$vowel == 'ah',]
plot(mostDistincVowels$F2.at.steady.state, mostDistincVowels$F1.at.steady.state, xlim=rev(range(700:3500)),
     ylim=rev(range(300:1300)), col="1", xlab="F2", ylab="F1", pch=1,
     main="The vowel space", cex.lab=1.3, cex.axis=1.3, cex.sub=1.3)

mostDistincVowels[mostDistincVowels==0]<-NA

#Task 3
aMeanF1 <- mean(mostDistincVowels$F1.at.steady.state[mostDistincVowels$vowel=='ah'], na.rm = TRUE) #892.568
aMeanF2 <- mean(mostDistincVowels$F2.at.steady.state[mostDistincVowels$vowel=='ah'], na.rm = TRUE) #1474.309
iMeanF1 <- mean(mostDistincVowels$F1.at.steady.state[mostDistincVowels$vowel=='iy'], na.rm = TRUE) #412
iMeanF2 <- mean(mostDistincVowels$F2.at.steady.state[mostDistincVowels$vowel=='iy'], na.rm = TRUE) #2723.554
uMeanF1 <- mean(mostDistincVowels$F1.at.steady.state[mostDistincVowels$vowel=='uw'], na.rm = TRUE) #444.691
uMeanF2 <- mean(mostDistincVowels$F2.at.steady.state[mostDistincVowels$vowel=='uw'], na.rm = TRUE) #1157.504

#covariance between F1 and F2
aCovF1F2 <- cov(cbind(mostDistincVowels$F1.at.steady.state[mostDistincVowels$vowel=='ah'],mostDistincVowels$F2.at.steady.state[mostDistincVowels$vowel=='ah']), use = "na.or.complete") 
iCovF1F2 <- cov(cbind(mostDistincVowels$F1.at.steady.state[mostDistincVowels$vowel=='iy'],mostDistincVowels$F2.at.steady.state[mostDistincVowels$vowel=='iy']), use = "na.or.complete")
uCovF1F2 <- cov(cbind(mostDistincVowels$F1.at.steady.state[mostDistincVowels$vowel=='uw'],mostDistincVowels$F2.at.steady.state[mostDistincVowels$vowel=='uw']), use = "na.or.complete")

#trainingset
train.a <- data.frame(mvrnorm(2000, mu = c(aMeanF1,aMeanF2), Sigma = aCovF1F2))
train.i <- data.frame(mvrnorm(2000, mu = c(iMeanF1,iMeanF2), Sigma = iCovF1F2))
train.u <- data.frame(mvrnorm(2000, mu = c(uMeanF1,uMeanF2), Sigma = uCovF1F2))

trainingset <- rbind(train.a, train.i, train.u)

#testset
test.a <- data.frame(mvrnorm(2000, mu = c(aMeanF1,aMeanF2), Sigma = aCovF1F2))
test.i <- data.frame(mvrnorm(2000, mu = c(iMeanF1,iMeanF2), Sigma = iCovF1F2))
test.u <- data.frame(mvrnorm(2000, mu = c(uMeanF1,uMeanF2), Sigma = uCovF1F2))

test.a$label <- 1
test.i$label <- 2
test.u$label <- 3

testset <- rbind(test.a, test.i, test.u)

testset <- testset[sample(nrow(testset), nrow(testset)), ]
row.names(testset) <- NULL

#plotting the trainingset
plot(trainingset$X2, trainingset$X1, xlim=rev(range(700:3500)),
     ylim=rev(range(300:1300)), col="1", xlab="F2", ylab="F1", pch=1,
     main="The vowel space", cex.lab=1.3, cex.axis=1.3, cex.sub=1.3)

#plotting the testset
plot(testset$X2, testset$X1, xlim=rev(range(700:3500)),
     ylim=rev(range(300:1300)), col="1", xlab="F2", ylab="F1", pch=1,
     main="The vowel space", cex.lab=1.3, cex.axis=1.3, cex.sub=1.3)


#INTERMEZZO 
#fit a model with 3 clusters
model <- Mclust((trainingset), modelName="VVV", G=3,prior=priorControl())

#check the parameters
model$parameters

#predict the categories of the test-set with the parameters
#this gives a probabilistic category assignment
predictions.prob <- estep("VVV", data=testset[,1:2],parameters=model$parameters)

#Maximum likelihood criterium to pick a discrete prediction
#This results in a vector with the category predictions
predictions.disc <- apply(predictions.prob$z, 1, function(x) which(x ==max(x)))

#add the predictions to the testset
testset = data.frame(cbind(testset, predictions.disc))
colnames(testset) = c("F1.steady", "F2.steady", "TrueCat", "Pred")

#calculate proportion of correct responses
accuracy = length(testset$TrueCat[testset$TrueCat == testset$Pred]) /length(testset$TrueCat)

#Task 4
#The accuracy is .00783

for (j in 1:length(testset[,1])){
  if (testset$Pred[j]==3){
    testset$Pred2[j] <- 2 
    testset$Pred3[j] <- 3
    testset$Pred4[j] <- 1
    testset$Pred5[j] <- 2 
    testset$Pred6[j] <- 1 
  }else if (testset$Pred[j]==2){
    testset$Pred2[j] <- 3
    testset$Pred3[j] <- 1 
    testset$Pred4[j] <- 3 
    testset$Pred5[j] <- 1 
    testset$Pred6[j] <- 2 
  }else if (testset$Pred[j]==1){
    testset$Pred2[j] <- 1
    testset$Pred3[j] <- 2 
    testset$Pred4[j] <- 2 
    testset$Pred5[j] <- 3 
    testset$Pred6[j] <- 3 
  }
}
colnames(testset) = c("F1.steady", "F2.steady", "TrueCat", "Pred1","Pred2", "Pred3","Pred4","Pred5","Pred6")

#calculate proportion of correct responses
for (i in 1:6){
  accuracy = length(testset$TrueCat[testset$TrueCat == testset[,3+i]]) /length(testset$TrueCat)
  print(accuracy)
}
#model 4 is the correst one (2,3,1)



