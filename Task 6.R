setwd('C:/Users/Myrna/Documents/Universiteit Utrecht/Cognitive modeling/Lab 3')

source('Task 1 and 2.R')

#Task 6
lessDistincVowels <- data[data$vowel == 'iy'|data$vowel == 'ih'|data$vowel == 'eh',]
# plot(lessDistincVowels$F2.at.steady.state, lessDistincVowels$F1.at.steady.state, xlim=rev(range(700:3500)),
#      ylim=rev(range(300:1300)), col="1", xlab="F2", ylab="F1", pch=1,
#      main="The vowel space", cex.lab=1.3, cex.axis=1.3, cex.sub=1.3)

lessDistincVowels[lessDistincVowels==0]<-NA

aMeanF1 <- mean(lessDistincVowels$F1.at.steady.state[lessDistincVowels$vowel=='eh'], na.rm = TRUE) #892.568
aMeanF2 <- mean(lessDistincVowels$F2.at.steady.state[lessDistincVowels$vowel=='eh'], na.rm = TRUE) #1474.309
iMeanF1 <- mean(lessDistincVowels$F1.at.steady.state[lessDistincVowels$vowel=='iy'], na.rm = TRUE) #412
iMeanF2 <- mean(lessDistincVowels$F2.at.steady.state[lessDistincVowels$vowel=='iy'], na.rm = TRUE) #2723.554
uMeanF1 <- mean(lessDistincVowels$F1.at.steady.state[lessDistincVowels$vowel=='ih'], na.rm = TRUE) #444.691
uMeanF2 <- mean(lessDistincVowels$F2.at.steady.state[lessDistincVowels$vowel=='ih'], na.rm = TRUE) #1157.504

#covariance between F1 and F2
aCovF1F2 <- cov(cbind(lessDistincVowels$F1.at.steady.state[lessDistincVowels$vowel=='eh'],lessDistincVowels$F2.at.steady.state[lessDistincVowels$vowel=='eh']), use = "na.or.complete") 
iCovF1F2 <- cov(cbind(lessDistincVowels$F1.at.steady.state[lessDistincVowels$vowel=='iy'],lessDistincVowels$F2.at.steady.state[lessDistincVowels$vowel=='iy']), use = "na.or.complete")
uCovF1F2 <- cov(cbind(lessDistincVowels$F1.at.steady.state[lessDistincVowels$vowel=='ih'],lessDistincVowels$F2.at.steady.state[lessDistincVowels$vowel=='ih']), use = "na.or.complete")

#trainingset
train.a <- data.frame(mvrnorm(1000, mu = c(aMeanF1,aMeanF2), Sigma = aCovF1F2))
train.i <- data.frame(mvrnorm(2000, mu = c(iMeanF1,iMeanF2), Sigma = iCovF1F2))
train.u <- data.frame(mvrnorm(2000, mu = c(uMeanF1,uMeanF2), Sigma = uCovF1F2))

trainingset <- rbind(train.a, train.i, train.u)

#testset
test.a <- data.frame(mvrnorm(1000, mu = c(aMeanF1,aMeanF2), Sigma = aCovF1F2))
test.i <- data.frame(mvrnorm(2000, mu = c(iMeanF1,iMeanF2), Sigma = iCovF1F2))
test.u <- data.frame(mvrnorm(2000, mu = c(uMeanF1,uMeanF2), Sigma = uCovF1F2))

test.a$label <- 1
test.i$label <- 2
test.u$label <- 3

testset <- rbind(test.a, test.i, test.u)

testset <- testset[sample(nrow(testset), nrow(testset)), ]
row.names(testset) <- NULL

#plotting the trainingset
# plot(trainingset$X2, trainingset$X1, xlim=rev(range(700:3500)),
#      ylim=rev(range(300:1300)), col="1", xlab="F2", ylab="F1", pch=1,
#      main="The vowel space", cex.lab=1.3, cex.axis=1.3, cex.sub=1.3)

#plotting the testset
# plot(testset$X2, testset$X1, xlim=rev(range(700:3500)),
#      ylim=rev(range(300:1300)), col="1", xlab="F2", ylab="F1", pch=1,
#      main="The vowel space", cex.lab=1.3, cex.axis=1.3, cex.sub=1.3)

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
