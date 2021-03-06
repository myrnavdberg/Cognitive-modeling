setwd('C:/Users/Myrna/Documents/Universiteit Utrecht/Cognitive modeling/Lab 3')

source('Task 1 and 2.R')

#Task 8

#Decide the genders
mostDistincVowels <- data[(data$vowel == 'iy'|data$vowel == 'uw'|data$vowel == 'ah')&(data$gender=='w'|data$gender=='b'|data$gender=='g'),]
lessDistinctVowels <- data[(data$vowel == 'iy'|data$vowel == 'ih'|data$vowel == 'eh')&(data$gender=='w'|data$gender=='b'|data$gender=='g'),]





mostDistincVowels[mostDistincVowels==0]<-NA
lessDistinctVowels[lessDistinctVowels==0]<-NA

mostDistincVowels<- mostDistincVowels[complete.cases(mostDistincVowels), ]
lessDistinctVowels<- lessDistinctVowels[complete.cases(lessDistinctVowels), ]

aMeanF1 <- mean(mostDistincVowels$F1.at.steady.state[mostDistincVowels$vowel=='ah'], na.rm = TRUE) #892.568
aMeanF2 <- mean(mostDistincVowels$F2.at.steady.state[mostDistincVowels$vowel=='ah'], na.rm = TRUE) #1474.309
aMeanF3 <- mean(mostDistincVowels$F3.at.steady.state[mostDistincVowels$vowel=='ah'], na.rm = TRUE)
iMeanF1 <- mean(mostDistincVowels$F1.at.steady.state[mostDistincVowels$vowel=='iy'], na.rm = TRUE) #412
iMeanF2 <- mean(mostDistincVowels$F2.at.steady.state[mostDistincVowels$vowel=='iy'], na.rm = TRUE) #2723.554
iMeanF3 <- mean(mostDistincVowels$F3.at.steady.state[mostDistincVowels$vowel=='iy'], na.rm = TRUE)
uMeanF1 <- mean(mostDistincVowels$F1.at.steady.state[mostDistincVowels$vowel=='uw'], na.rm = TRUE) #444.691
uMeanF2 <- mean(mostDistincVowels$F2.at.steady.state[mostDistincVowels$vowel=='uw'], na.rm = TRUE) #1157.504
uMeanF3 <- mean(mostDistincVowels$F3.at.steady.state[mostDistincVowels$vowel=='uw'], na.rm = TRUE)

ehMeanF1 <- mean(lessDistinctVowels$F1.at.steady.state[lessDistinctVowels$vowel=='eh'], na.rm = TRUE) #892.568
ehMeanF2 <- mean(lessDistinctVowels$F2.at.steady.state[lessDistinctVowels$vowel=='eh'], na.rm = TRUE) #1474.309
ehMeanF3 <- mean(lessDistinctVowels$F3.at.steady.state[lessDistinctVowels$vowel=='eh'], na.rm = TRUE)
iyMeanF1 <- mean(lessDistinctVowels$F1.at.steady.state[lessDistinctVowels$vowel=='iy'], na.rm = TRUE) #412
iyMeanF2 <- mean(lessDistinctVowels$F2.at.steady.state[lessDistinctVowels$vowel=='iy'], na.rm = TRUE) #2723.554
iyMeanF3 <- mean(lessDistinctVowels$F3.at.steady.state[lessDistinctVowels$vowel=='iy'], na.rm = TRUE)
ihMeanF1 <- mean(lessDistinctVowels$F1.at.steady.state[lessDistinctVowels$vowel=='ih'], na.rm = TRUE) #444.691
ihMeanF2 <- mean(lessDistinctVowels$F2.at.steady.state[lessDistinctVowels$vowel=='ih'], na.rm = TRUE) #1157.504
ihMeanF3 <- mean(lessDistinctVowels$F3.at.steady.state[lessDistinctVowels$vowel=='ih'], na.rm = TRUE)

#covariance between F1 and F2
aCovF1F2F3 <- cov(cbind(mostDistincVowels$F1.at.steady.state[mostDistincVowels$vowel=='ah'],mostDistincVowels$F2.at.steady.state[mostDistincVowels$vowel=='ah'], mostDistincVowels$F3.at.steady.state[mostDistincVowels$vowel=='ah']), use = "na.or.complete") 
iCovF1F2F3 <- cov(cbind(mostDistincVowels$F1.at.steady.state[mostDistincVowels$vowel=='iy'],mostDistincVowels$F2.at.steady.state[mostDistincVowels$vowel=='iy'],mostDistincVowels$F3.at.steady.state[mostDistincVowels$vowel=='iy']), use = "na.or.complete")
uCovF1F2F3 <- cov(cbind(mostDistincVowels$F1.at.steady.state[mostDistincVowels$vowel=='uw'],mostDistincVowels$F2.at.steady.state[mostDistincVowels$vowel=='uw'], mostDistincVowels$F3.at.steady.state[mostDistincVowels$vowel=='uw']), use = "na.or.complete")

ehCovF1F2F3 <- cov(cbind(lessDistinctVowels$F1.at.steady.state[lessDistinctVowels$vowel=='eh'],lessDistinctVowels$F2.at.steady.state[lessDistinctVowels$vowel=='eh'], lessDistinctVowels$F3.at.steady.state[lessDistinctVowels$vowel=='eh']), use = "na.or.complete") 
iyCovF1F2F3 <- cov(cbind(lessDistinctVowels$F1.at.steady.state[lessDistinctVowels$vowel=='iy'],lessDistinctVowels$F2.at.steady.state[lessDistinctVowels$vowel=='iy'], lessDistinctVowels$F3.at.steady.state[lessDistinctVowels$vowel=='iy']), use = "na.or.complete")
ihCovF1F2F3 <- cov(cbind(lessDistinctVowels$F1.at.steady.state[lessDistinctVowels$vowel=='ih'],lessDistinctVowels$F2.at.steady.state[lessDistinctVowels$vowel=='ih'], lessDistinctVowels$F3.at.steady.state[lessDistinctVowels$vowel=='ih']), use = "na.or.complete")

#trainingset
train.a <- data.frame(mvrnorm(2000, mu = c(aMeanF1,aMeanF2, aMeanF3), Sigma = aCovF1F2F3))
train.i <- data.frame(mvrnorm(2000, mu = c(iMeanF1,iMeanF2, iMeanF3), Sigma = iCovF1F2F3))
train.u <- data.frame(mvrnorm(2000, mu = c(uMeanF1,uMeanF2, uMeanF3), Sigma = uCovF1F2F3))

train.eh <- data.frame(mvrnorm(2000, mu = c(ehMeanF1,ehMeanF2, ehMeanF3), Sigma = ehCovF1F2F3))
train.iy <- data.frame(mvrnorm(2000, mu = c(iyMeanF1,iyMeanF2, iyMeanF3), Sigma = iyCovF1F2F3))
train.ih <- data.frame(mvrnorm(2000, mu = c(ihMeanF1,ihMeanF2, ihMeanF3), Sigma = ihCovF1F2F3))


trainingset1 <- rbind(train.a, train.i, train.u)
trainingset2 <- rbind(train.eh, train.iy, train.ih)

#testset
test.a <- data.frame(mvrnorm(2000, mu = c(aMeanF1,aMeanF2, aMeanF3), Sigma = aCovF1F2F3))
test.i <- data.frame(mvrnorm(2000, mu = c(iMeanF1,iMeanF2, iMeanF3), Sigma = iCovF1F2F3))
test.u <- data.frame(mvrnorm(2000, mu = c(uMeanF1,uMeanF2, uMeanF3), Sigma = uCovF1F2F3))

test.eh <- data.frame(mvrnorm(2000, mu = c(ehMeanF1,ehMeanF2, ehMeanF3), Sigma = ehCovF1F2F3))
test.iy <- data.frame(mvrnorm(2000, mu = c(iyMeanF1,iyMeanF2, iyMeanF3), Sigma = iyCovF1F2F3))
test.ih <- data.frame(mvrnorm(2000, mu = c(ihMeanF1,ihMeanF2, ihMeanF3), Sigma = ihCovF1F2F3))

test.a$label <- 1
test.i$label <- 2
test.u$label <- 3

test.eh$label <- 1
test.iy$label <- 2
test.ih$label <- 3

testset1 <- rbind(test.a, test.i, test.u)
testset2 <- rbind(test.eh, test.iy, test.ih)

testset1 <- testset1[sample(nrow(testset1), nrow(testset1)), ]
row.names(testset1) <- NULL

#plotting the trainingset1
plot(trainingset1$X2, trainingset1$X1, xlim=rev(range(700:3500)),
     ylim=rev(range(300:1300)), col="1", xlab="F2", ylab="F1", pch=1,
     main="The vowel space", cex.lab=1.3, cex.axis=1.3, cex.sub=1.3)

#plotting the testset1
plot(testset1$X2, testset1$X1, xlim=rev(range(700:3500)),
     ylim=rev(range(300:1300)), col="1", xlab="F2", ylab="F1", pch=1,
     main="The vowel space", cex.lab=1.3, cex.axis=1.3, cex.sub=1.3)


#INTERMEZZO 
#fit a model with 3 clusters
#'vvv' = ellipsoidal, varying volume, shape, and orientation
model <- Mclust((trainingset1), modelName="VVV", G=3,prior=priorControl())

#check the parameters
model$parameters

#predict the categories of the test-set with the parameters
#this gives a probabilistic category assignment
predictions.prob <- estep("VVV", data=testset1[,1:3],parameters=model$parameters)

#Maximum likelihood criterium to pick a discrete prediction
#This results in a vector with the category predictions
predictions.disc <- apply(predictions.prob$z, 1, function(x) which(x ==max(x)))

#add the predictions to the testset1
testset1 = data.frame(cbind(testset1, predictions.disc))
colnames(testset1) = c("F1.steady", "F2.steady","F3.steady", "TrueCat", "Pred")

#calculate proportion of correct responses
accuracy = length(testset1$TrueCat[testset1$TrueCat == testset1$Pred]) /length(testset1$TrueCat)


for (j in 1:length(testset1[,1])){
  if (testset1$Pred[j]==3){
    testset1$Pred2[j] <- 2 
    testset1$Pred3[j] <- 3
    testset1$Pred4[j] <- 1
    testset1$Pred5[j] <- 2 
    testset1$Pred6[j] <- 1 
  }else if (testset1$Pred[j]==2){
    testset1$Pred2[j] <- 3
    testset1$Pred3[j] <- 1 
    testset1$Pred4[j] <- 3 
    testset1$Pred5[j] <- 1 
    testset1$Pred6[j] <- 2 
  }else if (testset1$Pred[j]==1){
    testset1$Pred2[j] <- 1
    testset1$Pred3[j] <- 2 
    testset1$Pred4[j] <- 2 
    testset1$Pred5[j] <- 3 
    testset1$Pred6[j] <- 3 
  }
}
colnames(testset1) = c("F1.steady", "F2.steady", "F3.steady", "TrueCat", "Pred1","Pred2", "Pred3","Pred4","Pred5","Pred6")

#calculate proportion of correct responses
acc.list.3d.F3.aiu <- c()
for (i in 1:6){
  acc.list.3d.F3.aiu <- c(acc.list.3d.F3.aiu,length(testset1$TrueCat[testset1$TrueCat == testset1[,4+i]]) /length(testset1$TrueCat))

}
#.998 for a.i.u

labeled.3d.F3.aiu <- matrix(0,3,3)
dim <- 3+1
column <- which.max(acc.list.3d.F3.aiu) + dim 

for(i in 1:length(testset1[,dim])){
  if (testset1[i,dim] == 1){
    if (testset1[i,column] == 1){
      labeled.3d.F3.aiu[1,1] <- labeled.3d.F3.aiu[1,1] + 1
    }
    if (testset1[i,column] == 2){
      labeled.3d.F3.aiu[1,2] <- labeled.3d.F3.aiu[1,2] + 1
    }
    if (testset1[i,column] == 3){
      labeled.3d.F3.aiu[1,3] <- labeled.3d.F3.aiu[1,3] + 1
    }
  }
  if (testset1[i,dim] == 2){
    if (testset1[i,column] == 1){
      labeled.3d.F3.aiu[2,1] <- labeled.3d.F3.aiu[2,1] + 1
    }
    if (testset1[i,column] == 2){
      labeled.3d.F3.aiu[2,2] <- labeled.3d.F3.aiu[2,2] + 1
    }
    if (testset1[i,column] == 3){
      labeled.3d.F3.aiu[2,3] <- labeled.3d.F3.aiu[2,3] + 1
    }
  }
  if (testset1[i,dim] == 3){
    if (testset1[i,column] == 1){
      labeled.3d.F3.aiu[3,1] <- labeled.3d.F3.aiu[3,1] + 1
    }
    if (testset1[i,column] == 2){
      labeled.3d.F3.aiu[3,2] <- labeled.3d.F3.aiu[3,2] + 1
    }
    if (testset1[i,column] == 3){
      labeled.3d.F3.aiu[3,3] <- labeled.3d.F3.aiu[3,3] + 1
    }
  }
}










#Testset 2
testset2 <- testset2[sample(nrow(testset2), nrow(testset2)), ]
row.names(testset2) <- NULL

#plotting the trainingset2
plot(trainingset2$X2, trainingset2$X1, xlim=rev(range(700:3500)),
     ylim=rev(range(300:1300)), col="1", xlab="F2", ylab="F1", pch=1,
     main="The vowel space", cex.lab=1.3, cex.axis=1.3, cex.sub=1.3)

#plotting the testset2
plot(testset2$X2, testset2$X1, xlim=rev(range(700:3500)),
     ylim=rev(range(300:1300)), col="1", xlab="F2", ylab="F1", pch=1,
     main="The vowel space", cex.lab=1.3, cex.axis=1.3, cex.sub=1.3)


#INTERMEZZO 
#fit a model with 3 clusters
#'vvv' = ellipsoidal, varying volume, shape, and orientation
model <- Mclust((trainingset2), modelName="VVV", G=3,prior=priorControl())

#check the parameters
model$parameters

#predict the categories of the test-set with the parameters
#this gives a probabilistic category assignment
predictions.prob <- estep("VVV", data=testset2[,1:3],parameters=model$parameters)

#Maximum likelihood criterium to pick a discrete prediction
#This results in a vector with the category predictions
predictions.disc <- apply(predictions.prob$z, 1, function(x) which(x ==max(x)))

#add the predictions to the testset2
testset2 = data.frame(cbind(testset2, predictions.disc))
colnames(testset2) = c("F1.steady", "F2.steady","F3.steady", "TrueCat", "Pred")

#calculate proportion of correct responses
accuracy = length(testset2$TrueCat[testset2$TrueCat == testset2$Pred]) /length(testset2$TrueCat)

#Task 4
#The accuracy is .00783

for (j in 1:length(testset2[,1])){
  if (testset2$Pred[j]==3){
    testset2$Pred2[j] <- 2 
    testset2$Pred3[j] <- 3
    testset2$Pred4[j] <- 1
    testset2$Pred5[j] <- 2 
    testset2$Pred6[j] <- 1 
  }else if (testset2$Pred[j]==2){
    testset2$Pred2[j] <- 3
    testset2$Pred3[j] <- 1 
    testset2$Pred4[j] <- 3 
    testset2$Pred5[j] <- 1 
    testset2$Pred6[j] <- 2 
  }else if (testset2$Pred[j]==1){
    testset2$Pred2[j] <- 1
    testset2$Pred3[j] <- 2 
    testset2$Pred4[j] <- 2 
    testset2$Pred5[j] <- 3 
    testset2$Pred6[j] <- 3 
  }
}
colnames(testset2) = c("F1.steady", "F2.steady","F3.steady", "TrueCat", "Pred1","Pred2", "Pred3","Pred4","Pred5","Pred6")

#calculate proportion of correct responses
acc.list.3d.F3.ehiyih <- c()
for (i in 1:6){
  acc.list.3d.F3.ehiyih <- c(acc.list.3d.F3.ehiyih, length(testset2$TrueCat[testset2$TrueCat == testset2[,4+i]]) /length(testset2$TrueCat))

}
#.960 voor balanced eh.iy.ih

labeled.3d.F3.ehiyih <- matrix(0,3,3)
dim <- 3+1
column <- which.max(acc.list.3d.F3.ehiyih) + dim 

for(i in 1:length(testset2[,dim])){
  if (testset2[i,dim] == 1){
    if (testset2[i,column] == 1){
      labeled.3d.F3.ehiyih[1,1] <- labeled.3d.F3.ehiyih[1,1] + 1
    }
    if (testset2[i,column] == 2){
      labeled.3d.F3.ehiyih[1,2] <- labeled.3d.F3.ehiyih[1,2] + 1
    }
    if (testset2[i,column] == 3){
      labeled.3d.F3.ehiyih[1,3] <- labeled.3d.F3.ehiyih[1,3] + 1
    }
  }
  if (testset2[i,dim] == 2){
    if (testset2[i,column] == 1){
      labeled.3d.F3.ehiyih[2,1] <- labeled.3d.F3.ehiyih[2,1] + 1
    }
    if (testset2[i,column] == 2){
      labeled.3d.F3.ehiyih[2,2] <- labeled.3d.F3.ehiyih[2,2] + 1
    }
    if (testset2[i,column] == 3){
      labeled.3d.F3.ehiyih[2,3] <- labeled.3d.F3.ehiyih[2,3] + 1
    }
  }
  if (testset2[i,dim] == 3){
    if (testset2[i,column] == 1){
      labeled.3d.F3.ehiyih[3,1] <- labeled.3d.F3.ehiyih[3,1] + 1
    }
    if (testset2[i,column] == 2){
      labeled.3d.F3.ehiyih[3,2] <- labeled.3d.F3.ehiyih[3,2] + 1
    }
    if (testset2[i,column] == 3){
      labeled.3d.F3.ehiyih[3,3] <- labeled.3d.F3.ehiyih[3,3] + 1
    }
  }
}













aMeanF1 <- mean(mostDistincVowels$F1.at.steady.state[mostDistincVowels$vowel=='ah'], na.rm = TRUE) #892.568
aMeanF2 <- mean(mostDistincVowels$F2.at.steady.state[mostDistincVowels$vowel=='ah'], na.rm = TRUE) #1474.309
aMeanF3 <- mean(mostDistincVowels$duration.in.msec[mostDistincVowels$vowel=='ah'], na.rm = TRUE)
iMeanF1 <- mean(mostDistincVowels$F1.at.steady.state[mostDistincVowels$vowel=='iy'], na.rm = TRUE) #412
iMeanF2 <- mean(mostDistincVowels$F2.at.steady.state[mostDistincVowels$vowel=='iy'], na.rm = TRUE) #2723.554
iMeanF3 <- mean(mostDistincVowels$duration.in.msec[mostDistincVowels$vowel=='iy'], na.rm = TRUE)
uMeanF1 <- mean(mostDistincVowels$F1.at.steady.state[mostDistincVowels$vowel=='uw'], na.rm = TRUE) #444.691
uMeanF2 <- mean(mostDistincVowels$F2.at.steady.state[mostDistincVowels$vowel=='uw'], na.rm = TRUE) #1157.504
uMeanF3 <- mean(mostDistincVowels$duration.in.msec[mostDistincVowels$vowel=='uw'], na.rm = TRUE)

ehMeanF1 <- mean(lessDistinctVowels$F1.at.steady.state[lessDistinctVowels$vowel=='eh'], na.rm = TRUE) #892.568
ehMeanF2 <- mean(lessDistinctVowels$F2.at.steady.state[lessDistinctVowels$vowel=='eh'], na.rm = TRUE) #1474.309
ehMeanF3 <- mean(lessDistinctVowels$duration.in.msec[lessDistinctVowels$vowel=='eh'], na.rm = TRUE)
iyMeanF1 <- mean(lessDistinctVowels$F1.at.steady.state[lessDistinctVowels$vowel=='iy'], na.rm = TRUE) #412
iyMeanF2 <- mean(lessDistinctVowels$F2.at.steady.state[lessDistinctVowels$vowel=='iy'], na.rm = TRUE) #2723.554
iyMeanF3 <- mean(lessDistinctVowels$duration.in.msec[lessDistinctVowels$vowel=='iy'], na.rm = TRUE)
ihMeanF1 <- mean(lessDistinctVowels$F1.at.steady.state[lessDistinctVowels$vowel=='ih'], na.rm = TRUE) #444.691
ihMeanF2 <- mean(lessDistinctVowels$F2.at.steady.state[lessDistinctVowels$vowel=='ih'], na.rm = TRUE) #1157.504
ihMeanF3 <- mean(lessDistinctVowels$duration.in.msec[lessDistinctVowels$vowel=='ih'], na.rm = TRUE)

#covariance between F1 and F2
aCovF1F2F3 <- cov(cbind(mostDistincVowels$F1.at.steady.state[mostDistincVowels$vowel=='ah'],mostDistincVowels$F2.at.steady.state[mostDistincVowels$vowel=='ah'], mostDistincVowels$duration.in.msec[mostDistincVowels$vowel=='ah']), use = "na.or.complete") 
iCovF1F2F3 <- cov(cbind(mostDistincVowels$F1.at.steady.state[mostDistincVowels$vowel=='iy'],mostDistincVowels$F2.at.steady.state[mostDistincVowels$vowel=='iy'],mostDistincVowels$duration.in.msec[mostDistincVowels$vowel=='iy']), use = "na.or.complete")
uCovF1F2F3 <- cov(cbind(mostDistincVowels$F1.at.steady.state[mostDistincVowels$vowel=='uw'],mostDistincVowels$F2.at.steady.state[mostDistincVowels$vowel=='uw'], mostDistincVowels$duration.in.msec[mostDistincVowels$vowel=='uw']), use = "na.or.complete")

ehCovF1F2F3 <- cov(cbind(lessDistinctVowels$F1.at.steady.state[lessDistinctVowels$vowel=='eh'],lessDistinctVowels$F2.at.steady.state[lessDistinctVowels$vowel=='eh'], lessDistinctVowels$duration.in.msec[lessDistinctVowels$vowel=='eh']), use = "na.or.complete") 
iyCovF1F2F3 <- cov(cbind(lessDistinctVowels$F1.at.steady.state[lessDistinctVowels$vowel=='iy'],lessDistinctVowels$F2.at.steady.state[lessDistinctVowels$vowel=='iy'], lessDistinctVowels$duration.in.msec[lessDistinctVowels$vowel=='iy']), use = "na.or.complete")
ihCovF1F2F3 <- cov(cbind(lessDistinctVowels$F1.at.steady.state[lessDistinctVowels$vowel=='ih'],lessDistinctVowels$F2.at.steady.state[lessDistinctVowels$vowel=='ih'], lessDistinctVowels$duration.in.msec[lessDistinctVowels$vowel=='ih']), use = "na.or.complete")

#trainingset
train.a <- data.frame(mvrnorm(2000, mu = c(aMeanF1,aMeanF2, aMeanF3), Sigma = aCovF1F2F3))
train.i <- data.frame(mvrnorm(2000, mu = c(iMeanF1,iMeanF2, iMeanF3), Sigma = iCovF1F2F3))
train.u <- data.frame(mvrnorm(2000, mu = c(uMeanF1,uMeanF2, uMeanF3), Sigma = uCovF1F2F3))

train.eh <- data.frame(mvrnorm(2000, mu = c(ehMeanF1,ehMeanF2, ehMeanF3), Sigma = ehCovF1F2F3))
train.iy <- data.frame(mvrnorm(2000, mu = c(iyMeanF1,iyMeanF2, iyMeanF3), Sigma = iyCovF1F2F3))
train.ih <- data.frame(mvrnorm(2000, mu = c(ihMeanF1,ihMeanF2, ihMeanF3), Sigma = ihCovF1F2F3))


trainingset1 <- rbind(train.a, train.i, train.u)
trainingset2 <- rbind(train.eh, train.iy, train.ih)

#testset
test.a <- data.frame(mvrnorm(2000, mu = c(aMeanF1,aMeanF2, aMeanF3), Sigma = aCovF1F2F3))
test.i <- data.frame(mvrnorm(2000, mu = c(iMeanF1,iMeanF2, iMeanF3), Sigma = iCovF1F2F3))
test.u <- data.frame(mvrnorm(2000, mu = c(uMeanF1,uMeanF2, uMeanF3), Sigma = uCovF1F2F3))

test.eh <- data.frame(mvrnorm(2000, mu = c(ehMeanF1,ehMeanF2, ehMeanF3), Sigma = ehCovF1F2F3))
test.iy <- data.frame(mvrnorm(2000, mu = c(iyMeanF1,iyMeanF2, iyMeanF3), Sigma = iyCovF1F2F3))
test.ih <- data.frame(mvrnorm(2000, mu = c(ihMeanF1,ihMeanF2, ihMeanF3), Sigma = ihCovF1F2F3))

test.a$label <- 1
test.i$label <- 2
test.u$label <- 3

test.eh$label <- 1
test.iy$label <- 2
test.ih$label <- 3

testset1 <- rbind(test.a, test.i, test.u)
testset2 <- rbind(test.eh, test.iy, test.ih)

testset1 <- testset1[sample(nrow(testset1), nrow(testset1)), ]
row.names(testset1) <- NULL

#plotting the trainingset1
# plot(trainingset1$X2, trainingset1$X1, xlim=rev(range(700:3500)),
#      ylim=rev(range(300:1300)), col="1", xlab="F2", ylab="F1", pch=1,
#      main="The vowel space", cex.lab=1.3, cex.axis=1.3, cex.sub=1.3)
# 
# #plotting the testset1
# plot(testset1$X2, testset1$X1, xlim=rev(range(700:3500)),
#      ylim=rev(range(300:1300)), col="1", xlab="F2", ylab="F1", pch=1,
#      main="The vowel space", cex.lab=1.3, cex.axis=1.3, cex.sub=1.3)


#INTERMEZZO 
#fit a model with 3 clusters
#'vvv' = ellipsoidal, varying volume, shape, and orientation
model <- Mclust((trainingset1), modelName="VVV", G=3,prior=priorControl())

#check the parameters
model$parameters

#predict the categories of the test-set with the parameters
#this gives a probabilistic category assignment
predictions.prob <- estep("VVV", data=testset1[,1:3],parameters=model$parameters)

#Maximum likelihood criterium to pick a discrete prediction
#This results in a vector with the category predictions
predictions.disc <- apply(predictions.prob$z, 1, function(x) which(x ==max(x)))

#add the predictions to the testset1
testset1 = data.frame(cbind(testset1, predictions.disc))
colnames(testset1) = c("F1.steady", "F2.steady","duration", "TrueCat", "Pred")

#calculate proportion of correct responses
accuracy = length(testset1$TrueCat[testset1$TrueCat == testset1$Pred]) /length(testset1$TrueCat)

#Task 4
#The accuracy is .00783

for (j in 1:length(testset1[,1])){
  if (testset1$Pred[j]==3){
    testset1$Pred2[j] <- 2 
    testset1$Pred3[j] <- 3
    testset1$Pred4[j] <- 1
    testset1$Pred5[j] <- 2 
    testset1$Pred6[j] <- 1 
  }else if (testset1$Pred[j]==2){
    testset1$Pred2[j] <- 3
    testset1$Pred3[j] <- 1 
    testset1$Pred4[j] <- 3 
    testset1$Pred5[j] <- 1 
    testset1$Pred6[j] <- 2 
  }else if (testset1$Pred[j]==1){
    testset1$Pred2[j] <- 1
    testset1$Pred3[j] <- 2 
    testset1$Pred4[j] <- 2 
    testset1$Pred5[j] <- 3 
    testset1$Pred6[j] <- 3 
  }
}
colnames(testset1) = c("F1.steady", "F2.steady", "duration", "TrueCat", "Pred1","Pred2", "Pred3","Pred4","Pred5","Pred6")

#calculate proportion of correct responses
acc.list.3d.duration.aiu <- c()
for (i in 1:6){
  acc.list.3d.duration.aiu <- c(acc.list.3d.duration.aiu, length(testset1$TrueCat[testset1$TrueCat == testset1[,4+i]]) /length(testset1$TrueCat))
}
#.999 for a.i.u

labeled.3d.duration.aiu <- matrix(0,3,3)
dim <- 3 +1
column <- which.max(acc.list.3d.duration.aiu) + dim 

for(i in 1:length(testset1[,dim])){
  if (testset1[i,dim] == 1){
    if (testset1[i,column] == 1){
      labeled.3d.duration.aiu[1,1] <- labeled.3d.duration.aiu[1,1] + 1
    }
    if (testset1[i,column] == 2){
      labeled.3d.duration.aiu[1,2] <- labeled.3d.duration.aiu[1,2] + 1
    }
    if (testset1[i,column] == 3){
      labeled.3d.duration.aiu[1,3] <- labeled.3d.duration.aiu[1,3] + 1
    }
  }
  if (testset1[i,dim] == 2){
    if (testset1[i,column] == 1){
      labeled.3d.duration.aiu[2,1] <- labeled.3d.duration.aiu[2,1] + 1
    }
    if (testset1[i,column] == 2){
      labeled.3d.duration.aiu[2,2] <- labeled.3d.duration.aiu[2,2] + 1
    }
    if (testset1[i,column] == 3){
      labeled.3d.duration.aiu[2,3] <- labeled.3d.duration.aiu[2,3] + 1
    }
  }
  if (testset1[i,dim] == 3){
    if (testset1[i,column] == 1){
      labeled.3d.duration.aiu[3,1] <- labeled.3d.duration.aiu[3,1] + 1
    }
    if (testset1[i,column] == 2){
      labeled.3d.duration.aiu[3,2] <- labeled.3d.duration.aiu[3,2] + 1
    }
    if (testset1[i,column] == 3){
      labeled.3d.duration.aiu[3,3] <- labeled.3d.duration.aiu[3,3] + 1
    }
  }
}





#Testset 2
testset2 <- testset2[sample(nrow(testset2), nrow(testset2)), ]
row.names(testset2) <- NULL

#plotting the trainingset2
# plot(trainingset2$X2, trainingset2$X1, xlim=rev(range(700:3500)),
#      ylim=rev(range(300:1300)), col="1", xlab="F2", ylab="F1", pch=1,
#      main="The vowel space", cex.lab=1.3, cex.axis=1.3, cex.sub=1.3)
# 
# #plotting the testset2
# plot(testset2$X2, testset2$X1, xlim=rev(range(700:3500)),
#      ylim=rev(range(300:1300)), col="1", xlab="F2", ylab="F1", pch=1,
#      main="The vowel space", cex.lab=1.3, cex.axis=1.3, cex.sub=1.3)


#INTERMEZZO 
#fit a model with 3 clusters
#'vvv' = ellipsoidal, varying volume, shape, and orientation
model <- Mclust((trainingset2), modelName="VVV", G=3,prior=priorControl())

#check the parameters
model$parameters

#predict the categories of the test-set with the parameters
#this gives a probabilistic category assignment
predictions.prob <- estep("VVV", data=testset2[,1:3],parameters=model$parameters)

#Maximum likelihood criterium to pick a discrete prediction
#This results in a vector with the category predictions
predictions.disc <- apply(predictions.prob$z, 1, function(x) which(x ==max(x)))

#add the predictions to the testset2
testset2 = data.frame(cbind(testset2, predictions.disc))
colnames(testset2) = c("F1.steady", "F2.steady","duration", "TrueCat", "Pred")

#calculate proportion of correct responses
accuracy = length(testset2$TrueCat[testset2$TrueCat == testset2$Pred]) /length(testset2$TrueCat)

#Task 4
#The accuracy is .00783

for (j in 1:length(testset2[,1])){
  if (testset2$Pred[j]==3){
    testset2$Pred2[j] <- 2 
    testset2$Pred3[j] <- 3
    testset2$Pred4[j] <- 1
    testset2$Pred5[j] <- 2 
    testset2$Pred6[j] <- 1 
  }else if (testset2$Pred[j]==2){
    testset2$Pred2[j] <- 3
    testset2$Pred3[j] <- 1 
    testset2$Pred4[j] <- 3 
    testset2$Pred5[j] <- 1 
    testset2$Pred6[j] <- 2 
  }else if (testset2$Pred[j]==1){
    testset2$Pred2[j] <- 1
    testset2$Pred3[j] <- 2 
    testset2$Pred4[j] <- 2 
    testset2$Pred5[j] <- 3 
    testset2$Pred6[j] <- 3 
  }
}
colnames(testset2) = c("F1.steady", "F2.steady","duration", "TrueCat", "Pred1","Pred2", "Pred3","Pred4","Pred5","Pred6")

#calculate proportion of correct responses
acc.list.3d.duration.ehiyih <- c()
for (i in 1:6){
  acc.list.3d.duration.ehiyih <- c(acc.list.3d.duration.ehiyih,  length(testset2$TrueCat[testset2$TrueCat == testset2[,4+i]]) /length(testset2$TrueCat))

}
#.928 voor balanced eh.iy.ih

labeled.3d.duration.ehiyih <- matrix(0,3,3)
dim <- 3+ 1
column <- which.max(acc.list.3d.duration.ehiyih) + dim 

for(i in 1:length(testset2[,dim])){
  if (testset2[i,dim] == 1){
    if (testset2[i,column] == 1){
      labeled.3d.duration.ehiyih[1,1] <- labeled.3d.duration.ehiyih[1,1] + 1
    }
    if (testset2[i,column] == 2){
      labeled.3d.duration.ehiyih[1,2] <- labeled.3d.duration.ehiyih[1,2] + 1
    }
    if (testset2[i,column] == 3){
      labeled.3d.duration.ehiyih[1,3] <- labeled.3d.duration.ehiyih[1,3] + 1
    }
  }
  if (testset2[i,dim] == 2){
    if (testset2[i,column] == 1){
      labeled.3d.duration.ehiyih[2,1] <- labeled.3d.duration.ehiyih[2,1] + 1
    }
    if (testset2[i,column] == 2){
      labeled.3d.duration.ehiyih[2,2] <- labeled.3d.duration.ehiyih[2,2] + 1
    }
    if (testset2[i,column] == 3){
      labeled.3d.duration.ehiyih[2,3] <- labeled.3d.duration.ehiyih[2,3] + 1
    }
  }
  if (testset2[i,dim] == 3){
    if (testset2[i,column] == 1){
      labeled.3d.duration.ehiyih[3,1] <- labeled.3d.duration.ehiyih[3,1] + 1
    }
    if (testset2[i,column] == 2){
      labeled.3d.duration.ehiyih[3,2] <- labeled.3d.duration.ehiyih[3,2] + 1
    }
    if (testset2[i,column] == 3){
      labeled.3d.duration.ehiyih[3,3] <- labeled.3d.duration.ehiyih[3,3] + 1
    }
  }
}








#Only F1 and F2
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
#'vvv' = ellipsoidal, varying volume, shape, and orientation
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
acc.list.2d.aiu <- c()
for (i in 1:6){
  acc.list.2d.aiu <- c(acc.list.2d.aiu,length(testset$TrueCat[testset$TrueCat == testset[,3+i]]) /length(testset$TrueCat))

}


labeled.2d.aiu <- matrix(0,3,3)
dim <- 2 +1
column <- which.max(acc.list.2d.aiu) + dim 

for(i in 1:length(testset[,dim])){
  if (testset[i,dim] == 1){
    if (testset[i,column] == 1){
      labeled.2d.aiu[1,1] <- labeled.2d.aiu[1,1] + 1
    }
    if (testset[i,column] == 2){
      labeled.2d.aiu[1,2] <- labeled.2d.aiu[1,2] + 1
    }
    if (testset[i,column] == 3){
      labeled.2d.aiu[1,3] <- labeled.2d.aiu[1,3] + 1
    }
  }
  if (testset[i,dim] == 2){
    if (testset[i,column] == 1){
      labeled.2d.aiu[2,1] <- labeled.2d.aiu[2,1] + 1
    }
    if (testset[i,column] == 2){
      labeled.2d.aiu[2,2] <- labeled.2d.aiu[2,2] + 1
    }
    if (testset[i,column] == 3){
      labeled.2d.aiu[2,3] <- labeled.2d.aiu[2,3] + 1
    }
  }
  if (testset[i,dim] == 3){
    if (testset[i,column] == 1){
      labeled.2d.aiu[3,1] <- labeled.2d.aiu[3,1] + 1
    }
    if (testset[i,column] == 2){
      labeled.2d.aiu[3,2] <- labeled.2d.aiu[3,2] + 1
    }
    if (testset[i,column] == 3){
      labeled.2d.aiu[3,3] <- labeled.2d.aiu[3,3] + 1
    }
  }
}









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
acc.list.2d.ehiyih <- c()
for (i in 1:6){
  acc.list.2d.ehiyih <- c(acc.list.2d.ehiyih, length(testset$TrueCat[testset$TrueCat == testset[,3+i]]) /length(testset$TrueCat))

}


labeled.2d.ehiyih <- matrix(0,3,3)
dim <- 2 +1
column <- which.max(acc.list.2d.ehiyih) + dim 

for(i in 1:length(testset[,dim])){
  if (testset[i,dim] == 1){
    if (testset[i,column] == 1){
      labeled.2d.ehiyih[1,1] <- labeled.2d.ehiyih[1,1] + 1
    }
    if (testset[i,column] == 2){
      labeled.2d.ehiyih[1,2] <- labeled.2d.ehiyih[1,2] + 1
    }
    if (testset[i,column] == 3){
      labeled.2d.ehiyih[1,3] <- labeled.2d.ehiyih[1,3] + 1
    }
  }
  if (testset[i,dim] == 2){
    if (testset[i,column] == 1){
      labeled.2d.ehiyih[2,1] <- labeled.2d.ehiyih[2,1] + 1
    }
    if (testset[i,column] == 2){
      labeled.2d.ehiyih[2,2] <- labeled.2d.ehiyih[2,2] + 1
    }
    if (testset[i,column] == 3){
      labeled.2d.ehiyih[2,3] <- labeled.2d.ehiyih[2,3] + 1
    }
  }
  if (testset[i,dim] == 3){
    if (testset[i,column] == 1){
      labeled.2d.ehiyih[3,1] <- labeled.2d.ehiyih[3,1] + 1
    }
    if (testset[i,column] == 2){
      labeled.2d.ehiyih[3,2] <- labeled.2d.ehiyih[3,2] + 1
    }
    if (testset[i,column] == 3){
      labeled.2d.ehiyih[3,3] <- labeled.2d.ehiyih[3,3] + 1
    }
  }
}

