setwd('C:/Users/Myrna/Documents/Universiteit Utrecht/Cognitive modeling/Lab 3')

source('Task 1 and 2.R')




#Task 8

#Decide the genders
mostDistincVowels <- data
lessDistinctVowels <- data

mostDistincVowels[mostDistincVowels==0]<-NA
lessDistinctVowels[lessDistinctVowels==0]<-NA

mostDistincVowels<- mostDistincVowels[complete.cases(mostDistincVowels), ]
lessDistinctVowels<- lessDistinctVowels[complete.cases(lessDistinctVowels), ]

ahMeanF1 <- mean(mostDistincVowels$F1.at.steady.state[mostDistincVowels$vowel=='ah'], na.rm = TRUE) #892.568
ahMeanF2 <- mean(mostDistincVowels$F2.at.steady.state[mostDistincVowels$vowel=='ah'], na.rm = TRUE) #1474.309
ahMeanF3 <- mean(mostDistincVowels$F3.at.steady.state[mostDistincVowels$vowel=='ah'], na.rm = TRUE)
eiMeanF1 <- mean(mostDistincVowels$F1.at.steady.state[mostDistincVowels$vowel=='ei'], na.rm = TRUE) #412
eiMeanF2 <- mean(mostDistincVowels$F2.at.steady.state[mostDistincVowels$vowel=='ei'], na.rm = TRUE) #2723.554
eiMeanF3 <- mean(mostDistincVowels$F3.at.steady.state[mostDistincVowels$vowel=='ei'], na.rm = TRUE)
uwMeanF1 <- mean(mostDistincVowels$F1.at.steady.state[mostDistincVowels$vowel=='uw'], na.rm = TRUE) #444.691
uwMeanF2 <- mean(mostDistincVowels$F2.at.steady.state[mostDistincVowels$vowel=='uw'], na.rm = TRUE) #1157.504
uwMeanF3 <- mean(mostDistincVowels$F3.at.steady.state[mostDistincVowels$vowel=='uw'], na.rm = TRUE)

ehMeanF1 <- mean(lessDistinctVowels$F1.at.steady.state[lessDistinctVowels$vowel=='eh'], na.rm = TRUE) #892.568
ehMeanF2 <- mean(lessDistinctVowels$F2.at.steady.state[lessDistinctVowels$vowel=='eh'], na.rm = TRUE) #1474.309
ehMeanF3 <- mean(lessDistinctVowels$F3.at.steady.state[lessDistinctVowels$vowel=='eh'], na.rm = TRUE)
iyMeanF1 <- mean(lessDistinctVowels$F1.at.steady.state[lessDistinctVowels$vowel=='iy'], na.rm = TRUE) #412
iyMeanF2 <- mean(lessDistinctVowels$F2.at.steady.state[lessDistinctVowels$vowel=='iy'], na.rm = TRUE) #2723.554
iyMeanF3 <- mean(lessDistinctVowels$F3.at.steady.state[lessDistinctVowels$vowel=='iy'], na.rm = TRUE)
ihMeanF1 <- mean(lessDistinctVowels$F1.at.steady.state[lessDistinctVowels$vowel=='ih'], na.rm = TRUE) #444.691
ihMeanF2 <- mean(lessDistinctVowels$F2.at.steady.state[lessDistinctVowels$vowel=='ih'], na.rm = TRUE) #1157.504
ihMeanF3 <- mean(lessDistinctVowels$F3.at.steady.state[lessDistinctVowels$vowel=='ih'], na.rm = TRUE)

aeMeanF1 <- mean(mostDistincVowels$F1.at.steady.state[mostDistincVowels$vowel=='ae'], na.rm = TRUE) #892.568
aeMeanF2 <- mean(mostDistincVowels$F2.at.steady.state[mostDistincVowels$vowel=='ae'], na.rm = TRUE) #1474.309
aeMeanF3 <- mean(mostDistincVowels$F3.at.steady.state[mostDistincVowels$vowel=='ae'], na.rm = TRUE)
erMeanF1 <- mean(mostDistincVowels$F1.at.steady.state[mostDistincVowels$vowel=='er'], na.rm = TRUE) #412
erMeanF2 <- mean(mostDistincVowels$F2.at.steady.state[mostDistincVowels$vowel=='er'], na.rm = TRUE) #2723.554
erMeanF3 <- mean(mostDistincVowels$F3.at.steady.state[mostDistincVowels$vowel=='er'], na.rm = TRUE)
uhMeanF1 <- mean(mostDistincVowels$F1.at.steady.state[mostDistincVowels$vowel=='uh'], na.rm = TRUE) #444.691
uhMeanF2 <- mean(mostDistincVowels$F2.at.steady.state[mostDistincVowels$vowel=='uh'], na.rm = TRUE) #1157.504
uhMeanF3 <- mean(mostDistincVowels$F3.at.steady.state[mostDistincVowels$vowel=='uh'], na.rm = TRUE)

awMeanF1 <- mean(mostDistincVowels$F1.at.steady.state[mostDistincVowels$vowel=='aw'], na.rm = TRUE) #892.568
awMeanF2 <- mean(mostDistincVowels$F2.at.steady.state[mostDistincVowels$vowel=='aw'], na.rm = TRUE) #1474.309
awMeanF3 <- mean(mostDistincVowels$F3.at.steady.state[mostDistincVowels$vowel=='aw'], na.rm = TRUE)
oaMeanF1 <- mean(mostDistincVowels$F1.at.steady.state[mostDistincVowels$vowel=='oa'], na.rm = TRUE) #412
oaMeanF2 <- mean(mostDistincVowels$F2.at.steady.state[mostDistincVowels$vowel=='oa'], na.rm = TRUE) #2723.554
oaMeanF3 <- mean(mostDistincVowels$F3.at.steady.state[mostDistincVowels$vowel=='oa'], na.rm = TRUE)
ooMeanF1 <- mean(mostDistincVowels$F1.at.steady.state[mostDistincVowels$vowel=='oo'], na.rm = TRUE) #444.691
ooMeanF2 <- mean(mostDistincVowels$F2.at.steady.state[mostDistincVowels$vowel=='oo'], na.rm = TRUE) #1157.504
ooMeanF3 <- mean(mostDistincVowels$F3.at.steady.state[mostDistincVowels$vowel=='oo'], na.rm = TRUE)

#covariance between F1 and F2
ahCovF1F2F3 <- cov(cbind(mostDistincVowels$F1.at.steady.state[mostDistincVowels$vowel=='ah'],mostDistincVowels$F2.at.steady.state[mostDistincVowels$vowel=='ah'], mostDistincVowels$F3.at.steady.state[mostDistincVowels$vowel=='ah']), use = "na.or.complete") 
iyCovF1F2F3 <- cov(cbind(mostDistincVowels$F1.at.steady.state[mostDistincVowels$vowel=='iy'],mostDistincVowels$F2.at.steady.state[mostDistincVowels$vowel=='iy'],mostDistincVowels$F3.at.steady.state[mostDistincVowels$vowel=='iy']), use = "na.or.complete")
uwCovF1F2F3 <- cov(cbind(mostDistincVowels$F1.at.steady.state[mostDistincVowels$vowel=='uw'],mostDistincVowels$F2.at.steady.state[mostDistincVowels$vowel=='uw'], mostDistincVowels$F3.at.steady.state[mostDistincVowels$vowel=='uw']), use = "na.or.complete")

ehCovF1F2F3 <- cov(cbind(lessDistinctVowels$F1.at.steady.state[lessDistinctVowels$vowel=='eh'],lessDistinctVowels$F2.at.steady.state[lessDistinctVowels$vowel=='eh'], lessDistinctVowels$F3.at.steady.state[lessDistinctVowels$vowel=='eh']), use = "na.or.complete") 
eiCovF1F2F3 <- cov(cbind(lessDistinctVowels$F1.at.steady.state[lessDistinctVowels$vowel=='ei'],lessDistinctVowels$F2.at.steady.state[lessDistinctVowels$vowel=='ei'], lessDistinctVowels$F3.at.steady.state[lessDistinctVowels$vowel=='ei']), use = "na.or.complete")
ihCovF1F2F3 <- cov(cbind(lessDistinctVowels$F1.at.steady.state[lessDistinctVowels$vowel=='ih'],lessDistinctVowels$F2.at.steady.state[lessDistinctVowels$vowel=='ih'], lessDistinctVowels$F3.at.steady.state[lessDistinctVowels$vowel=='ih']), use = "na.or.complete")

aeCovF1F2F3 <- cov(cbind(mostDistincVowels$F1.at.steady.state[mostDistincVowels$vowel=='ae'],mostDistincVowels$F2.at.steady.state[mostDistincVowels$vowel=='ae'], mostDistincVowels$F3.at.steady.state[mostDistincVowels$vowel=='ae']), use = "na.or.complete") 
erCovF1F2F3 <- cov(cbind(mostDistincVowels$F1.at.steady.state[mostDistincVowels$vowel=='er'],mostDistincVowels$F2.at.steady.state[mostDistincVowels$vowel=='er'],mostDistincVowels$F3.at.steady.state[mostDistincVowels$vowel=='er']), use = "na.or.complete")
uhCovF1F2F3 <- cov(cbind(mostDistincVowels$F1.at.steady.state[mostDistincVowels$vowel=='uh'],mostDistincVowels$F2.at.steady.state[mostDistincVowels$vowel=='uh'], mostDistincVowels$F3.at.steady.state[mostDistincVowels$vowel=='uh']), use = "na.or.complete")

awCovF1F2F3 <- cov(cbind(mostDistincVowels$F1.at.steady.state[mostDistincVowels$vowel=='aw'],mostDistincVowels$F2.at.steady.state[mostDistincVowels$vowel=='aw'], mostDistincVowels$F3.at.steady.state[mostDistincVowels$vowel=='aw']), use = "na.or.complete") 
oaCovF1F2F3 <- cov(cbind(mostDistincVowels$F1.at.steady.state[mostDistincVowels$vowel=='oa'],mostDistincVowels$F2.at.steady.state[mostDistincVowels$vowel=='oa'],mostDistincVowels$F3.at.steady.state[mostDistincVowels$vowel=='oa']), use = "na.or.complete")
ooCovF1F2F3 <- cov(cbind(mostDistincVowels$F1.at.steady.state[mostDistincVowels$vowel=='oo'],mostDistincVowels$F2.at.steady.state[mostDistincVowels$vowel=='oo'], mostDistincVowels$F3.at.steady.state[mostDistincVowels$vowel=='oo']), use = "na.or.complete")



#trainingset
train.ah <- data.frame(mvrnorm(2000, mu = c(ahMeanF1,ahMeanF2, ahMeanF3), Sigma = ahCovF1F2F3))
train.iy <- data.frame(mvrnorm(2000, mu = c(iyMeanF1,iyMeanF2, iyMeanF3), Sigma = iyCovF1F2F3))
train.uw <- data.frame(mvrnorm(2000, mu = c(uwMeanF1,uwMeanF2, uwMeanF3), Sigma = uwCovF1F2F3))

train.eh <- data.frame(mvrnorm(2000, mu = c(ehMeanF1,ehMeanF2, ehMeanF3), Sigma = ehCovF1F2F3))
train.ei <- data.frame(mvrnorm(2000, mu = c(eiMeanF1,eiMeanF2, eiMeanF3), Sigma = eiCovF1F2F3))
train.ih <- data.frame(mvrnorm(2000, mu = c(ihMeanF1,ihMeanF2, ihMeanF3), Sigma = ihCovF1F2F3))

train.ae <- data.frame(mvrnorm(2000, mu = c(aeMeanF1,aeMeanF2, aeMeanF3), Sigma = aeCovF1F2F3))
train.er <- data.frame(mvrnorm(2000, mu = c(erMeanF1,erMeanF2, erMeanF3), Sigma = erCovF1F2F3))
train.uh <- data.frame(mvrnorm(2000, mu = c(uhMeanF1,uhMeanF2, uhMeanF3), Sigma = uhCovF1F2F3))

train.aw <- data.frame(mvrnorm(2000, mu = c(awMeanF1,awMeanF2, awMeanF3), Sigma = awCovF1F2F3))
train.oa <- data.frame(mvrnorm(2000, mu = c(oaMeanF1,oaMeanF2, oaMeanF3), Sigma = oaCovF1F2F3))
train.oo <- data.frame(mvrnorm(2000, mu = c(ooMeanF1,ooMeanF2, ooMeanF3), Sigma = ooCovF1F2F3))


trainingset1 <- rbind(train.ah, train.iy, train.uw, train.eh, train.ei, train.ih, train.ae, train.er, train.uh, train.aw, train.oa, train.oo)

#testset
test.ah <- data.frame(mvrnorm(2000, mu = c(ahMeanF1,ahMeanF2, ahMeanF3), Sigma = ahCovF1F2F3))
test.iy <- data.frame(mvrnorm(2000, mu = c(iyMeanF1,iyMeanF2, iyMeanF3), Sigma = iyCovF1F2F3))
test.uw <- data.frame(mvrnorm(2000, mu = c(uwMeanF1,uwMeanF2, uwMeanF3), Sigma = uwCovF1F2F3))

test.eh <- data.frame(mvrnorm(2000, mu = c(ehMeanF1,ehMeanF2, ehMeanF3), Sigma = ehCovF1F2F3))
test.ei <- data.frame(mvrnorm(2000, mu = c(eiMeanF1,eiMeanF2, eiMeanF3), Sigma = eiCovF1F2F3))
test.ih <- data.frame(mvrnorm(2000, mu = c(ihMeanF1,ihMeanF2, ihMeanF3), Sigma = ihCovF1F2F3))

test.ae <- data.frame(mvrnorm(2000, mu = c(aeMeanF1,aeMeanF2, aeMeanF3), Sigma = aeCovF1F2F3))
test.er <- data.frame(mvrnorm(2000, mu = c(erMeanF1,erMeanF2, erMeanF3), Sigma = erCovF1F2F3))
test.uh <- data.frame(mvrnorm(2000, mu = c(uhMeanF1,uhMeanF2, uhMeanF3), Sigma = uhCovF1F2F3))

test.aw <- data.frame(mvrnorm(2000, mu = c(awMeanF1,awMeanF2, awMeanF3), Sigma = awCovF1F2F3))
test.oa <- data.frame(mvrnorm(2000, mu = c(oaMeanF1,oaMeanF2, oaMeanF3), Sigma = oaCovF1F2F3))
test.oo <- data.frame(mvrnorm(2000, mu = c(ooMeanF1,ooMeanF2, ooMeanF3), Sigma = ooCovF1F2F3))

test.ah$label <- 1
test.iy$label <- 2
test.uw$label <- 3

test.eh$label <- 4
test.ei$label <- 5
test.ih$label <- 6

test.ae$label <- 7
test.er$label <- 8
test.uh$label <- 9

test.aw$label <- 10
test.oa$label <- 11
test.oo$label <- 12

testset1 <- rbind(test.ah, test.iy, test.uw, test.eh, test.ei, test.ih, test.ae, test.er, test.uh, test.aw, test.oa, test.oo)

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

model <- Mclust((trainingset1), modelName="VVV", G=12,prior=priorControl())

testset1 <- testset1[sample(nrow(testset1), nrow(testset1)), ]
row.names(testset1) <- NULL
oldaccuracy <- 0
count <- 1
for (i in 1:1){
  
#INTERMEZZO 
#fit a model with 3 clusters
#'vvv' = ellipsoidal, varying volume, shape, and orientation

#check the parameters
#model$parameters
  # model <- Mclust((trainingset1), modelName="VVV", G=12,prior=priorControl())
  
#predict the categories of the test-set with the parameters
#this gives a probabilistic category assignment
predictions.prob <- estep("VVV", data=testset1[,1:3],parameters=model$parameters)

#Maximum likelihood criterium to pick a discrete prediction
#This results in a vector with the category predictions
predictions.disc <- apply(predictions.prob$z, 1, function(x) which(x ==max(x)))

#add the predictions to the testset1
testset2 = data.frame(cbind(testset1, predictions.disc))
colnames(testset2) = c("F1.steady", "F2.steady","F3.steady", "TrueCat", "Pred")

#calculate proportion of correct responses
accuracy = length(testset2$TrueCat[testset2$TrueCat == testset2$Pred]) /length(testset2$TrueCat)
for (l in 1:10){
count <- 0
for (k in 1:12){
  
  MostCommon <- as.numeric(names(which.max(table(testset2$Pred[testset2$TrueCat==k]))))
  print(MostCommon)
  if (MostCommon != k){
    count <- count + 1
    testset2$Pred<-replace(testset2$Pred, testset2$Pred == k, 24)
    testset2$Pred<-replace(testset2$Pred,testset2$Pred == MostCommon,k)
    testset2$Pred<-replace(testset2$Pred,testset2$Pred == 24,MostCommon)
    print(k)
    MostCommon <- as.numeric(names(which.max(table(testset2$Pred[testset2$TrueCat==k]))))
    print(MostCommon)
  }
  
}
as.numeric(names(which.max(table(testset2$Pred[testset2$TrueCat==1]))))
print('count')
print(count)


print(accuracy)

# char.var <- c(1,2,3,4,5,6,7,8,9,10,11,12)
# df = as.data.frame(permutations(n=length(char.var), r=12, v=char.var))
# if (accuracy > oldaccuracy){
#   print(count)
#   finaltestset1 <- testset2
#   oldaccuracy <- accuracy
#   oldaccuracy1 <- accuracy
#   print(accuracy)
# }
#   count = count + 1
}
}








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
acc.list.3d.F3  <- c()
for (i in 1:6){
  acc.list.3d.F3  <- c(acc.list.3d.F3 ,length(testset1$TrueCat[testset1$TrueCat == testset1[,4+i]]) /length(testset1$TrueCat))
  
}
#.998 for a.i.u

task9labeled.3d.F3  <- matrix(0,3,3)
dim <- 3+1
column <- which.max(acc.list.3d.F3 ) + dim 

for(i in 1:length(testset1[,dim])){
  if (testset1[i,dim] == 1){
    if (testset1[i,column] == 1){
      task9labeled.3d.F3 [1,1] <- task9labeled.3d.F3 [1,1] + 1
    }
    if (testset1[i,column] == 2){
      task9labeled.3d.F3 [1,2] <- task9labeled.3d.F3 [1,2] + 1
    }
    if (testset1[i,column] == 3){
      task9labeled.3d.F3 [1,3] <- task9labeled.3d.F3 [1,3] + 1
    }
  }
  if (testset1[i,dim] == 2){
    if (testset1[i,column] == 1){
      task9labeled.3d.F3 [2,1] <- task9labeled.3d.F3 [2,1] + 1
    }
    if (testset1[i,column] == 2){
      task9labeled.3d.F3 [2,2] <- task9labeled.3d.F3 [2,2] + 1
    }
    if (testset1[i,column] == 3){
      task9labeled.3d.F3 [2,3] <- task9labeled.3d.F3 [2,3] + 1
    }
  }
  if (testset1[i,dim] == 3){
    if (testset1[i,column] == 1){
      task9labeled.3d.F3 [3,1] <- task9labeled.3d.F3 [3,1] + 1
    }
    if (testset1[i,column] == 2){
      task9labeled.3d.F3 [3,2] <- task9labeled.3d.F3 [3,2] + 1
    }
    if (testset1[i,column] == 3){
      task9labeled.3d.F3 [3,3] <- task9labeled.3d.F3 [3,3] + 1
    }
  }
}



ahMeanF1 <- mean(mostDistincVowels$F1.at.steady.state[mostDistincVowels$vowel=='ah'], na.rm = TRUE) #892.568
ahMeanF2 <- mean(mostDistincVowels$F2.at.steady.state[mostDistincVowels$vowel=='ah'], na.rm = TRUE) #1474.309
ahMeanF3 <- mean(mostDistincVowels$F3.at.steady.state[mostDistincVowels$vowel=='ah'], na.rm = TRUE)
eiMeanF1 <- mean(mostDistincVowels$F1.at.steady.state[mostDistincVowels$vowel=='ei'], na.rm = TRUE) #412
eiMeanF2 <- mean(mostDistincVowels$F2.at.steady.state[mostDistincVowels$vowel=='ei'], na.rm = TRUE) #2723.554
eiMeanF3 <- mean(mostDistincVowels$F3.at.steady.state[mostDistincVowels$vowel=='ei'], na.rm = TRUE)
uwMeanF1 <- mean(mostDistincVowels$F1.at.steady.state[mostDistincVowels$vowel=='uw'], na.rm = TRUE) #444.691
uwMeanF2 <- mean(mostDistincVowels$F2.at.steady.state[mostDistincVowels$vowel=='uw'], na.rm = TRUE) #1157.504
uwMeanF3 <- mean(mostDistincVowels$F3.at.steady.state[mostDistincVowels$vowel=='uw'], na.rm = TRUE)

ehMeanF1 <- mean(lessDistinctVowels$F1.at.steady.state[lessDistinctVowels$vowel=='eh'], na.rm = TRUE) #892.568
ehMeanF2 <- mean(lessDistinctVowels$F2.at.steady.state[lessDistinctVowels$vowel=='eh'], na.rm = TRUE) #1474.309
ehMeanF3 <- mean(lessDistinctVowels$F3.at.steady.state[lessDistinctVowels$vowel=='eh'], na.rm = TRUE)
iyMeanF1 <- mean(lessDistinctVowels$F1.at.steady.state[lessDistinctVowels$vowel=='iy'], na.rm = TRUE) #412
iyMeanF2 <- mean(lessDistinctVowels$F2.at.steady.state[lessDistinctVowels$vowel=='iy'], na.rm = TRUE) #2723.554
iyMeanF3 <- mean(lessDistinctVowels$F3.at.steady.state[lessDistinctVowels$vowel=='iy'], na.rm = TRUE)
ihMeanF1 <- mean(lessDistinctVowels$F1.at.steady.state[lessDistinctVowels$vowel=='ih'], na.rm = TRUE) #444.691
ihMeanF2 <- mean(lessDistinctVowels$F2.at.steady.state[lessDistinctVowels$vowel=='ih'], na.rm = TRUE) #1157.504
ihMeanF3 <- mean(lessDistinctVowels$F3.at.steady.state[lessDistinctVowels$vowel=='ih'], na.rm = TRUE)

aeMeanF1 <- mean(mostDistincVowels$F1.at.steady.state[mostDistincVowels$vowel=='ae'], na.rm = TRUE) #892.568
aeMeanF2 <- mean(mostDistincVowels$F2.at.steady.state[mostDistincVowels$vowel=='ae'], na.rm = TRUE) #1474.309
aeMeanF3 <- mean(mostDistincVowels$F3.at.steady.state[mostDistincVowels$vowel=='ae'], na.rm = TRUE)
erMeanF1 <- mean(mostDistincVowels$F1.at.steady.state[mostDistincVowels$vowel=='er'], na.rm = TRUE) #412
erMeanF2 <- mean(mostDistincVowels$F2.at.steady.state[mostDistincVowels$vowel=='er'], na.rm = TRUE) #2723.554
erMeanF3 <- mean(mostDistincVowels$F3.at.steady.state[mostDistincVowels$vowel=='er'], na.rm = TRUE)
uhMeanF1 <- mean(mostDistincVowels$F1.at.steady.state[mostDistincVowels$vowel=='uh'], na.rm = TRUE) #444.691
uhMeanF2 <- mean(mostDistincVowels$F2.at.steady.state[mostDistincVowels$vowel=='uh'], na.rm = TRUE) #1157.504
uhMeanF3 <- mean(mostDistincVowels$F3.at.steady.state[mostDistincVowels$vowel=='uh'], na.rm = TRUE)

awMeanF1 <- mean(mostDistincVowels$F1.at.steady.state[mostDistincVowels$vowel=='aw'], na.rm = TRUE) #892.568
awMeanF2 <- mean(mostDistincVowels$F2.at.steady.state[mostDistincVowels$vowel=='aw'], na.rm = TRUE) #1474.309
awMeanF3 <- mean(mostDistincVowels$F3.at.steady.state[mostDistincVowels$vowel=='aw'], na.rm = TRUE)
oaMeanF1 <- mean(mostDistincVowels$F1.at.steady.state[mostDistincVowels$vowel=='oa'], na.rm = TRUE) #412
oaMeanF2 <- mean(mostDistincVowels$F2.at.steady.state[mostDistincVowels$vowel=='oa'], na.rm = TRUE) #2723.554
oaMeanF3 <- mean(mostDistincVowels$F3.at.steady.state[mostDistincVowels$vowel=='oa'], na.rm = TRUE)
uhMeanF1 <- mean(mostDistincVowels$F1.at.steady.state[mostDistincVowels$vowel=='uh'], na.rm = TRUE) #444.691
uhMeanF2 <- mean(mostDistincVowels$F2.at.steady.state[mostDistincVowels$vowel=='uh'], na.rm = TRUE) #1157.504
uhMeanF3 <- mean(mostDistincVowels$F3.at.steady.state[mostDistincVowels$vowel=='uh'], na.rm = TRUE)

#covariance between F1 and F2
ahCovF1F2F3 <- cov(cbind(mostDistincVowels$F1.at.steady.state[mostDistincVowels$vowel=='ah'],mostDistincVowels$F2.at.steady.state[mostDistincVowels$vowel=='ah'], mostDistincVowels$F3.at.steady.state[mostDistincVowels$vowel=='ah']), use = "na.or.complete") 
iyCovF1F2F3 <- cov(cbind(mostDistincVowels$F1.at.steady.state[mostDistincVowels$vowel=='iy'],mostDistincVowels$F2.at.steady.state[mostDistincVowels$vowel=='iy'],mostDistincVowels$F3.at.steady.state[mostDistincVowels$vowel=='iy']), use = "na.or.complete")
uwCovF1F2F3 <- cov(cbind(mostDistincVowels$F1.at.steady.state[mostDistincVowels$vowel=='uw'],mostDistincVowels$F2.at.steady.state[mostDistincVowels$vowel=='uw'], mostDistincVowels$F3.at.steady.state[mostDistincVowels$vowel=='uw']), use = "na.or.complete")

ehCovF1F2F3 <- cov(cbind(lessDistinctVowels$F1.at.steady.state[lessDistinctVowels$vowel=='eh'],lessDistinctVowels$F2.at.steady.state[lessDistinctVowels$vowel=='eh'], lessDistinctVowels$F3.at.steady.state[lessDistinctVowels$vowel=='eh']), use = "na.or.complete") 
eiCovF1F2F3 <- cov(cbind(lessDistinctVowels$F1.at.steady.state[lessDistinctVowels$vowel=='ei'],lessDistinctVowels$F2.at.steady.state[lessDistinctVowels$vowel=='ei'], lessDistinctVowels$F3.at.steady.state[lessDistinctVowels$vowel=='ei']), use = "na.or.complete")
ihCovF1F2F3 <- cov(cbind(lessDistinctVowels$F1.at.steady.state[lessDistinctVowels$vowel=='ih'],lessDistinctVowels$F2.at.steady.state[lessDistinctVowels$vowel=='ih'], lessDistinctVowels$F3.at.steady.state[lessDistinctVowels$vowel=='ih']), use = "na.or.complete")

aeCovF1F2F3 <- cov(cbind(mostDistincVowels$F1.at.steady.state[mostDistincVowels$vowel=='ae'],mostDistincVowels$F2.at.steady.state[mostDistincVowels$vowel=='ae'], mostDistincVowels$F3.at.steady.state[mostDistincVowels$vowel=='ae']), use = "na.or.complete") 
erCovF1F2F3 <- cov(cbind(mostDistincVowels$F1.at.steady.state[mostDistincVowels$vowel=='er'],mostDistincVowels$F2.at.steady.state[mostDistincVowels$vowel=='er'],mostDistincVowels$F3.at.steady.state[mostDistincVowels$vowel=='ir']), use = "na.or.complete")
uhCovF1F2F3 <- cov(cbind(mostDistincVowels$F1.at.steady.state[mostDistincVowels$vowel=='uh'],mostDistincVowels$F2.at.steady.state[mostDistincVowels$vowel=='uh'], mostDistincVowels$F3.at.steady.state[mostDistincVowels$vowel=='uh']), use = "na.or.complete")

awCovF1F2F3 <- cov(cbind(mostDistincVowels$F1.at.steady.state[mostDistincVowels$vowel=='aw'],mostDistincVowels$F2.at.steady.state[mostDistincVowels$vowel=='aw'], mostDistincVowels$F3.at.steady.state[mostDistincVowels$vowel=='aw']), use = "na.or.complete") 
oaCovF1F2F3 <- cov(cbind(mostDistincVowels$F1.at.steady.state[mostDistincVowels$vowel=='oa'],mostDistincVowels$F2.at.steady.state[mostDistincVowels$vowel=='oa'],mostDistincVowels$F3.at.steady.state[mostDistincVowels$vowel=='oa']), use = "na.or.complete")
uhCovF1F2F3 <- cov(cbind(mostDistincVowels$F1.at.steady.state[mostDistincVowels$vowel=='uh'],mostDistincVowels$F2.at.steady.state[mostDistincVowels$vowel=='uh'], mostDistincVowels$F3.at.steady.state[mostDistincVowels$vowel=='uh']), use = "na.or.complete")



#trainingset
train.ah <- data.frame(mvrnorm(2000, mu = c(ahMeanF1,ahMeanF2, ahMeanF3), Sigma = ahCovF1F2F3))
train.iy <- data.frame(mvrnorm(2000, mu = c(iyMeanF1,iyMeanF2, iyMeanF3), Sigma = iyCovF1F2F3))
train.uw <- data.frame(mvrnorm(2000, mu = c(uwMeanF1,uwMeanF2, uwMeanF3), Sigma = uwCovF1F2F3))

train.eh <- data.frame(mvrnorm(2000, mu = c(ehMeanF1,ehMeanF2, ehMeanF3), Sigma = ehCovF1F2F3))
train.ei <- data.frame(mvrnorm(2000, mu = c(eiMeanF1,eiMeanF2, eiMeanF3), Sigma = eiCovF1F2F3))
train.ih <- data.frame(mvrnorm(2000, mu = c(ihMeanF1,ihMeanF2, ihMeanF3), Sigma = ihCovF1F2F3))

train.ae <- data.frame(mvrnorm(2000, mu = c(aeMeanF1,aeMeanF2, aeMeanF3), Sigma = aeCovF1F2F3))
train.er <- data.frame(mvrnorm(2000, mu = c(erMeanF1,erMeanF2, erMeanF3), Sigma = erCovF1F2F3))
train.uh <- data.frame(mvrnorm(2000, mu = c(uhMeanF1,uhMeanF2, uhMeanF3), Sigma = uhCovF1F2F3))

train.aw <- data.frame(mvrnorm(2000, mu = c(awMeanF1,awMeanF2, awMeanF3), Sigma = awCovF1F2F3))
train.oa <- data.frame(mvrnorm(2000, mu = c(oaMeanF1,oaMeanF2, oaMeanF3), Sigma = oaCovF1F2F3))
train.uh <- data.frame(mvrnorm(2000, mu = c(uhMeanF1,uhMeanF2, uhMeanF3), Sigma = uhCovF1F2F3))


trainingset1 <- rbind(train.ah, train.iy, train.uw, train.eh, train.ei, train.ih, train.ae, train.er, train.uh, train.aw, train.oa, train.uh)

#testset
test.ah <- data.frame(mvrnorm(2000, mu = c(ahMeanF1,ahMeanF2, ahMeanF3), Sigma = ahCovF1F2F3))
test.iy <- data.frame(mvrnorm(2000, mu = c(iyMeanF1,iyMeanF2, iyMeanF3), Sigma = iyCovF1F2F3))
test.uw <- data.frame(mvrnorm(2000, mu = c(uwMeanF1,uwMeanF2, uwMeanF3), Sigma = uwCovF1F2F3))

test.eh <- data.frame(mvrnorm(2000, mu = c(ehMeanF1,ehMeanF2, ehMeanF3), Sigma = ehCovF1F2F3))
test.ei <- data.frame(mvrnorm(2000, mu = c(eiMeanF1,eiMeanF2, eiMeanF3), Sigma = eiCovF1F2F3))
test.ih <- data.frame(mvrnorm(2000, mu = c(ihMeanF1,ihMeanF2, ihMeanF3), Sigma = ihCovF1F2F3))

test.ae <- data.frame(mvrnorm(2000, mu = c(aeMeanF1,aeMeanF2, aeMeanF3), Sigma = aeCovF1F2F3))
test.er <- data.frame(mvrnorm(2000, mu = c(erMeanF1,erMeanF2, erMeanF3), Sigma = erCovF1F2F3))
test.uh <- data.frame(mvrnorm(2000, mu = c(uhMeanF1,uhMeanF2, uhMeanF3), Sigma = uhCovF1F2F3))

test.aw <- data.frame(mvrnorm(2000, mu = c(awMeanF1,awMeanF2, awMeanF3), Sigma = awCovF1F2F3))
test.oa <- data.frame(mvrnorm(2000, mu = c(oaMeanF1,oaMeanF2, oaMeanF3), Sigma = oaCovF1F2F3))
test.uh <- data.frame(mvrnorm(2000, mu = c(uhMeanF1,uhMeanF2, uhMeanF3), Sigma = uhCovF1F2F3))

test.ah$label <- 1
test.iy$label <- 2
test.uw$label <- 3

test.eh$label <- 4
test.ei$label <- 5
test.ih$label <- 6

test.ar$label <- 7
test.er$label <- 8
test.uh$label <- 9

test.aw$label <- 10
test.ow$label <- 11
test.uh$label <- 12

testset1 <- rbind(test.ah, test.iy, test.uw, test.eh, test.ei, test.ih, test.ae, test.er, test.uh, test.aw, test.oa, test.uh)


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
acc.list.3d.duration  <- c()
for (i in 1:6){
  acc.list.3d.duration  <- c(acc.list.3d.duration , length(testset1$TrueCat[testset1$TrueCat == testset1[,4+i]]) /length(testset1$TrueCat))
}
#.999 for a.i.u

task9labeled.3d.duration  <- matrix(0,3,3)
dim <- 3 +1
column <- which.max(acc.list.3d.duration ) + dim 

for(i in 1:length(testset1[,dim])){
  if (testset1[i,dim] == 1){
    if (testset1[i,column] == 1){
      task9labeled.3d.duration [1,1] <- task9labeled.3d.duration [1,1] + 1
    }
    if (testset1[i,column] == 2){
      task9labeled.3d.duration [1,2] <- task9labeled.3d.duration [1,2] + 1
    }
    if (testset1[i,column] == 3){
      task9labeled.3d.duration [1,3] <- task9labeled.3d.duration [1,3] + 1
    }
  }
  if (testset1[i,dim] == 2){
    if (testset1[i,column] == 1){
      task9labeled.3d.duration [2,1] <- task9labeled.3d.duration [2,1] + 1
    }
    if (testset1[i,column] == 2){
      task9labeled.3d.duration [2,2] <- task9labeled.3d.duration [2,2] + 1
    }
    if (testset1[i,column] == 3){
      task9labeled.3d.duration [2,3] <- task9labeled.3d.duration [2,3] + 1
    }
  }
  if (testset1[i,dim] == 3){
    if (testset1[i,column] == 1){
      task9labeled.3d.duration [3,1] <- task9labeled.3d.duration [3,1] + 1
    }
    if (testset1[i,column] == 2){
      task9labeled.3d.duration [3,2] <- task9labeled.3d.duration [3,2] + 1
    }
    if (testset1[i,column] == 3){
      task9labeled.3d.duration [3,3] <- task9labeled.3d.duration [3,3] + 1
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
acc.list.2d  <- c()
for (i in 1:6){
  acc.list.2d  <- c(acc.list.2d ,length(testset$TrueCat[testset$TrueCat == testset[,3+i]]) /length(testset$TrueCat))
  
}


task9labeled.2d  <- matrix(0,3,3)
dim <- 2 +1
column <- which.max(acc.list.2d ) + dim 

for(i in 1:length(testset[,dim])){
  if (testset[i,dim] == 1){
    if (testset[i,column] == 1){
      task9labeled.2d [1,1] <- task9labeled.2d [1,1] + 1
    }
    if (testset[i,column] == 2){
      task9labeled.2d [1,2] <- task9labeled.2d [1,2] + 1
    }
    if (testset[i,column] == 3){
      task9labeled.2d [1,3] <- task9labeled.2d [1,3] + 1
    }
  }
  if (testset[i,dim] == 2){
    if (testset[i,column] == 1){
      task9labeled.2d [2,1] <- task9labeled.2d [2,1] + 1
    }
    if (testset[i,column] == 2){
      task9labeled.2d [2,2] <- task9labeled.2d [2,2] + 1
    }
    if (testset[i,column] == 3){
      task9labeled.2d [2,3] <- task9labeled.2d [2,3] + 1
    }
  }
  if (testset[i,dim] == 3){
    if (testset[i,column] == 1){
      task9labeled.2d [3,1] <- task9labeled.2d [3,1] + 1
    }
    if (testset[i,column] == 2){
      task9labeled.2d [3,2] <- task9labeled.2d [3,2] + 1
    }
    if (testset[i,column] == 3){
      task9labeled.2d [3,3] <- task9labeled.2d [3,3] + 1
    }
  }
}

