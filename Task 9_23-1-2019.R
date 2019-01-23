setwd('C:/Users/Myrna/Documents/Universiteit Utrecht/Cognitive modeling/Lab 3')

source('Task 1 and 2.R')


##### F3 ########

#Decide the genders
vowels <- data
#vowels <- data

vowels[vowels==0]<-NA
#vowels[vowels==0]<-NA

vowels<- vowels[complete.cases(vowels), ]
#vowels<- vowels[complete.cases(vowels), ]

ahMeanF1 <- mean(vowels$F1.at.steady.state[vowels$vowel=='ah'], na.rm = TRUE) #892.568
ahMeanF2 <- mean(vowels$F2.at.steady.state[vowels$vowel=='ah'], na.rm = TRUE) #1474.309
ahMeanF3 <- mean(vowels$F3.at.steady.state[vowels$vowel=='ah'], na.rm = TRUE)
eiMeanF1 <- mean(vowels$F1.at.steady.state[vowels$vowel=='ei'], na.rm = TRUE) #412
eiMeanF2 <- mean(vowels$F2.at.steady.state[vowels$vowel=='ei'], na.rm = TRUE) #2723.554
eiMeanF3 <- mean(vowels$F3.at.steady.state[vowels$vowel=='ei'], na.rm = TRUE)
uwMeanF1 <- mean(vowels$F1.at.steady.state[vowels$vowel=='uw'], na.rm = TRUE) #444.691
uwMeanF2 <- mean(vowels$F2.at.steady.state[vowels$vowel=='uw'], na.rm = TRUE) #1157.504
uwMeanF3 <- mean(vowels$F3.at.steady.state[vowels$vowel=='uw'], na.rm = TRUE)

ehMeanF1 <- mean(vowels$F1.at.steady.state[vowels$vowel=='eh'], na.rm = TRUE) #892.568
ehMeanF2 <- mean(vowels$F2.at.steady.state[vowels$vowel=='eh'], na.rm = TRUE) #1474.309
ehMeanF3 <- mean(vowels$F3.at.steady.state[vowels$vowel=='eh'], na.rm = TRUE)
iyMeanF1 <- mean(vowels$F1.at.steady.state[vowels$vowel=='iy'], na.rm = TRUE) #412
iyMeanF2 <- mean(vowels$F2.at.steady.state[vowels$vowel=='iy'], na.rm = TRUE) #2723.554
iyMeanF3 <- mean(vowels$F3.at.steady.state[vowels$vowel=='iy'], na.rm = TRUE)
ihMeanF1 <- mean(vowels$F1.at.steady.state[vowels$vowel=='ih'], na.rm = TRUE) #444.691
ihMeanF2 <- mean(vowels$F2.at.steady.state[vowels$vowel=='ih'], na.rm = TRUE) #1157.504
ihMeanF3 <- mean(vowels$F3.at.steady.state[vowels$vowel=='ih'], na.rm = TRUE)

aeMeanF1 <- mean(vowels$F1.at.steady.state[vowels$vowel=='ae'], na.rm = TRUE) #892.568
aeMeanF2 <- mean(vowels$F2.at.steady.state[vowels$vowel=='ae'], na.rm = TRUE) #1474.309
aeMeanF3 <- mean(vowels$F3.at.steady.state[vowels$vowel=='ae'], na.rm = TRUE)
erMeanF1 <- mean(vowels$F1.at.steady.state[vowels$vowel=='er'], na.rm = TRUE) #412
erMeanF2 <- mean(vowels$F2.at.steady.state[vowels$vowel=='er'], na.rm = TRUE) #2723.554
erMeanF3 <- mean(vowels$F3.at.steady.state[vowels$vowel=='er'], na.rm = TRUE)
uhMeanF1 <- mean(vowels$F1.at.steady.state[vowels$vowel=='uh'], na.rm = TRUE) #444.691
uhMeanF2 <- mean(vowels$F2.at.steady.state[vowels$vowel=='uh'], na.rm = TRUE) #1157.504
uhMeanF3 <- mean(vowels$F3.at.steady.state[vowels$vowel=='uh'], na.rm = TRUE)

awMeanF1 <- mean(vowels$F1.at.steady.state[vowels$vowel=='aw'], na.rm = TRUE) #892.568
awMeanF2 <- mean(vowels$F2.at.steady.state[vowels$vowel=='aw'], na.rm = TRUE) #1474.309
awMeanF3 <- mean(vowels$F3.at.steady.state[vowels$vowel=='aw'], na.rm = TRUE)
oaMeanF1 <- mean(vowels$F1.at.steady.state[vowels$vowel=='oa'], na.rm = TRUE) #412
oaMeanF2 <- mean(vowels$F2.at.steady.state[vowels$vowel=='oa'], na.rm = TRUE) #2723.554
oaMeanF3 <- mean(vowels$F3.at.steady.state[vowels$vowel=='oa'], na.rm = TRUE)
ooMeanF1 <- mean(vowels$F1.at.steady.state[vowels$vowel=='oo'], na.rm = TRUE) #444.691
ooMeanF2 <- mean(vowels$F2.at.steady.state[vowels$vowel=='oo'], na.rm = TRUE) #1157.504
ooMeanF3 <- mean(vowels$F3.at.steady.state[vowels$vowel=='oo'], na.rm = TRUE)

#covariance between F1 and F2
ahCovF1F2F3 <- cov(cbind(vowels$F1.at.steady.state[vowels$vowel=='ah'],vowels$F2.at.steady.state[vowels$vowel=='ah'], vowels$F3.at.steady.state[vowels$vowel=='ah']), use = "na.or.complete") 
iyCovF1F2F3 <- cov(cbind(vowels$F1.at.steady.state[vowels$vowel=='iy'],vowels$F2.at.steady.state[vowels$vowel=='iy'],vowels$F3.at.steady.state[vowels$vowel=='iy']), use = "na.or.complete")
uwCovF1F2F3 <- cov(cbind(vowels$F1.at.steady.state[vowels$vowel=='uw'],vowels$F2.at.steady.state[vowels$vowel=='uw'], vowels$F3.at.steady.state[vowels$vowel=='uw']), use = "na.or.complete")

ehCovF1F2F3 <- cov(cbind(vowels$F1.at.steady.state[vowels$vowel=='eh'],vowels$F2.at.steady.state[vowels$vowel=='eh'], vowels$F3.at.steady.state[vowels$vowel=='eh']), use = "na.or.complete") 
eiCovF1F2F3 <- cov(cbind(vowels$F1.at.steady.state[vowels$vowel=='ei'],vowels$F2.at.steady.state[vowels$vowel=='ei'], vowels$F3.at.steady.state[vowels$vowel=='ei']), use = "na.or.complete")
ihCovF1F2F3 <- cov(cbind(vowels$F1.at.steady.state[vowels$vowel=='ih'],vowels$F2.at.steady.state[vowels$vowel=='ih'], vowels$F3.at.steady.state[vowels$vowel=='ih']), use = "na.or.complete")

aeCovF1F2F3 <- cov(cbind(vowels$F1.at.steady.state[vowels$vowel=='ae'],vowels$F2.at.steady.state[vowels$vowel=='ae'], vowels$F3.at.steady.state[vowels$vowel=='ae']), use = "na.or.complete") 
erCovF1F2F3 <- cov(cbind(vowels$F1.at.steady.state[vowels$vowel=='er'],vowels$F2.at.steady.state[vowels$vowel=='er'],vowels$F3.at.steady.state[vowels$vowel=='er']), use = "na.or.complete")
uhCovF1F2F3 <- cov(cbind(vowels$F1.at.steady.state[vowels$vowel=='uh'],vowels$F2.at.steady.state[vowels$vowel=='uh'], vowels$F3.at.steady.state[vowels$vowel=='uh']), use = "na.or.complete")

awCovF1F2F3 <- cov(cbind(vowels$F1.at.steady.state[vowels$vowel=='aw'],vowels$F2.at.steady.state[vowels$vowel=='aw'], vowels$F3.at.steady.state[vowels$vowel=='aw']), use = "na.or.complete") 
oaCovF1F2F3 <- cov(cbind(vowels$F1.at.steady.state[vowels$vowel=='oa'],vowels$F2.at.steady.state[vowels$vowel=='oa'],vowels$F3.at.steady.state[vowels$vowel=='oa']), use = "na.or.complete")
ooCovF1F2F3 <- cov(cbind(vowels$F1.at.steady.state[vowels$vowel=='oo'],vowels$F2.at.steady.state[vowels$vowel=='oo'], vowels$F3.at.steady.state[vowels$vowel=='oo']), use = "na.or.complete")



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

model <- Mclust((trainingset1), modelName="VVV", G=12, prior=priorControl())

#testset1 <- testset1[sample(nrow(testset1), nrow(testset1)), ]
#row.names(testset1) <- NULL
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
  count <- 99
  #while (count > 0){
  #count <- 0
  for (k in 1:12){
    MostCommon <- as.numeric(names(which.max(table(testset2$TrueCat[testset2$Pred==k]))))
    print(paste('Most common label for prediction ', k, ' is ', MostCommon))
    
    print(paste('Changing ', k, ' to ', MostCommon+12))
    print(table(testset2$TrueCat[testset2$Pred==k]))
    count <- count + 1
    testset2$Pred<-replace(testset2$Pred, testset2$Pred == k, 25)
    #testset2$Pred<-replace(testset2$Pred,testset2$Pred == MostCommon+12,k)
    testset2$Pred<-replace(testset2$Pred,testset2$Pred == 25,MostCommon+12)
    #print(paste('k is now ', k, '. MostCommon is now ', MostCommon, '.'))
    #MostCommon <- as.numeric(names(which.max(table(testset2$Pred[testset2$TrueCat==k]))))
    #print(paste('k is now ', k, '. MostCommon is now ', MostCommon, '.'))
  }
  testset2$Pred <- testset2$Pred-12
  
  accuracy = length(testset2$TrueCat[testset2$TrueCat == testset2$Pred]) /length(testset2$TrueCat)
  print(accuracy)
}










####Duration####

vowels <- data
#vowels <- data

vowels[vowels==0]<-NA
#vowels[vowels==0]<-NA

vowels<- vowels[complete.cases(vowels), ]
#vowels<- vowels[complete.cases(vowels), ]

ahMeanF1 <- mean(vowels$F1.at.steady.state[vowels$vowel=='ah'], na.rm = TRUE) #892.568
ahMeanF2 <- mean(vowels$F2.at.steady.state[vowels$vowel=='ah'], na.rm = TRUE) #1474.309
ahMeanF3 <- mean(vowels$duration.in.msec[vowels$vowel=='ah'], na.rm = TRUE)
eiMeanF1 <- mean(vowels$F1.at.steady.state[vowels$vowel=='ei'], na.rm = TRUE) #412
eiMeanF2 <- mean(vowels$F2.at.steady.state[vowels$vowel=='ei'], na.rm = TRUE) #2723.554
eiMeanF3 <- mean(vowels$duration.in.msec[vowels$vowel=='ei'], na.rm = TRUE)
uwMeanF1 <- mean(vowels$F1.at.steady.state[vowels$vowel=='uw'], na.rm = TRUE) #444.691
uwMeanF2 <- mean(vowels$F2.at.steady.state[vowels$vowel=='uw'], na.rm = TRUE) #1157.504
uwMeanF3 <- mean(vowels$duration.in.msec[vowels$vowel=='uw'], na.rm = TRUE)

ehMeanF1 <- mean(vowels$F1.at.steady.state[vowels$vowel=='eh'], na.rm = TRUE) #892.568
ehMeanF2 <- mean(vowels$F2.at.steady.state[vowels$vowel=='eh'], na.rm = TRUE) #1474.309
ehMeanF3 <- mean(vowels$duration.in.msec[vowels$vowel=='eh'], na.rm = TRUE)
iyMeanF1 <- mean(vowels$F1.at.steady.state[vowels$vowel=='iy'], na.rm = TRUE) #412
iyMeanF2 <- mean(vowels$F2.at.steady.state[vowels$vowel=='iy'], na.rm = TRUE) #2723.554
iyMeanF3 <- mean(vowels$duration.in.msec[vowels$vowel=='iy'], na.rm = TRUE)
ihMeanF1 <- mean(vowels$F1.at.steady.state[vowels$vowel=='ih'], na.rm = TRUE) #444.691
ihMeanF2 <- mean(vowels$F2.at.steady.state[vowels$vowel=='ih'], na.rm = TRUE) #1157.504
ihMeanF3 <- mean(vowels$duration.in.msec[vowels$vowel=='ih'], na.rm = TRUE)

aeMeanF1 <- mean(vowels$F1.at.steady.state[vowels$vowel=='ae'], na.rm = TRUE) #892.568
aeMeanF2 <- mean(vowels$F2.at.steady.state[vowels$vowel=='ae'], na.rm = TRUE) #1474.309
aeMeanF3 <- mean(vowels$F3.at.steady.state[vowels$vowel=='ae'], na.rm = TRUE)
erMeanF1 <- mean(vowels$F1.at.steady.state[vowels$vowel=='er'], na.rm = TRUE) #412
erMeanF2 <- mean(vowels$F2.at.steady.state[vowels$vowel=='er'], na.rm = TRUE) #2723.554
erMeanF3 <- mean(vowels$F3.at.steady.state[vowels$vowel=='er'], na.rm = TRUE)
uhMeanF1 <- mean(vowels$F1.at.steady.state[vowels$vowel=='uh'], na.rm = TRUE) #444.691
uhMeanF2 <- mean(vowels$F2.at.steady.state[vowels$vowel=='uh'], na.rm = TRUE) #1157.504
uhMeanF3 <- mean(vowels$F3.at.steady.state[vowels$vowel=='uh'], na.rm = TRUE)

awMeanF1 <- mean(vowels$F1.at.steady.state[vowels$vowel=='aw'], na.rm = TRUE) #892.568
awMeanF2 <- mean(vowels$F2.at.steady.state[vowels$vowel=='aw'], na.rm = TRUE) #1474.309
awMeanF3 <- mean(vowels$duration.in.msec[vowels$vowel=='aw'], na.rm = TRUE)
oaMeanF1 <- mean(vowels$F1.at.steady.state[vowels$vowel=='oa'], na.rm = TRUE) #412
oaMeanF2 <- mean(vowels$F2.at.steady.state[vowels$vowel=='oa'], na.rm = TRUE) #2723.554
oaMeanF3 <- mean(vowels$duration.in.msec[vowels$vowel=='oa'], na.rm = TRUE)
ooMeanF1 <- mean(vowels$F1.at.steady.state[vowels$vowel=='oo'], na.rm = TRUE) #444.691
ooMeanF2 <- mean(vowels$F2.at.steady.state[vowels$vowel=='oo'], na.rm = TRUE) #1157.504
ooMeanF3 <- mean(vowels$duration.in.msec[vowels$vowel=='oo'], na.rm = TRUE)

#covariance between F1 and F2
ahCovF1F2F3 <- cov(cbind(vowels$F1.at.steady.state[vowels$vowel=='ah'],vowels$F2.at.steady.state[vowels$vowel=='ah'], vowels$duration.in.msec[vowels$vowel=='ah']), use = "na.or.complete") 
iyCovF1F2F3 <- cov(cbind(vowels$F1.at.steady.state[vowels$vowel=='iy'],vowels$F2.at.steady.state[vowels$vowel=='iy'],vowels$duration.in.msec[vowels$vowel=='iy']), use = "na.or.complete")
uwCovF1F2F3 <- cov(cbind(vowels$F1.at.steady.state[vowels$vowel=='uw'],vowels$F2.at.steady.state[vowels$vowel=='uw'], vowels$duration.in.msec[vowels$vowel=='uw']), use = "na.or.complete")

ehCovF1F2F3 <- cov(cbind(vowels$F1.at.steady.state[vowels$vowel=='eh'],vowels$F2.at.steady.state[vowels$vowel=='eh'], vowels$duration.in.msec[vowels$vowel=='eh']), use = "na.or.complete") 
eiCovF1F2F3 <- cov(cbind(vowels$F1.at.steady.state[vowels$vowel=='ei'],vowels$F2.at.steady.state[vowels$vowel=='ei'], vowels$duration.in.msec[vowels$vowel=='ei']), use = "na.or.complete")
ihCovF1F2F3 <- cov(cbind(vowels$F1.at.steady.state[vowels$vowel=='ih'],vowels$F2.at.steady.state[vowels$vowel=='ih'], vowels$duration.in.msec[vowels$vowel=='ih']), use = "na.or.complete")

aeCovF1F2F3 <- cov(cbind(vowels$F1.at.steady.state[vowels$vowel=='ae'],vowels$F2.at.steady.state[vowels$vowel=='ae'], vowels$duration.in.msec[vowels$vowel=='ae']), use = "na.or.complete") 
erCovF1F2F3 <- cov(cbind(vowels$F1.at.steady.state[vowels$vowel=='er'],vowels$F2.at.steady.state[vowels$vowel=='er'],vowels$duration.in.msec[vowels$vowel=='er']), use = "na.or.complete")
uhCovF1F2F3 <- cov(cbind(vowels$F1.at.steady.state[vowels$vowel=='uh'],vowels$F2.at.steady.state[vowels$vowel=='uh'], vowels$duration.in.msec[vowels$vowel=='uh']), use = "na.or.complete")

awCovF1F2F3 <- cov(cbind(vowels$F1.at.steady.state[vowels$vowel=='aw'],vowels$F2.at.steady.state[vowels$vowel=='aw'], vowels$duration.in.msec[vowels$vowel=='aw']), use = "na.or.complete") 
oaCovF1F2F3 <- cov(cbind(vowels$F1.at.steady.state[vowels$vowel=='oa'],vowels$F2.at.steady.state[vowels$vowel=='oa'],vowels$duration.in.msec[vowels$vowel=='oa']), use = "na.or.complete")
ooCovF1F2F3 <- cov(cbind(vowels$F1.at.steady.state[vowels$vowel=='oo'],vowels$F2.at.steady.state[vowels$vowel=='oo'], vowels$duration.in.msec[vowels$vowel=='oo']), use = "na.or.complete")



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


model <- Mclust((trainingset1), modelName="VVV", G=12, prior=priorControl())

#testset1 <- testset1[sample(nrow(testset1), nrow(testset1)), ]
#row.names(testset1) <- NULL
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
  colnames(testset2) = c("F1.steady", "F2.steady","Duration", "TrueCat", "Pred")
  
  #calculate proportion of correct responses
  accuracy = length(testset2$TrueCat[testset2$TrueCat == testset2$Pred]) /length(testset2$TrueCat)
  count <- 99
  #while (count > 0){
  #count <- 0
  for (k in 1:12){
    MostCommon <- as.numeric(names(which.max(table(testset2$TrueCat[testset2$Pred==k]))))
    print(paste('Most common label for prediction ', k, ' is ', MostCommon))
    
    print(paste('Changing ', k, ' to ', MostCommon+12))
    print(table(testset2$TrueCat[testset2$Pred==k]))
    count <- count + 1
    testset2$Pred<-replace(testset2$Pred, testset2$Pred == k, 25)
    #testset2$Pred<-replace(testset2$Pred,testset2$Pred == MostCommon+12,k)
    testset2$Pred<-replace(testset2$Pred,testset2$Pred == 25,MostCommon+12)
    #print(paste('k is now ', k, '. MostCommon is now ', MostCommon, '.'))
    #MostCommon <- as.numeric(names(which.max(table(testset2$Pred[testset2$TrueCat==k]))))
    #print(paste('k is now ', k, '. MostCommon is now ', MostCommon, '.'))
  }
  testset2$Pred <- testset2$Pred-12
  
  accuracy = length(testset2$TrueCat[testset2$TrueCat == testset2$Pred]) /length(testset2$TrueCat)
  print(accuracy)
}