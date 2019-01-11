setwd('C:/Users/Myrna/Documents/Universiteit Utrecht/Cognitive modeling/Lab 3')

library(MASS)

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


#Task 3
aMeanF1 <- mean(mostDistincVowels$F1.at.steady.state[mostDistincVowels$vowel=='ah']) #892.568
aMeanF2 <- mean(mostDistincVowels$F2.at.steady.state[mostDistincVowels$vowel=='ah']) #1474.309
iMeanF1 <- mean(mostDistincVowels$F1.at.steady.state[mostDistincVowels$vowel=='iy']) #412
iMeanF2 <- mean(mostDistincVowels$F2.at.steady.state[mostDistincVowels$vowel=='iy']) #2723.554
uMeanF1 <- mean(mostDistincVowels$F1.at.steady.state[mostDistincVowels$vowel=='uw']) #444.691
uMeanF2 <- mean(mostDistincVowels$F2.at.steady.state[mostDistincVowels$vowel=='uw']) #1157.504

#covariance between what?
#F1 and F2?
aCovF1F2 <- cov(mostDistincVowels$F1.at.steady.state[mostDistincVowels$vowel=='ah'],mostDistincVowels$F2.at.steady.state[mostDistincVowels$vowel=='ah']) 
iCovF1F2 <- cov(mostDistincVowels$F1.at.steady.state[mostDistincVowels$vowel=='iy'],mostDistincVowels$F2.at.steady.state[mostDistincVowels$vowel=='iy']) 
uCovF1F2 <- cov(mostDistincVowels$F1.at.steady.state[mostDistincVowels$vowel=='uw'],mostDistincVowels$F2.at.steady.state[mostDistincVowels$vowel=='uw']) 

#a, i, and u?....
aVowel  <- c(aMeanF1,aMeanF2)
iVowel <- c(iMeanF1,iMeanF2)
uVowel <- c(uMeanF1,uMeanF2)
meanData <- cbind(aVowel, iVowel,uVowel)
cov(meanData)

#I think they mean correlation
aCorF1F2 <- cor(mostDistincVowels$F1.at.steady.state[mostDistincVowels$vowel=='ah'],mostDistincVowels$F2.at.steady.state[mostDistincVowels$vowel=='ah']) 
iCorF1F2 <- cor(mostDistincVowels$F1.at.steady.state[mostDistincVowels$vowel=='iy'],mostDistincVowels$F2.at.steady.state[mostDistincVowels$vowel=='iy']) 
uCorF1F2 <- cor(mostDistincVowels$F1.at.steady.state[mostDistincVowels$vowel=='uw'],mostDistincVowels$F2.at.steady.state[mostDistincVowels$vowel=='uw']) 




