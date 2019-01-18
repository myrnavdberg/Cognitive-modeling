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

# plot(data$`F2 at steady state`, data$`F1 at steady state`, xlim=rev(range(700:3500)),
#      ylim=rev(range(300:1300)), col="1", xlab="F2", ylab="F1", pch=1,
#      main="The vowel space", cex.lab=1.3, cex.axis=1.3, cex.sub=1.3)

#Task 2
mostDistincVowels <- data[data$vowel == 'iy'|data$vowel == 'uw'|data$vowel == 'ah',]
plot(mostDistincVowels$F2.at.steady.state, mostDistincVowels$F1.at.steady.state, xlim=rev(range(700:3500)),
     ylim=rev(range(300:1300)), col="1", xlab="F2", ylab="F1", pch=1,
     main="The vowel space", cex.lab=1.3, cex.axis=1.3, cex.sub=1.3)

mostDistincVowels[mostDistincVowels==0]<-NA
mostDistincVowels<- mostDistincVowels[complete.cases(mostDistincVowels), ]

char.var <- c(1,2,3)
df = as.data.frame(permutations(n=length(char.var), r=3, v=char.var))
