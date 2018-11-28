setwd('C:/Users/Myrna/Documents/Universiteit Utrecht/Cognitive modeling/Lab 1')

library(ggplot2)


data = read.csv('keyPressDataWithLaneDeviation.csv', header = TRUE, sep = ',')

#########################################
###############Question 1################
#########################################

usableData <- data[data$typingErrorMadeOnTrial == 0,]
condDualSteerFocus <- usableData[usableData$partOfExperiment == 'dualSteerFocus',]
condDualDialFocus <- usableData[usableData$partOfExperiment == 'dualDialFocus',]


statistics <- function(vec){
  m <- mean(vec)
  stdev <- sd(vec)
  sterr <- stdev/(sqrt(length(vec)))
  print(list(c('mean:', m), c('sd:', stdev), c('SE:', sterr)))
  
  return(c(m,stdev,sterr))
}



#A
#dualSteerFocus
aggregate(condDualSteerFocus$timeRelativeToTrialStart, list(condDualSteerFocus$phoneNrLengthBeforeKeyPress), FUN =mean, data= condDualSteerFocus)
aggregate(condDualSteerFocus$timeRelativeToTrialStart, list(condDualSteerFocus$phoneNrLengthBeforeKeyPress), FUN = sd)
sdC1 <- aggregate(condDualSteerFocus$timeRelativeToTrialStart[condDualSteerFocus$phoneNrLengthBeforeKeyPress == 11], list(condDualSteerFocus$phoneNrLengthBeforeKeyPress[condDualSteerFocus$phoneNrLengthBeforeKeyPress==11]), FUN = sd)
SECond1 <- as.numeric(sdC1[2])/sqrt(length(condDualSteerFocus$timeRelativeToTrialStart[condDualSteerFocus$phoneNrLengthBeforeKeyPress == 11]))

#dualDialFocus
aggregate(condDualDialFocus$timeRelativeToTrialStart, list(condDualDialFocus$phoneNrLengthBeforeKeyPress), FUN =mean, data= condDualDialFocus)
aggregate(condDualDialFocus$timeRelativeToTrialStart, list(condDualDialFocus$phoneNrLengthBeforeKeyPress), FUN = sd)
sdC2 <- aggregate(condDualDialFocus$timeRelativeToTrialStart[condDualDialFocus$phoneNrLengthBeforeKeyPress == 11], list(condDualDialFocus$phoneNrLengthBeforeKeyPress[condDualDialFocus$phoneNrLengthBeforeKeyPress==11]), FUN = sd)
SECond2 <- as.numeric(sdC1[2])/sqrt(length(condDualDialFocus$timeRelativeToTrialStart[condDualDialFocus$phoneNrLengthBeforeKeyPress == 11]))


#B
#Grand average drift, sd, and SE
statistics(abs(condDualSteerFocus$lanePosition))
statistics(abs(condDualDialFocus$lanePosition))


#C (de legenda moet nog in de plot)
subpartDial <- aggregate(c(condDualDialFocus$lanePosition), list(condDualDialFocus$phoneNrLengthAfterKeyPress), FUN =mean, data= condDualDialFocus)
subpartSteer <- aggregate(c(condDualSteerFocus$lanePosition), list(condDualSteerFocus$phoneNrLengthAfterKeyPress), FUN =mean, data= condDualSteerFocus)
part2Dial <- aggregate(c(condDualDialFocus$timeRelativeToTrialStart), list(condDualDialFocus$phoneNrLengthAfterKeyPress), FUN =mean, data= condDualDialFocus)
part2Steer <- aggregate(c(condDualSteerFocus$timeRelativeToTrialStart), list(condDualSteerFocus$phoneNrLengthAfterKeyPress), FUN =mean, data= condDualSteerFocus)

subpartDialsd <- aggregate(c(condDualDialFocus$lanePosition), list(condDualDialFocus$phoneNrLengthAfterKeyPress), FUN =sd)
subpartSteersd <- aggregate(c(condDualSteerFocus$lanePosition), list(condDualSteerFocus$phoneNrLengthAfterKeyPress), FUN =sd)
subpartDialSE <- aggregate(c(condDualDialFocus$lanePosition), list(condDualDialFocus$phoneNrLengthAfterKeyPress), FUN =sd)
subpartSteerSE <- aggregate(c(condDualSteerFocus$lanePosition), list(condDualSteerFocus$phoneNrLengthAfterKeyPress), FUN =sd)

count = 1
while (count < 14){
  D <- condDualDialFocus[condDualDialFocus$phoneNrLengthAfterKeyPress==(count-1),]
  S <- condDualSteerFocus[condDualSteerFocus$phoneNrLengthAfterKeyPress==(count-1),]
  print('new trial')
  print(length(D[,1]))
  subpartDialSE[count,2] <- subpartDialsd[count,2]/sqrt(length(D[,1]))
  subpartSteerSE[count,2] <- subpartSteersd[count,2]/sqrt(length(S[,1]))
  print(count)
  print(subpartDialSE[count,2])
  print(countvecDial[count])
  count <- count + 1
}

subpartDial <- merge(subpartDial, part2Dial, by = 'Group.1')
subpartDial <- merge(subpartDial, subpartDialSE, by = 'Group.1')

subpartSteer <- merge(subpartSteer, part2Steer, by = 'Group.1')
subpartSteer <- merge(subpartSteer, subpartSteerSE, by = 'Group.1')

colnames(subpartDial) <- c('Keypressed', 'LateralDeviation', 'DialingTime', 'SE')
colnames(subpartSteer) <- c('Keypressed', 'LateralDeviation', 'DialingTime', 'SE')

plot <- qplot(subpartDial$DialingTime, subpartDial$LateralDeviation, xlab = 'Dialing Time (ms)', ylab = 'Lateral Deviation (m)',main = 'Lateral deviation for each keypress in time seperated for the steering (red) and dialing (black) condition.', geom = c('line', 'point'))
plot <- plot + theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             panel.border = element_blank(),
             panel.background = element_blank())
plot <- plot + geom_point(aes(subpartSteer$DialingTime, subpartSteer$LateralDeviation), col = 'red', shape = 5, size = 2)
plot <- plot + geom_line(aes(subpartSteer$DialingTime, subpartSteer$LateralDeviation), col = 'red')
plot <- plot + scale_colour_manual("", values = c("Dial focus"="black", "Steer focus"="red"))
plot <- plot + geom_errorbar(aes(ymin=subpartDial$LateralDeviation - subpartDial$SE, ymax = subpartDial$LateralDeviation + subpartDial$SE), colour = 'black')
plot <- plot + geom_errorbar(aes(subpartSteer$DialingTime, ymin=subpartSteer$LateralDeviation - subpartSteer$SE, ymax = subpartSteer$LateralDeviation + subpartSteer$SE), colour = 'red')
plot




#qplot(subpartSteer$DialingTime, subpartSteer$LateralDeviation)



#  dualSteerFocus: mean = 5851.2128 ms


#########################################
###############Question 2################
#########################################
#Testing
a	<- rnorm(10000,	0.0,	0.13)
hist(a,	col="grey")
mean(a)
median(a)
sd(a)

DriftData = read.csv('tableOfDriftValuesCalibration.csv', header = TRUE, sep = ',')

#A
necessaryDriftData <- DriftData[DriftData$trialTime >= 15000 & DriftData$trialTime <= 18000,]
plot <- ggplot(data = necessaryDriftData)
plot <- plot + geom_line(data = necessaryDriftData, aes(x = trialTime, y = posX, colour = as.factor(trial), group = as.factor(trial)))
plot

#B

