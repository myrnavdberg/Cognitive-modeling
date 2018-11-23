setwd('C:/Users/Myrna/Documents/Universiteit Utrecht/Cognitive modeling/Lab 1')

library(ggplot2)


data = read.csv('keyPressDataWithLaneDeviation.csv', header = TRUE, sep = ',')
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


#C
subpartDial <- aggregate(c(condDualDialFocus$lanePosition), list(condDualDialFocus$phoneNrLengthAfterKeyPress), FUN =mean, data= condDualDialFocus)
subpartSteer <- aggregate(c(condDualSteerFocus$lanePosition), list(condDualSteerFocus$phoneNrLengthAfterKeyPress), FUN =mean, data= condDualSteerFocus)
part2Dial <- aggregate(c(condDualDialFocus$timeRelativeToTrialStart), list(condDualDialFocus$phoneNrLengthAfterKeyPress), FUN =mean, data= condDualDialFocus)
part2Steer <- aggregate(c(condDualSteerFocus$timeRelativeToTrialStart), list(condDualSteerFocus$phoneNrLengthAfterKeyPress), FUN =mean, data= condDualSteerFocus)

subpartDial <- merge(subpartDial, part2Dial, by = 'Group.1')
subpartSteer <- merge(subpartSteer, part2Steer, by = 'Group.1')

colnames(subpartDial) <- c('Keypressed', 'LateralDeviation', 'DialingTime')
colnames(subpartSteer) <- c('Keypressed', 'LateralDeviation', 'DialingTime')

#plot2 <- qplot(subpartDial, aes(DialingTime, LateralDeviation), xlab = 'Dialing Time (ms)', ylab = 'Lateral Deviation (m)',main = 'Lateral deviation for each keypress in time seperated for the steering and dialing condition.')
#plot2 + geom_point()

plot <- qplot(subpartDial$DialingTime, subpartDial$LateralDeviation, xlab = 'Dialing Time (ms)', ylab = 'Lateral Deviation (m)',main = 'Lateral deviation for each keypress in time seperated for the steering and dialing condition.', geom = c('line', 'point'))
plot <- plot + theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             panel.border = element_blank(),
             panel.background = element_blank())
plot <- plot + geom_point(aes(subpartSteer$DialingTime, subpartDial$LateralDeviation), col = 'red', shape = 3)
plot<- plot + geom_line(aes(subpartSteer$DialingTime, subpartDial$LateralDeviation), col = 'red')
plot + scale_colour_manual("", values = c("Dial focus"="black", "Steer focus"="red"))
plot
#de legenda en de error bars moeten nog in the plot


#qplot(subpartSteer$DialingTime, subpartSteer$LateralDeviation)



#  dualSteerFocus: mean = 5851.2128 ms
