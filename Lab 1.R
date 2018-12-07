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
  count <- count + 1
}

subpartDial <- merge(subpartDial, part2Dial, by = 'Group.1')
subpartDial <- merge(subpartDial, subpartDialSE, by = 'Group.1')

subpartSteer <- merge(subpartSteer, part2Steer, by = 'Group.1')
subpartSteer <- merge(subpartSteer, subpartSteerSE, by = 'Group.1')

colnames(subpartDial) <- c('Keypressed', 'LateralDeviation', 'DialingTime', 'SE')
colnames(subpartSteer) <- c('Keypressed', 'LateralDeviation', 'DialingTime', 'SE')

plot <- qplot(subpartDial$DialingTime, subpartDial$LateralDeviation, xlab = 'Dialing Time (ms)', ylab = 'Lateral Deviation (m)',main = 'Lateral deviation for each keypress in time seperated for the \nsteering (red) and dialing (black) condition.', geom = c('line', 'point'))
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
plot <- plot + labs(col="trial") + xlab('Time') + ylab('Drift')
plot

#B
df <- data.frame(trial = integer(1220),time = double(1220), drift = double(1220))
createDf <- function(df, sd){
  row <- 1
  for (i in 1:20){
    j <- 0
    drift <- 0
    oldDrift <- 0
    while (j <= 60){
      df$trial[row] <- i
      df$time[row] <- (50*j)
      drift <- rnorm(1,0,sd)
      df$drift[row] <- oldDrift + drift
      oldDrift <- oldDrift + drift
      j <- j + 1
      row <- row + 1
    }
  }
  return(df)
}
df <- createDf(df, 0.13)

plot <- ggplot(data = df)
plot <- plot + geom_line(data = df, aes(x = time, y = drift, colour = as.factor(trial), group = as.factor(trial)))
plot <- plot + labs(col="trial")
plot

#C
hist(df$drift)
hist(necessaryDriftData$posX)
plot <- ggplot(data = df) + xlab('drift') + ylab('frequency') + ggtitle('The drift for modelled data (red) and human data (blue)')
plot <- plot + theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.border = element_blank(),
                     panel.background = element_blank())
plot <- plot + geom_histogram(mapping = aes(df$drift), data = df, color = 'red', fill="red", alpha = 0.5)
plot <- plot + geom_histogram(mapping = aes(necessaryDriftData$posX), data = necessaryDriftData, color = 'blue', fill="blue", alpha=0.5)
plot

#D
#for human data: 
statistics(necessaryDriftData$posX) 
#the mean = -0.03249377
#the sd = 0.35961255
#the SE = 0.01036817

#for modelled data:
statistics(df$drift) 
#The mean = 0.10366627
#the sd = 0.7505585233
#the SE = 0.0214884280

#E
done <- FALSE
sd <- 0.13
stepsize <- 0.01 #arbitrary stepsize
humansd <- sd(necessaryDriftData$posX)
previoussd <- 0
count <- 1
alt.df <- data.frame(trial = integer(1220),time = double(1220), drift = double(1220))
while (!done){
  newdf <- createDf(alt.df, sd)
  modelsd <- sd(newdf$drift)
  if (modelsd > humansd){
    if (count > 1 && previoussd < humansd){
      finaldf <- newdf
      done <- TRUE
      next
    }
    sd <- sd - stepsize
    previoussd <- modelsd
    count <- count + 1
  }else if (modelsd < humansd){
    if (count > 1 && previoussd > humansd){
      finaldf <- newdf
      done <- TRUE
      next
    }
    sd <- sd + stepsize
    previoussd <- modelsd
    count <- count + 1
  }else if (modelsd == humansd){
    finaldf <- newdf
    done = TRUE
  }
}

#(I) The final sd to which we want to set our model
# sd = 0.05

#(II) A plot	of	how	lane	position	changes	over	time	for	the	individual	simulated	trials
plot <- ggplot(data = finaldf) + ggtitle('Modelled drift over time per trial')
plot <- plot + geom_line(data = finaldf, aes(x = time, y = drift, colour = as.factor(trial), group = as.factor(trial)))
plot <- plot + labs(col="trial")
plot

#(III) A plot	of the	resulting	distribution (against human data)
plot <- ggplot(data = finaldf) + xlab('drift') + ylab('frequency') + ggtitle('The drift for modelled data (red) and human data (blue)')
plot <- plot + theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.border = element_blank(),
                     panel.background = element_blank())
plot <- plot + geom_histogram(mapping = aes(finaldf$drift), data = finaldf, color = 'red', fill="red", alpha = 0.5)
plot <- plot + geom_histogram(mapping = aes(necessaryDriftData$posX), data = necessaryDriftData, color = 'blue', fill="blue", alpha=0.5)
plot

#(IV) the	SD (and more)	of	this	resulting	distribution.
statistics(finaldf$drift) 
#The mean = 0.0221523601
#the sd = 0.28830222238
#the SE = 0.008254068616


#########################################
###############Question 3################
#########################################
dataQ3 = read.csv('keyPressDataWithLaneDeviation.csv', header = TRUE, sep = ',')
usableDataQ3 <- data[data$typingErrorMadeOnTrial == 0,]
usableDataQ3 <- usableDataQ3[usableDataQ3$partOfExperiment == 'singleDialing2',]
for (i in 1:length(usableDataQ3[,1])){
  if (usableDataQ3$phoneNrLengthAfterKeyPress[i] != 0){
    usableDataQ3$iki[i] <- usableDataQ3$timeRelativeToTrialStart[i] - usableDataQ3$timeRelativeToTrialStart[i-1]
  }else{
    usableDataQ3$iki[i] <- 0
  }
}

participantMean <- c()
for (participant in 1:max(usableDataQ3$pp)){
  participantMean[participant] <- mean(usableDataQ3$iki[usableDataQ3$pp == participant])
}
#mean per participant:
#172.1410 319.0571 291.8507 289.0746 282.5526 272.1231 220.8000 253.9853 318.1316 233.6970
#259.7949 205.7576
#without keypresserror
#172.1410 291.4151 279.6462 276.4462 261.0923 272.1231 220.8000 253.9853 291.1346 233.6970
#250.6154 216.4906


#grand mean (mean of the means of participants)
mean(participantMean)
#259.9138 ms (in the model this is rounded to 260)
#without keypresserror
mean(participantMean)
#251.6322 ms (in the model this is rounded to 252)
#we chose the grand mean to prevent overfitting and because we want to model the behaviour of an 
# average individual rather than using the specific value per participant to (later) model some other 
# specific behaviour. 

#########################################
###############Question 4################
#########################################



