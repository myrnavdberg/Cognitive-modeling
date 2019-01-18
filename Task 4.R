setwd('C:/Users/Myrna/Documents/Universiteit Utrecht/Cognitive modeling/Lab 3')
source('Task 1 and 2.R')
source('Task 3.R')


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


