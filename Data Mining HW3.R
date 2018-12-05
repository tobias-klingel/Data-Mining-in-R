setwd("/HW3/HW3 Me/part1") 
#x_train.txt contains 7000 observations of accelerometer and gyroscope in Samsung Galaxy S II
#http://www.youtube.com/watch?v=XOEN9W05_4A
#561 featuresare in inputvariables.txt and inputvariables_info.txt
x_train_observations <- read.table("X_train.txt")

#y_train.txt contains the corresponding 7000 response statuses: 
#1 WALKING, 2 WALKING_UPSTAIRS, 3 WALKING_DOWNSTAIRS, 4 SITTING, 5 STANDING, 6 LAYING.
y_train_labels <- read.table("y_train.txt")
library(class)


x_train[1:3,]
ncol(x_train)
nrow(x_train)


####################################################
#Cross validation for finding the best K
#K is adustiable
#Find a good ‘k’ value among k=3,4,5,6,7,8
K_low <- 3
K_high <- 8
#60% training and 40% testing
calcSplit <- 4200 #=60%
meanmeas <- rep(0, K_high)

#Loop for finding the best K
for(i2 in K_low:K_high) {
  TotIter <- 1    # change to the number of the cross validations to be done. 
  accuracyAll <- rep(0, TotIter)
  for (i in 1:TotIter ) {
    myRandID <- sample(nrow(x_train_observations))
    myPermutedData <- x_train_observations[myRandID,]
    train_CrossVal <- myPermutedData[1:calcSplit,]
    testCrossVal <- myPermutedData[(calcSplit+1):nrow(x_train_observations),]
    y_train_CrossVal <- y_train_labels[1:calcSplit,1]
    y_test_CrossVal <- y_train_labels[(calcSplit+1):nrow(y_train_labels),1]
    
    knnresult <- knn(train_CrossVal, testCrossVal, y_train_CrossVal,  k = i2)
    
    conftbl <- table(knnresult,y_test_CrossVal)
    
    classifiedAsRight <- 0
    
    for(z in 1:nrow(conftbl)) {
      classifiedAsRight <- classifiedAsRight + conftbl[z,z]
    }
    accuracy <-  classifiedAsRight/sum(conftbl)
    accuracyAll[i] <- accuracy
  }
  meanmeas[i2] <- mean(accuracyAll)
}


####################################################
##Finding the best K
bestVal <- 0
bestK <- 0
for(r in 1:length(meanmeas)){
  if(bestVal < meanmeas[r]) {
    bestVal <- meanmeas[r]
    bestK <- r
  }
}
####################################################

#Using Knn with the best found K
#Predict the statuses of the observations in X_test.txt
X_test_data <- read.table("X_test.txt")

trainF <- x_train_observations
testFinal <- X_test_data
y_trainFinal <- y_train_labels[1:nrow(y_train_labels), 1]
finalKNN <- knn(trainF, testFinal, y_trainFinal, k = bestK)

# write results to file
write.table(finalKNN, "y_test_predeicted.txt", quote = F, row.names = F, col.names = F)

plot(finalKNN)







