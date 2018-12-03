setwd("/Mining/Assignment/HW2/HW2 Me") 

dev.off()
rm(list=ls())

# read data
#Sham.data, contains 25 observations of blood pressure at a certain position of normal rats
dtSham <- read.table("Sham.data", header=TRUE)
#PAB.data, contains 15 observations of blood pressure at the same position of irregular rats
dtPAB <- read.table("PAB.data", header=TRUE)

colnames(dtSham) <- 'x'
colnames(dtPAB) <- 'x'
trlabel1 <- rep('control', nrow(dtSham))
trlabel2 <- rep('case', nrow(dtPAB))

#stack up data for convenience
dtall <- rbind(dtSham, dtPAB)
labelAll <- c(trlabel1, trlabel2)

labelAllVal <- rep(0, length(labelAll))
labelAllVal[labelAll == "control"] <- 0
labelAllVal[labelAll == "case"] <- 1

id1 <- (labelAllVal == 1)
id0 <- (labelAllVal == 0)


#set the direction for testing
detcasek <- 1
if (mean(dtSham[,1]) > mean(dtPAB[,1])) {
  detcasek <- 2
}


#prepare threshold values
#If the blood pressure is less than a threshold value (t), then we predict that it is a normal rate
# Find closest point to the best separation case
minv <- min( dtall[,1] )
maxv <- max( dtall[,1] )
TotLen <- 10
threshall <- seq(minv,maxv,length.out=TotLen)

tprall <- rep(0,TotLen)
fnrall <- rep(0,TotLen)

for( i in 1:TotLen) {
  # to do some implementations here
}


################HW2 Task 1######################
library(ROCR)
labelNew <- c(rep(0,25), rep(1,15))

# saving the plot
jpeg('1. DataMining HW2 Task1 ROC curve.jpg')

#show ROC curve
#pred <- prediction (dtall,labelAll)
pred <- prediction (dtall,labelNew)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
dev.off()
#data.frame(predval, labels)

#plot(fnrall, tprall, type="o", xlim=c(0,1), ylim=c(0,1), xlab="1-specificity", ylab="sensitivity", cex.lab=1.5)
#dev.copy(png,"myROC.png")
#dev.off()

