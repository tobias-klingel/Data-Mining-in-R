#Data Mining HW1
#Task1

wine <- read.table("HWtrain.data.txt", sep=",")
names(wine)[1]<-paste("Quality")
names(wine)[2]<-paste("Descriptive values")
names(wine)[14]<-paste("Descriptive values")



#scale option does the standardization
stdconcentr <- as.data.frame(scale(wine[,2:14]))

#Just show it
mean(stdconcentr$V2)
mean(stdconcentr$V3)

WinePCA<- prcomp(stdconcentr)
##################################################################################################
#Task2 HW1
#---------------->Result of HW1 2.a
summary(WinePCA)
#->55,3%
#The Information we use is 55,3% when we performing the PCA. It is also called cumulative proportion of the second component
#Before Standardizating the amout of information we use is 99,98% of the two principle components

screeplot(WinePCA, type="lines")
##################################################################################################

#------------->Result of HW1 2.b
sd_val <- sapply(wine[,2:14],sd)
sd_val
#------------->Result
#sd_val
#V8: 3.3640030
#V9: 312.0442803
#v10: 14.1394881
#The three found variables are found contribute the most to the original data
##################################################################################################
#HW2.c
#Read data
HWtrain_data <-  read.csv("HWtrain.data.txt", header =  FALSE) 
HWtest_data <-  read.csv("HWtest.data.txt", header =  FALSE)

#################################################
# scale data

stdConcentrations_train <- as.data.frame(scale(HWtrain_data[,2:14]))
stdConcentrations_test <- as.data.frame(scale(HWtest_data[,2:14]))

#################################################
#Create PCA
HWtrain_data.pca <- prcomp(stdConcentrations_train)                
HWtest_data.pca <- prcomp(stdConcentrations_test) 

summary(HWtrain_data.pca)


#################################################
#Print  scores of PCA 1 & PCA 2

# create a scatterplot1
plot(HWtrain_data.pca$x[,1],HWtrain_data.pca$x[,2], col=HWtrain_data$V1, pch=as.character(HWtrain_data$V1) ) 

# create a scatterplot2
points(HWtest_data.pca$x[,1],HWtest_data.pca$x[,2], col=4, pch=as.character(c(1,2,3,4,5))) 
'The blue points are the test data (These points we have to predict)

#################################################
# get the loads (rotations)

print(HWtrain_data.pca)
#------------->Result
#Using the Model (plot) to classify the testing data. The classification is:d
# Point 1 = class 1
# Point 2 = class 2
# Point 3 = class 3
# Point 4 = class 1
# Point 5 = class 2
