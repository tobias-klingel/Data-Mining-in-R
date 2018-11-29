setwd("/Mining/Assignment/HW1")
mydata <- read.table("data_1.txt",header = TRUE)

library("plot3D")

#Get data columns
x <- mydata[,3]     #KM
y <- mydata[,10]    #Weight
z <- mydata[,1]     #Price


# Plot
plot3D::points3D(x,y,z, xlab="KM", ylab="Weight", zlab="Price")

