setwd("/Assignment/HW2/HW2 Me") 
Data_car <- read.table("Data_car.txt", header=TRUE)

#Variable selection
fullfit <- lm(Price ~ Age_08_04+KM+HP+Met_Color+Automatic+CC+Doors+Quarterly_Tax+Weight+Fuel_Type_CNG+Fuel_Type_Diesel,data=Data_car)

#Linear-regression model AIC values based on each of “forward”, “backward”, and “both” methods
null=lm(Price~1, data=Data_car)


###########################
#Linear-regression models

#Forward
#Find the predicted responses for the two future observations in data_future.txt.
step(null, scope=list(lower=null, upper=fullfit), direction="forward")
fit_forward <- lm(Price ~ Age_08_04+KM+HP+Met_Color+Automatic+CC+Doors+Quarterly_Tax+Weight+Fuel_Type_CNG+Fuel_Type_Diesel,data=Data_car)
summary(fit_forward)$adj.r.squared

#Backward
step(fullfit, scope=list(lower=null, upper=fullfit), direction="backward")
fit_backward <- lm(Price ~ Age_08_04+KM+HP+Met_Color+Automatic+CC+Doors+Quarterly_Tax+Weight+Fuel_Type_CNG+Fuel_Type_Diesel,data=Data_car)
summary(fit_backward)$adj.r.squared

#Both
step(null, scope=list(lower=null, upper=fullfit), direction="both")
fit_both <- lm(Price ~ Age_08_04+KM+HP+Met_Color+Automatic+CC+Doors+Quarterly_Tax+Weight+Fuel_Type_CNG+Fuel_Type_Diesel,data=Data_car)
summary(fit_both)$adj.r.squared

#----->Result of all three = 0.868

#Predicted the two future observations
mydata_future <- read.table("data_future.txt", header = TRUE)

#sink('output_DataMining HW2 Task3  predicted responses for the two future observations.txt', append=FALSE)
predict(fit_both, newdata = mydata_future)
#------->Result 
#    1     2 
#12195  8985 
