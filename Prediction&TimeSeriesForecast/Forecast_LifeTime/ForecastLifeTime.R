###########################################################
####        EXAMPLE N°3 - PREDICTIVE MAINTENANCE       ####
###########################################################


rm(list=ls(all=TRUE))

# Let's load the data
data=read.table('DATA_4.03_MNT.csv',sep=',',header=TRUE)

str(data) # The str() function shows the structure of your dataset and details the type of variables that it contains
summary(data) # The summary() function provides for each variable in your dataset the minimum, mean, maximum and quartiles


linregmodel = lm(lifetime~.-broken,data=data)  # Build a linear regression model
summary(linregmodel) # The summary() function shows the output of your model

install.packages("survival") # Install the survival package to your computer
library(survival) # Load the survival package

dependantvars = Surv(data$lifetime, data$broken) # choose the dependant variables to be used in the survival regression model with the Surv() function
survreg = survreg(dependantvars~pressureInd+moistureInd+temperatureInd+team+provider, dist="gaussian",data=data) # Create your survival regression model
summary(survreg)  # The summary() function shows the output of your model

Ebreak=predict(survreg, newdata=data, type="quantile", p=.5) # Make predictions based on the model. Here we estimate the median lifetime as the expected moment of "death"

Forecast=data.frame(Ebreak) # Create a dataframe to store the ouput of Ebreak

Forecast$lifetime=data$lifetime  # Add a column in the Forecast dataframe indicating the lifetime of the piece
Forecast$broken=data$broken # Add a column in the Forecast dataframe indicating whether or not the piece is broken
Forecast$RemainingLT=Forecast$Ebreak-data$lifetime # Computed Expected Remaining Lifetime

View(Forecast) # View the complete Forecast dataframe

Forecast=Forecast[order(Forecast$RemainingLT),] # Order the elements by Expected Remaining Lifetime
ActionsPriority=Forecast[Forecast$broken==0,] # And keep only those who are not broken yet
View(ActionsPriority) # View the output and take actions!

write.csv(ActionsPriority, file = "ActionsPriority.csv", row.names=FALSE) # Let's save the output in a csv to work on it in Excel later
