############################################################
####     EXAMPLE N°1 - CREDIT SCORING 2                 ####
############################################################
rm(list=ls(all=TRUE))

dataold=read.table('DATA_3.01_CREDIT.csv',sep=',',header=TRUE) # The function read.table enables us to read flat files such as .csv files
datanew=read.table('DATA_4.01_CREDIT2.csv',sep=',',header=TRUE) # The function read.table enables us to read flat files such as .csv files

# Now let's have a look at our variables and see some summary statistics
str(datanew) # The str() function shows the structure of your dataset and details the type of variables that it contains
summary(datanew) # The summary() function provides for each variable in your dataset the minimum, mean, maximum and quartiles

linreg=lm(Rating~.,data=dataold) # Estimate a linear regression model of Rating as a function of everything else.
predcreditscore = predict(linreg,newdata=datanew,type="response") 

cor(linreg$fitted.values,dataold$Rating) # Computes the correlation between the fitted values and the actual ones
plot(dataold$Rating,linreg$fitted.values) # Plot the fitted values vs. the actual ones
cor(predcreditscore,datanew$Rating) # Computes the correlation between the fitted values and the actual ones
plot(datanew$Rating,predcreditscore) # Plot the fitted values vs. the actual ones

