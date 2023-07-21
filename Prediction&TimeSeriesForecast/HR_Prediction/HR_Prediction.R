
rm(list=ls(all=TRUE))

# Let's load our dataset
dataold=read.table('DATA_3.02_HR2.csv', header = T,sep=',') # The function read.table enables us to read flat files such as .csv files
datanew=read.table('DATA_4.02_HR3.csv', header = T,sep=',') # The new dataset on which we want to make the prediction

str(datanew) # The str() function shows the structure of your dataset and details the type of variables that it contains
summary(datanew) # The summary() function provides for each variable in your dataset the minimum, mean, maximum and quartiles

logreg = glm(left ~ ., family=binomial(logit), data=dataold) # Estimate the drivers of attrition
probaToLeave=predict(logreg,newdata=datanew,type="response") # Make predictions on the out-of-sample data

predattrition = data.frame(probaToLeave) # Structure the prediction output in a table
View(predattrition) # View the predattrition dataframe

predattrition$performance=datanew$LPE # Add a column to the predattrition dataframe containing the performance
View(predattrition) # View the predattrition dataframe

plot(predattrition$probaToLeave,predattrition$performance)
abline(h=0.53,col="red")
abline(v=0.39,col="red")
text(0.27,0.76,"As Usual", col = "red")
text(0.4,0.47,"Up or Out", col = "red")
text(0.7,0.76,"To Be Retained", col = "red")


predattrition$priority=predattrition$performance*predattrition$probaToLeave
View(predattrition)

orderpredattrition=predattrition[order(predattrition$priority,decreasing = TRUE),]
View(orderpredattrition)
