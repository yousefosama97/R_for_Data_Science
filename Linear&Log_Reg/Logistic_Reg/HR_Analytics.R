############################################################
####        EXAMPLE NÂ°2 - HR ANALYTICS 2               ####
############################################################

rm(list=ls(all=TRUE))

# Let's load our dataset and call it data

data=read.table('DATA_3.02_HR2.csv', header = T,sep=',')


# Now let's have a look at our variables and see some summary statistics

str(data) # The str() function shows the structure of your dataset and details the type of variables that it contains
summary(data) # The summary() function provides for each variable in your dataset the minimum, mean, maximum and quartiles

table(data$left) # look at the frequencies for the left variable
table(data$left)/nrow(data) # look at percentages for the left variable
hist(data$left) # alternatively, plot a histogram

cor(data) # Let's check out the correlations
logreg = glm(left ~ ., family=binomial(logit), data=data) # Estimate the drivers of attrition

hist(logreg$fitted.values) # See the proportion of employee attrition according to the model
cor(logreg$fitted.values,data$left) # Assess the correlation between estimated attrition and actual

cutoff=.3 # Cutoff to determine when P[leaving] should be considered as a leaver or not. Note you can play with it...

table(logreg$fitted.values>=cutoff, data$left) #Confusion Matrix

#calculating TruePositives and TrueNegatives

sum((logreg$fitted.values<=cutoff)&(data$left==0))/sum(data$left==0) # Compute the percentage of correctly classified employees who stayed

sum((logreg$fitted.values>cutoff)&(data$left==1))/sum(data$left==1) # Compute the percentage of correctly classified employees who left

mean((logreg$fitted.values>cutoff)==(data$left==1)) # Compute the overall percentage of correctly classified employees

summary(logreg) # Report the results of the logistic regression

# Let's use a more visual way to see the effect of one of the most important driver: TIC

plot(data$TIC,data$left,main= "Time and Employee Attrition", ylab="Attrition", xlab= "Time spent")

# An aggregated plot

tempdata=data

aggbTimeRank=aggregate(left~ TIC, data=tempdata, FUN=mean) # We compute the average attrition rate for each value of TIC

plot(aggbTimeRank$TIC,aggbTimeRank$left,main= "Time and Employee Attrition", ylab="Average Attrition Rate", xlab= "Time spent")



# An even better one!

TimeRank=aggregate(left~ TIC, data=tempdata, FUN=length) # We compute the number of employees for each value of TIC

symbols(aggbTimeRank$TIC,aggbTimeRank$left,circles=TimeRank$left, inches=.4, fg="white", bg="red",main= "Time and Employee Attrition", ylab="Average Attrition Rate", xlab= "Time spent") # we


# Let's use a more visual way to see the effect of the most important driver: Satisfaction

tempdata=data

tempdata$rankSatis = round(rank(-tempdata$S)/600) # We create categories of employee satisfaction ranking. We create 20 groups (because it will work well later...)

aggbSatisRank = aggregate(left~ rankSatis, data=tempdata, FUN=mean) # We compute the average attrition rate for each category

SatisRank = aggregate(left~ rankSatis, data=tempdata, FUN=length) # We compute the number of employees for each value of TIC

symbols(aggbSatisRank$rankSatis,aggbSatisRank$left,circles=SatisRank$left, inches=.1, fg="white", bg="red",main= "Satisfaction and Employee Attrition", ylab="Average Attrition Rate", xlab= "Rank of Satisfaction")







