
rm(list=ls(all=TRUE))

# Let's load our dataset and call it data
data=read.table('DATA_4.04_CHOC.csv',sep=',',header=TRUE) # The function read.table enables us to read flat files such as .csv files

# Now let's have a look at our variables and see some summary statistics
str(data) # The str() function shows the structure of your dataset and details the type of variables that it contains
summary(data$sales) # The summary() function provides for each variable in your dataset the minimum, mean, maximum and quartiles


plot(data$time,data$sales,main="Chocolate sales over time",xlab="Time (in month)",ylab="Monthly sales",ylim=c(0,max(data$sales*1.2)),type='l')
regres=lm(sales~month,data=data) # Build a linear regression model
summary(regres)


# Recovery thanks to the model:
plot(data$time,data$sales,main="Chocolate sales over time",xlab="Time (in month)",ylab="Monthly sales",ylim=c(0,max(data$sales)*1.2),type='l')
lines(data$time,regres$fitted.values,type='l',col='blue',lty=2)
legend("topleft",c("Actual sales","Sales by the model"),lty=c(1,2),col=c('black','blue'))

