############################################################
#       Module 3 - Understanding causes and consequences	 #
#                   ESSEC BUSINESS SCHOOL                  #
############################################################
############################################################
####     EXAMPLE NÂ°1 - CREDIT SCORING                  ####
############################################################

rm(list=ls(all=TRUE))

data=read.table('DATA_3.01_CREDIT.csv',sep=',',header=TRUE) # The function read.table enables us to read flat files such as .csv files

str(data)
summary(data)

hist(data$Rating)

cor(data[,c(1:5,10)])

linreg=lm(Rating~.,data=data)


cor(linreg$fitted.values,data$Rating) # Computes the correlation between the fitted values and the actual ones

plot(data$Rating,linreg$fitted.values) # Plot the fitted values vs. the actual ones

summary(linreg) # Reports the results of the regression



plot(data$Balance,data$Rating) # Allows to visualize the relationship between Balance and Rating

plot(data$Income,data$Rating) # Allows to visualize the relationship between Income and Rating






