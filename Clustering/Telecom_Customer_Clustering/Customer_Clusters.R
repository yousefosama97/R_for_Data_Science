rm(list=ls(all=TRUE))

# Let's load the data
data=read.table('DATA_2.03_Telco.csv', header = T,sep=',')# The function read.table enables us to read flat files such as .csv files

# Now let's have a look at our variables and see some summary statistics
str(data) # The str() function shows the structure of your dataset and details the type of variables that it contains
summary(data) 


# Now let's normalize our variables
testdata=data # To keep our dataset safe, let's create a copy of it called "testdata"
testdata = scale(testdata) # the scale function automatically performs data normalization on all your variables

d = dist(testdata, method = "euclidean") # the dist() function computes the distances of all the observations in our dataset
hcward = hclust(d, method="ward.D") # hclust() function performs hiearchical clustering, we pass it the distances, and we set the method argument to "ward.D"


data$groups=cutree(hcward,k=8) # assign our points to our k=8 clusters 
aggdata= aggregate(.~ groups, data=data, FUN=mean) # Aggregation by group and computation of the mean values
proptemp=aggregate(Calls~ groups, data=data, FUN=length) # Computation of the number of observations by group
aggdata$proportion=(proptemp$Calls)/sum(proptemp$Calls) # Computation of the proportion by group
aggdata=aggdata[order(aggdata$proportion,decreasing=T),] # Ordering from the largest group to the smallest

aggdata

# Let's try again with 5 segments
data$groups= cutree(hcward,k=5) #Create segments for k=5
aggdata= aggregate(.~ groups, data=data, FUN=mean) # Aggregation by group and computation of the mean values
proptemp=aggregate(Calls~ groups, data=data, FUN=length) # Computation of the number of observations by group
aggdata$proportion=(proptemp$Calls)/sum(proptemp$Calls) # Computation of the proportion by group
aggdata=aggdata[order(aggdata$proportion,decreasing=T),] # Ordering from the largest group to the smallest

aggdata

# Let's draw the radar chart with the function stars()
palette(rainbow(12, s = 0.6, v = 0.75)) # Select the colors to use
stars(aggdata[,2:(ncol(data))], len = 0.6, key.loc = c(11, 6),xlim=c(2,12),main = "Segments", draw.segments = TRUE,nrow = 2, cex = .75,labels=aggdata$groups)

write.csv(aggdata, file = "aggdataTelco5seg.csv", row.names=FALSE) # Let's save the output in a csv to work on it in Excel later

