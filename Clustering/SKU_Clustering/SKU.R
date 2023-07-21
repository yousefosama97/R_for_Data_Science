############################################################
############################################################
####        EXAMPLE N°1 - STOCK KEEPING UNIT            ####
############################################################

# To clean up the memory of your current R session run the following line
rm(list=ls(all=TRUE))

data = read.table("DATA_2.01_SKU.csv", header = T, sep = ',')
str(data)
summary(data)

plot(data$CV, data$ADS, main= 'SKUs', ylab = 'Average Daily Sales', xlab = 'Coefficient of Variation')

abline(v= 0.2, col ="red")

abline(h= 4, col ="red")

text(0.15,9.7,"Horses",col = "green")

text(0.65,9,"Wild Bulls",col = "blue")

text(0.8,2,"Crickets",col = "red")

testdata = data
testdata = scale(testdata)

d = dist(testdata, method = "euclidean")
hcward = hclust(d, method = "ward.D")

data$groups <- cutree(hcward, k=3)

install.packages("lattice")

library(lattice)
xyplot(ADS~ CV,main = "After Clustering", type="p",group=groups,data=data, 
       auto.key=list(title="Group", space = "left", cex=1.0, just = 0.95),
       par.settings = list(superpose.line=list(pch = 0:18, cex=1)), 
       col=c('blue','green','red')) 

