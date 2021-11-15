library(stats)
library(MASS)

source("D:/00. mvadata/readMatrix.r")
data = readMatrix()

country.name = scan("D:/00. mvadata/countryname.txt", what="")
colnames(data) = country.name 
rownames(data) = country.name 
data


mds1 = isoMDS(data, k=2)
x = mds1$points[,1]
y = mds1$points[,2]

plot(mds1[,1], mds1[,2], type = "n", xlab="", ylab = "", main="cmdscale(Auto)")
abline(h=0, v=0, lty=3)



###############

data <- read.csv("D:/00. mvadata/city_airdistance.csv")
data
data <- as.matrix(data[,-1])
data
rownames(data) = colnames(data)

data
mds1 = cmdscale(data, k=2)
plot(mds1[,1], mds1[,2], type = "n", xlab="", ylab = "", main="cmdscale(Auto)")
text(mds1[,1], mds1[,2], rownames(data), cex=0.9)
abline(h=0, v=0, lty=3)


#######################
library(smacof)

mds2 <- smacofSym(data, ndim = 2, type = "interval")
#----->   type =  "interval"  "ratio"   "ordinal"

plot(mds2$conf[ , 1], q2.mds2$conf[ ,2], type = "n", xlab = "", ylab = "", main = "smacofSym")
text(mds2$conf[ , 1], q2.mds2$conf[ ,2], rownames(data), cex = 0.9)
abline(h = 0, v = 0, lty = 3)


#######################


library(readxl)
q2 <- read_excel("D:/00. mvadata/MVA_p168_mileages.xlsx")
q2[q2 == "."] <-"0" 
q2.name <- q2$City
q2.name.length <- length(q2.name)
q2.matrix <- matrix(as.numeric(as.matrix(q2[ , -1])), nrow = q2.name.length)
q2.final <- q2.matrix + t(q2.matrix)

colnames(q2.final) <- q2.name
rownames(q2.final) <- q2.name

q2.final

mds1 = cmdscale(q2.final, k=2)
plot(mds1[,1], mds1[,2], type = "n", xlab="", ylab = "", main="cmdscale(Auto)")
text(mds1[,1], mds1[,2], rownames(q2.final), cex=0.9)
abline(h=0, v=0, lty=3)
