install.packages("HSAUR2")
install.packages("MVA")

library(HSAUR2)
library(MVA)

usa <- data.frame(USairpollution)
head(USairpollution)

#1
par(mfrow=c(1, 1))
pairs(usa)
stars(usa)
library(aplpack)
par("mar") 
par(mar=c(1,1,1,1))

faces(usa, face.type = 1)
#2
plot(usa$temp, usa$wind)
x = usa[, c(2, 5)]
bvbox(x, xlab = "temp", ylab = "wind")
title("bivariate boxplot of temp and wind")
identify(x)
rownames(x)[c(23,31)]
#3
plot(manu~popul, xlab = "manu", ylab = "popul", data=usa, pch=9)
with(usa, symbols(manu, popul, circles=SO2, inches=0.5, add=T))
title("bubble plot")


y = usa[, c(1, 3, 4)]
pairs(y)
