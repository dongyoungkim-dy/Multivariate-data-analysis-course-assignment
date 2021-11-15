
beer=read.csv(""D:/00. multivariate/assignment/beerbrand.csv")
beer=beer[,-1]
head(beer)
summary(beer)
pairs(beer[,c(-1,-3)])
#install.packages("psych")
#install.packages("GPArotation")

library(psych)
library(GPArotation)

#### factor analysis - eigen value
beer.factor = principal(beer, rotate="none")
beer.factor$values
plot(beer.factor$values, type="b")
abline(h=1, lty=3, col="blue")

### factor matrix
beer.factor.matrix = principal(beer, nfactors = 2, rotate = "none")
beer.factor.matrix
biplot(beer.factor.matrix, main = "none")
barplot(t(loadings(beer.factor.matrix)), beside = T)


### varimax method
beer.varimax = principal(beer, nfactors = 2, rotate = "varimax")
beer.varimax
biplot(beer.varimax, main = "varimax")
barplot(t(loadings(beer.varimax)), beside = T)

### oblimin method
beer.oblimin = principal(beer, nfactors = 2, rotate = "oblimin")
beer.oblimin
biplot(beer.oblimin, main = "oblimin")
barplot(t(loadings(beer.oblimin)), beside = T)

