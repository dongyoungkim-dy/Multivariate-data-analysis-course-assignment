
beer=read.csv("D:/00. multivariate/assignment/beerbrand.csv")
beer=beer[,-1]
head(beer)
summary(beer)


#install.packages("stats")
#install.packages("GPArotation")

library(stats)

beer.fact = factanal(beer, factors = 2, rotation = "none")
beer.fact1 = factanal(beer, factors = 2, rotation = "varimax")
beer.fact2 = factanal(beer, factors = 2, rotation = "oblimin")

beer.fact
beer.fact1
beer.fact2

barplot(t(loadings(beer.fact)), beside = T)
barplot(t(loadings(beer.fact1)), beside = T)
barplot(t(loadings(beer.fact2)), beside = T)
