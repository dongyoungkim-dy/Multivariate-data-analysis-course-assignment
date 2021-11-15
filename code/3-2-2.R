
subject=read.csv(""D:/00. multivariate/assignment/favoritesujects.csv")
subject=subject[,-1] #to get rid of subject in data
head(subject)
summary(subject)

#install.packages("stats")
#install.packages("GPArotation")

library(stats)

sub.fact = factanal(subject, factors = 2, rotation = "none")
sub.fact1 = factanal(subject, factors = 2, rotation = "varimax")
sub.fact2 = factanal(subject, factors = 2, rotation = "promax")

sub.fact
sub.fact1
sub.fact2

barplot(t(loadings(sub.fact)), beside = T)
barplot(t(loadings(sub.fact1)), beside = T)
barplot(t(loadings(sub.fact2)), beside = T)

