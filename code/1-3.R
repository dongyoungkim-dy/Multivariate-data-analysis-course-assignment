tdist <- rt(100, 5)
tdist
hist(tdist, main = "histogram of random t distribution", freq = TRUE)
hist(tdist, main = "histogram of random t distribution", freq = FALSE)
curve(dt(x=x, df=5), lty=2, add=TRUE)
boxplot(tdist, main = "boxplot of random t distribution")
stem(tdist)
?hist

