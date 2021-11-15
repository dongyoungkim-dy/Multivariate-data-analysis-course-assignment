
crime=read.csv("D:/00. multivariate/assignment/ex2-4.csv")
head(crime)
crime=crime[,-1]
boxplot(crime, main="Boxplot")
pairs(crime)

data.pca =princomp(crime, cor=T,scores=T)
data.pca

summary(data.pca)
screeplot(data.pca, type="lines", pch=19, main="Scree Plot")
abline(h=1, lty=3, col="blue")

data.pca$loadings[,1:2]
##### BIPLOT
biplot(data.pca, cex=0.7, col=c("red", "blue"), main="Biplot")
