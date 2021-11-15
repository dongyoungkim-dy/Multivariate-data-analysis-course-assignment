library(ade4)
data(deug)
data <- deug$tab
data
head(data)
#####1. summary
summary(data)
boxplot(data)
pairs(data)
#####2
R = cor(data)  #correlation matrix
S = cov(data)  #covariance matrix

write.csv(R, quote=F, col.names=T, row.names = T, ""D:/00. multivariate/assignment/r.csv")
write.csv(S, quote=F, col.names=T, row.names = T, ""D:/00. multivariate/assignment/s.csv")

#####3. eigenvector

ei = eigen(R)
write.csv(ei$values, quote=F, col.names=T, row.names = T, "D:/00. 방통대 통계/다변량분석/과제/eigen.csv")

#####4. the number of eigen over 1

data.pca =princomp(data, cor=T,scores=T)
data.pca
eig.val = data.pca$sdev^2
eig.val

#####5. scree plot
screeplot(data.pca, type="lines", pch=19, main="Scree Plot")
abline(h=1, lty=3, col="blue")

####6. 
data.pca$loadings[,1:3]

#####7 BIPLOT
biplot(data.pca, cex=0.7, col=c("red", "blue"), main="Biplot")
