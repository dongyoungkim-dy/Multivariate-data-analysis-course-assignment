
subject=read.csv(""D:/00. multivariate/assignment/favoritesujects.csv")
subject=subject[,-1] #to get rid of subject in data
head(subject)
summary(subject)
pairs(subject)
#install.packages("psych")
#install.packages("GPArotation")

library(psych)
library(GPArotation)

#### 초기 인자분석 실행 - eigen value
sub.factor = principal(subject, rotate="none")
sub.factor
#write.csv(sub.factor, quote=F, col.names=T, row.names = T, "D:/00. 방통대 통계/다변량분석/과제/sub.factor.csv")
sub.factor$values
plot(sub.factor$values, type="b", main = "Scree plot of factor analysis")
abline(h=1, lty=3, col="blue")

### 인자부하행렬
sub.factor.matrix = principal(subject, nfactors = 2, rotate = "none")
sub.factor.matrix
biplot(sub.factor.matrix)

barplot(t(loadings(sub.factor.matrix)), beside = T)

### varimax method
sub.varimax = principal(subject, nfactors = 2, rotate = "varimax")
sub.varimax
biplot(sub.varimax)
barplot(t(loadings(sub.varimax)), beside = T)

### promax method
sub.promax = principal(subject, nfactors = 2, rotate = "promax")
sub.promax
biplot(sub.promax)

barplot(t(loadings(sub.promax)), beside = T)

