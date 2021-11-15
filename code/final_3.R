

city <- read.csv("D:/00. mvadata/q3.csv")
head(city)
city

##### lda

library(MASS)
city.lda = lda(TYPE~ . , data=city)
city.lda

plot(city.lda, type = "both")

###### pred

pred.lda <- predict(city.lda, newdata = city)
pred.lda$class
names(pred.lda)

head(pred.lda$posterior)
pred.lda$posterior

head(pred.lda$x)
pred.lda$x
confm.lda = table(city$TYPE, pred.lda$class)
confm.lda 

error <- (1 - sum(diag(confm.lda)) / sum(confm.lda)) * 100
error 
