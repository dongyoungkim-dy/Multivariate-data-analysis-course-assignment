library(MASS)

#### 

data(menarche)
plot( (Menarche / Total) * 100 ~ Age, data = menarche, pch = 19, main = "menarche on age", 
      xlab = "age", ylab = "probablility")

#### logistic

menar.out <- glm(cbind(Menarche, Total - Menarche) ~ Age, data = menarche, 
                   family = binomial(link = "logit"))
summary(menar.out)

plot(Menarche / Total ~ Age, data = menarche, pch = 19)
lines(menarche$Age, menar.out$fitted.values, type = "l", col = "blue")
title(main = "Menarche Data with Fitted Logistic Regression Line")  

1-pchisq(26.703, 23)


##### 

menar <- menar.out$y

menar.data <- ifelse(menar< 0.5, "no", "yes")
menar.data <- factor(menar.data)

me.pred = predict(menar.out, newdata = menarche, type = "response")
me.pred

pred = ifelse(me.pred< 0.5, "no", "yes")
pred
pred = factor(pred)
pred

confusion.matrix = table(menar.data, pred)

error <- 1.0 - (sum(diag(confusion.matrix)) / sum(confusion.matrix)) 
error

confusion.matrix
diag(confusion.matrix)
sum(diag(confusion.matrix))
sum(confusion.matrix)

