
library(rpart)
library(rpart.plot)
library(dplyr)

firis = iris 
head(firis)     
names(firis) = c("SL","SW","PL","PW","SP") 
levels(firis$SP)<-c("st","vc","vg")  
head(firis)     


### decision tree
cartfit <- rpart(SP ~ SL+SW+PL+PW, data = firis)

plot(cartfit, uniform = T, compress = T, margin = 0.1)
text(cartfit, use.n = T, col = "blue")
summary(cartfit)

pred = predict(cartfit, newdata = firis, type = "class")
check = table(iris$Species, pred)
check
error <- 1 - sum(diag(check)) / sum(check)
error

### stopping rule
cartfit <- rpart(SP ~ SL+SW+PL+PW, data = firis,
                   control = rpart.control(minsplit = 5,xval = 10))
plot(cartfit, uniform = T, compress = T, margin = 0.1)
text(cartfit, use.n = T, col = "blue")
summary(cartfit)
pred = predict(cartfit, newdata = firis, type = "class")
check = table(iris$Species, pred)
check
error <- 1 - sum(diag(check)) / sum(check)
error




cartfit <- rpart(SP ~ SL+SW+PL+PW, data = firis,
                 control = rpart.control(minsplit = 8,xval = 0))
plot(cartfit, uniform = T, compress = T, margin = 0.1)
text(cartfit, use.n = T, col = "blue")

pred = predict(cartfit, newdata = firis, type = "class")
check = table(iris$Species, pred)
error <- 1 - sum(diag(check)) / sum(check)
error






# 도형화 

plot(cartfit, uniform = T, compress = T, margin = 0.1)
text(cartfit, use.n = T, col = "blue")

rpart.plot(cartfit)


###############
# (2) 분류표, 오분류율
###############


pred = predict(cartfit, newdata = firis, type = "class")

check = table(iris$Species, pred)

error <- 1 - sum(diag(check)) / sum(check)

error


###############
# (3) 정기규칙 변경 
###############

# 모형 생성 
fit2.nocv <- rpart(SP ~. , data = firis,
                   control = rpart.control(minsplit = 5, 
                                           minbucket = round(5/3),  
                                           xval = 10, cp = 0.01),
                   method = "class",
                   parms = list(split = "gini") )

# 도형화 
plot(fit2.nocv, uniform = T, compress = T, margin = 0.1)
text(fit2.nocv, use.n = T, col = "blue")

rpart.plot(fit2.nocv)

# 오분류율
tree.pred <- predict(fit2.nocv, newdata = firis, type = "class")
(cm <- table(iris$Species, tree.pred))
tree.error <- 1 - sum(diag(cm)) / sum(cm)
print(paste0("error rate = ", round(tree.error * 100, digits = 2), " %"))



summary(fit1.nocv)
summary(fit2.nocv)


###############
# 가지치기 전, no cross validation, 나무모형 생성 
###############

fit1.nocv <- rpart(Species ~. , data = firis,
                   control = rpart.control(minsplit = 20, minbucket = 10,  xval = 0),
                   method = "class",
                   parms = list(split = "gini", prior = c(1/3, 1/3, 1/3)))



# 도형화 

plot(fit1.nocv, uniform = T, compress = T, margin = 0.1)
text(fit1.nocv, use.n = T, col = "blue")

rpart.plot(fit1.nocv)

# summary

summary(fit1.nocv)


# 오분류율

tree.pred <- predict(fit1.nocv, newdata = firis, type = "class")

(cm <- table(iris$Species, tree.pred))

(tree.error <- 1 - sum(diag(cm)) / sum(cm))













###############
# 가지치기 전, 10 fold cross validation, 나무모형 생성 
###############

set.seed(1919)

fit1.10cv <- rpart(Species ~. , data = firis,
                   control = rpart.control(minsplit = 20, minbucket = 10,  xval = 10),
                   method = "class",
                   parms = list(split = "gini", prior = c(1/3, 1/3, 1/3)))



# 도형화 

plot(fit1.10cv, uniform = T, compress = T, margin = 0.1)
text(fit1.10cv, use.n = T, col = "blue")

rpart.plot(fit1.10cv, type = 4)

prp(fit1.10cv, type=4, extra=2, digits=3)

# summary

summary(fit1.10cv)





# 오분류율

tree.pred <- predict(fit1.10cv, newdata = firis, type = "class")

(cm <- table(iris$Species, tree.pred))

(tree.error <- 1 - sum(diag(cm)) / sum(cm))








###############
# 가지치기 후 나무모형
###############

printcp(fit1.10cv)

plotcp(fit1.10cv)

fit1.pruned <- prune(fit1.10cv, cp = 0.47)

rpart.plot(fit1.pruned)



###############
# 정지규칙변경, no CV,  나무모형 생성 
###############



fit2.nocv <- rpart(Species ~. , data = firis,
                   control = rpart.control(minsplit = 10, minbucket = 5,  xval = 0),
                   method = "class",
                   parms = list(split = "gini", prior = c(1/3, 1/3, 1/3)))



# 도형화 

plot(fit2.nocv, uniform = T, compress = T, margin = 0.1)
text(fit2.nocv, use.n = T, col = "blue")

rpart.plot(fit2.nocv)

# summary

summary(fit2.nocv)


# 오분류율

tree.pred <- predict(fit2.nocv, newdata = firis, type = "class")

(cm <- table(iris$Species, tree.pred))

(tree.error <- 1 - sum(diag(cm)) / sum(cm))






###############
# 정지규칙변경, 10 fold-CV,  나무모형 생성 
###############

set.seed(1919)

fit2.10cv <- rpart(Species ~. , data = firis,
                   control = rpart.control(minsplit = 10, minbucket = 5,  xval = 10),
                   method = "class",
                   parms = list(split = "gini", prior = c(1/3, 1/3, 1/3)))



# 도형화 

plot(fit2.10cv, uniform = T, compress = T, margin = 0.1)
text(fit2.10cv, use.n = T, col = "blue")

rpart.plot(fit2.10cv)

# summary

summary(fit2.10cv)


# 오분류율

tree.pred <- predict(fit2.10cv, newdata = firis, type = "class")

(cm <- table(iris$Species, tree.pred))

(tree.error <- 1 - sum(diag(cm)) / sum(cm))



###############
# 정지규칙변경, 10 fold-CV, 나무모형, 가지치기 
###############

printcp(fit2.10cv)


fit2.pruned <- prune(fit1.10cv, cp = 0.02)

rpart.plot(fit2.pruned)



# 오분류율

tree.pred <- predict(fit2.pruned, newdata = firis, type = "class")

(cm <- table(iris$Species, tree.pred))

(tree.error <- 1 - sum(diag(cm)) / sum(cm))