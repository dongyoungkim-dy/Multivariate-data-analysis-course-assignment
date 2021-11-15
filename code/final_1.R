dni3 <- dimnames(iris3)

library(help=datasets) 

####data
library(dplyr)

iris.data <- iris
iris.category <- as.factor(iris.data$Species)
iris.category
iris.data

iris.data <- iris.data[,-5]
head(iris.data)
summary(iris.data)

####standardize

library(pls)
ziris.data = stdize(as.matrix(iris.data))
ziris.data
head(ziris.data)

#### 3clusters 
kmc <- kmeans(ziris.data, 3)
kmc

pairs(ziris.data, col=kmc$cluster, pch=16, main = "3 groups cluster analysis of iris")

pairs(ziris.data, col=iris.category, pch=16, main = "3 groups of iris")

kmc$cluster

compare <- cbind(iris.category, kmc$cluster)
compare


# error rate 1

error <- data.frame(Y = iris.category, cluster = kmc$cluster) %>%
  group_by(Y, cluster) %>%
  mutate(cluster_name = ifelse(cluster == 1, "setosa", 
                               ifelse(cluster ==2 , "virginica", "versicolor"))) %>%
  mutate(error = ifelse(Y == cluster_name, T, F))

error

q1.error_rate <- (nrow(q1.error) - sum(q1.error$error)) / nrow(q1.error) * 100

print(paste0("error rate = ", round(q1.error_rate, digits = 2), " %"))



# error rate 2


q1.er <- cbind(q1, cluster = q1.kmc$cluster)

q1.er %>%
  group_by(cluster, Species) %>%
  summarise(count = n()) %>%
  arrange(cluster, desc(count)) %>%
  filter(!duplicated(cluster))

q1.predict <- factor(q1.kmc$cluster, levels = c(1, 2, 3) , 
                     labels = c("virginica", "setosa",  "versicolor"))

Y <- factor(Y, c("virginica", "setosa",  "versicolor"))

q1.cm <- table(Y, q1.predict)

q1.cm

q1.error_rate <- ( 1.0 - ( sum(diag(q1.cm)) / sum(q1.cm) ) ) * 100

print(paste0("error rate = ", round(q1.error_rate, digits = 2), " %"))

