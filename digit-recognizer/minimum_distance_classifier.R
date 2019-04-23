setwd('G:/dev/R/digit-recognizer')
require('stats')
train <- read.csv('./data/train.csv')

par(mfrow = c(10, 10), mar=c(0.1, 0.1, 0.1, 0.1))

set.seed(2008)
data <- sample(as.integer(row.names(train[train$label == 7,])), 100)
for (k in data) {
  mt <- matrix(as.numeric(train[k, 2:785]), 28, 28, byrow=FALSE)
  image(mt, axes=FALSE, col=topo.colors(12))
}

set.seed(2008)
idx <- sample(42000, 10000)
train_sub <- train[-idx, ]
test <- train[idx, ]

# 得到新的训练数据
train_new <- NULL
for (i in 0:9) {
  subdata <- filter(train_sub, label == i)
  fit <- kmeans(subdata[, -1], centers = 50, iter.max = 20) # centers的数量待考
  tmp <- cbind(label=rep(i, 50), fit$centers)
  train_new <- rbind(train_new, tmp)
}

# 预测数据距离测算
distances <- NULL
n_test <- nrow(test)
n_train <- nrow(train_new)
train_label <- train_new[, 1]
test_label <- test[, 1]
train_x <- as.matrix(train_new[, -1])
test_x <- as.matrix(test[, -1])

for (j in 1:n_test) {
  dist <- rowSums((train_x - t(replicate(n_train, test_x[j, ])))^2)
  distances <- cbind(distances, dist)
}

distances <- t(distances)
pred <- NULL
for (i in 1:n_test) {
  pred <- c(pred, train_label[which.min(distances[i,])])
}

accuracy <- function (pred,actual)
{
  conf_matrix <- table(predictions = pred, actual = actual)
  sum <- 0
  for (j in 1:10)
    for (i in 1:10)
      if (j == i)
        sum<-sum+conf_matrix[i,j]
  return(list(conf_matrix,sum/length(pred)))
}

accuracy(pred, test_label)
