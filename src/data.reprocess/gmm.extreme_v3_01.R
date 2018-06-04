#
require(mclust)
#
# X1 <- c(rnorm(200, 10, 3), rnorm(200, 25,3), rnorm(200,35,3),  rnorm(200,80,5))
# Y1 <- c(rnorm(800, 30, 2))
# xyMclust <- Mclust(data.frame (X1,Y1))
# plot(xyMclust)
# #
# xyMclust4 <- Mclust(data.frame (X1,Y1), G=3)
# plot(xyMclust4)
######
#
data.train <- read.csv("data/tuning/rainfall_train.csv", header=TRUE)
data.train.extreme <- data.frame()
for(i in c(1:nrow(data.train))){
  rainfall <- data.train[i,]$rainfall
  if(rainfall > 0){
    data.train.extreme <- rbind(data.train.extreme, data.train[i,][-ncol(data.train)])
  }
}
#
data.test <- read.csv("data/tuning/rainfall_test.csv", header=TRUE)
data.test.extreme <- data.frame()
for(i in c(1: nrow(data.test))){
  rainfall <- data.test[i,]$rainfall
  if(rainfall > 0){
    data.test.extreme <- rbind(data.test.extreme, data.test[i,][-ncol(data.test)])
  }
}
#
#
rain.sc <- c(data.train.extreme$rainfall, data.test.extreme$rainfall)
rain.sc <- rain.sc[which(rain.sc > 1)]
# rain.km <- kmeans(rain.sc, 3)
model <- Mclust(rain.sc, G=2)
#
#
cluster.x <- rain.sc[which(model$classification == 1)]
plot(c(1:length(cluster.x)), cluster.x); lines(cluster.x)
cx.max <- max(cluster.x)
cx.min <- min(cluster.x)
cluster.y <- rain.sc[which(model$classification == 2)]
plot(c(1:length(cluster.y)), cluster.y); lines(cluster.y)
cy.max <- max(cluster.y)
cy.min <- min(cluster.y)
cluster.z <- rain.sc[which(model$classification == 3)]
plot(c(1:length(cluster.z)), cluster.z); lines(cluster.z)
cz.max <- max(cluster.z)
cz.min <- min(cluster.z)
interval.list <- sort(c(cx.max, cx.min, cy.max, cy.min, cz.max, cz.min))
interval.a <- (interval.list[2]+interval.list[3])/2 # 7.4225
interval.b <- (interval.list[4]+interval.list[5])/2 # 47.101
# Cluster -1: (1.001, 7.421) || instance: 3387
# Cluster 0: (7.424, 46.978) || instance: 2919
# Cluster 1: (47.224, 564.175) || instance: 423
extreme <- c()
for(i in c(1:nrow(data.train.extreme))){
  if(data.train.extreme[i, ]$rainfall > interval.b){
    extreme <- c(extreme, 1)
  } else if(data.train.extreme[i, ]$rainfall < interval.a){
    extreme <- c(extreme, -1)
  } else {
    extreme <- c(extreme, 0)
  }
}
data.train.extreme <- cbind(data.train.extreme, extreme)
colnames(data.train.extreme) <- names(data.train)
#
extreme <- c()
for(i in c(1:nrow(data.test.extreme))){
  if(data.test.extreme[i, ]$rainfall > interval.b){
    extreme <- c(extreme, 1)
  } else if(data.test.extreme[i, ]$rainfall < interval.a){
    extreme <- c(extreme, -1)
  }else {
    extreme <- c(extreme, 0)
  }
}
data.test.extreme <- cbind(data.test.extreme, extreme)
colnames(data.test.extreme) <- names(data.test)
#
write.table(data.train.extreme, file="data/tuning/extreme/data.train.extreme.gmm.csv", row.names=FALSE, sep=",", quote=FALSE)
write.table(data.test.extreme, file="data/tuning/extreme/data.test.extreme.gmm.csv", row.names=FALSE, sep=",", quote=FALSE)
