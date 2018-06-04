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
rain.km <- kmeans(rain.sc, 2)
cluster.x <- rain.sc[which(rain.km$cluster == 1)]
plot(c(1:length(cluster.x)), cluster.x); lines(cluster.x)
cx.max <- max(cluster.x)
cx.min <- min(cluster.x)
cluster.y <- rain.sc[which(rain.km$cluster == 2)]
plot(c(1:length(cluster.y)), cluster.y); lines(cluster.y)
cy.max <- max(cluster.y)
cy.min <- min(cluster.y)
interval.list <- sort(c(cx.max, cx.min, cy.max, cy.min))
interval <- (interval.list[2]+interval.list[3])/2
#
extreme <- c()
for(i in c(1:nrow(data.train.extreme))){
  if(data.train.extreme[i, ]$rainfall > interval){
    extreme <- c(extreme, 1)
  } else {
    extreme <- c(extreme, -1)
  }
}
data.train.extreme <- cbind(data.train.extreme, extreme)
colnames(data.train.extreme) <- names(data.train)
#
extreme <- c()
for(i in c(1:nrow(data.test.extreme))){
  if(data.test.extreme[i, ]$rainfall > interval){
    extreme <- c(extreme, 1)
  } else {
    extreme <- c(extreme, -1)
  }
}
data.test.extreme <- cbind(data.test.extreme, extreme)
colnames(data.test.extreme) <- names(data.test)
#
write.table(data.train.extreme, file="data/tuning/extreme/data.train.extreme.csv", row.names=FALSE, sep=",", quote=FALSE)
write.table(data.test.extreme, file="data/tuning/extreme/data.test.extreme.csv", row.names=FALSE, sep=",", quote=FALSE)

