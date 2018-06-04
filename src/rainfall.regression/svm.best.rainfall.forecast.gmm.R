#
source("src/data.analyse/svm.regression.R")
source("src/utils/common.R")

# get cluster of data
get.cluster <- function(data, cluster){
  ind <- which(data$clazz == cluster)
  get.data <- data[ind, ]
  return (get.data)
}

# get data statistic info
get.statistic <- function(origin.data, pred.data){
  sd.real <- sd(origin.data); mean.real <- mean(origin.data)
  sd.predict <- sd(pred.data); mean.predict <- mean(pred.data)
  info <- sprintf("origin: {sd: %0.3f, mean: %0.3f}, predicted: {sd: %0.3f, mean: %0.3f}", sd.real, mean.real, sd.predict, mean.predict)
  return (info)
}

#
cluster <- 0
data.train <- get.cluster(read.csv("data/tuning/forecast/data.train.extreme.gmm.csv", header=TRUE), cluster)
data.test <- get.cluster(read.csv("data/tuning/forecast/data.test.extreme.gmm.csv", header=TRUE), cluster)
#
ft <- "shum400, uswrf, R500, U850, P300, shum925, rainfall"
ft.choose <- unlist(strsplit(ft, ", "))
col.name <- colnames(data.train)
ind.choose <- c()
for(ft_i in c(1:length(ft.choose))){
  ind <- which(col.name == ft.choose[ft_i])
  ind.choose <- c(ind.choose, ind)
}
ind.choose <- sort(ind.choose)
name.ft.choose <- col.name[ind.choose]
# data preparation
data.train.ft <- data.train[, ind.choose]
data.train.x <- as.data.frame(data.train.ft[-ncol(data.train.ft)])
data.train.y <- data.train.ft$rainfall
data.test.ft <- data.test[, ind.choose]
data.test.x <- as.data.frame(data.test.ft[-ncol(data.test.ft)])
data.test.y <- data.test.ft$rainfall
#
epsilon = 0.1
cost = 1
gamma = 1
# svm.classifier <- get.regression.svm(data.train, data.test, ft.ind, epsilon, cost, gamma)
tuneResult <- tune(svm, rainfall ~.,  data = data.train.ft, ranges = list(epsilon = epsilon, cost = cost, gamma = gamma))
tunedModel <- tuneResult$best.model
y.predict.test <- predict(tunedModel, data.test.x)
error.test <- data.test.y - y.predict.test  
rmse.test <- sqrt(mean(error.test^2)) # rmse(error)
# rain acc
y.predict.train <- predict(tunedModel, data.train.x)
error.train <- data.train.y - y.predict.train  
rmse.train <- sqrt(mean(error.train^2)) # rmse(error)
#
par(mfrow = c(3, 2))
# plot data test
x.bar.test <- c(1:length(data.test.y))
plot(x.bar.test, data.test.y, col = "blue", type="b", lwd = 2, lty = 1, xlab = "index [line: {blue: Observe, red: SVR}]", ylab = "precipitation", main = "Precipitation regression on test set")
points(x.bar.test, y.predict.test, col = "red", type="b", lwd = 2, lty = 2)
# legend(15, 560, legend = c("Observe", "SVR"), lty=c(1, 2),col=c("blue","red"), lwd = c(2, 2), cex=0.2)
test.range <- range(1, 8)
qqplot(data.test.y, y.predict.test, xlim = test.range, ylim = test.range, xlab = "Observe value", ylab = "Predicted value", main = "Quantile-to-quantile on test set")
abline(a=1, b=1, lty="dotted")
#
x.bar.train <- c(1:length(data.train.y))
plot(x.bar.train, data.train.y, col = "blue", type="b", lwd = 2, lty = 1, xlab = "index [line: {blue: Observe, red: SVR}]", ylab = "precipitation", main = "Precipitation regression on train set")
points(x.bar.train, y.predict.train, col = "red", type="b", lwd = 2, lty = 2)
# legend(15, 560, legend = c("Observe", "SVR"), lty=c(1, 2),col=c("blue","red"), lwd = c(2, 2), cex=0.6)
train.range <- range(1, 8)
qqplot(data.train.y, y.predict.train, xlim = train.range, ylim = train.range, xlab = "Observe value", ylab = "Predicted value", main = "Quantile-to-quantile on training set")
abline(a=1, b=1, lty="dotted")
# qq-plot on the whole
y.pred <- as.vector(c(y.predict.test, y.predict.train))
y.real <- as.vector(c(data.test.y, data.train.y))
x.bar.whole <- c(1:length(y.pred))
plot(x.bar.whole, y.real, col = "blue", type="b", lwd = 2, lty = 1, xlab = "index [line: {blue: Observe, red: SVR}]", ylab = "precipitation", main = "Precipitation regression on whole data")
points(x.bar.whole, y.pred, col = "red", type="b", lwd = 2, lty = 2)
# legend(34, 560, legend = c("Observe", "SVR"), lty=c(1, 2),col=c("blue","red"), lwd = c(2, 2), cex=0.6)
whole.range <- range(1, 8)
qqplot(y.real, y.pred, xlim = whole.range, ylim = whole.range, xlab = "Observe value", ylab = "Predicted value", main = "Quantile-to-quantile on whole dataset")
abline(a=1, b=1, lty="dotted")
# get statistic info
test.info <- get.statistic(data.test.y, y.predict.test)
print(test.info)
#
train.info <- get.statistic(data.train.y, y.predict.train)
print(train.info)
