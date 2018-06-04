#
source("src/data.analyse/svm.regression.R")
source("src/utils/common.R")
source("src/utils/file.utils.R")
# get cluster of data
get.cluster <- function(data, cluster){
  ind <- which(data$clazz == cluster)
  get.data <- data[ind, ]
  return (get.data)
}
#
cluster <- 1
data.train <- get.cluster(read.csv("data/tuning/extreme/data.train.extreme.gmm.csv", header=TRUE), cluster)
data.test <- get.cluster(read.csv("data/tuning/extreme/data.test.extreme.gmm.csv", header=TRUE), cluster)
log <- sprintf("result/forecast/svm.rainfall.regression.scan_param(cluster0%s).log", cluster)
#
ft <- "ncep_lftx, ncep_p5zh, ncep_p5_z, rainfall"
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
# begin training model with tuning parameters for SVM
list.epsilon = seq(0.1,10,0.1) # seq(0.08,10,0.01)
list.cost = c(1:20)
list.gamma = seq(0, 10, 0.1)
for(epsilon in list.epsilon){
  for(cost in list.cost){
    for(gamma in list.gamma){
      tuneResult <- tune(svm, rainfall ~.,  data = data.train.ft, ranges = list(epsilon = epsilon, cost = cost, gamma = gamma))
      tunedModel <- tuneResult$best.model
      y.predict.test <- predict(tunedModel, data.test.x)
      error.test <- data.test.y - y.predict.test  
      rmse.test <- sqrt(mean(error.test^2)) # rmse(error)
      # rain acc
      y.predict.train <- predict(tunedModel, data.train.x)
      error.train <- data.train.y - y.predict.train  
      rmse.train <- sqrt(mean(error.train^2)) # rmse(error)
      result <- sprintf("epsilon: %0.3f, cost: %d, gamma: %0.3f, test.rmse: %0.3f, train.rmse: %0.3f", epsilon, cost, gamma, rmse.test, rmse.train)
      print(result)
      file.safewrite(result, file=log, is.append=TRUE) 
    }
  }
}
