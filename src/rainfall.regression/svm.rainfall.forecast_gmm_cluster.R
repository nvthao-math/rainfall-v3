rm(list=ls(all=TRUE))
# load library
source("src/data.analyse/svm.regression.R")
source("src/utils/common.R")
source("src/utils/file.utils.R")

# get cluster of data
get.cluster <- function(data, cluster){
  ind <- which(data$clazz == cluster)
  get.data <- data[ind, ]
  return (get.data)
}

library(caret)
cluster <- 1
data.train <- get.cluster(read.csv("data/tuning/extreme/data.train.extreme.gmm.csv", header=TRUE), cluster)
data.test <- get.cluster(read.csv("data/tuning/extreme/data.test.extreme.gmm.csv", header=TRUE), cluster)
svm.rs.log <- sprintf("result/forecast/svm.rainfall.forecast.ft.gmm(cluster_%02d).log", cluster)
acc.filters <- c("max") # "train", "test", "and", "or", "mean", "harmonic-mean"
for(filter in acc.filters) {
  # feature selection
  xft.try.ind <- c(2:(ncol(data.train) - 2))
  xft.choose.ind <- c()
  features <- names(data.train)
  cat("range feature recusive: "); print(features[xft.try.ind])
  loop <- 0
  acc.train.max <- .Machine$integer.max
  acc.test.max <- .Machine$integer.max
  acc.mean.max <- .Machine$integer.max
  acc.hmean.max <- .Machine$integer.max
  rmse.test.max <- .Machine$integer.max
  svm.classifier.max <- NULL
  print(filter)
  file.safewrite(filter, file=svm.rs.log, is.append=TRUE)
  while(TRUE) {
    loop <- loop + 1
    step <- 0
    run.signal <- TRUE
    n.xft.choose <- length(xft.choose.ind)
    n.ft <- length(xft.try.ind)
    for(i in xft.try.ind) {
      ft.ind <- c(i, xft.choose.ind)
      # begin training model with tuning parameters for SVM
      epsilon = 0.1
      cost = 1
      gamma = 1
      svm.classifier <- get.regression.svm(data.train, data.test, ft.ind, epsilon, cost, gamma)
      if(filter == "train"){
        if(svm.classifier$rmse.train <= acc.train.max){
          svm.classifier.max <- svm.classifier
          print("current best classifier: "); print(svm.classifier)
        }
      } else if(filter == "test"){
        if(svm.classifier$rmse.test <= acc.test.max){
          svm.classifier.max <- svm.classifier
          print("current best classifier: "); print(svm.classifier)
        }
      } else if(filter == "and"){
        if(svm.classifier$rmse.test <= acc.test.max & svm.classifier$rmse.train <= acc.train.max){
          svm.classifier.max <- svm.classifier
          print("current best classifier: "); print(svm.classifier)
        }
      } else if(filter == "or"){
        if(svm.classifier$rmse.test <= acc.test.max | svm.classifier$rmse.train <= acc.train.max){
          svm.classifier.max <- svm.classifier
          print("current best classifier: "); print(svm.classifier)
        }
      } else if(filter == "mean"){
        if(svm.classifier$mean.error <= acc.mean.max){
          svm.classifier.max <- svm.classifier
          print("current best classifier: "); print(svm.classifier)
        }
      } else if(filter == "harmonic-mean"){
        if(svm.classifier$hmean.error <= acc.hmean.max){
          svm.classifier.max <- svm.classifier
          print("current best classifier: "); print(svm.classifier)
        }
      }else if(filter == "max"){
        if(svm.classifier$rmse.test.max <= rmse.test.max){
          svm.classifier.max <- svm.classifier
          print("current best classifier: "); print(svm.classifier)
        }
      }
      # setiing acc to current model
      acc.train.max <- svm.classifier.max$rmse.train
      acc.test.max <- svm.classifier.max$rmse.test
      acc.mean.max <- svm.classifier.max$mean.error
      acc.hmean.max <- svm.classifier.max$hmean.error
      rmse.test.max <- svm.classifier.max$rmse.test.max
      # info features for debug
      step <- step + 1
      # actual.ft.name <- paste(features[ft.ind], collapse = ",")
      info <- sprintf("loop: %d, features: %s || step = %d/%d", loop, svm.classifier$features, step, n.ft)
      print(info)
    }
    #
    ft.ind.max <- svm.classifier.max$feature.index
    cat("[ft.ind.max]: "); print(ft.ind.max)
    cat("[xft.choose.ind]: "); print(xft.choose.ind)
    diff.ind <- find.diff(ft.ind.max, xft.choose.ind)
    cat("[diff.ind]: "); print(diff.ind)
    if(length(diff.ind) == 1){
      xft.try.ind <- xft.try.ind[-which(xft.try.ind == diff.ind)]
      xft.choose.ind <- ft.ind.max
      line.write <- sprintf("features = %s, rmse.train = %f, rmse.test = %f, rmse.test.max = %f", 
                            svm.classifier.max$features, svm.classifier.max$rmse.train, svm.classifier.max$rmse.test,
                            svm.classifier$rmse.test.max)
      print(line.write)
      file.safewrite(line.write, file=svm.rs.log, is.append=TRUE) 
    } else {
      print("End of selecting feature process.")
      break
    }
    print("< =========================================================== >")
  }
}
