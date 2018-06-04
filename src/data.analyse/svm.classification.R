# description
# model.type="C-svc"
# kernel = "rbf"
get.classifier.svm <- function(data.train, data.test, ft.ind, c.list, sigma.list, kernel, model.type){
  library(kernlab)
  get.info <- NULL
  if(ncol(data.train) != ncol(data.test)){
    info <- sprintf("ERROR number column of train and test set are different, ncol(data.train) = %d, ncol(data.test) = %d", ncol(data.train), ncol(data.test))
    print(info)
    quit(save = "default", status = 0, runLast = TRUE)
  } else {
    # data preparation
    data.train.x <- data.matrix(data.train[, ft.ind])
    data.train.y <- data.train$clazz
    data.test.x <- data.matrix(data.test[, ft.ind])
    data.test.y <- data.test$clazz
    ft.name <-paste(colnames(data.train)[ft.ind], collapse = ", ")
    max.acc.train <- 0
    max.acc.test <- 0
    for(i_c in c.list){
      for(i_sig in sigma.list){
        svp <- ksvm(data.train.x, data.train.y, type=model.type, kernel=kernel, kpar=list(sigma=i_sig), C=i_c)
        # predict labels on test
        train.data.ypred = predict(svp, data.train.x)
        test.data.ypred = predict(svp, data.test.x)
        # compute accuracy
        train.acc <- c(sum(train.data.ypred==data.train.y)/length(data.train.y))
        train.rain.acc <- c(sum(train.data.ypred[which(data.train.y==1)] == 1)/(sum(train.data.ypred==1)))
        train.dry.acc <- c(sum(train.data.ypred[which(data.train.y==-1)]==-1)/(sum(train.data.ypred==-1)))
        test.rain.acc <- c(sum(test.data.ypred[which(data.test.y==1)] == 1)/(sum(test.data.ypred==1)))
        test.dry.acc <- c(sum(test.data.ypred[which(data.test.y==-1)]==-1)/(sum(test.data.ypred==-1)))
        test.acc <- c(sum(test.data.ypred==data.test.y)/length(data.test.y))
        classifier.info <- sprintf("features: %s, c = %d, sigma = %f, train.acc = %f, test.acc = %f", ft.name, i_c, i_sig, train.acc, test.acc)
        print(classifier.info)
        get.info <- list(features = ft.name, feature.index = ft.ind, c = i_c, sigma = i_sig, train.acc = train.acc, test.acc = test.acc, model = svp,
                         mean.acc = mean(c(train.acc, test.acc)), hmean.acc = harmonic.mean(c(train.acc, test.acc)),
                         train.rain.acc = train.rain.acc, train.dry.acc = train.dry.acc, test.rain.acc = test.rain.acc,
                         test.dry.acc = test.dry.acc)
      }
    }
  }
  return (get.info)
}


## 
### SVM three class classification
get.three.classifier.svm <- function(data.train, data.test, ft.ind, c.list, sigma.list, kernel, model.type){
  library(kernlab)
  get.info <- NULL
  if(ncol(data.train) != ncol(data.test)){
    info <- sprintf("ERROR number column of train and test set are different, ncol(data.train) = %d, ncol(data.test) = %d", ncol(data.train), ncol(data.test))
    print(info)
    quit(save = "default", status = 0, runLast = TRUE)
  } else {
    # data preparation
    data.train.x <- data.matrix(data.train[, ft.ind])
    data.train.y <- data.train$clazz
    data.test.x <- data.matrix(data.test[, ft.ind])
    data.test.y <- data.test$clazz
    ft.name <-paste(colnames(data.train)[ft.ind], collapse = ", ")
    max.acc.train <- 0
    max.acc.test <- 0
    for(i_c in c.list){
      for(i_sig in sigma.list){
        svp <- ksvm(data.train.x, data.train.y, type=model.type, kernel=kernel, kpar=list(sigma=i_sig), C=i_c)
        # predict labels on test
        train.data.ypred = predict(svp, data.train.x)
        test.data.ypred = predict(svp, data.test.x)
        # compute accuracy
        train.acc <- c(sum(train.data.ypred==data.train.y)/length(data.train.y))
        train.high.acc <- c(sum(train.data.ypred[which(data.train.y==1)] == 1)/(sum(train.data.ypred==1)))
        train.medium.acc <- c(sum(train.data.ypred[which(data.train.y==0)] == 0)/(sum(train.data.ypred==0)))
        train.low.acc <- c(sum(train.data.ypred[which(data.train.y==-1)]==-1)/(sum(train.data.ypred==-1)))
        # test acc
        test.acc <- c(sum(test.data.ypred==data.test.y)/length(data.test.y))
        test.high.acc <- c(sum(test.data.ypred[which(data.test.y==1)] == 1)/(sum(test.data.ypred==1)))
        test.medium.acc <- c(sum(test.data.ypred[which(data.test.y==0)] == 0)/(sum(test.data.ypred==0)))
        test.low.acc <- c(sum(test.data.ypred[which(data.test.y==-1)]==-1)/(sum(test.data.ypred==-1)))
        classifier.info <- sprintf("features: %s, c = %d, sigma = %f, train.acc = %f, test.acc = %f", ft.name, i_c, i_sig, train.acc, test.acc)
        print(classifier.info)
        get.info <- list(features = ft.name, feature.index = ft.ind, c = i_c, sigma = i_sig, train.acc = train.acc, test.acc = test.acc, model = svp,
                         mean.acc = mean(c(train.acc, test.acc)), hmean.acc = harmonic.mean(c(train.acc, test.acc)),
                         train.high.acc = train.high.acc, train.medium.acc = train.medium.acc, train.low.acc = train.low.acc,
                         test.high.acc = test.high.acc, test.medium.acc = test.medium.acc, test.low.acc = test.low.acc)
      }
    }
  }
  return (get.info)
}
