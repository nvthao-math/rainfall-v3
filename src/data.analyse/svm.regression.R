# epsilon = 0.1
# cost = 1
# gamma = 1
require(e1071)
get.regression.svm <- function(data.train, data.test, ft.ind, epsilon, cost, gamma){
  get.info <- NULL
  if(ncol(data.train) != ncol(data.test)){
    info <- sprintf("ERROR number column of train and test set are different, ncol(data.train) = %d, ncol(data.test) = %d", ncol(data.train), ncol(data.test))
    print(info)
    quit(save = "default", status = 0, runLast = TRUE)
  } else {
    # data preparation
    ft.name <-paste(colnames(data.train)[ft.ind], collapse = ", ")
    data.train.x <- data.frame(cbind(data.train[, ft.ind], data.train$rainfall))
    colnames(data.train.x) <- c(colnames(data.train)[ft.ind], "rainfall")
    data.train.y <- data.train$rainfall
    data.test.x <- data.frame(data.test[, ft.ind])
    colnames(data.test.x) <- colnames(data.test)[ft.ind]
    data.test.y <- data.test$rainfall
    #
    tuneResult <- tune(svm, rainfall ~.,  data = data.train.x, ranges = list(epsilon = epsilon, cost = cost, gamma = gamma))
    tunedModel <- tuneResult$best.model
    y.predict.test <- predict(tunedModel, data.test.x)
    error.test <- data.test.y - y.predict.test  
    rmse.test <- sqrt(mean(error.test^2)) # rmse(error)
    # rain acc
    y.predict.train <- predict(tunedModel, data.train.x[-ncol(data.train.x)])
    error.train <- data.train.y - y.predict.train  
    rmse.train <- sqrt(mean(error.train^2)) # rmse(error)
    # max 
    # indmax <- which(data.test.y > 70)
    # print(indmax)
    # error.test.max <- data.test.y[indmax] - y.predict.test[indmax]  
    # rmse.test.max <- sqrt(mean(error.test^2)) # rmse(error)
    #
    classifier.info <- sprintf("features: %s, rmse.train = %f, rmse.test = %f", ft.name, rmse.train, rmse.test)
    print(classifier.info)
    get.info <- list(features = ft.name, feature.index = ft.ind, model = tunedModel,
                     rmse.train = rmse.train, rmse.test = rmse.test, rmse.test.max = rmse.test.max,
                     mean.error = mean(c(rmse.train, rmse.test)), hmean.error = harmonic.mean(c(rmse.train, rmse.test)))
  }
  return (get.info)
}