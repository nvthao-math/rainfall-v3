# load library
source("src/data.analyse/svm.classification.R")
source("src/utils/common.R")
source("src/utils/file.utils.R")
library(caret)
# load data
x.month <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
for(month in x.month){
  data.train.file <- sprintf("data/tuning/month/data.train.month_%s.csv", month)
  data.test.file <- sprintf("data/tuning/month/data.test.month_%s.csv", month)
  svm.rs.log <- sprintf("result/month/m%s/svm.m%s.wet.dry.classification.ft.log", month, month)
  data.train <- read.csv(data.train.file, header=TRUE)
  data.test <- read.csv(data.test.file, header=TRUE)
  #
  acc.filters <- c("train", "test", "and", "or", "mean", "harmonic-mean")
  for(filter in acc.filters) {
    # feature selection
    xft.try.ind <- c(2:(ncol(data.train) -2))
    xft.choose.ind <- c()
    features <- names(data.train)
    cat("range feature recusive: "); print(features[xft.try.ind])
    loop <- 0
    acc.train.max <- 0
    acc.test.max <- 0
    acc.mean.max <- 0
    acc.hmean.max <- 0
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
        c.list <-  c(1) # c(1,10,100,1000)
        sigma.list <-  c(0.1) # c(0.001,0.01,0.1)
        svm.classifier <- get.classifier.svm(data.train, data.test, ft.ind, c.list, sigma.list, "rbf", "C-svc")
        if(filter == "train"){
          if(svm.classifier$train.acc >= acc.train.max){
            svm.classifier.max <- svm.classifier
            print("current best classifier: "); print(svm.classifier)
          }
        } else if(filter == "test"){
          if(svm.classifier$test.acc >= acc.test.max){
            svm.classifier.max <- svm.classifier
            print("current best classifier: "); print(svm.classifier)
          }
        } else if(filter == "and"){
          if(svm.classifier$test.acc >= acc.test.max & svm.classifier$train.acc >= acc.train.max){
            svm.classifier.max <- svm.classifier
            print("current best classifier: "); print(svm.classifier)
          }
        } else if(filter == "or"){
          if(svm.classifier$test.acc >= acc.test.max | svm.classifier$train.acc >= acc.train.max){
            svm.classifier.max <- svm.classifier
            print("current best classifier: "); print(svm.classifier)
          }
        } else if(filter == "mean"){
          if(svm.classifier$mean.acc >= acc.mean.max){
            svm.classifier.max <- svm.classifier
            acc.mean.max <- svm.classifier.max$mean.acc
            print("current best classifier: "); print(svm.classifier)
          }
        } else if(filter == "harmonic-mean"){
          if(svm.classifier$hmean.acc >= acc.hmean.max){
            svm.classifier.max <- svm.classifier
            acc.hmean.max <- svm.classifier.max$hmean.acc
            print("current best classifier: "); print(svm.classifier)
          }
        }
        # setiing acc to current model
        acc.train.max <- svm.classifier.max$train.acc
        acc.test.max <- svm.classifier.max$test.acc
        acc.mean.max <- svm.classifier.max$mean.acc
        acc.hmean.max <- svm.classifier.max$hmean.acc
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
        xft.try.ind <- xft.try.ind[-diff.ind]
        xft.choose.ind <- ft.ind.max
        line.write <- sprintf("features = %s, train.acc = %f, test.acc = %f", svm.classifier.max$features, svm.classifier.max$train.acc, svm.classifier.max$test.acc)
        print(line.write)
        file.safewrite(line.write, file=svm.rs.log, is.append=TRUE) 
      } else {
        print("End of selecting feature process.")
        break
      }
      print("< =========================================================== >")
    }
  }
}
