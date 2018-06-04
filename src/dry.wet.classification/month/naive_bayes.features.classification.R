# load library
source("src/data.analyse/naive_bayes.classification.R")
source("src/utils/common.R")
source("src/utils/file.utils.R")
# load data
x.month <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
for(month in x.month){
  data.train.file <- sprintf("data/tuning/month/data.train.month_%s.csv", month)
  data.test.file <- sprintf("data/tuning/month/data.test.month_%s.csv", month)
  bayes.rs.log <- sprintf("result/month/m%s/bayes.m%s.wet.dry.classification.ft.log", month, month)
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
    bayes.classifier.max <- NULL
    print(filter)
    file.safewrite(filter, file=bayes.rs.log, is.append=TRUE)
    while(TRUE) {
      loop <- loop + 1
      step <- 0
      run.signal <- TRUE
      n.xft.choose <- length(xft.choose.ind)
      n.ft <- length(xft.try.ind)
      for(i in xft.try.ind) {
        ft.ind <- c(i, xft.choose.ind)
        # begin training model with tuning parameters for naive bayes
        bayes.classifier <- get.classifier.naive_bayes(data.train, data.test, ft.ind)
        if(filter == "train"){
          if(bayes.classifier$train.acc >= acc.train.max){
            bayes.classifier.max <- bayes.classifier
            print("current best classifier: "); print(bayes.classifier)
          }
        } else if(filter == "test"){
          if(bayes.classifier$test.acc >= acc.test.max){
            bayes.classifier.max <- bayes.classifier
            print("current best classifier: "); print(bayes.classifier)
          }
        } else if(filter == "and"){
          if(bayes.classifier$test.acc >= acc.test.max & bayes.classifier$train.acc >= acc.train.max){
            bayes.classifier.max <- bayes.classifier
            print("current best classifier: "); print(bayes.classifier)
          }
        } else if(filter == "or"){
          if(bayes.classifier$test.acc >= acc.test.max | bayes.classifier$train.acc >= acc.train.max){
            bayes.classifier.max <- bayes.classifier
            print("current best classifier: "); print(bayes.classifier)
          }
        } else if(filter == "mean"){
          if(bayes.classifier$mean.acc >= acc.mean.max){
            bayes.classifier.max <- bayes.classifier
            acc.mean.max <- bayes.classifier.max$mean.acc
            print("current best classifier: "); print(bayes.classifier)
          }
        } else if(filter == "harmonic-mean"){
          if(bayes.classifier$hmean.acc >= acc.hmean.max){
            bayes.classifier.max <- bayes.classifier
            acc.hmean.max <- bayes.classifier.max$hmean.acc
            print("current best classifier: "); print(bayes.classifier)
          }
        }
        # setiing acc to current model
        acc.train.max <- bayes.classifier.max$train.acc
        acc.test.max <- bayes.classifier.max$test.acc
        acc.mean.max <- bayes.classifier.max$mean.acc
        acc.hmean.max <- bayes.classifier.max$hmean.acc
        # info features for debug
        step <- step + 1
        # actual.ft.name <- paste(features[ft.ind], collapse = ",")
        info <- sprintf("loop: %d, features: %s || step = %d/%d", loop, bayes.classifier$features, step, n.ft)
        print(info)
      }
      #
      ft.ind.max <- bayes.classifier.max$feature.index
      cat("[ft.ind.max]: "); print(ft.ind.max)
      cat("[xft.choose.ind]: "); print(xft.choose.ind)
      diff.ind <- find.diff(ft.ind.max, xft.choose.ind)
      cat("[diff.ind]: "); print(diff.ind)
      if(length(diff.ind) == 1){
        xft.try.ind <- xft.try.ind[-diff.ind]
        xft.choose.ind <- ft.ind.max
        line.write <- sprintf("features = %s, train.acc = %f, test.acc = %f", bayes.classifier.max$features, bayes.classifier.max$train.acc, bayes.classifier.max$test.acc)
        print(line.write)
        file.safewrite(line.write, file=bayes.rs.log, is.append=TRUE) 
      } else {
        print("End of selecting feature process.")
        break
      }
      print("< =========================================================== >")
    }
  }
}
