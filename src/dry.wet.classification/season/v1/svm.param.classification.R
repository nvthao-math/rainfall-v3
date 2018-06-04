# load library
rm(list=ls(all=TRUE))
#
library(kernlab)
library(jsonlite)
library(caret)
require(gdata)
#
source("src/utils/common.R")
source("src/utils/file.utils.R")
#
x.season <- c("rain", "dry")
rain.ft <- "U925, ncep_p5th, ncep_lftx, ncep_p__f, dswrf, ncep_p__z, V925, rhum, ncep_p8_z, U500, Uas, V700, ncep_shum, mslp, Vas, U700, temp700, ncep_p8th, ncep_pr_wtr, clazz"
dry.ft <- "temp850, ncep_p__f, ncep_p5_z, R850, ncep_p5th, Uas, U700, R500, ncep_p__v, P500, shum700, clazz"
x.ft <- c(rain.ft, dry.ft)
for(i in c(1:length(x.season))){
  data.train.path <- sprintf("data/tuning/season/data.train.%s.csv", x.season[i])
  data.test.path <- sprintf("data/tuning/season/data.test.%s.csv", x.season[i])
  svm.rs.log <- sprintf("result/season/svm.param.%s.classification.ft.log", x.season[i])
  data.train <- read.csv(data.train.path, header=TRUE)
  data.test <- read.csv(data.test.path, header=TRUE)
  # features
  physic.ft <- x.ft[i]
  best.ft <- unlist(trim(strsplit(physic.ft, ",")))
  all.ft <- colnames(data.train)
  index <- find.sl.index(best.ft, all.ft)
  ####
  data.train.ft <- data.train[, index]
  data.test.ft <- data.test[, index]
  dim.seperate <- nrow(data.train.ft)
  # prepare data for training
  data.train.x <- data.matrix(data.train.ft[, -ncol(data.train.ft)])
  data.train.y <- data.train.ft$clazz
  data.test.x <- data.matrix(data.test.ft[, -ncol(data.train.ft)])
  data.test.y <- data.test.ft$clazz
  #
  data.scale <- rbind(data.train.x, data.test.x)
  data.scale <- scale(data.scale)
  data.train.x <- data.scale[c(1:dim.seperate),]
  data.test.x <- data.scale[-c(1:dim.seperate),]
  #
  # begin training model with tuning parameters for SVM
  c_value <- c(1:1000)
  sig_value <- c(0.001:0.1)
  for(i_c in c_value){
    for(i_sig in sig_value){
      svp <- ksvm(data.train.x, data.train.y, type="C-svc", kernel='rbf', kpar=list(sigma=i_sig), C=i_c)
      # predict labels on test
      train.data.ypred = predict(svp, data.train.x)
      test.data.ypred = predict(svp, data.test.x)
      # table(data.test.y, data.ypred)
      # compute accuracy
      train.rain.acc <- c(sum(train.data.ypred[which(data.train.y==1)] == 1)/(sum(train.data.ypred==1)))
      train.dry.acc <- c(sum(train.data.ypred[which(data.train.y==-1)]==-1)/(sum(train.data.ypred==-1)))
      train.acc <- c(sum(train.data.ypred==data.train.y)/length(data.train.y))
      test.rain.acc <- c(sum(test.data.ypred[which(data.test.y==1)] == 1)/(sum(test.data.ypred==1)))
      test.dry.acc <- c(sum(test.data.ypred[which(data.test.y==-1)]==-1)/(sum(test.data.ypred==-1)))
      test.acc <- c(sum(test.data.ypred==data.test.y)/length(data.test.y))
      c <- c(i_c)
      sigma <- c(i_sig)
      #
      line.write <- sprintf("c = %s, sigma = %s, train.rain.acc = %f, train.dry.acc = %f,train.acc = %f, test.rain.acc = %f, test.dry.acc = %f, test.acc = %f", 
                            c, sigma, train.rain.acc, train.dry.acc, train.acc, test.rain.acc, test.dry.acc, test.acc)
      print(line.write)
      file.safewrite(line.write, file.path=svm.rs.log, is.append=TRUE)
    }
  }
}





