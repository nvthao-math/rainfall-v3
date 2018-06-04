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
data.train <- read.csv("data/tuning/extreme/data.train.extreme.gmm.csv", header=TRUE)
data.test <- read.csv("data/tuning/extreme/data.test.extreme.gmm.csv", header=TRUE)
# svm.rs.log <- "result/year/svm.scan.param.log"
# features
physic.ft <- "V500, V850, R500, mslp, V700, U850, clazz"
best.ft <- unlist(trim(strsplit(physic.ft, ",")))
all.ft <- colnames(data.train)
index <- find.sl.index(best.ft, all.ft)
###
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
c_value <-c(1:1000)
sig_value <- c(0.001:0.1)
for(i_c in c_value){
    for(i_sig in sig_value){
      svp <- ksvm(data.train.x, data.train.y, type="C-svc", kernel='rbf', kpar=list(sigma=i_sig), C=i_c)
      # predict labels on test
      train.data.ypred = predict(svp, data.train.x)
      test.data.ypred = predict(svp, data.test.x)
      # table(data.test.y, data.ypred)
      # compute accuracy
      train.high.acc <- c(sum(train.data.ypred[which(data.train.y==1)] == 1)/(sum(train.data.ypred==1)))
      train.medium.acc <- c(sum(train.data.ypred[which(data.train.y==0)] == 0)/(sum(train.data.ypred==0)))
      train.low.acc <- c(sum(train.data.ypred[which(data.train.y==-1)]==-1)/(sum(train.data.ypred==-1)))
      train.acc <- c(sum(train.data.ypred==data.train.y)/length(data.train.y))
      # test acc
      test.high.acc <- c(sum(test.data.ypred[which(data.test.y==1)] == 1)/(sum(test.data.ypred==1)))
      test.medium.acc <- c(sum(test.data.ypred[which(data.test.y==0)] == 0)/(sum(test.data.ypred==0)))
      test.low.acc <- c(sum(test.data.ypred[which(data.test.y==-1)]==-1)/(sum(test.data.ypred==-1)))
      test.acc <- c(sum(test.data.ypred==data.test.y)/length(data.test.y))
      c <- c(i_c)
      sigma <- c(i_sig)
      #
      line.write <- sprintf("c = %s, sigma = %s, train.acc = %f, test.acc = %f, train.high.acc = %f, train.medium.acc = %f, train.low.acc = %f, test.high.acc = %f, test.medium.acc = %f, test.low.acc = %f", 
                            c, sigma, train.acc, test.acc, train.high.acc, train.medium.acc, train.low.acc, test.high.acc, test.medium.acc, test.low.acc)
      print(line.write)
      # file.safewrite(line.write, file.path=svm.rs.log, is.append=TRUE)
    }
}

