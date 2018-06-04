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
physic.ft <- "shum500, ncep_p__v, U500, U925, P500, shum700, ncep_p8_z, clazz"
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
# Cluster -1: (1.001, 7.421) || instance: 3387
# Cluster 0: (7.424, 46.978) || instance: 2919
# Cluster 1: (47.224, 564.175) || instance: 423
#
    i_sig <- 0.003
    i_c <- 2
    svp <- ksvm(data.train.x, data.train.y, type="C-svc", kernel='rbf', kpar=list(sigma=i_sig), C=i_c)
    # predict labels on test
    train.data.ypred = predict(svp, data.train.x)
    test.data.ypred = predict(svp, data.test.x)
    # table(data.test.y, data.ypred)
    # compute accuracy
    train.high.acc <- c(sum(train.data.ypred[which(data.train.y==1)] == 1)/(sum(data.train.y==1)))
    train.medium.acc <- c(sum(train.data.ypred[which(data.train.y==0)] == 0)/(sum(data.train.y==0)))
    train.low.acc <- c(sum(train.data.ypred[which(data.train.y==-1)]==-1)/(sum(data.train.y==-1)))
    train.acc <- c(sum(train.data.ypred==data.train.y)/length(data.train.y))
    # test acc
    test.high.acc <- c(sum(test.data.ypred[which(data.test.y==1)] == 1)/(sum(data.test.y==1)))
    test.medium.acc <- c(sum(test.data.ypred[which(data.test.y==0)] == 0)/(sum(data.test.y==0)))
    test.low.acc <- c(sum(test.data.ypred[which(data.test.y==-1)]==-1)/(sum(data.test.y==-1)))
    test.acc <- c(sum(test.data.ypred==data.test.y)/length(data.test.y))
    c <- c(i_c)
    sigma <- c(i_sig)
    #
    line.write <- sprintf("c = %s, sigma = %s, train.acc = %f, test.acc = %f, train.high.acc = %f, train.medium.acc = %f, train.low.acc = %f, test.high.acc = %f, test.medium.acc = %f, test.low.acc = %f", 
                          c, sigma, train.acc, test.acc, train.high.acc, train.medium.acc, train.low.acc, test.high.acc, test.medium.acc, test.low.acc)
    print(line.write)

    
    
    train.medium.acc <- c(sum(train.data.ypred[which(data.train.y==0)] == 0)/(sum(data.train.y==0)))
    ###
    ind <- which(test.data.ypred == 0)
    xx <- data.test$rainfall[ind]
    
    
    
    
    
    
    
    