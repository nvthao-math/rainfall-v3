library(stringr)
# dry season (11-04)
# rain season (05-10)
data.train <- read.csv("data/tuning/rainfall_train.csv", header=TRUE)
month_01 <- data.frame()
month_02 <- data.frame()
month_03 <- data.frame()
month_04 <- data.frame()
month_05 <- data.frame()
month_06 <- data.frame()
month_07 <- data.frame()
month_08 <- data.frame()
month_09 <- data.frame()
month_10 <- data.frame()
month_11 <- data.frame()
month_12 <- data.frame()
for(i in c(1:nrow(data.train))){
  curr.row <- data.train[i,]
  month <- data.matrix(strsplit(as.character(curr.row$date), "/"))[[1]][1]
  print(month); 
  if(month == "1"){
    month_01 <- rbind(month_01, curr.row)
  } else if(month == "2"){
    month_02 <- rbind(month_02, curr.row)
  }else if(month == "3"){
    month_03 <- rbind(month_03, curr.row)
  }else if(month == "4"){
    month_04 <- rbind(month_04, curr.row)
  }else if(month == "5"){
    month_05 <- rbind(month_05, curr.row)
  }else if(month == "6"){
    month_06 <- rbind(month_06, curr.row)
  }else if(month == "7"){
    month_07 <- rbind(month_07, curr.row)
  }else if(month == "8"){
    month_08 <- rbind(month_08, curr.row)
  }else if(month == "9"){
    month_09 <- rbind(month_09, curr.row)
  }else if(month == "10"){
    month_10 <- rbind(month_10, curr.row)
  }else if(month == "11"){
    month_11 <- rbind(month_11, curr.row)
  }else if(month == "12"){
    month_12 <- rbind(month_12, curr.row)
  }
}
cols.name <- names(data.train)
colnames(month_01) <- cols.name
colnames(month_02) <- cols.name
colnames(month_03) <- cols.name
colnames(month_04) <- cols.name
colnames(month_05) <- cols.name
colnames(month_06) <- cols.name
colnames(month_07) <- cols.name
colnames(month_08) <- cols.name
colnames(month_09) <- cols.name
colnames(month_10) <- cols.name
colnames(month_11) <- cols.name
colnames(month_12) <- cols.name
write.table(month_01, file="data/tuning/month/data.train.month_01.csv", row.names=FALSE, sep=",", quote=FALSE)
write.table(month_02, file="data/tuning/month/data.train.month_02.csv", row.names=FALSE, sep=",", quote=FALSE)
write.table(month_03, file="data/tuning/month/data.train.month_03.csv", row.names=FALSE, sep=",", quote=FALSE)
write.table(month_04, file="data/tuning/month/data.train.month_04.csv", row.names=FALSE, sep=",", quote=FALSE)
write.table(month_05, file="data/tuning/month/data.train.month_05.csv", row.names=FALSE, sep=",", quote=FALSE)
write.table(month_06, file="data/tuning/month/data.train.month_06.csv", row.names=FALSE, sep=",", quote=FALSE)
write.table(month_07, file="data/tuning/month/data.train.month_07.csv", row.names=FALSE, sep=",", quote=FALSE)
write.table(month_08, file="data/tuning/month/data.train.month_08.csv", row.names=FALSE, sep=",", quote=FALSE)
write.table(month_09, file="data/tuning/month/data.train.month_09.csv", row.names=FALSE, sep=",", quote=FALSE)
write.table(month_10, file="data/tuning/month/data.train.month_10.csv", row.names=FALSE, sep=",", quote=FALSE)
write.table(month_11, file="data/tuning/month/data.train.month_11.csv", row.names=FALSE, sep=",", quote=FALSE)
write.table(month_12, file="data/tuning/month/data.train.month_12.csv", row.names=FALSE, sep=",", quote=FALSE)

#  ## ### ###
# ## ## ## ##
## ### ### ##
#  ## ### ###
# ## ## ## ##
## ### ### ##

data.test <- read.csv("data/tuning/rainfall_test.csv", header=TRUE)
month_01 <- data.frame()
month_02 <- data.frame()
month_03 <- data.frame()
month_04 <- data.frame()
month_05 <- data.frame()
month_06 <- data.frame()
month_07 <- data.frame()
month_08 <- data.frame()
month_09 <- data.frame()
month_10 <- data.frame()
month_11 <- data.frame()
month_12 <- data.frame()
for(i in c(1:nrow(data.test))){
  curr.row <- data.test[i,]
  month <- data.matrix(strsplit(as.character(curr.row$date), "/"))[[1]][1]
  if(month == "1"){
    month_01 <- rbind(month_01, curr.row)
  } else if(month == "2"){
    month_02 <- rbind(month_02, curr.row)
  }else if(month == "3"){
    month_03 <- rbind(month_03, curr.row)
  }else if(month == "4"){
    month_04 <- rbind(month_04, curr.row)
  }else if(month == "5"){
    month_05 <- rbind(month_05, curr.row)
  }else if(month == "6"){
    month_06 <- rbind(month_06, curr.row)
  }else if(month == "7"){
    month_07 <- rbind(month_07, curr.row)
  }else if(month == "8"){
    month_08 <- rbind(month_08, curr.row)
  }else if(month == "9"){
    month_09 <- rbind(month_09, curr.row)
  }else if(month == "10"){
    month_10 <- rbind(month_10, curr.row)
  }else if(month == "11"){
    month_11 <- rbind(month_11, curr.row)
  }else if(month == "12"){
    month_12 <- rbind(month_12, curr.row)
  }
}
cols.name <- names(data.test)
colnames(month_01) <- cols.name
colnames(month_02) <- cols.name
colnames(month_03) <- cols.name
colnames(month_04) <- cols.name
colnames(month_05) <- cols.name
colnames(month_06) <- cols.name
colnames(month_07) <- cols.name
colnames(month_08) <- cols.name
colnames(month_09) <- cols.name
colnames(month_10) <- cols.name
colnames(month_11) <- cols.name
colnames(month_12) <- cols.name
write.table(month_01, file="data/tuning/month/data.test.month_01.csv", row.names=FALSE, sep=",", quote=FALSE)
write.table(month_02, file="data/tuning/month/data.test.month_02.csv", row.names=FALSE, sep=",", quote=FALSE)
write.table(month_03, file="data/tuning/month/data.test.month_03.csv", row.names=FALSE, sep=",", quote=FALSE)
write.table(month_04, file="data/tuning/month/data.test.month_04.csv", row.names=FALSE, sep=",", quote=FALSE)
write.table(month_05, file="data/tuning/month/data.test.month_05.csv", row.names=FALSE, sep=",", quote=FALSE)
write.table(month_06, file="data/tuning/month/data.test.month_06.csv", row.names=FALSE, sep=",", quote=FALSE)
write.table(month_07, file="data/tuning/month/data.test.month_07.csv", row.names=FALSE, sep=",", quote=FALSE)
write.table(month_08, file="data/tuning/month/data.test.month_08.csv", row.names=FALSE, sep=",", quote=FALSE)
write.table(month_09, file="data/tuning/month/data.test.month_09.csv", row.names=FALSE, sep=",", quote=FALSE)
write.table(month_10, file="data/tuning/month/data.test.month_10.csv", row.names=FALSE, sep=",", quote=FALSE)
write.table(month_11, file="data/tuning/month/data.test.month_11.csv", row.names=FALSE, sep=",", quote=FALSE)
write.table(month_12, file="data/tuning/month/data.test.month_12.csv", row.names=FALSE, sep=",", quote=FALSE)
