library(stringr)
# dry season (11-04)
# rain season (05-10)
data.train <- read.csv("data/tuning/rainfall_train.csv", header=TRUE)
data.train.dry <- data.frame()
data.train.rain <- data.frame()
for(i in c(1:nrow(data.train))){
  curr.row <- data.train[i,]
  dry.month <- c("11", "12", "01", "02", "03", "04")
  month <- data.matrix(strsplit(as.character(curr.row$date), "-"))[[1]][2]
  xbl.dry <- length(which(str_detect(month, dry.month) == TRUE))
  if(xbl.dry > 0){
    data.train.dry <- rbind(data.train.dry, curr.row)
  } else {
    data.train.rain <- rbind(data.train.rain, curr.row)
  }
}
colnames(data.train.dry) <- names(data.train)
colnames(data.train.rain) <- names(data.train)
write.table(data.train.dry, file="data/tuning/season/data.train.dry.csv", row.names=FALSE, sep=",", quote=FALSE)
write.table(data.train.rain, file="data/tuning/season/data.train.rain.csv", row.names=FALSE, sep=",", quote=FALSE)
#
#
data.test <- read.csv("data/tuning/rainfall_test.csv", header=TRUE)
data.test.dry <- data.frame()
data.test.rain <- data.frame()
for(i in c(1:nrow(data.test))){
  curr.row <- data.test[i,]
  dry.month <- c("11", "12", "01", "02", "03", "04")
  month <- data.matrix(strsplit(as.character(curr.row$date), "-"))[[1]][2]
  xbl.dry <- length(which(str_detect(month, dry.month) == TRUE))
  if(xbl.dry > 0){
    data.test.dry <- rbind(data.test.dry, curr.row)
  } else {
    data.test.rain <- rbind(data.test.rain, curr.row)
  }
}
colnames(data.test.dry) <- names(data.test)
colnames(data.test.rain) <- names(data.test)
write.table(data.test.dry, file="data/tuning/season/data.test.dry.csv", row.names=FALSE, sep=",", quote=FALSE)
write.table(data.test.rain, file="data/tuning/season/data.test.rain.csv", row.names=FALSE, sep=",", quote=FALSE)
