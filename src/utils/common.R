# convert number to factor || create factor vector class for lda or naive bayes
convert.mask.factor <- function(clazz.vector){
  result <- c()
  if(is.vector(clazz.vector)){
    for(i in clazz.vector){
      if(i == 1){
        result <- c(result, "yes")
      } else{
        result <- c(result, "no")
      }
    }
  } else{
    quit(save = "default", status = 0, runLast = TRUE)
  }
  return (factor(result))
}

# hamonic mean 
harmonic.mean <- function(vector){
  sum <- 0
  for(el in vector){
    sum = sum + 1/el
  }
  return (length(vector)/sum)
}

# find different element two vector
find.diff <- function(x, y){
  result <- c()
  if(length(x) >= length(y)){
    for(xi in x){
      len.position <- length(which(y==xi))
      if(len.position == 0){
        result <- c(result, xi)
      }
    }
  } else {
    for(yi in y){
      len.position <- length(which(x==yi))
      if(len.position == 0){
        result <- c(result, yi)
      }
    }
  }
  return (result)
}


# convert number to factor
convert.mask.factor <- function(clazz.vector){
  result <- c()
  if(is.vector(clazz.vector)){
    for(i in clazz.vector){
      if(i == 1){
        result <- c(result, "yes")
      } else{
        result <- c(result, "no")
      }
    }
  } else{
    quit(save = "default", status = 0, runLast = TRUE)
  }
  return (factor(result))
}

find.sl.index <- function(best, all){
  index <- c()
  for(i in c(1: length(best))){
    cs.ft <- best[i]
    ind <- which(all == cs.ft)
    if(length(ind) != 0){
      index <- c(index, ind)
    }
  }
  return (index)
}
