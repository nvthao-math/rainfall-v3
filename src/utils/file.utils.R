# write line to file safe mode
file.safewrite <- function(line, file.path, is.append){
  if(!file.exists(file.path)){
    parts <- unlist(strsplit(file.path, "/"))
    # file.name <- parts[length(parts)]
    folder <- paste(parts[c(1:(length(parts) - 1))], collapse = "/")
    if(!dir.exists(folder)){
      dir.create(folder, showWarnings = TRUE, recursive = TRUE)
    }
  }
  write(line, file=file.path,append=is.append)
}