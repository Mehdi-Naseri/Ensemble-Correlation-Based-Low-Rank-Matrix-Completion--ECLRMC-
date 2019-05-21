append.xlsx <- function(newdata, file) {
  if(file.exists(file)){
    existing.data <- read.xlsx(file)
    newdata <- rbind(existing.data, newdata)
  }
  write.xlsx(newdata,file)
}