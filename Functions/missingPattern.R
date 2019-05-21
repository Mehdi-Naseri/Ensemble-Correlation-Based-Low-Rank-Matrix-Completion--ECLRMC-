source("Functions/MissingPatternMatrix.R")
library(readxl)
library(openxlsx)
check_monotone <- function(sr){
  monotone=TRUE
  if(nrow(sr) == 1 && all(sr[1,] == 1)) {
    return(FALSE)
  }
  for(i in 1:nrow(sr)) {
    zero_flage = FALSE
    for(j in 1:ncol(sr)) {
      if(sr[i,j] == 0) {
        if(!all(sr[i:nrow(sr),j] == 0)){
          monotone=FALSE
          return(monotone)
        }
      }
    }
  }
  return(monotone)
}
missingPattern <- function(dm,datasetName, datasetSheet){
  pattern = ""
  dim = dim(dm)
  
  for(i in 1:nrow(dm)){
    for(j in 1:ncol(dm)) {
      if(!is.na(dm[i,j])){
        dm[i,j] = 1;
      }
    }
  }
  class(dm) <- 'numeric'
  # Missing data pattern image
  label=paste("Missing Data Pattern for", datasetName, datasetSheet)
  
  path = paste0("MissingPatterns/Images/", datasetName, "/")
  if(!dir.exists(path))
    dir.create(path)
  filename = paste0(path, datasetSheet, ".png")
  png(filename=filename)
  image(dm, xlab=label)
  dev.off()
  image(dm, xlab=label)
  # Get details about missing data
  #s<-prelim.cat(dm)
  s <- MissingPatternMatrix(dm)
  print("Missing Data Matrix:")
  # Save missing pattern matrix
  path = paste0("MissingPatterns/Matrices/", datasetName, "/")
  if(!dir.exists(path))
    dir.create(path)
  write.xlsx(s$r,paste0(path,datasetSheet))
  print(s$r)
  if (sum(s$nmis) == 0){
    pattern = ("No Missing Data")
  }else if (sum(s$nmis == 1)){
    pattern = ("Univariate")
  }else {
    sr=s$r
    monotone=check_monotone(sr)
    if(monotone){
      pattern = ("Monotone")
    } else {
      pattern = ("Arbitrary")
    }
  }
  print(pattern)
  append.xlsx(matrix(c(datasetSheet, pattern), ncol=2), paste0("MissingPatterns/Results/", datasetName,".xlsx"))
  return(pattern)
}