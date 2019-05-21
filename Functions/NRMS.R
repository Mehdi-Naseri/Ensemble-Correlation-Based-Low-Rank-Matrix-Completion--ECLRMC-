NRMS <- function(estimate, original){
  for(j in 1:ncol(estimate)){
    if(class(estimate[,j]) == "character") {
      estimate[,j] <- 0
      original[,j] <- 0
    }
  }
  class(estimate) <- 'numeric'
  original = data.matrix(original)
  
  numerator = sqrt(sum((estimate-original)^2))
  if(numerator == 0) {
    return(0)
  }
  result = numerator/sqrt(sum(original^2))
  print(result)
  return(result)
}