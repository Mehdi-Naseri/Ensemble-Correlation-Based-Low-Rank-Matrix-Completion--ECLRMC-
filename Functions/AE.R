f <- function(x_hat, x) {
  if(x_hat == x) {
    return(1)
  } else {
    return(0)
  }
}
AE <- function(estimate, original){
  original <- as.matrix(original)
  rows = nrow(estimate)
  cols = c()
  sums = c()
  for(j in 1:ncol(estimate)) {
    if(class(estimate[,j]) == "character") {
      x = 0
      for(i in 1:rows) {
        x = x + f(estimate[i,j], original[i,j])
      }
      cols = cbind(cols, j)
      sums = cbind(sums, x)
    }
  }
  if(length(cols) == 0)
    return(FALSE)
  result <- matrix(c(cols, (sums/rows)), nrow=2, byrow = TRUE)
  return(result)
}