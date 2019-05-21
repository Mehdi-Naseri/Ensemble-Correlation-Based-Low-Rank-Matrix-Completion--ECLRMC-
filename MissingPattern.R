swipe <- function(dm, i, j){
  # swipe data
  tmp <- dm[,i]
  dm[,i] <- dm[,j]
  dm[,j] <- tmp
  # swipe col names
  names <- colnames(dm)
  tmp <- names[i]
  names[i] <- names[j]
  names[j] <- tmp
  colnames(dm) <- names
  return(dm)
}
mp <- function(){
  # Load data
  incomplete_path = paste0("Datasets/Incomplete+Datasets/Bupa/Comp20.xlsx")
  ds <- read_excel(incomplete_path, col_names=FALSE)
  dm_ <- as.matrix.data.frame(ds)
  View(dm_)
  
  # convert to zero and one
  dm_[!is.na(dm_)] <- as.integer(0)
  dm_[is.na(dm_)] <- as.integer(1)
  
  # remove duplicates
  #uniques <- unique(dm_)
  dm <- c()
  counts <- c()
  col <- ncol(dm_)
  while(nrow(dm_) > 0) {
    row <- dm_[1,]
    dm <- rbind(dm, row)
    dups <- c()
    for(i in 1:nrow(dm_)) {
      if(all.equal(row, dm_[i,]) == TRUE){
        dups <- c(dups, i)
      }
    }
    dm_ <- matrix(dm_[-dups,], ncol=col)
    counts <- c(counts, length(dups))
  }
  rownames(dm) <- counts
  # sort columns based on the sum so that the last column has more ones (missing data)
  for(i in 1:ncol(dm)) {
    for(j in i:ncol(dm)){
      if(i == j) next;
      if(sum(dm[,i] == 0) > sum(dm[,j] == 0)){
        dm <- swipe(dm, i, j)
      }
    }
  }
  # sort rows based on zeros so that last column is fully sorted
  for(i in 1:ncol(dm)) {
    dm <- dm[order(dm[,i]),]
  }
  # Show pattern
  for(i in nrow(dm):1) {
    for(j in ncol(dm):1) {
      if(dm[i,j] == 0) {
        break
      }
      dm[i,j] <- dm[i,j]+1
    }
  }
  result <- cbind(matrix(counts, ncol=1), dm)
  result <- rbind(c("Counts",colnames(dm)), result)
  colnames(result) <- c()
  rownames(result) <- c()
  write.xlsx(result, "pattern.xlsx", row.names = F, col.names = F)
  print(result)
  return(result)
}
mp()