MissingPatternMatrix <- function(incompleteData){
  # incompleteData = matrix(c(1,2,NA,4), ncol=2, byrow = TRUE)
  # Number of attributess
  cols = ncol(incompleteData)
  # Filter complete cases
  completeFilter = complete.cases(incompleteData)
  # Get incomplete cases
  incompleteCases <- incompleteData[!completeFilter,]
  # replace not null value by 1 
  incompleteCases[!is.na(incompleteCases)] <- as.integer(1)
  # replace null value by 0
  incompleteCases[is.na(incompleteCases)] <- as.integer(0)
  # Remove duplicate rows
  if(!is.null(nrow(incompleteCases)) && nrow(incompleteCases)){
    incompleteCases <- unique(incompleteCases)
  }
  # Convert to matrix
  dm <- data.matrix(incompleteCases)
  # Sort based on the last column
  if(ncol(dm)==1) {
    dm <- t(dm)
  }
  if(!is.null(ncol(dm)) && nrow(dm) != 1){
    for(i in cols:1){
      dm <- dm[order(dm[,i]),]
    }
  }
  # Sort based on sum of each row
  dm <- dm[order(rowSums(dm),decreasing=T),]
  # Add the 1 row for the complete data if there is any
  if(sum(completeFilter) > 0) {
    dm <- rbind(rep(1, cols), dm)
  }
  # Remove column names
  colnames(dm) <- NULL
  rownames(dm) <- NULL
  # Calculate number of columns with missing data
  rows = nrow(dm)
  nmis = 0
  for(i in 1:ncol(dm))
    if(sum(dm[,i]) < rows)
      nmis = nmis+1
  # Create the result
  result = list()
  result$r = dm
  result$nmis = nmis
  return(result)
}