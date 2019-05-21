codes <- function(ds){
  result = list()
  counter = 0
  for(attr in attributes(ds)$names) {
    counter = counter + 1
    classType = (sapply(ds[attr], class))
    # Filter character attributes 
    if(classType != "character") next
    encodes = unique(ds[attr])
    result[attr] = encodes
  }
  print("** Categorical columns:")
  print(length(result))
  return(result)
}
encoder <- function(completeData, incompleteData){
  # Get unique values in each attribute
  codes = codes(completeData)
  i = 0
  for(code in codes) {
    i = i + 1
    attr = names(codes)[i]
    j = 1;
    for(char in code) {
      filter <- incompleteData[attr] == char
      filter[is.na(filter)] = FALSE
      incompleteData[filter,attr] = j
      j = j+1
    }
  }
  return(incompleteData)
}
decoder <- function(completeData, imputedData){
  # Get unique values in each attribute
  codes = codes(completeData)
  i = 0
  for(code in codes) {
    i = i + 1
    imputedData[,i] <- round(imputedData[,i])
  }
  i = 0
  for(code in codes) {
    i = i + 1
    # Round the column data
    #attr = names(codes)[i]
    j = 1;
    for(char in code) {
      filter <- imputedData[,i] == j
      filter[is.na(filter)] <- FALSE
      imputedData[filter,i] = char
      j = j+1
    }
  }
  return(imputedData)
}