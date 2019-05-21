library(softImpute)

ECLRMC <- function(inputDataset, datasetSheet, datasetName, debug){
  #----------------------------------------------------------------
  #Import Input Data
  #----------------------------------------------------------------
  #inputDataset = data.frame(
  #  X1 = c(5.1, 4.9, NA, 4.6),
  #  X2 = c(3.5, 3.0, 3.2, 3.1),
  #  X3 = c(1.4, NA, 1.3, 1.5)
  #)
  InputMatrixRows = nrow(inputDataset)
  InputMatrixColumns = ncol(inputDataset)
  
  result = list()
  #----------------------------------------------------------------
  #Step1 LRMC
  #----------------------------------------------------------------
  print("-------------Start Step 1----------------")
  mySvd = softImpute(x = inputDataset , type = "svd")
  lrmcMatrix = complete(inputDataset, mySvd)
  result$LRMC = lrmcMatrix
  
  
  #----------------------------------------------------------------
  #Step2 Distance MAtrix
  #----------------------------------------------------------------
  print("-------------Start Step 2----------------")
  distanceMatrix = data.frame()
  
  #weightedMatrix
  weights = array(
    data = NA,
    dim = c(InputMatrixRows, InputMatrixRows, InputMatrixColumns)
  )
  for (i in  1:InputMatrixRows)
  {
    if(debug)
      print(paste0("Step 2-1: ", datasetName, "/", datasetSheet, " (", i, "/", InputMatrixRows, ")"))
    for (j in  1:InputMatrixRows)
    {
      for (n in  1:InputMatrixColumns)
      {
        if (is.na(inputDataset[i, n]) || is.na(inputDataset[j, n]))
          weights[i, j, n] = 0.1
        else
          weights[i, j, n] = 1
      }
    }
  }
  
  for (i in  1:InputMatrixRows)
  {
    if(debug)
      print(paste0("Step 2-2: ", datasetName, "/", datasetSheet, " (", i, "/", InputMatrixRows, ")"))
    for (j in  1:InputMatrixRows)
    {
      weightSum = sum(weights[i, j, ])
      weightNormalized = c()
      XiNormalized = c()
      XjNormalized = c()
      SumWeightedXi = 0
      SumWeightedXj = 0
      for (n in  1:InputMatrixColumns)
      {
        weightNormalized[n] = weights[i, j, n] / weightSum
        #print(paste("weightSum: %s", weightSum))
        #print(paste("weightNormalized: %s", weightNormalized[n]))
        SumWeightedXi = SumWeightedXi + (weightNormalized[n] * lrmcMatrix[i, n])
        SumWeightedXj = SumWeightedXj + (weightNormalized[n] * lrmcMatrix[j, n])
      }
      for (n in  1:InputMatrixColumns)
      {
        XiNormalized[n] = lrmcMatrix[i, n] - SumWeightedXi
        XjNormalized[n] = lrmcMatrix[j, n] - SumWeightedXj
      }
      
      v1 = sum(weightNormalized[] * XiNormalized[] * XjNormalized[])
      v2 = sum(weightNormalized[] * (XiNormalized[] ^ 2))
      v3 = sum(weightNormalized[] * (XjNormalized[] ^ 2))
      
      if (v1==0)
        cor = 0
      else
        cor = v1 / sqrt(v2 * v3)
      
      absValue = round(abs(cor), 8)
      distanceMatrix[i, j] = 1 - absValue
    }
  }
  
  #----------------------------------------------------------------
  #Step 3 - Adaptive KNN
  #----------------------------------------------------------------
  print("-------------Start Step 3----------------")
  #Input Parameter--------
  beta = 0.1
  #-----------------------
  
  orderedDistanceMatrix = t(apply(distanceMatrix, 1, sort))
  distanceRatioMatrix = data.frame(matrix(NA, InputMatrixRows, InputMatrixRows))
  distanceNeighborsNumber = c()
  distanceNeighborsValue = c()
  for (i in  1:InputMatrixRows)
  {
    if(debug)
      print(paste0("Step 3: ", datasetName, "/", datasetSheet, " (", i, "/", InputMatrixRows, ")"))
    #distanceNeighbors => Number of Nearest Neighbors
    distanceNeighborsNumber[i] = 0
    distanceNeighborsValue[i] = 0
    booldistanceNeighbors = FALSE
    
    v2 = sum(orderedDistanceMatrix[i, ])
    
    for (p in  1:InputMatrixRows)
    {
      v1 = sum(orderedDistanceMatrix[i, 1:p])
      distanceRatioMatrix[i, p] = v1 / v2
      
      #print(paste("DistanceRatio V1: %s",v1))
      #print(paste("DistanceRatio V2",v2))
      #print(paste("DistanceRatio Result",distanceRatioMatrix[i,p]))
      
      if (distanceRatioMatrix[i, p] >= beta &&
          booldistanceNeighbors == FALSE)
      {
        distanceNeighborsNumber[i] = p
        distanceNeighborsValue[i] = distanceRatioMatrix[i, p]
        booldistanceNeighbors = TRUE
      }
    }
  }
  
  #----------------------------------------------------------------
  #Step 4 - Correlation-Based LRMC
  #----------------------------------------------------------------
  print("-------------Start Step 4----------------")
  knnLRMC = list(matrix(NA, InputMatrixRows, InputMatrixColumns),
                 InputMatrixRows)
  # knnIndex
  knnIndex = data.frame(matrix(NA, nrow = InputMatrixRows + 1 , ncol = InputMatrixColumns))
  
  for (i in 1:InputMatrixRows) {
    if(debug)
      print(paste0("Step 4: ", datasetName, "/", datasetSheet, " (", i, "/", InputMatrixRows, ")"))
    kNN = data.frame(matrix(NA, nrow = distanceNeighborsNumber + 1 , ncol = InputMatrixColumns))
    
    for (j in 1:InputMatrixColumns) {
      kNN[1, j] = inputDataset[i, j]
      #    print(paste("kNN[1,j]: %s",kNN[1,j]))
    }
    
    distanceCoordinate <-
      c(apply(distanceMatrix[i, ], 1, function(x)
        order(x)[1:distanceNeighborsNumber[i]]))
    
    #print(paste("distanceNeighborsNumber[i]: %s",distanceNeighborsNumber[i]))
    
    for (k in 1:distanceNeighborsNumber[i]) {
      for (j in 1:InputMatrixColumns) {
        kNN[k, j] = inputDataset[distanceCoordinate[k], j]
        #print(paste("kNN[k+1,j]: %s",kNN[k,j]))
      }
      knnIndex[k, i] = distanceCoordinate[k]
    }
    
    mySvd2 = softImpute(x = kNN , type = "svd")
    knnLRMC[[i]] = complete(kNN, mySvd2)
  }
  
  clrmc = c()
  for (i in 1:InputMatrixRows)
  {
    clrmc = rbind(clrmc, knnLRMC[[i]][1,])
  }
  result$CLRMC = data.matrix(clrmc)

  #----------------------------------------------------------------
  #Step 5 - Ensemble Learning
  #----------------------------------------------------------------
  print("-------------Start Step 5----------------")
  
  #finalMatrix = data.frame(matrix(NA,InputMatrixRows+1,InputMatrixColumns))
  standardDeviation = c()
  
  
  finalMatrix = matrix(0, InputMatrixRows, InputMatrixColumns)
  
  duplicateRowsMatrix = matrix(0, InputMatrixRows, InputMatrixRows)
  duplicateRowsNumber = c()
  duplicateRowsDistanceMatrix = matrix(0, InputMatrixRows, InputMatrixRows)
  
  for (i in 1:InputMatrixRows) {
    if(debug)
      print(paste0("Step 5-1: ", datasetName, "/", datasetSheet, " (", i, "/", InputMatrixRows, ")"))
    counter = 1
    duplicateRowsDistances = c()
    for (j in 1:InputMatrixRows) {
      existRow = which(knnIndex[, j] == i)[1]
      #print(paste("which(knnIndex[,j]==i): ",which(knnIndex[,j]==i)))
      #print(paste("existRow: ",existRow))
      if (!is.na(existRow))
      {
        duplicateRowsMatrix[i, counter] = j
        #print(paste("distanceMatrix[i,j]: ",distanceMatrix[i,j]))
        
        #append(duplicateRowsDistances,distanceMatrix[i,j])
        duplicateRowsDistances = c(duplicateRowsDistances, distanceMatrix[i, j])
        counter = counter + 1
      }
      else{
        
      }
    }
    
    a = sd(duplicateRowsDistances)
    if(is.na(a)) {
      standardDeviation[i] = 0
    } else {
      standardDeviation[i] = sd(duplicateRowsDistances)
    }
    
    #standardDeviation[i]=sqrt(sum((a-mean(duplicateRowsDistances))^2/(length(duplicateRowsDistances)-1)))
    s = standardDeviation[i]
    #print(paste("[-i= %s,standard deviation: %s-]: ",i,s))
    duplicateRowsNumber[i] = counter - 1
  }
  
  #print("----------------Test-------------------------")
  rWeight = matrix(0, InputMatrixRows, InputMatrixRows)
  for (i in 1:InputMatrixRows) {
    if(debug)
      print(paste0("Step 5-2: ", datasetName, "/", datasetSheet, " (", i, "/", InputMatrixRows, ")"))
    #print("----------------Row-------------------------")
    #print(paste("standardDeviation: ", standardDeviation[i]))
    for (p in 1:duplicateRowsNumber[i])
    {
      distanceIP = distanceMatrix[i, duplicateRowsMatrix[i, p]]
      #print(paste("[distanceIP]: ", distanceIP))
      #print(paste("[rw]: ", (distanceIP ^ 2) / (standardDeviation[i]^2)))
      
      if (standardDeviation[i]==0)
      {
        rWeight[i,p] = 1
      }
      else
      {
        rWeight[i,p] = exp(-((distanceIP ^ 2) / (standardDeviation[i]^2)))
      }
      
      #print(paste("[i,p],rWeight: ", i,p, rWeight[i,p]))
    }
  }
  
  for (i in 1:InputMatrixRows) {
    if(debug)
      print(paste0("Step 5-3: ", datasetName, "/", datasetSheet, " (", i, "/", InputMatrixRows, ")"))
    for (j in 1:InputMatrixColumns)
    {
      s = standardDeviation[i]
      #print(paste("[i,standard deviation:]: ", i, s))
      for (p in 1:duplicateRowsNumber[i])
      {
        
        knnNumber = strtoi(duplicateRowsMatrix[i, p])
        KnnRow = strtoi(which(knnIndex[, knnNumber] == i))
        #print(paste("[knnNumber - KnnRow]: ", knnNumber , KnnRow))
        rWeightValue = rWeight[i,p] / sum(rWeight[i,])
        
        if (length(knnNumber) == 0){
          finalMatrix[i, j] = lrmcMatrix[i,j]
        } else if (knnNumber==0)
        {
          finalMatrix[i, j] = lrmcMatrix[i,j]
        }
        else
        {
         finalMatrix[i, j] = finalMatrix[i, j] + rWeightValue * knnLRMC[[knnNumber]][KnnRow, j]
        }
        #print(paste("[i,j,p]: ", i, j, p))
        #print(paste("finalMatrix[i,j]: ", finalMatrix[i, j]))
        #browser()
      }
    }
  }
  result$ECLRMC = finalMatrix
  return(result)
}
