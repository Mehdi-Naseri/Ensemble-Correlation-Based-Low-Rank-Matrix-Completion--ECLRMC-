library(readxl)
source("Functions/Coder.R")

datasetName = "TTTTEG"
datasetSheet = "planned 1.xlsx"
# Read Complete Excel File
completeFile <- paste0("Datasets/Original+Datasets/",datasetName,".xlsx");
completeData <- read_excel(completeFile, col_names=FALSE)
incompleteFile <- paste("Datasets/Incomplete+Datasets/",datasetName, "/", datasetSheet, sep="");
incompleteData <- read_excel(incompleteFile, col_names=FALSE)

encoded = encoder(completeData, incompleteData)
decoded = decoder(completeData, incompleteData)