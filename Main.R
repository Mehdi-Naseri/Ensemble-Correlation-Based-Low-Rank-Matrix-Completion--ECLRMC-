# Clear variables
rm(list = ls())
library(readxl)
library(openxlsx)
library(parallel)
source("Functions/missingPattern.R")
source("Functions/NRMS.R")
source("Functions/AE.R")
source("ECLRMC.R")
source("process.R")

# List of datasets
#done = c("Iris")
#alldata= c("4-gauss","Abalone","BCW","Bupa","CNP","DERM","Difdoug","glass","Ionosphere","PID","sheart","sonar","wine","WS","zoo")
# HOV, TTTEG, Yeast, Chess, Adult
datasetName = "Abalone"

# Path to incomplete datasets
incomplete_dir = paste0("Datasets/Incomplete+Datasets/", datasetName)
# Read complete dataset
complete_path <- paste0("Datasets/Original+Datasets/",datasetName,".xlsx");
complete_ds <- read_excel(complete_path, col_names=FALSE)
files = list.files(path=incomplete_dir)
#files = c("Simple1.xlsx")
nrmsList = c();
i=0

print(mclapply(files, process, mc.cores=15))
for(file in files)
  process(file)
