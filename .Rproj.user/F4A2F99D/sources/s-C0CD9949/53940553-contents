source("Functions/append.xlsx.R")
source("Functions/coder.R")
process <- function(datasetSheet){
#  datasetName = "Ionosphere"
  print(paste("Dataset: ", datasetName, ", Datasheet:", datasetSheet))
  # Path
  imputedDir_LRMC = paste0("ImputedFiles/LRMC/",datasetName)
  imputedDir_CLRMC = paste0("ImputedFiles/CLRMC/",datasetName)
  imputedDir_ECLRMC = paste0("ImputedFiles/ECLRMC/",datasetName)
  path_imputed_LRMC = paste0(imputedDir_LRMC,"/",datasetSheet)
  path_imputed_CLRMC = paste0(imputedDir_CLRMC,"/",datasetSheet)
  path_imputed_ECLRMC = paste0(imputedDir_ECLRMC,"/",datasetSheet)
  # NRMS XLSX Path
  path_ae_LRMC = paste0("AE/LRMC/", datasetName,"_", datasetSheet)
  path_ae_CLRMC = paste0("AE/CLRMC/", datasetName,"_", datasetSheet)
  path_ae_ECLRMC = paste0("AE/ECLRMC/", datasetName,"_", datasetSheet)
  path_nrms_LRMC = paste0("NRMS/LRMC/",datasetName,".xlsx")
  path_nrms_CLRMC = paste0("NRMS/CLRMC/",datasetName,".xlsx")
  path_nrms_ECLRMC = paste0("NRMS/ECLRMC/",datasetName,".xlsx")
  if(file.exists(path_imputed_LRMC) && file.exists(path_imputed_CLRMC) && file.exists(path_imputed_ECLRMC)) {
    print(paste0(path_imputed_LRMC, " is already processed!"))
    return(TRUE)
  }
  # Read incomplete dataset
  incomplete_path = paste0(incomplete_dir, "/", datasetSheet)
  incomplete_ds <- read_excel(incomplete_path, col_names=FALSE)
  # Categorical encoder
   incomplete_ds <- encoder(complete_ds, incomplete_ds)
  # Convert incomplete dataframe to matrix
  incomplete_dm <- data.matrix(incomplete_ds)
  # Impute missing data using ECLRMC
  result <- ECLRMC(incomplete_dm, datasetSheet, datasetName, FALSE)
  imputed_dm_LRMC <- result$LRMC
  imputed_dm_CLRMC <- result$CLRMC
  imputed_dm_ECLRMC <- result$ECLRMC
  
  imputed_dm_LRMC <- decoder(complete_ds, imputed_dm_LRMC)
  imputed_dm_CLRMC <- decoder(complete_ds, imputed_dm_CLRMC)
  imputed_dm_ECLRMC <- decoder(complete_ds, imputed_dm_ECLRMC)
  
  if(!dir.exists(imputedDir_LRMC))
    dir.create(imputedDir_LRMC)
  if(!dir.exists(imputedDir_CLRMC))
    dir.create(imputedDir_CLRMC)
  if(!dir.exists(imputedDir_ECLRMC))
    dir.create(imputedDir_ECLRMC)
  write.xlsx(imputed_dm_LRMC, path_imputed_LRMC, col.names=FALSE)
  write.xlsx(imputed_dm_CLRMC, path_imputed_CLRMC, col.names=FALSE)
  write.xlsx(imputed_dm_ECLRMC, path_imputed_ECLRMC, col.names=FALSE)
  nrms_LRMC <- NRMS(imputed_dm_LRMC, complete_ds)
  nrms_CLRMC <- NRMS(imputed_dm_CLRMC, complete_ds)
  nrms_ECLRMC <- NRMS(imputed_dm_ECLRMC, complete_ds)
  result_LRMC = matrix(c(datasetSheet, nrms_LRMC), ncol=2)
  result_CLRMC = matrix(c(datasetSheet, nrms_CLRMC), ncol=2)
  result_ECLRMC = matrix(c(datasetSheet, nrms_ECLRMC), ncol=2)
  append.xlsx(result_LRMC, path_nrms_LRMC)
  append.xlsx(result_CLRMC, path_nrms_CLRMC)
  append.xlsx(result_ECLRMC, path_nrms_ECLRMC)
  
  # Calculate AE
  AE_LRMC <- AE(imputed_dm_LRMC, complete_ds)
  AE_CLRMC <- AE(imputed_dm_CLRMC, complete_ds)
  AE_ECLRMC <- AE(imputed_dm_ECLRMC, complete_ds)
  if(AE_LRMC != FALSE) {
    write.xlsx(AE_LRMC, path_ae_LRMC)
  }
  if(AE_CLRMC != FALSE) {
    write.xlsx(AE_CLRMC, path_ae_CLRMC)
  }
  if(AE_ECLRMC != FALSE) {
    write.xlsx(AE_ECLRMC, path_ae_ECLRMC)
  }
}
