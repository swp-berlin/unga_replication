
# loop over MCMCs we need -------------------------------------------------
getwd()

for (issue in c("All","health","peace and security")){
  if(issue == "All"){
      Path			= here::here('03_code/United-Nations-General-Assembly-Votes-and-Ideal-Points')   ## This indicates a subfolder where the organizing .R code (UN_DataProcessing.R), Rcpp functions and data reside
      DataCode		= issue
      FileSuffix 		= paste(DataCode, "_replication_2024", sep = "")		## Include data and any subsetting information
      source(file.path(Path, "UN_Ideal_Rcpp.R"))
  } else {
    for(percent in c(10,15)){
      Path			= here::here('03_code/United-Nations-General-Assembly-Votes-and-Ideal-Points')   ## This indicates a subfolder where the organizing .R code (UN_DataProcessing.R), Rcpp functions and data reside
      DataCode		= issue
      FileSuffix 		= paste(DataCode, "_replication_2024_",percent, sep = "")		## Include data and any subsetting information
      source(file.path(Path, "UN_Ideal_Rcpp.R"))
    }
  }
}
