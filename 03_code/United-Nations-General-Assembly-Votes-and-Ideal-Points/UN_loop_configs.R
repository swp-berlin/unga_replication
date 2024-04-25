
# loop over MCMCs we need -------------------------------------------------
getwd()

# for (issue in c("All","health","peace and security")){
#   if(issue == "All"){
#       Path			= here::here('03_code/United-Nations-General-Assembly-Votes-and-Ideal-Points')   ## This indicates a subfolder where the organizing .R code (UN_DataProcessing.R), Rcpp functions and data reside
#       DataCode		= issue
#       FileSuffix 		= paste(DataCode, "_replication_2024", sep = "")		## Include data and any subsetting information
#       source(file.path(Path, "UN_Ideal_Rcpp.R"))
#   } else {
#     for(percent in c(10,15)){
#       Path			= here::here('03_code/United-Nations-General-Assembly-Votes-and-Ideal-Points')   ## This indicates a subfolder where the organizing .R code (UN_DataProcessing.R), Rcpp functions and data reside
#       DataCode		= issue
#       FileSuffix 		= paste(DataCode, "_replication_2024_",percent, sep = "")		## Include data and any subsetting information
#       source(file.path(Path, "UN_Ideal_Rcpp.R"))
#     }
#   }
# }

pacman::p_load("future","furrr", "tidyverse")


combinations <- expand_grid(Path = here::here('03_code/United-Nations-General-Assembly-Votes-and-Ideal-Points'), 
                            DataCode =  c("All","health","peace and security"), 
                            percent = c("10","15","sd")) |> 
  mutate(FileSuffix = case_when(
    DataCode == "All"~paste(DataCode, "_replication_2024", sep = ""),
    T~ paste(DataCode, "_replication_2024_",percent, sep = "")	
  )) |> 
  mutate(percent = if_else(DataCode =="All", "10", percent)) |> 
  distinct(Path,DataCode, percent)  

### see https://cran.r-project.org/web/packages/future/vignettes/future-4-issues.html 
### for why all source() calls are specified with local = FALSE

estimate_model <- function(Path, DataCode, percent) {
  Path <- here::here('03_code/United-Nations-General-Assembly-Votes-and-Ideal-Points')
  
  if (DataCode == "All") {
    FileSuffix <-paste(DataCode, "_replication_2024", sep = "")
  } else {
    FileSuffix <- paste(DataCode, "_replication_2024_", percent, sep = "")
  }
    source(file.path(Path, "UN_Ideal_Rcpp.R"), local = TRUE)
}





## only new model

combinations <- combinations |> 
  filter(percent =="sd")


future::plan("multisession", workers = 5)

furrr::future_pmap(combinations, estimate_model)
