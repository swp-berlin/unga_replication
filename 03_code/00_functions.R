estimate_model <- function(input_matrix, mcmc, burnin, thin, data){
  result <- MCMCordfactanal(x = input_matrix,
                            factors = 1,
                            mcmc = mcmc, 
                            burnin = burnin, 
                            thin = thin, 
                            verbose = 1,
                            store.lambda=TRUE,
                            store.scores=TRUE)
  
  
  summary <-  summary(result)
  
  stats <-summary$statistics |> as_tibble() |> 
    mutate(var = attr(summary$statistics, "dimnames")[[1]]) |> 
    mutate(param_name = case_when(
      str_detect(var,"^Lambda.*.1$")~"l1",
      str_detect(var,"^Lambda.*.2$")~"l2",
      str_detect(var,"^Lambda.*.3$")~"l3",
      str_detect(var,"gamma2")~"g2",
      str_detect(var,"phi")~"phi"
    ))
  
  ideal_points <- stats |> filter(param_name =="phi") |> transmute(phi = Mean)
  
  discr_params <- stats |> filter(!param_name == "phi") |> pivot_longer(cols = c(1:4), 
                                                                        names_to = "param_type", 
                                                                        values_to = "param_value",
                                                                        names_transform = function(x){tolower(x) |> 
                                                                            str_replace_all(" |-","_")}) |> 
    mutate(param_name = str_c(param_name,"_",param_type),
           rcid = str_remove_all(var, "Lambda|gamma2|\\.1$|\\.2$|\\.")) |> 
    dplyr::select(param_name, param_value, rcid) |> 
    pivot_wider(id_cols = c(rcid), names_from ="param_name", values_from = "param_value")
  
  #### calculate predictions 
  
  session_ideal <- bind_cols(data, ideal_points) |>
    pivot_longer(
      cols = -c(phi, ccode, Country),
      names_to = "rcid",
      values_to = "vote"
    ) |>
    left_join(discr_params)  |>
    group_by(rcid) |>
    mutate(voting_types = sum(unique(vote), na.rm = T)) |>
    rowwise() |>
    mutate(
      z = l1_mean + l2_mean * phi,
      p_no = case_when(voting_types == 4 ~ z > 0,
                       voting_types == 3 ~ FALSE,
                       voting_types == 6 ~ z > g2_mean),
      p_abstain = case_when(voting_types == 4 ~ FALSE,
                            voting_types == 3 ~ z > 0,
                            voting_types == 6 ~ z <g2_mean & z > 0),
      p_yes = case_when(voting_types == 4 ~ z < 0,
                        voting_types == 3 ~ z < 0,
                        voting_types == 6 ~ z < 0)
    ) |>
    ungroup() 
  
  return(session_ideal)
}



parse_resolution_number_voeten <- function(list) {
  result <- NA
  if (!str_detect(list, "A/RES/")) {
    return("NA")
  } else {
    list <- strsplit(list, "/", fixed = T) %>% .[[1]]
    list <- list[3:length(list)]
    if (length(list) == 2) {
      result <- stringr::str_c("R/", list[1], "/", list[2]) %>% stringr::str_squish()
    }
    if (length(list) == 1) {
      if (str_detect(list, "ES|S")) {
        session <-
          stringr::str_extract_all(list, "\\([^()]+\\)") %>%
          unlist %>% stringr::str_replace_all(., "\\(|\\)", "") %>% stringr::str_remove_all(., ".*-") %>%  as.roman() %>% as.numeric()
        s_es <-
          stringr::str_extract_all(list, "\\([^()]+\\)") %>%
          unlist %>% stringr::str_replace_all(., "\\(|\\)", "") %>% stringr::str_remove_all(., "-.*")
        resolution <- extract_digit(list) %>% as.numeric()
        result <-
          stringr::str_c("R/", session, "/", resolution) %>% stringr::str_squish() %>% stringr::str_remove_all(., " ") %>% stringr::str_c(., "-", s_es)
      } else {
        session <-
          stringr::str_extract_all(list, "\\([^()]+\\)") %>%
          unlist %>% stringr::str_replace_all(., "\\(|\\)", "") %>% as.roman() %>% as.numeric()
        resolution <- extract_digit(list) %>% as.numeric()
        result <-
          stringr::str_c("R/", session, "/", resolution) %>% stringr::str_squish() %>% stringr::str_remove_all(., " ")
      }
      
      
    }
    result <-
      stringr::str_remove_all(result, "\\[|\\]") %>% stringr::str_replace(., "-", "/")
    return(result)
  }
}

as.num = function(x, na.strings = "NA") {
  stopifnot(is.character(x))
  na = x %in% na.strings
  x[na] = 0
  x = as.numeric(x)
  x[na] = NA_real_
  x
}

extract_digit <- function(x) {
  purrr::map_chr(x, function(x)
    stringr::str_extract_all(x, "\\d") %>% unlist() %>% paste(., collapse = "")) %>% as.num()
}

make_ideal_point_csv <- function(Path, FileSuffix){
  file <- paste(Path, "/Output/ThetaSummaryRW_",FileSuffix, ".txt", sep="")
  posterior <- paste(Path, "/Output/ThetaStoreRW_",FileSuffix, ".txt", sep="")
  if(file.exists(file)){
    df <- read.table(file = file)
    posterior <- readr::read_table(file = posterior,col_names = FALSE) 
    ci <- posterior |> 
      mutate(across(everything(),~bayestestR::ci(.x,ci = 0.9))) |> 
      unnest() |> 
      pivot_longer(cols = everything()) |> 
      mutate(id = str_extract(name, "\\d+") |> as.numeric()) |> 
      mutate(id = if_else(is.na(id),1,id+1),
             name = str_remove_all(name, "\\d+")) |> 
      distinct() |> 
      pivot_wider(id_cols = id, names_from = "name", values_from = "value") |> 
      dplyr::select(-id)

    #Note that we are going to work with the mean ideal point as the point estimate. There are also Quantile estimates (Q50 is the median of the posterior distribution)
    colnames(df)<- c("ccode", "session", 
                     paste("NVotes", sep=""), 
                     paste("IdealPoint", sep=""), 
                     paste("QO%" , sep=""),	
                     paste("Q5%" , sep=""), 
                     paste("Q10%" , sep=""), 
                     paste("Q50%" , sep=""),	
                     paste("Q90%" , sep=""), 
                     paste("Q95%" , sep=""), 
                     paste("Q100%", sep=""))
    
    
    df$iso3c  <- countrycode(df$ccode, "cown", "iso3c", warn = T)
    df$Countryname <- countrycode(df$ccode, "cown", "country.name", warn=T)
    #Fixing missing iso3c codes
    df <- df %>%  mutate(iso3c = if_else(ccode == 265, "DDR", iso3c),
                         iso3c = if_else(ccode == 260, "DEU", iso3c),
                         iso3c = if_else(ccode == 315, "CSK", iso3c),
                         iso3c = if_else(ccode == 345, "YUG", iso3c),
                         iso3c = if_else(ccode == 678, "YAR", iso3c),
                         iso3c = if_else(ccode == 680, "YAR", iso3c),
                         Countryname = if_else(ccode == 260, "German Federal Republic", Countryname),
    ) |> 
      bind_cols(ci)
    
    write.csv(df, file = paste(Path, "/Output/Idealpointestimates",FileSuffix, ".csv", sep="")) 
  }
}

read_ideal_point_csv <- function(Path, FileSuffix){
  file <- paste(Path, "/Output/Idealpointestimates",FileSuffix, ".csv", sep="")
  if(file.exists(file)){
    read_csv(file) |> 
      dplyr::distinct(ccode, phi = IdealPoint, session, CI, CI_low, CI_high) |> 
      mutate(suffix = FileSuffix, 
             datacode = str_remove_all(FileSuffix,"_\\d+.*") |> str_remove_all("_replication"),
             percent = str_extract(FileSuffix,"_\\d{2}$|_sd$") |> str_remove_all("_"),
             issue = case_when(
               datacode == "All"~paste0(datacode),
                T~paste0(datacode, "\ncut-off: ", percent, "%"))) 
  }
}

read_ideal_point_csv_barebones <- function(Path, FileSuffix){
  file <- paste(Path, "/Output/Idealpointestimates",FileSuffix, ".csv", sep="")
  if(file.exists(file)){
    read_csv(file) |> 
      mutate(suffix = FileSuffix, 
             datacode = str_remove_all(FileSuffix,"_\\d+.*") |> str_remove_all("_replication"),
             percent = str_extract(FileSuffix,"_\\d{2}$|_sd$") |> str_remove_all("_"),
             issue = case_when(
               datacode == "All"~paste0(datacode),
                T~paste0(datacode, "\ncut-off: ", percent, "%"))) 
  }
}



