#### PAUL BOCHTLER
#### 21.12.2022
#### Scrape resolutions and parse pdfs
#-----------------------------------------#

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd('../')
getwd()

## load packages and install missing packages
if (!require(pacman)) install.packages("pacman") else library(pacman)

## empty potential rests from other scripts 
rm(list = ls())
## define libraries to be loaded
p_load(
  char = c(
    'tidyverse',
    'readxl',
    "janitor",
    "countrycode","lubridate",
    "readr",
    "furrr","pdftools","jsonlite","irr","writexl","SWPcdR", "yardstick","ggtext","patchwork","fst")
)

source("03_code/00_functions.R")


# load unres data ---------------------------------------------------------

un_resolutions_topic_path <- list.files("05_results", full.names = T) %>%
  .[str_detect(., "res_level_info.rds")] %>%
  sort(., decreasing = T)

un_resolutions_topic <- read_rds(un_resolutions_topic_path[1])

un_resolutions_data_path <- list.files("01_raw_data", full.names = T) %>%
  .[str_detect(., "un_resolutions.fst")] %>%
  sort(., decreasing = T)

un_resolutions_data <- fst::read_fst(un_resolutions_data_path[1])

# download voeten data ----------------------------------------------------
if(F){
  key <-  Sys.getenv("DATAVERSE_KEY")
  
  file <- "01_raw_data/voeten.zip"
  
  datasets <-
    httr2::request("https://dataverse.harvard.edu/api/access/dataset/:persistentId/versions/:latest?persistentId=doi:10.7910/DVN/LEJUQZ") |>
    httr2::req_headers("X-Dataverse-key" = key) |>
    httr2::req_perform() |>
    httr2::resp_body_raw() |>
    base::writeBin(file)
  
  
  dir.create("01_raw_data/voeten", showWarnings = F)
  zip::unzip(file, exdir = "01_raw_data/voeten")
}


voeten <- load("01_raw_data/voeten/UNVotes.RData")
voeten <- get(voeten)
rm(completeVotes)

voeten_clean <- voeten |> 
  distinct(resid, unres, date) |> 
  mutate(unres = if_else(str_detect(unres, "A/RES/"), 
                         map_chr(unres, parse_resolution_number_voeten), 
                         unres))|> 
  filter(date %within% interval(min(un_resolutions_data$vote_date), max(un_resolutions_data$vote_date)))


## there is 26 resolutions which do not match with any resolution in our set
## mostly due to the way we count numbering, etc.
voeten_not_in_swp <- voeten_clean |> 
  filter(!unres %in% un_resolutions_data$unres)


## given that we parse some pdfs only erroneously and we exclude some
## such as budget resolutions, there is more missing
## total of 47
voeten_not_in_swp <- voeten_clean |> 
  filter(!unres %in% un_resolutions_topic$unres)

differences <- un_resolutions_data |> filter(unres %in% voeten_not_in_swp$unres)

voeten_id <- voeten_clean  |> 
  filter(unres %in% un_resolutions_data$unres) |> 
  dplyr::select(voeten_id = resid, 
         date, 
         unres)


voeten_topic <- voeten |> 
  left_join(voeten_id |> dplyr::select(resid = voeten_id, unres_swp = unres)) |> 
  filter(!is.na(unres_swp)) |> 
  left_join(un_resolutions_topic |> ungroup() |> dplyr::select(unres_swp = unres,
                                           contains("health"), 
                                           contains("int_sec"), 
                                           contains("paragraphs"))) |> 
  mutate(he = if_else(perc_res_health >=15, 1, 0))|> 
  mutate(pc = if_else(perc_res_int_sec >=15, 1, 0))
# 
# count_health <- voeten_topic |> distinct(he,rcid,session) |> ungroup() |> count(he, session) |> arrange(session)
# 
# test <- voeten_topic |> filter(he ==1 & session == 77) |> distinct(unres, descr)
# 
# test2 <- un_resolutions_topic |> 
#   filter(unres %in% test$unres)

voeten_topic <- voeten_topic |>
  mutate(session_original = session,
         session = case_when(session >= 70 ~ 1,
                             session < 70 ~ 2))

write_rds(voeten_topic,"05_results/final_voeten_data_15.RData")

completeVotes <- voeten_topic

save(completeVotes,file = "03_code/United-Nations-General-Assembly-Votes-and-Ideal-Points/Data/UNVotes_new_15.RData")


voeten_topic <- voeten |> 
  left_join(voeten_id |> dplyr::select(resid = voeten_id, unres_swp = unres)) |> 
  filter(!is.na(unres_swp)) |> 
  left_join(un_resolutions_topic |> ungroup() |> dplyr::select(unres_swp = unres,
                                                               contains("health"), 
                                                               contains("int_sec"), 
                                                               contains("paragraphs"))) |> 
  mutate(he = if_else(perc_res_health >=10, 1, 0))|> 
  mutate(pc = if_else(perc_res_int_sec >=10, 1, 0))

voeten_topic <- voeten_topic |>
  mutate(session_original = session,
         session = case_when(session >= 70 ~ 1,
                             session < 70 ~ 2))

write_rds(voeten_topic,"05_results/final_voeten_data_10.RData")

completeVotes <- voeten_topic

save(completeVotes,file = "03_code/United-Nations-General-Assembly-Votes-and-Ideal-Points/Data/UNVotes_new_10.RData")

