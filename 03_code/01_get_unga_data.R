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
    "furrr","pdftools","jsonlite","irr","writexl","SWPcdR", "yardstick","ggtext","patchwork")
)

dir <- file.path("05_results","validation_graphs")

dir.create(dir, showWarnings = F)

source("03_code/00_functions.R")

metadata_path <- list.files("04_clean_data", full.names = T) %>%
  .[str_detect(., "metadata.rds")] %>%
  sort(., decreasing = T)

metadata <- read_rds(metadata_path[1]) 

min_max_session <- metadata %>% 
  select(vote_date,session) %>% 
  group_by(session)%>%
  summarise(min_session = min(vote_date),
            max_session = max(vote_date)) %>% 
  full_join(metadata %>% filter(is.na(session)) %>% select(vote_date),by = character()) %>% 
  filter(vote_date >= min_session & vote_date <=max_session) %>% 
  filter(!is.na(session)) %>% 
  distinct() %>% 
  select(session, vote_date)

metadata <- metadata %>% 
  rows_patch(min_max_session, by = "vote_date") %>% 
  mutate(session = if_else(unres =="R/ES/10/12",58,session))

clean_data_path <- list.files("04_clean_data", full.names = T) %>%  
  .[str_detect(., "_clean_full_data.rds")] |>   
  sort(decreasing = T)

clean_data <- read_rds(clean_data_path[1]) |> 
  filter(!is.na(source_doc)) |> 
  select(-url) |> 
  left_join(metadata |> select(url, detail_vote_number), 
            by = "detail_vote_number")

start_year <- 1946
current_year <- as.integer(format(Sys.Date(), "%Y"))

# Create the dataframe using tidy logic
un_sessions <- tibble(
  session = 1:(current_year - start_year + 1),
  year = start_year:current_year,
  years = str_c(year,"/", year+1)
)


#### read predictions 

files <- list.files("04_clean_data/preds_health_2024", full.names = T, pattern = "csv")

future::plan("multisession", workers = 32)

predictions_health <- furrr::future_map_dfr(files, 
                                            read_csv, 
                                            show_col_types = FALSE, 
                                            .progress = TRUE) |>
  clean_names() |>
  distinct() |> 
  rename(
    text = x0,
    detail_vote_number = x1,
    group_id_alt = x2,
    label_health = x4,
    prob_health = x5
  ) |> 
  select(-text,-x3) |> 
  mutate(detail_vote_number = as.character(detail_vote_number)) 

files <- list.files("04_clean_data/preds_int_sec_2024", full.names = T, pattern = "csv")

predictions_int_sec <- furrr::future_map_dfr(files, read_csv, show_col_types = FALSE) |>
  clean_names() |>
  distinct() |> 
  rename(
    text = x0,
    detail_vote_number = x1,
    group_id_alt = x2,
    label_int_sec = x3,
    prob_int_sec = x4
  ) |> 
  select(-text) |> 
  mutate(detail_vote_number = as.character(detail_vote_number)) 

prediction_data <- clean_data |>  
  mutate(res_no_original = str_remove_all(url,".*files/") |> 
           str_remove_all("-.*") |> 
           str_replace_all(pattern = "_",replacement = "/")) |> 
  left_join(predictions_int_sec, 
            by = c("detail_vote_number","group_id_alt"))|> 
  left_join(predictions_health, 
            by = c("detail_vote_number","group_id_alt")) |> 
  mutate(year = ymd(vote_date) |> year()) |> 
  left_join(un_sessions) |> 
  filter(!is.na(text))

write_rds(prediction_data, file.path("05_results",str_c(Sys.Date(),"para_level_info.rds")))

prediction_data <- read_rds( file.path("05_results",str_c(Sys.Date(),"para_level_info.rds"))) 

# test <- prediction_data |> 
#   filter(detail_resolution_number ==249070)
# 
# test <- prediction_data |> group_by(unres, title, vote_date, detail_vote_number, source_doc, session) |> 
#   summarise(text = paste0(text, collapse = "; ")) |> 
#  group_by(text, session) |> 
#   mutate(duplicates = n()) |> 
#   arrange(desc(duplicates))

res_level <- prediction_data |> 
  group_by(unres,
           detail_vote_number,
           vote_date,
           title,
           session,
           years,
           res_no_original) |>
  summarise( 
    n_paragraphs = max(group_id_alt),
    n_health = sum(label_health == "health"),
    n_int_sec = sum(label_int_sec == "int_sec"),
    perc_res_health = n_health/n_paragraphs*100,
    perc_res_int_sec = n_int_sec/n_paragraphs*100,
    .groups = "drop") |> 
  mutate(vote_date = ymd(vote_date),
         month = floor_date(vote_date, unit = "month")) |> 
  group_by(session, years) |> 
  mutate(mean_perc_session_res_health = mean(perc_res_health),
         sd_perc_session_res_health = sd(perc_res_health),
         n_paragraphs_session = sum(n_paragraphs),
         n_session_health = sum(n_health),
         perc_paragraph_session_health = n_session_health/n_paragraphs_session*100) |> 
  mutate(mean_perc_session_res_int_sec = mean(perc_res_int_sec),
         sd_perc_session_res_int_sec = sd(perc_res_int_sec),
         n_session_int_sec = sum(n_int_sec),
         perc_paragraph_session_int_sec = n_session_int_sec/n_paragraphs_session*100) |> 
  select(unres, detail_vote_number, vote_date, session, years, title, res_no_original,month, sort(tidyselect::peek_vars()))

write_rds(res_level, file.path("05_results",str_c(Sys.Date(),"res_level_info.rds")))


res_level_longer <- res_level |> 
  pivot_longer(cols = c(contains("health"),contains("int_sec")),names_to = "type") |> 
  mutate(topic = str_extract(type, "health|int_sec"),
         type = str_remove_all(type,"_health|_int_sec"))

write_rds(res_level_longer, file.path("05_results",str_c(Sys.Date(),"res_level_info_long.rds")))

