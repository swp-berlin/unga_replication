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


# regional_codes ----------------------------------------------------------


regional_codes <- un_resolutions_data |>
  dplyr::select(
    unres,
    session,
    year,
    vote_date,
    title,
    yes,
    no,
    abstentions,
    non_voting,
    total_voting_membership,
    adopted_without_vote,
    main_category
  ) |> 
  filter(!adopted_without_vote ==1) |> 
  separate_rows(main_category) |> 
  filter(str_detect(main_category, "^2")) |> 
  mutate(value = 1) |> 
  pivot_wider(names_from = main_category, names_prefix = "main_category_",values_fill = 0) |> 
  dplyr::select(unres_swp = unres, contains("main_category"))



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
  replace_na(list(para = 0, amend = 0)) |> 
  filter(!para ==1) |> 
  filter(!amend ==1) |> 
  distinct(resid, unres, date, descr, amend, para) |> 
  mutate(unres = if_else(str_detect(unres, "A/RES/"),
                         map_chr(unres, parse_resolution_number_voeten),
                         unres))|>
  filter(date %within% interval(min(un_resolutions_data$vote_date), max(un_resolutions_data$vote_date)))


## remaining duplicates on unres are uncoded amendments ... 
test_unres <- voeten_clean |> 
  distinct(resid, unres, descr, amend, para) |> 
  group_by(unres) |> 
  mutate(n = n())

# 
# test_voeten_unres <- voeten |> 
#   distinct(resid, unres, date) |> 
#   group_by(resid) |> 
#   mutate(n = n())
# 
# voeten_unres_correct <- voeten |> 
#   group_by(resid, unres) |> 
#   mutate(n = n()) |>
#   group_by(resid) |> 
#   slice_max(order_by = n, n = 1) |> 
#   select(-ccode, -vote, -member, -Country, -Countryname,-n) |> 
#   distinct()
#   


## there is 12 resolutions which do not match with any resolution in our set
## mostly due to the way we count numbering, etc.
voeten_not_in_swp <- voeten_clean |> 
  filter(!unres %in% un_resolutions_data$unres)


## given that we parse some pdfs only erroneously and we exclude some
## such as budget resolutions, there is more missing
## total of 25
voeten_not_in_swp <- voeten_clean |> 
  filter(!unres %in% un_resolutions_topic$unres)

differences <- un_resolutions_data |> filter(unres %in% voeten_not_in_swp$unres)

voeten_id <- voeten_clean  |> 
  filter(unres %in% un_resolutions_data$unres) |> 
  dplyr::select(voeten_id = resid, 
         date, 
         unres)

test <- voeten_id |> 
  group_by(voeten_id) |>
  mutate(n = n()) |> 
  filter(n>1) |> ungroup()
# 
# 
# test_resid <- voeten |> filter(resid == "78081")
# test_unres <- voeten |> filter(unres == "A/RES/78/239")

voeten_topic <- voeten  |> 
  replace_na(list(para = 0, amend = 0)) |> 
  filter(!para ==1) |> 
  filter(!amend ==1) |>  
  left_join(voeten_id |> dplyr::select(resid = voeten_id, unres_swp = unres)) |> 
  filter(!is.na(unres_swp)) |> 
  left_join(un_resolutions_topic |> ungroup() |> dplyr::select(unres_swp = unres,
                                           contains("health"), 
                                           contains("int_sec"), 
                                           contains("paragraphs"))) |> 
  mutate(he = if_else(perc_res_health >=15, 1, 0))|> 
  mutate(pc = if_else(perc_res_int_sec >=15, 1, 0)) 


# test_unres <- voeten_topic |> filter(unres =="R/60/231")
# 
# count_health <- voeten_topic |> distinct(he,rcid,session) |> ungroup() |> count(he, session) |> arrange(session)
# 
# test <- voeten_topic |> filter(he ==1 & session == 77) |> distinct(unres, descr)
# 
# test2 <- un_resolutions_topic |> 
#   filter(unres %in% test$unres)

voeten_topic <- voeten_topic |>
  mutate(session_original = session,
         session = case_when(session >= 70 ~ 2,
                             session < 70 ~ 1))
# 
# test <- voeten_topic |> 
#   group_by(resid, descr) |> 
#   summarise(n = n())

write_rds(voeten_topic,"05_results/final_voeten_data_15.RData")

completeVotes <- voeten_topic

save(completeVotes,file = "03_code/United-Nations-General-Assembly-Votes-and-Ideal-Points/Data/UNVotes_new_15.RData")


voeten_topic <- voeten  |> 
  replace_na(list(para = 0, amend = 0)) |> 
  filter(!para ==1) |> 
  filter(!amend ==1) |>  
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
         session = case_when(session >= 70 ~ 2,
                             session < 70 ~ 1))

write_rds(voeten_topic,"05_results/final_voeten_data_10.RData")

completeVotes <- voeten_topic

save(completeVotes,file = "03_code/United-Nations-General-Assembly-Votes-and-Ideal-Points/Data/UNVotes_new_10.RData")





voeten_topic <- voeten  |>
  replace_na(list(para = 0, amend = 0)) |>
  filter(!para == 1) |>
  filter(!amend == 1) |>
  left_join(voeten_id |> dplyr::select(resid = voeten_id, unres_swp = unres)) |>
  filter(!is.na(unres_swp)) |>
  left_join(
    un_resolutions_topic |> ungroup() |> dplyr::select(
      unres_swp = unres,
      contains("health"),
      contains("int_sec"),
      contains("paragraphs")
    )
  ) |>
  mutate(he = if_else(
    perc_res_health >= mean_perc_session_res_health + sd_perc_session_res_health,
    1,
    0
  ))|>
  mutate(pc = if_else(
    perc_res_int_sec >= mean_perc_session_res_int_sec  + sd_perc_session_res_int_sec ,
    1,
    0
  )) 


voeten_topic <- voeten_topic |>
  mutate(session_original = session,
         session = case_when(session >= 70 ~ 2,
                             session < 70 ~ 1))

write_rds(voeten_topic,"05_results/final_voeten_data_sd.RData")

completeVotes <- voeten_topic

save(completeVotes,file = "03_code/United-Nations-General-Assembly-Votes-and-Ideal-Points/Data/UNVotes_new_sd.RData")







regional_codes$unres_swp[!regional_codes$unres_swp %in% voeten_topic$unres_swp]

test_regional <- voeten_topic |> 
  filter(pc==1) |> 
  dplyr::select(rcid, unres_swp, session, contains("main_category_")) |> 
  distinct() |> 
  pivot_longer(cols = -c(rcid,session, unres_swp),names_to = "main_category", values_to = "main_category_value" ) |> 
  group_by(unres_swp, rcid) |> 
  summarise(sum_cat = sum(main_category_value, na.rm = T)) |> 
  filter(sum_cat == 0)


test <- un_resolutions_data |> 
  filter(unres %in% test_regional$unres_swp) |> 
  distinct(unres, session, year, subjects_used, subjects_not_used, main_category, title)

writexl::write_xlsx(test,"04_clean_data/test_regional_codes.xlsx")


### problems with new voeten data

voeten <- load("01_raw_data/voeten/UNVotes_problematic.RData")
voeten <- get(voeten)
rm(completeVotes)

voeten_clean <- voeten |> 
  distinct(rcid, unres, resid, date) |> 
  group_by(rcid) |> 
  mutate(n = n()) |> 
  filter(n >1)


write_csv(voeten_clean, "05_results/problematic_rcids.csv")




