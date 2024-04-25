#### PAUL BOCHTLER
#### 17.04.2024
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
    "MCMCpack","pdftools","conflicted","SWPcdR","scales","ggrepel","countrycode", "tidytext")
)
conflicted::conflict_prefer("select","dplyr")
conflicts_prefer(dplyr::filter)


source("03_code/00_functions.R")


# Basic Descriptive Text Analysis of paragraphs

prediction_data <- list.files("05_results", full.names = T) %>%
  .[str_detect(., "para_level_info.rds")] %>%
  sort(., decreasing = T) |> 
  nth(1) |> 
  read_rds() 

data <- read_rds("05_results/final_voeten_data_15.RData") |> 
  distinct(rcid, unres, he, resid, session_original, session) |> 
  filter(he ==1)

start_year <- 1946
current_year <- as.integer(format(Sys.Date(), "%Y"))

un_sessions <- tibble(
  session = 1:(current_year - start_year + 1),
  year = start_year:current_year,
  years = str_c(year,"/", year+1)
) |> filter(session >= min(data$session_original)) |> 
  mutate(session_collapse = case_when(session >= 70 ~ 1,
                                      session < 70 ~ 2)) |>
  group_by(session_collapse) |> 
  mutate(years = paste0(min(year),"-",max(year))) |> 
  distinct(session, years)



predictions_text_count <- prediction_data |>
  select(-years) |> 
  left_join(un_sessions, by = "session") |> 
  filter(unres %in% data$unres) |> 
  select(text, title, unres, years, label_health) |> 
  filter(label_health =="health") |> 
  unnest_tokens(output = "word", input = "text") |> 
  filter(!str_detect(word, "^[0-9.,]+$"))%>%
  anti_join(get_stopwords()) %>%
  count(years, word, sort = TRUE) %>% 
  group_by(years) %>% 
  mutate(rank = row_number(), 
         total = sum(n),
         tf = n/total) %>%
  ungroup()

excluded <- c("including","united","nations","states","health")

predictions_text_count %>%
  filter(!word %in% excluded) |>
  group_by(years) %>%
  slice_max(tf, n = 10) %>%
  ungroup() %>%
  ggplot(aes(tf, reorder_within(word, tf, years), fill = word)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~years, ncol = 2, scales = "free") +
  labs(x = "tf", y = NULL) +
  scale_y_reordered()  +
  theme_swp() +
  labs(title = "Most frequent words", subtitle = "by relative frequency", caption = str_c("Excluded words:", paste0(excluded, collapse = ", ")) |> str_wrap(40))



most_frequent_words <- predictions_text_count %>%
  filter(!word %in% excluded)  |>
  group_by(word) |> 
  summarise(n_word = sum(tf)) |> 
  slice_max(n_word, n = 11)

predictions_text_count |> 
  filter(word %in% most_frequent_words$word) |> 
  ggplot() +
  geom_line(aes(years, tf, group = word, color = word), linewidth = 2) +
  theme_swp(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  scale_color_brewer(type = "div")



## Keywords


predictions_text_tf_idf <-  prediction_data |>
  select(-years) |> 
  left_join(un_sessions, by = "session") |> 
  filter(unres %in% data$unres)  |> 
  select(text, title, unres, years, label_health) |> 
  filter(label_health =="health") |> 
  unnest_tokens(output = "word", input = "text") |> 
  filter(!str_detect(word, "^[0-9.,]+$"))%>%
  anti_join(get_stopwords()) |> 
  count(years, word) |> 
  bind_tf_idf(word, years, n)

predictions_text_tf_idf %>%
  group_by(years) %>%
  slice_max(tf_idf, n = 5) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, reorder_within(word, tf_idf, years), fill = word)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~years, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL) +
  scale_y_reordered()  +
  theme_swp()
