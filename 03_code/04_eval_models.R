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
  select(
    # -text,
    -x3) |> 
  mutate(detail_vote_number = as.character(detail_vote_number)) 

files <- list.files("04_clean_data/preds_int_sec_2024", full.names = T, pattern = "csv")

future::plan("multisession", workers = 32)

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
  # select(-text) |> 
  mutate(detail_vote_number = as.character(detail_vote_number)) 

prediction_data <- clean_data |>  
  mutate(res_no_original = str_remove_all(url,".*files/") |> 
           str_remove_all("-.*") |> 
           str_replace_all(pattern = "_",replacement = "/")) |> 
  left_join(predictions_int_sec |> rename(text_new = text), 
            by = c("detail_vote_number","group_id_alt"))|> 
  left_join(predictions_health |> select(-text), 
            by = c("detail_vote_number","group_id_alt")) |> 
  mutate(year = ymd(vote_date) |> year()) |> 
  left_join(un_sessions) |> 
  filter(!is.na(text))



#### validation data

validation_data <- list.files("04_clean_data/validation_data/",full.names = T) |> 
  map(read_csv) |> 
  map(~.x |> 
        select(label, label_text, id, text_old = text) |> 
        mutate(type = if_else(str_detect(label_text,"health"), "health","int_sec")) |> 
        separate(id, into = c("detail_vote_number","group_id_alt"), sep = "_") |> 
        mutate(group_id_alt = as.numeric(group_id_alt)) |> 
        left_join(bind_rows(
          predictions_health |> mutate(type = "health") |> rename(pred = label_health, prob = prob_health),
          predictions_int_sec|> mutate(type = "int_sec")|> rename(pred = label_int_sec, prob = prob_int_sec))) |> 
        select(-label) |> 
        mutate(truth = as.factor(label_text),
               estimate = as.factor(pred)) |> 
        mutate(type = if_else(type == "health", "Health", "International Peace and Security")) |> 
        filter(text == text_old)) 


metrics <- map_dfr(validation_data,
                   function(x) {
                     bind_rows(
                       yardstick::accuracy(x, truth = truth, estimate = estimate) |> mutate(type = unique(x$type)),
                       yardstick::precision(x, truth = truth, estimate = estimate,estimator = "binary") |> mutate(type = unique(x$type)),
                       yardstick::f_meas(x, truth = truth, estimate = estimate,estimator = "binary") |> mutate(type = unique(x$type)),
                       yardstick::bal_accuracy(x, truth = truth, estimate = estimate,estimator = "binary") |> mutate(type = unique(x$type))
                     )
                   })

confusion_matrix <- map(validation_data,
                        function(x) {
                          yardstick::conf_mat(x, truth = truth, estimate = estimate) |> autoplot(type = "heatmap") +
                            scale_fill_gradient(low = swp_cols("gruen6"), high = swp_cols("gruen"))+
                            labs(subtitle = str_wrap(glue::glue("{unique(x$type)}"),20), caption = glue::glue("n={nrow(x)}"))+
                            theme(text = element_text(size = 10), 
                                  axis.text = element_text(size = 10),
                                  title = element_text(size = 14))
                        })

reduce(confusion_matrix,`+`) + patchwork::plot_annotation(title = "Confusion Matrix") &
  theme_swp(plot.subtitle = element_text(vjust = 1))

ggsave(file.path(dir, "confusion_matrix.png"), width = 16, height = 9)
ggsave(file.path(dir, "confusion_matrix_doc.png"), width = 7, height = 5)

ggplot(metrics |> 
         mutate(.metric = case_when(
           .metric =="accuracy" ~"Accuracy",
           .metric =="bal_accuracy" ~"Accuracy (bal.)",
           .metric =="f_meas" ~"F1-Score",
           .metric =="precision" ~"Precision",
         ))) +
  geom_col(aes(x = .metric, y = .estimate, fill = .metric), show.legend = FALSE) +
  geom_label(aes(x = .metric, y = .estimate, label = round(.estimate,2), color = .metric), 
             position = position_dodge(width = 0.9), vjust = 1.5, fontface = "bold",
             show.legend = FALSE,
             label.r = unit(0.1,"lines"),
             label.size = NA) +
  scale_fill_manual(values = swp_cols("ocker2", "ocker3", "blau2", "ocker4")) +
  scale_color_manual(values = swp_cols("ocker2", "ocker3", "blau2", "ocker4")) +
  facet_wrap(. ~ type, strip.position = "bottom") +
  theme_swp(strip.background = element_blank(),
            strip.text = element_text(face = "bold"),
            strip.placement = "outside", 
            axis.text.x = element_text(angle = 45,vjust = 1, hjust = 1), 
            plot.caption = element_markdown()) +
  labs(
    x = "",
    y = "estimate",
    title = "Classification Metrics",
    caption = "Calculated based on test set of *n*=249 for Health and *n*=110 for Peace and International Security"
  )

ggsave(file.path(dir, "metrics.png"), width = 16, height = 9)
ggsave(file.path(dir, "metrics_doc.png"), width = 7, height = 5)

res_level_health <- prediction_data |> 
  group_by(unres, detail_vote_number, vote_date, title, session, years,res_no_original) |> 
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
         n_paragraphs_session = sum(n_paragraphs),
         n_session_int_sec = sum(n_int_sec),
         perc_paragraph_session_int_sec = n_session_int_sec/n_paragraphs_session*100) 

apriori <- read_xlsx("04_clean_data/apriori_health_related_res.xlsx") |> 
  clean_names() |> 
  left_join(res_level_health, by = c("resolution_no" = "res_no_original")) |> 
  mutate(diff_mean = perc_res_health - mean_perc_session_res_health,
         diff_sd = diff_mean/sd_perc_session_res_health) |> 
  filter(!is.na(perc_res_health))

p1 <- ggplot(apriori ,
             aes(y = perc_res_health, x = 1)) +
  geom_violin() +
  geom_hline(aes(yintercept = min(perc_res_health, na.rm = T)),
             color = swp_cols("rot3"),
             linewidth = 1.5) +
  geom_label(data = tibble(x = 1, y = min(apriori$perc_res_health, na.rm = T)),
             aes(
               x = x+1,
               y = y+10,
               label = glue::glue("Minimum percentage in qualitatively coded resolutions: {round(y,1)}%") |> str_wrap(45)
             ),
             fill = swp_cols("rot3"), show.legend = FALSE) +
  geom_jitter(height = 0, width = 0.05) +
  theme_swp(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank()
  ) +
  scale_y_continuous(limits = c(0, 100)) +
  coord_flip() +
  labs(x = str_wrap("Qualitatively coded resolutions",30), y = "Percentage of paragraphs that are health related") +
  expand_limits(x =3)

p2 <- ggplot(res_level_health |> distinct(mean_perc_session_res_health),
             aes(y = mean_perc_session_res_health, x = 1)) +
  geom_violin() +
  
  geom_jitter(height = 0, width = 0.05) +
  theme_swp(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank()
  ) +
  scale_y_continuous(limits = c(0, 100)) +
  coord_flip() +
  labs(x = "Session mean", y = "Mean percentage of health related paragraphs")

p1 / p2 &
  plot_annotation(title = "Resolution-level Cut-off value",
                  subtitle = "Comparison Session Health percentage vs. \nqualitatively selected health resolutions") & theme_swp(
                    axis.text.y = element_blank(),
                    axis.ticks.y = element_blank(),
                    axis.line.y = element_blank())


ggsave(file.path(dir, "cut-off.png"), width = 16, height = 9)
ggsave(file.path(dir, "cut-off_doc.png"), width = 7, height = 5)

