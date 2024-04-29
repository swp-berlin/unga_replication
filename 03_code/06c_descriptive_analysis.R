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
    "MCMCpack",
    "pdftools",
    "conflicted",
    "SWPcdR",
    "scales",
    "ggrepel",
    "countrycode",
    "tidytext","ggtext","changepoint"
  )
)
conflicted::conflict_prefer("select", "dplyr")
conflicts_prefer(dplyr::filter)
conflicts_prefer(tidyr::replace_na)

source("03_code/00_functions.R")


# Basic Descriptive Analysis of paragraphs


prediction_data <- list.files("05_results", full.names = T) %>%
  .[str_detect(., "para_level_info.rds")] %>%
  sort(., decreasing = T) |>
  nth(1) |>
  read_rds()

res_level_data <- list.files("05_results", full.names = T) %>%
  .[str_detect(., "res_level_info.rds")] %>%
  sort(., decreasing = T) |>
  nth(1) |>
  read_rds()

res_level_data_long <- list.files("05_results", full.names = T) %>%
  .[str_detect(., "res_level_info_long.rds")] %>%
  sort(., decreasing = T) |>
  nth(1) |>
  read_rds() |> 
  mutate(topic = if_else(topic =="health","Global Health", "International Peace and Security"))


un_resolutions_data_path <- list.files("01_raw_data", full.names = T) %>%
  .[str_detect(., "un_resolutions.fst")] %>%
  sort(., decreasing = T)

un_resolutions_data <- fst::read_fst(un_resolutions_data_path[1])


data <- read_rds("05_results/final_voeten_data_15.RData") |> 
  distinct(rcid, unres_swp, he, resid, session_original, session) 




dir <- file.path("05_results","descriptive_graphs")
dir.create(dir, showWarnings = FALSE)


# Data distribution in general
counts_resolutions <- res_level_data |> 
  ungroup( ) |> 
  distinct(unres, vote_date, session, years, title, perc_res_health, perc_res_int_sec) |> 
  mutate(he_10 = if_else(perc_res_health >=10, 1, 0))|> 
  mutate(pc_10 = if_else(perc_res_int_sec >=10, 1, 0)) |> 
  mutate(he_15 = if_else(perc_res_health >=15, 1, 0))|> 
  mutate(pc_15 = if_else(perc_res_int_sec >=15, 1, 0)) |> 
  left_join(un_resolutions_data |> distinct(unres, adopted_without_vote)) |> 
  pivot_longer(cols = c(contains("he_"),contains("pc_")), names_to = "topic", values_to = "topic_value") |> 
  filter(topic_value ==1) |> 
  mutate(adopted_without_vote = if_else(unres %in% data$unres_swp, 3, adopted_without_vote)) |> 
  count(adopted_without_vote, years, topic) |> 
  complete(adopted_without_vote, years, topic) |> 
  replace_na(list(n = 0)) |> 
  mutate(adopted_without_vote = case_when(
    adopted_without_vote ==0~"roll call vote, not present in data",
    adopted_without_vote ==3~ "roll call vote, present in data",
    T~"consensus vote, not useful for ideal-points"),
    topic = case_when(
      str_detect(topic, "^he_") ~ paste("Global Health: cut-off", str_extract(topic, "\\d+$"),"%"),
      str_detect(topic, "^pc_") ~ paste("International Peace and Security: cut-off", str_extract(topic, "\\d+$"),"%")
    )
  ) 

consensus_rate <- counts_resolutions |> 
  mutate(adopted_without_vote = if_else(adopted_without_vote=="consensus vote, not useful for ideal-points", "consensus", "vote")) |> 
  group_by(adopted_without_vote, topic) |>
  summarise(sum_n = sum(n)) |> 
  pivot_wider(names_from = "adopted_without_vote", values_from = "sum_n") |> 
  rowwise() |> 
  mutate(total = consensus+vote, 
         rate = round(consensus/total*100,1)) |> 
  ungroup()

years <- unique(counts_resolutions$years)[seq(1,30,2)]

ggplot(counts_resolutions)+
  geom_col(aes(x = years, 
               y = n, 
               group=adopted_without_vote, 
               fill = adopted_without_vote)) +
  geom_label(data = consensus_rate, aes(x = "2004/2005", y = c(100,100,160,160), label = str_c("Overall Consensus Rate: ",rate,"%"))) +
  facet_wrap(.~topic) +
  theme_swp(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
  scale_fill_manual(values = swp_cols("ocker3", "rot4", "blau2"), name = "Voting Type") +
  scale_x_discrete(breaks = years)+
  labs(title = "Data Availability and Consensus Rate",
       subtitle = "by year and topic")

ggsave(file.path(dir, "consensus.png"), width = 10, height = 7)
ggsave(file.path(dir, "consensus_pres.png"), width = 16, height = 9)

consensus_global_health_yearly <- res_level_data |> 
  ungroup( ) |> 
  distinct(unres, vote_date, session, years, title, perc_res_health, perc_res_int_sec) |> 
  mutate(he_10 = if_else(perc_res_health >=10, 1, 0))|> 
  mutate(pc_10 = if_else(perc_res_int_sec >=10, 1, 0)) |> 
  mutate(he_15 = if_else(perc_res_health >=15, 1, 0))|> 
  mutate(pc_15 = if_else(perc_res_int_sec >=15, 1, 0)) |> 
  left_join(un_resolutions_data |> distinct(unres, adopted_without_vote)) |> 
  pivot_longer(cols = c(contains("he_"),contains("pc_")), names_to = "topic", values_to = "topic_value") |> 
  filter(topic_value ==1) |> 
  mutate(topic = case_when(
    str_detect(topic, "^he_") ~ paste("Global Health: cut-off", str_extract(topic, "\\d+$"),"%"),
    str_detect(topic, "^pc_") ~ paste("International Peace and Security: cut-off", str_extract(topic, "\\d+$"),"%")
  )) |> 
  count(adopted_without_vote, years, topic)|> 
  complete(adopted_without_vote, years, topic) |> 
  replace_na(list(n = 0))  |> 
  filter(str_detect(topic, "Global Health: cut-off 15 %")) |> 
  group_by(years, topic) |> 
  summarise(consensus_share = n[adopted_without_vote==1]/sum(n)*100, total = sum(n)) |> 
  mutate(year = str_extract(years,".*/") |> str_remove("/") |> as.numeric()) |> 
  ungroup() |> 
  mutate(year_bin = split_var(year, n = 5)) |> 
  group_by(year_bin) |> 
  mutate(mean_consensus = mean(consensus_share), segment_start = min(years), segment_end = max(years))

ggplot(consensus_global_health_yearly)+
  geom_segment(aes(x = segment_start, xend = segment_end, y = mean_consensus), linewidth = 2, alpha = 0.8, color = swp_cols("weinrot4"))+
  geom_step(aes(x = years, y = consensus_share, group = 1), color = swp_cols("blau3"), linewidth = 2) +
  geom_point(aes(x = years, y = consensus_share, size = total),color = swp_cols("ocker3"))+
  scale_size(range = c(2, 8),name = "Total # of resolutions in session") +
  theme_swp(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100,20))+
  labs(title = "Share of consensual votes in the UNGA", 
       subtitle = glue::glue("in {unique(consensus_global_health_yearly$topic)}"), 
       caption = "in red the mean consensus share across 5 periods of time", x = "Sessions", y = "Consensus share in %")

ggsave(file.path(dir, "consensus_health.png"), width = 10, height = 7)
ggsave(file.path(dir, "consensus_health_pres.png"), width = 16, height = 9)



top_health <-  res_level_data |> 
  ungroup( ) |> 
  left_join(un_resolutions_data |> distinct(unres, adopted_without_vote))|> 
  distinct(unres, vote_date, session, years, title, perc_res_health, perc_res_int_sec,adopted_without_vote) |> 
  slice_max(order_by = perc_res_health, n = 50)

writexl::write_xlsx(top_health, file.path(dir,"top_50_health.xlsx"))

count(top_health, adopted_without_vote)

top_ipas <-  res_level_data |> 
  ungroup( ) |> 
  left_join(un_resolutions_data |> distinct(unres, adopted_without_vote))|> 
  distinct(unres, vote_date, session, years, title, perc_res_health, perc_res_int_sec,adopted_without_vote) |> 
  slice_max(order_by = perc_res_int_sec, n = 50)

writexl::write_xlsx(top_ipas, file.path(dir,"top_50_ipas.xlsx"))

## paragraph distribution 

year_data <- prediction_data |> group_by(years) |> 
  summarise(n_paragraphs = n(),
            n_res = length(unique(unres)))


ggplot(year_data |> filter(!str_detect(years,"2025")),aes(x = years, y = n_paragraphs))+
  geom_col(fill = swp_cols("blau3"))+
  geom_text(aes(label = str_c("Res: ",n_res)),
             color = "white", fontface = "bold", nudge_y = -2000)+
  labs(title = "Number of paragraphs", subtitle = "per session", x = "", y = "")+
  coord_flip()+
  theme_swp(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

ggsave(file.path(dir,"paragraphs_sessions_pres.png"),width = 7, height = 9)

# across single resolutions
ggplot(res_level_data_long |> filter(str_detect(type, "perc_res"))) +
  geom_violin(aes(y = value, x = 0), fill = swp_cols("blau4")) +
  geom_boxplot(aes(y = value)) +
  facet_wrap(.~topic, ncol = 1) +
  labs(title = "Distribution of % of issue related paragraphs",
       y = "% of issue related paragraphs in resolution", caption = glue::glue("Number of resolutions: {unique(res_level_data_long$unres) |> length()}"))+
  geom_label(data = res_level_data_long |> 
               filter(str_detect(type, "perc_res")) |>
               group_by(topic) |> 
               summarise(label = glue::glue("Mean: {mean(value) |> round(2)}\nSD: {sd(value) |> round(2)}")), 
             aes(y = 50, x = .25, label = label), fill = swp_cols("blau4"), color = "white",fontface="bold") +
  theme_swp() +
  coord_flip()

ggsave(file.path(dir, "distribution_res.png"), width = 10, height = 7)
ggsave(file.path(dir, "distribution_res_pres.png"), width = 16, height = 9)

count_perc_res <- res_level_data_long |> 
  filter(str_detect(type, "perc_res")) |>
  group_by(topic) |> 
  mutate(interval = cut_width(value, 10, boundary = 0)) |> 
  count(interval) 

ggplot(count_perc_res) +
  geom_col(aes(x = interval,
               y = n), fill = swp_cols("blau2")) +
  geom_label(
    aes(x = interval,
        y = n +
          100,
        label = n),
    fill = swp_cols("weinrot2"),
    color = "white"
  )+
  labs(title = "Number of resolutions",
       subtitle = " with determined % of issue related paragraphs",
       y = "# in interval", 
       x = "interval",
       caption = glue::glue("Number of resolutions: {unique(res_level_data_long$unres) |> length()}"))+
  facet_wrap(.~topic) +
  theme_swp(axis.text.x = element_text(
    angle = 45,
    hjust = 1,
    vjust = 1
  )) 
ggsave(file.path(dir, "number_res_interval.png"), width = 10, height = 7)

mean_perc_share_session <- res_level_data_long |> 
  filter(type == "mean_perc_session_res") |> 
  distinct(value, years, topic) |> 
  filter(!years =="2024/2025")

covid_in_health_paragraphs <- prediction_data |> 
  ungroup() |> 
  group_by(years) |> 
  mutate(n_session = n()) |> 
  filter(label_health == "health") |> 
  mutate(corona = if_else(str_detect(tolower(text), "covid|corona"), 1,0)) |> 
  group_by(years) |> 
  mutate(n_health = n()) |> 
  count(years,corona,n_session,n_health) |> 
  mutate(share_health_total = n/n_session*100 |> round(1)) |> 
  filter(corona ==0)

int_sec_paragraphs <- prediction_data |> 
  ungroup() |> 
  group_by(years) |> 
  mutate(n_session = n()) |> 
  filter(label_int_sec == "int_sec") |> 
  group_by(years) |> 
  mutate(n_int_sec = n()) |> 
  count(years,n_session,n_int_sec) |> 
  mutate(share_int_sec_total = n/n_session*100 |> round(1))


ggplot(mean_perc_share_session) +
  geom_line(
    data = covid_in_health_paragraphs,
    aes(x = years, y = share_health_total, group = 1),
    color = swp_cols("rot4"),
    linewidth = 2
  ) +
  geom_line(
    data = int_sec_paragraphs,
    aes(x = years, y = share_int_sec_total, group = 1),
    color = swp_cols("rot4"),
    linewidth = 2
  ) +
  geom_line(aes(
    x = years,
    y = value,
    group = topic,
    color = topic
  ),
  linewidth = 2) +
  geom_label_repel(
    data = mean_perc_share_session |>
      filter(years == "2023/2024"),
    aes(
      x = "2023/2024",
      y = value,
      group = topic,
      fill = topic,
      label = str_wrap("Mean share of paragraphs in resolutions \n(equal weight for each resolution)", 40)
    ),nudge_y = 5,fontface = "bold",
    color = "white", show.legend = F
  )+  
  geom_label_repel(
    data = tibble(years = "2004/2005", y = 10),
    aes(
      x = "2004/2005",
      y = y,
      label = str_wrap("Share of paragraphs in session \n(equal weight for each paragraph) \nfor health excluding Corona/Covid",40)
    ),fontface = "bold",
    color = "white", fill = swp_cols("rot4"),show.legend = F
  )+
scale_color_manual(values = swp_cols("blau2", "ocker3"), name = "Issue Area") +
scale_fill_manual(values = swp_cols("blau2", "ocker3"), name = "Issue Area") +
  labs(title = "Issue Share", subtitle = "by session and change point") +
  theme_swp(axis.text.x = element_text(
    angle = 45,
    hjust = 1,
    vjust = 1
  )) +
  theme(plot.caption = element_markdown())+
  ylab("in %") +
  xlab("")

ggsave(file.path(dir, "mean_perc_session.png"), width = 10, height = 7)
ggsave(file.path(dir, "mean_perc_session_pres.png"), width = 16, height = 9)

