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
    "MCMCpack","pdftools","conflicted","SWPcdR","scales","ggrepel","countrycode","ggnewscale")
)
conflicted::conflict_prefer("select","dplyr")
conflicts_prefer(dplyr::filter)


dir <- file.path("05_results","results_graphs")
dir.create(dir, showWarnings = FALSE)


source("03_code/00_functions.R")


data <- read_rds("05_results/final_voeten_data_15.RData")
prediction_data <- list.files("05_results", full.names = T) %>%
  .[str_detect(., "para_level_info.rds")] %>%
  sort(., decreasing = T) |>
  nth(1) |>
  read_rds()


start_year <- 1946
current_year <- as.integer(format(Sys.Date(), "%Y"))

un_sessions <- tibble(
  session = 1:(current_year - start_year + 1),
  year = start_year:current_year,
  years = str_c(year,"/", year+1)
) |> filter(session >= min(data$session_original)) |> 
  mutate(session_collapse = case_when(session >= 70 ~ 2,
                    session < 70 ~ 1)) |>
  group_by(session_collapse) |> 
  summarise(session = paste0(min(year),"-",max(year))) |> 
  dplyr::select(session = session_collapse , year = session )



combinations <- expand_grid(Path = here::here('03_code/United-Nations-General-Assembly-Votes-and-Ideal-Points'), 
                            DataCode =  c("All","health","peace and security"), 
                            percent = c("10","15","sd")) |> 
  mutate(FileSuffix = case_when(
    DataCode == "All"~paste(DataCode, "_replication_2024", sep = ""),
    T~ paste(DataCode, "_replication_2024_",percent, sep = "")	
  )) |> 
  mutate(percent = if_else(DataCode =="All", "10", percent)) |> 
  distinct(Path,FileSuffix)  

pmap(combinations, make_ideal_point_csv)

country_estimates_voeten <- pmap_dfr(combinations,read_ideal_point_csv) |> 
  group_by(ccode) |> 
  left_join(data |> distinct(ccode, Country) |> filter(str_length(Country)==3)) |>
  left_join(un_sessions) |> 
  group_by(issue) |> 
  mutate(phi = rescale(phi, to = c(-1,1)))

countries <- country_estimates_voeten |> 
  group_by(issue) |> 
  filter(phi == max(phi)|phi==min(phi)) |> 
  pull(Country) |> 
  c(x = _,
    c("USA",  "CHN", "RUS", "DEU",  "ZAF",  "MEX")) |>
  unique()

country_estimates_voeten <- country_estimates_voeten |> 
  filter(Country %in% countries)

colors <-
  swp_cols(
    "rot2",
    "blau4",
    "gruen4",
    "gruen2",
    "rot4",
    "rot",
    "ocker4",
    "blau3",
    "ocker2",
    "grey80"
  )

## health vs all

ggplot(country_estimates_voeten |> 
         filter(str_detect(issue,"All|health")) |> 
         filter(is.na(percent)|percent ==10),
       aes(x = year, y = phi, group = Country, color = Country)) +
  geom_line(aes(color =  Country), linewidth = 1.5) +
  geom_label_repel(aes(fill = Country, label = str_c(Country,":\n",round(phi,2))),
                   fontface = "bold",family = "Cambria", 
                   show.legend = F, color = "white",
                   label.size = NA) +
  facet_wrap(.~issue, scales = "free_x", strip.position = "bottom",ncol = 2) +
  theme_swp(
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    panel.grid.major.x = element_blank()) +
  labs(x ="", y = "Ideal Point Estimate", 
       title = "Ideal Point Estimates",
       subtitle = "for sub-issue 'Global Health' compared to all votes", 
       caption = "Ideal Points are rescaled to values between -1 and +1, different cut-offs for the amount of paragraphs in a resolution had to be issue related are chosen." |> str_wrap(70)) +
  theme(
    panel.spacing = unit(1.5, "lines")
  )+
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors)

ggsave(file.path(dir, "ideal_points_health.png"), width = 10, height = 7)
ggsave(file.path(dir, "ideal_points_health_pres.png"), width = 16, height = 9)

ggplot(country_estimates_voeten |> 
         filter(str_detect(issue,"All|health")) |> 
         filter(is.na(percent)|percent ==15|percent=="sd"),
       aes(x = year, y = phi, group = Country, color = Country)) +
  geom_line(aes(color =  Country), linewidth = 1.5) +
  geom_label_repel(aes(fill = Country, label = str_c(Country,":\n",round(phi,2))),
                   fontface = "bold",family = "Cambria", 
                   show.legend = F, color = "white",
                   label.size = NA) +
  facet_wrap(.~issue, scales = "free_x", strip.position = "bottom",ncol = 2) +
  theme_swp(
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    panel.grid.major.x = element_blank()) +
  labs(x ="", y = "Ideal Point Estimate", 
       title = "Ideal Point Estimates",
       subtitle = "for sub-issue 'Global Health' compared to all votes", 
       caption = "Ideal Points are rescaled to values between -1 and +1, different cut-offs for the amount of paragraphs in a resolution had to be issue related are chosen." |> str_wrap(70)) +
  theme(
    panel.spacing = unit(1.5, "lines")
  )+
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors)

ggsave(file.path(dir, "ideal_points_health_robust_15.png"), width = 10, height = 7)


## peace and security vs all
ggplot(country_estimates_voeten |> 
         filter(str_detect(issue,"All|peace")) |> 
         filter(is.na(percent)|percent ==10),
       aes(x = year, y = phi, group = Country, color = Country)) +
  geom_line(aes(color =  Country), linewidth = 1.5) +
  geom_label_repel(aes(fill = Country, label = str_c(Country,":\n",round(phi,2))),
                   fontface = "bold",family = "Cambria", 
                   show.legend = F, color = "white",
                   label.size = NA) +
  facet_wrap(.~issue, scales = "free_x", strip.position = "bottom",ncol = 2) +
  theme_swp(
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    panel.grid.major.x = element_blank()) +
  labs(x ="", y = "Ideal Point Estimate", 
       title = "Ideal Point Estimates",
       subtitle = "for sub-issue 'International Peace and Security' compared to all votes", 
       caption = "Ideal Points are rescaled to values between -1 and +1, different cut-offs for the amount of paragraphs in a resolution had to be issue related are chosen." |> str_wrap(70)) +
  theme(
    panel.spacing = unit(1.5, "lines")
  )+
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors)

ggsave(file.path(dir, "ideal_points_ipas.png"), width = 10, height = 7)
ggsave(file.path(dir, "ideal_points_ipas_pres.png"), width = 16, height = 9)

ggplot(country_estimates_voeten |> 
         filter(str_detect(issue,"All|peace")) |> 
         filter(is.na(percent)|percent ==15|percent =="sd"),
       aes(x = year, y = phi, group = Country, color = Country)) +
  geom_line(aes(color =  Country), linewidth = 1.5) +
  geom_label_repel(aes(fill = Country, label = str_c(Country,":\n",round(phi,2))),
                   fontface = "bold",family = "Cambria", 
                   show.legend = F, color = "white",
                   label.size = NA) +
  facet_wrap(.~issue, scales = "free_x", strip.position = "bottom",ncol = 2) +
  theme_swp(
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    panel.grid.major.x = element_blank()) +
  labs(x ="", y = "Ideal Point Estimate", 
       title = "Ideal Point Estimates",
       subtitle = "for sub-issue 'International Peace and Security' compared to all votes", 
       caption = "Ideal Points are rescaled to values between -1 and +1, different cut-offs for the amount of paragraphs in a resolution had to be issue related are chosen." |> str_wrap(70)) +
  theme(
    panel.spacing = unit(1.5, "lines")
  )+
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors)

ggsave(file.path(dir, "ideal_points_ipas_robust_15.png"), width = 10, height = 7)





#### voting blocs in a more general point cloud health vs all

country_estimates_voeten <- pmap_dfr(combinations,read_ideal_point_csv) |> 
  group_by(ccode) |> 
  left_join(data |> distinct(ccode, Country) |> filter(str_length(Country)==3)) |>
  left_join(un_sessions) |> 
  group_by(issue) |> 
  mutate(phi = rescale(phi, to = c(-1,1))) |>
  mutate(percent = if_else(is.na(percent),"all", percent)) |> 
  filter(str_detect(suffix,"health|All") ) |> 
  filter(percent %in% c(10, "all"))

regional_groups <- readxl::read_xlsx("04_clean_data/2024_regional_groups.xlsx") |> 
  pivot_longer(cols = -iso_alpha3_code, names_to = "rg", values_to = "value") |> 
  filter(value ==1) |> 
  filter(!(iso_alpha3_code == "TUR" & rg =="rg_western_european_and_other_states"))
  
  
two_dimensionsal_matrix <- country_estimates_voeten |> 
  ungroup() |> 
  dplyr::select(Country, session, phi, year, suffix) |> 
  pivot_wider(names_from = suffix, values_from = phi) |> 
  left_join(regional_groups, by = c("Country" = "iso_alpha3_code")) |> 
  mutate(rg = case_when(
    rg == "rg_african_states" ~ "African States",
    rg == "rg_asia_pacific_states" ~ "Asia Pacific States",
    rg == "rg_eastern_european_states" ~ "Eastern European States",
    rg == "rg_latin_american_and_caribbean_states" ~ "Latin American and Caribbean States",
    rg == "rg_western_european_and_other_states" ~ "Western European and Other States"
  ))

colors <-
  swp_cols(
    "rot2",
    "ocker3",
    "gruen2",
    "weinrot2",
    "blau2"
  )

countries <- country_estimates_voeten |> 
  group_by(issue) |> 
  filter(phi == max(phi)|phi==min(phi)) |> 
  pull(Country) |> 
  c(x = _,
    c("USA",  "CHN", "RUS", "DEU",  "ZAF",  "MEX", "ISR")) |>
  unique()

eu_countries <- giscoR::gisco_countrycode |> filter(eu) |> pull(ISO3_CODE)


ggplot(two_dimensionsal_matrix) +
  geom_abline(aes(slope = 1, intercept = 0), color = "black", linewidth = 2, alpha = .5) +
  geom_smooth(
    data = two_dimensionsal_matrix |>
      dplyr::filter(Country %in% eu_countries),
    aes(x = All_replication_2024, y = health_replication_2024_10), method = "lm", color = "black", linewidth = 2, alpha = .5,se = FALSE
  ) +
  annotate(geom = "label",x = 0.8, y = 0, label = "Regression line \nfor EU countries", fill = "black", color = "white", fontface = "bold", label.size = NA)+
  geom_point(
    data = two_dimensionsal_matrix ,
    aes(
      x = All_replication_2024,
      y = health_replication_2024_10,
      group = Country,
      color = rg
    ),
    key_glyph = "rect"
  ) +
  scale_color_manual(values = colors, name = "Regional Groups: ") +
  geom_label_repel(
    data = two_dimensionsal_matrix |>
      filter(Country %in% countries),
    aes(
      x = All_replication_2024,
      y = health_replication_2024_10,
      group = Country,
      label = Country
    ),
    fill = swp_cols("ocker4"),
    label.size = NA,
    min.segment.length = 0.0,
    fontface = "bold",
    color = "white",
    max.time = 3,
    force = 100,
    max.overlaps = 6,
    show.legend = FALSE
  ) +
  facet_wrap(. ~ year) +
  theme_swp(aspect.ratio = 1) +
  labs(
    x = "Ideal Points across all issues",
    y = "Ideal Points across Global Health",
    title = "Ideal Points in two dimensions",
    subtitle = "for Global Health compared to overall ideal points"
  ) +
  guides(color = guide_legend(override.aes = list(fill = colors[1:5]), nrow = 2))


ggsave(file.path(dir, "ideal_points_scatter_trend_health_all.png"), width = 10, height = 7)


# #### voting blocs in a more general point cloud health vs peace and security
# 
# country_estimates_voeten <- pmap_dfr(combinations,read_ideal_point_csv) |> 
#   group_by(ccode) |> 
#   left_join(data |> distinct(ccode, Country) |> filter(str_length(Country)==3)) |>
#   left_join(un_sessions) |> 
#   group_by(issue) |> 
#   mutate(phi = rescale(phi, to = c(-1,1))) |>
#   mutate(percent = if_else(is.na(percent),"all", percent)) |> 
#   filter(str_detect(suffix,"peace|All") ) |> 
#   filter(percent %in% c("sd", "all"))
# 
# regional_groups <- readxl::read_xlsx("04_clean_data/2024_regional_groups.xlsx") |> 
#   pivot_longer(cols = -iso_alpha3_code, names_to = "rg", values_to = "value") |> 
#   filter(value ==1) |> 
#   filter(!(iso_alpha3_code == "TUR" & rg =="rg_western_european_and_other_states"))
#   
#   
# two_dimensionsal_matrix <- country_estimates_voeten |> 
#   ungroup() |> 
#   dplyr::select(Country, session, phi, year, suffix) |> 
#   mutate(suffix = case_when(
#     str_detect(suffix,"All")~"all",
#     str_detect(suffix,"peace")~"peace",
#     str_detect(suffix,"health")~"health"
#   )) |> 
#   pivot_wider(names_from = suffix, values_from = phi) |> 
#   left_join(regional_groups, by = c("Country" = "iso_alpha3_code")) |> 
#   mutate(rg = case_when(
#     rg == "rg_african_states" ~ "African States",
#     rg == "rg_asia_pacific_states" ~ "Asia Pacific States",
#     rg == "rg_eastern_european_states" ~ "Eastern European States",
#     rg == "rg_latin_american_and_caribbean_states" ~ "Latin American and Caribbean States",
#     rg == "rg_western_european_and_other_states" ~ "Western European and Other States"
#   )) 
# 
# colors <-
#   swp_cols(
#     "rot2",
#     "ocker3",
#     "gruen2",
#     "weinrot2",
#     "blau2"
#   )
# 
# countries <- country_estimates_voeten |> 
#   group_by(issue) |> 
#   filter(phi == max(phi)|phi==min(phi)) |> 
#   pull(Country) |> 
#   c(x = _,
#     c("USA",  "CHN", "RUS", "DEU",  "ZAF",  "MEX", "ISR")) |>
#   unique()
# 
# eu_countries <- giscoR::gisco_countrycode |> filter(eu) |> pull(ISO3_CODE)
# 
# 
# ggplot(two_dimensionsal_matrix) +
#   geom_abline(aes(slope = 1, intercept = 0), color = "black", linewidth = 2, alpha = .5) +
#   geom_smooth(
#     data = two_dimensionsal_matrix |>
#       dplyr::filter(Country %in% eu_countries),
#     aes(x = all, y = peace ), method = "lm", color = "black", linewidth = 2, alpha = .5,se = FALSE
#   ) +
#   annotate(geom = "label",x = 0.8, y = 0, label = "Regression line \nfor EU countries", fill = "black", color = "white", fontface = "bold", label.size = NA)+
#   geom_point(
#     data = two_dimensionsal_matrix ,
#     aes(
#       x = all,
#       y = peace ,
#       group = Country,
#       color = rg
#     ),
#     key_glyph = "rect"
#   ) +
#   scale_color_manual(values = colors, name = "Regional Groups: ") +
#   geom_label_repel(
#     data = two_dimensionsal_matrix |>
#       filter(Country %in% countries),
#     aes(
#       x = all,
#       y = peace ,
#       group = Country,
#       label = Country
#     ),
#     fill = swp_cols("ocker4"),
#     label.size = NA,
#     min.segment.length = 0.0,
#     fontface = "bold",
#     color = "white",
#     max.time = 3,
#     force = 100,
#     max.overlaps = 6,
#     show.legend = FALSE
#   ) +
#   facet_wrap(. ~ year) +
#   theme_swp(aspect.ratio = 1) +
#   labs(
#     # x = "Ideal Points across all issues",
#     # y = "Ideal Points across Global Health",
#     title = "Ideal Points in two dimensions",
#     subtitle = "for Global Health compared to overall ideal points"
#   ) +
#   guides(color = guide_legend(override.aes = list(fill = colors[1:5]), nrow = 2))
# 
# 
# ggsave(file.path(dir, "ideal_points_scatter_trend_health_all.png"), width = 10, height = 7)
# 

#### co-voting of selected countries


## CHN PRK all


all <- data |> filter(Country %in% c("CHN","PRK")) |> 
  dplyr::select(rcid, Country, year, session_original, vote, descr)


co_voting <- all |> 
  left_join(all |> select(rcid, Country, vote), by = "rcid") |> 
  filter(Country.y>Country.x) |> 
  group_by(rcid) |> 
  mutate(n = n()) |> 
  group_by(year) |> 
  filter(!vote.x == 8) |> 
  filter(!vote.y == 8) |> 
  mutate(n = n(),
         both_yes = sum(vote.x == 1 & vote.y == 1))


co_voting_yearly <- co_voting |> distinct(year, n, both_yes, Country.y, Country.x) |> 
  mutate(perc = both_yes/n)

all <- data |> filter(Country %in% c("SYR","PRK")) |> 
  dplyr::select(rcid, Country, year, session_original, vote, descr)


co_voting <- all |> 
  left_join(all |> select(rcid, Country, vote), by = "rcid") |> 
  filter(Country.y>Country.x) |> 
  group_by(rcid) |> 
  mutate(n = n()) |> 
  group_by(year) |> 
  filter(!vote.x == 8) |> 
  filter(!vote.y == 8) |> 
  mutate(n = n(),
         both_yes = sum(vote.x == 1 & vote.y == 1))


co_voting_yearly <- co_voting |> distinct(year, n, both_yes, Country.y, Country.x) |> 
  mutate(perc = both_yes/n)




# MEX und RUS in health ---------------------------------------------------


health <- data |> filter(he ==1, Country %in% c("RUS","MEX")) |> 
  dplyr::select(rcid, Country, year, session_original,session, vote, descr)

co_voting_mex <- health |> 
  left_join(health |> dplyr::select(rcid, Country, vote), by = "rcid") |> 
  filter(Country.y>Country.x) |> 
  group_by(rcid) |> 
  mutate(n = n()) |> 
  group_by(year) |> 
  filter(!vote.x == 8) |> 
  filter(!vote.y == 8) |> 
  mutate(n = n(),
         both_yes = sum(vote.x == 1& vote.y == 1),
         both_no = sum(vote.x == 3& vote.y == 3),
         both_abstain = sum(vote.x == 2& vote.y == 2),
         both_disagree = sum(vote.x!=vote.y)) |> 
  rowwise() |> 
  pivot_longer(cols = contains("both_"), 
               names_to = "voting_type", 
               values_to = "voting_type_n",
               names_transform = function(x)str_replace(x,"_"," ")) |> 
  distinct(year, voting_type, voting_type_n) |> 
  mutate(voting_type = if_else(str_detect(voting_type,"disagree"), "disagree", voting_type))


p1 <- ggplot(co_voting_mex)+
  geom_vline(aes(xintercept = 2014.5), color = swp_cols("weinrot"), linewidth = 3, alpha = .3)+
  geom_col(aes(x = year, y = voting_type_n, group = voting_type, fill = voting_type))+
  theme_swp(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  scale_fill_manual(values = swp_cols("grey80","blau2","gruen4","rot4"), name = "Co-Voting Type")+
  scale_x_continuous(breaks = c(1994:2024))+
  labs(subtitle = "for MEX and RUS in 'Global Health'", 
       caption = "vertical line is cut for session grouping", y = "Number of votes") +
  scale_y_continuous(breaks = seq(0,15,5), limits = c(0,15))


health <- data |> filter(he ==1, Country %in% c("RUS","CHN")) |> 
  dplyr::select(rcid, Country, year, session_original,session, vote, descr)

co_voting_chn <- health |> 
  left_join(health |> dplyr::select(rcid, Country, vote), by = "rcid") |> 
  filter(Country.y>Country.x) |> 
  group_by(rcid) |> 
  mutate(n = n()) |> 
  group_by(year) |> 
  filter(!vote.x == 8) |> 
  filter(!vote.y == 8) |> 
  mutate(n = n(),
         both_yes = sum(vote.x == 1& vote.y == 1),
         both_no = sum(vote.x == 3& vote.y == 3),
         both_abstain = sum(vote.x == 2& vote.y == 2),
         both_disagree = sum(vote.x!=vote.y)) |> 
  rowwise() |> 
  pivot_longer(cols = contains("both_"), 
               names_to = "voting_type", 
               values_to = "voting_type_n",
               names_transform = function(x)str_replace(x,"_"," ")) |> 
  distinct(year, voting_type, voting_type_n) |> 
  mutate(voting_type = if_else(str_detect(voting_type,"disagree"), "disagree", voting_type))


p2 <- ggplot(co_voting_chn)+
  geom_vline(aes(xintercept = 2014.5), color = swp_cols("weinrot"), linewidth = 3, alpha = .3)+
  geom_col(aes(x = year, y = voting_type_n, group = voting_type, fill = voting_type))+
  theme_swp(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  scale_fill_manual(values = swp_cols("grey80","blau2","gruen4","rot4"), name = "Co-Voting Type")+
  scale_x_continuous(breaks = c(1994:2024))+
  labs(subtitle = "for CHN and RUS in 'Global Health'", 
       caption = "vertical line is cut for session grouping", y = "Number of votes")+
  scale_y_continuous(breaks = seq(0,15,5), limits = c(0,15))

p1/p2 + plot_annotation(
  title = 'Co-Voting behavior'
) &theme_swp(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

ggsave(file.path(dir, "covoting_mex_rus_chn.png"), width = 10, height = 7)

paragraphs <- data |> filter(he ==1, Country %in% c("RUS","CHN"))  |> 
  left_join(health |> dplyr::select(rcid, Country, vote), by = "rcid") |> 
  filter(Country.y>Country.x) |> 
  group_by(rcid) |> 
  mutate(n = n()) |> 
  group_by(year) |> 
  filter(!vote.x == 8) |> 
  filter(!vote.y == 8) |> 
  filter(vote.x == 3 & vote.y == 3) |> 
  left_join(prediction_data |> select(text, unres_swp = unres, title, label_health) |> filter(label_health =="health"))

writexl::write_xlsx(paragraphs, file.path(dir, "health_paragraphs_selected_res.xlsx"))


rcid_no <- data |> filter(he ==1, Country %in% c("RUS","CHN"))  |> 
  left_join(health |> dplyr::select(rcid, Country, vote), by = "rcid") |> 
  filter(Country.y>Country.x) |> 
  group_by(rcid) |> 
  mutate(n = n()) |> 
  group_by(year) |> 
  filter(!vote.x == 8) |> 
  filter(!vote.y == 8) |> 
  filter(vote.x == 3 & vote.y == 3) |> 
  pull(rcid)

voting_behavior <- data |> filter(rcid %in% rcid_no, Country%in% c("MEX","RUS","CHN")) |> 
  select(unres_swp, vote, Country, descr) |> 
  pivot_wider(names_from = "Country", values_from = "vote")

writexl::write_xlsx(voting_behavior, file.path(dir, "voting_behavior_chn_rus_mex.xlsx"))
