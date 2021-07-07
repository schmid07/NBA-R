library(nbastatR)
library(tidyverse)
library(janitor)
library(showtext)
library(imager)
library(ggrepel)
library(ggeconodist)

font_add_google("Nunito", "Nunito")

showtext_auto()

# fun_season <- function(.x) {
#   return(teams_shots(teams = vec_teams, seasons = .x))
# }
# 
# vec_teams <- dictionary_bref_teams() %>% 
#   filter(seasonLast == 2021) %>% 
#   pull(nameTeamBREF)
# 
# df_team_shots <- map_df(.x = c(2008:2021),
#                         .f = fun_season) %>% 
#   clean_names()
# 
# write_csv(df_team_shots, here::here("df_team_shots.csv"))

# horizontal dotted line for chart
horizontals <- seq(.5, 14.5, 1)

df_team_shots <- read_csv(here::here("data", "df_team_shots.csv"))

trae <- df_team_shots %>% filter(name_player == "Trae Young",
                                 year_season == 2021,
                                 type_shot == "3PT Field Goal") %>% 
  summarise(shot = mean(distance_shot))

logo <- load.image(here::here("images", "nba_logo.png"))

df_three <- df_team_shots %>% 
  group_by(year_season, name_player) %>% 
  mutate(games = n_distinct(date_game)) %>% 
  # removed half court shots and beyond.  Also removed 256 instances where 3 PT 
  # FGA was recorded despite being below corner 3 distance of 22 ft.
  filter(type_shot == "3PT Field Goal", 
         distance_shot < 47 & distance_shot >= 22) %>% 
  add_count(year_season, name_player) %>%
  mutate(three_fga_per = round(n / games, 1)) %>% 
  # removed players with fewer than 41 3 pt fga
  filter(n > 41) %>% 
  mutate(average_dist = mean(distance_shot)) %>% 
  ungroup() %>% 
  distinct(across(c(year_season, name_player)), 
           .keep_all = TRUE) %>% 
  mutate(year_season = as_factor(year_season),
         name_season = glue::glue("{name_player} {year_season}"))

# to allow for annotation of player that shot from furthest avg distance
vec_players <- df_distance %>% 
  group_by(year_season) %>% 
  slice_max(average_dist, n = 1) %>%
  pull(name_season)

df_distance <- df_three %>% 
  mutate(label = 
           case_when(
             name_player == "Trae Young" & year_season == 2021 ~ glue::glue("{name_player} (3 PT FGA/Game: {three_fga_per})"),
             name_season %in% vec_players ~ glue::glue("{name_player} ({three_fga_per})"),
             TRUE ~ ""))

# to add in annotations for 10th percentile, median, 90th percentile
vec_perc <- df_distance %>% 
  filter(year_season == 2021) %>% 
  summarise(percent = quantile(average_dist, c(.1, .5, .9))) %>% 
  pull(percent)

ggplot(df_distance, aes(year_season, average_dist)) + 
  geom_jitter(size = 1.2, alpha = .25, width = .1) +
  geom_econodist(tenth_col = "#17408B",
                 ninetieth_col = "#c9082a",
                 median_col = "#b54213",
                 median_point_size = 7,
                 fill = "white",
                 color = "#D3D3D3",
                 endcap_adjust = 2.5,
                 show.legend = TRUE)+
  geom_text_repel(
    aes(label = label),
    family = "Nunito",
    color = "black",
    size = 9/ .pt,
    fontface = "bold",
    point.padding = .1,
    box.padding = .4,
    seed = 12,
    min.segment.length = 0
  ) + 
  geom_vline(xintercept = horizontals, linetype = "dashed", color = "#BEBEBE") +
  scale_y_continuous(labels = paste0(seq(23, 27, 1), c(rep("", 4), " Feet")), 
                     breaks = seq(23, 27, 1)) + 
  annotation_raster(logo, xmin = 17.8, xmax = 19, ymin = 27.25, ymax = 27.35) +
  labs(title = "Average Distance of Three Point Field Goal Attempts",
       subtitle = "The below chart shows the average distance of three point attempts for each individual player in a given season. It excludes players with fewer than 41 attempts \nin a season and shots beyond halfcourt. The players that shot from the greatest average distance beyond the arc for each season are noted.",
       caption = "Data Viz: Bill Schmid @schmid_07 | Source: {nbastatR}") +
  coord_flip(clip = "off") +
  annotate("segment", 
           x = rep(14.3, 3), 
           xend = rep( 14.8, 3), 
           y = vec_perc, 
           yend = vec_perc) + 
  annotate("text", x = rep(15, 3), y = vec_perc, 
           label = c("10th Percentile", "Median", "90th Percentile"),
           family = "Nunito",
           size = 4) + 
  theme_minimal(base_family = "Nunito", base_size = 30) +
  theme(
    plot.title = element_text(size = 50,
                              face = "bold",
                              margin = margin(t = 35, b = 18)),
    plot.title.position = "plot",
    plot.subtitle = element_text(size = 17,
                                 family = "Nunito" ,
                                 margin = margin(b = 40)),
    plot.margin = margin(20, 60, 20, 60),
    plot.caption = element_text(hjust = .95,
                                size = 15,
                                margin = margin(t = 20),
                                color = "#BEBEBE"),
    plot.background = element_rect(color = NA, fill = "#ece5d5"),
    axis.title = element_blank(),
    axis.text = element_text(size = 25, face = "bold"),
    axis.ticks.x = element_line(color = "#989898"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank())


ggsave(here::here("plots", "07", "distance.png"), height = 11, width = 20)

