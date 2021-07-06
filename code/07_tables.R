library(nbastatR)
library(tidyverse)
library(janitor)
library(showtext)
library(imager)
library(ggrepel)
library(ggeconodist)
library(reactable)
library(reactablefmtr)
library(teamcolors)
library(blastula)

  

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

logo <- load.image(here::here("images", "nba_logo.png"))

bref_players_stats(seasons = 2008:2021)
df_headshots <- dataBREFPlayerAdvanced %>% 
  clean_names() %>%
  # if player played on multiple teams in a year, used team player was on at end of year
  mutate(slugs_team_bref = str_extract_all(slug_teams_bref, "...$")) %>% 
  select(name_player, url_player_headshot, year_season, slugs_team_bref) %>% 
  # add 1 here. season says 2020 for 2020-2021 season. so adjusted this so that 
  # the 2020-2021 season would be read as 2021 to link up to other dataset.
  mutate(year_season = year_season + 1) 

df_team_logos <- teamcolors %>% 
  filter(league == "nba") %>% 
  mutate(
    img_name = str_replace(logo, ".*[/]([^.]+)[.].*", "\\1"),
    img_name = glue::glue("https://raw.githubusercontent.com/schmid07/R-Reactable-NBA-Raptor/main/logos/{img_name}.png"))

df_three <- df_team_shots %>% 
  group_by(year_season, name_player) %>% 
  mutate(games = n_distinct(date_game)) %>% 
  # removed half court shots and beyond.  Also removed 256 instances where 3 PT 
  # FGA was recorded despite being below corner 3 distance of 22 ft.
  filter(type_shot == "3PT Field Goal", 
         distance_shot < 47 & distance_shot >= 22) %>% 
  add_count(year_season, name_player) %>%
  mutate(three_fga_per = round(n / games, 1),
         fgm = sum(is_shot_made == TRUE),
         perc = round(fgm / n * 100, 1)) %>% 
  # removed players with fewer than 41 3 pt fga
  filter(n > 41, games > 41) %>% 
  mutate(average_dist = round(mean(distance_shot), 2)) %>% 
  ungroup() %>% 
  distinct(
    across(
      c(year_season, name_player)
      ), 
    .keep_all = TRUE) %>% 
  left_join(df_headshots, by = c("name_player", "year_season")) %>% 
  left_join(df_team_logos, by = c("name_team" = "name")) %>% 
  mutate(year_season = as_factor(year_season)) %>% 
  select(url_player_headshot, name_player, img_name, slugs_team_bref,
         year_season, games, average_dist, three_fga_per, perc) %>% 
  tidyr::fill(url_player_headshot) %>% 
  mutate(url_player_headshot = if_else(
    is.na(url_player_headshot), 
    "https://raw.githubusercontent.com/schmid07/30-Day-Chart-Challenge/main/images/nba_logo.png", 
    url_player_headshot))
  

pal <- rcartocolor::carto_pal(n = 7, name = "DarkMint")

theme_538 <- function() {
  reactable::reactableTheme(
    searchInputStyle = list(width = "31%", backgroundColor = "#F9F9F9"),
    backgroundColor = "#ece5d5",
    headerStyle = list(
      "&:hover[aria-sort]" = list(
        background = "hsl(0, 0%, 80%)"),
      "&[aria-sort='ascending'], &[aria-sort='descending']" = list(
        background = "#555",
        borderWidth = "3px",
        color = "#FFF"
      ),
      borderColor = "#333"
    ),
    borderColor = "#CDCDCD"
  )
}

options(reactable.theme = reactableTheme(
  backgroundColor = "#ece5d5"
))

reactable(df_three, 
          theme = theme_538,
          defaultSorted = "average_dist",
          showSortIcon = FALSE,
          pagination = FALSE,
          defaultColDef = colDef(align = "center"),
          columns = list(
            url_player_headshot = colDef(
              cell = embed_img(df_three, 
                               height = "33", 
                               width = "40"
                               ),
              name = "",
              align = "right"),
            img_name = colDef(
              cell = embed_img(df_three,
                               height = "33",
                               width = "50",
                               label = "slugs_team_bref"
                     ),
              name = "Team",
              minWidth = 60),
            slugs_team_bref = colDef(show= FALSE),
            name_player = colDef(
              name = "Player",
              align = "left",
              minWidth = 100
              ),
            year_season = colDef(
              name = "Season"
              ),
            games = colDef(
              name = "Games",
              defaultSortOrder = "desc"
              ),
            average_dist = colDef(
              name = "Avg. Distance (ft.)",
              defaultSortOrder = "desc",
              cell = color_tiles(df_three, pal,
                               number_fmt = scales::label_number(accuracy = .1))
              ),
            three_fga_per = colDef(
              name = "3PT FGA/Game",
              defaultSortOrder = "desc",
              cell = color_tiles(df_three, pal)
              ),
            perc = colDef(
              name = "3PT FG%",
              defaultSortOrder = "desc",
              cell = color_tiles(df_three, pal)
            )
          )
)


                   
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

