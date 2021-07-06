library(nbastatR)
library(tidyverse)
library(BasketballAnalyzeR)
library(showtext)
library(ggtext)

font_add_google(name = "Anton", family = "Anton")
showtext_auto()

# for some reason, entering vector of seasons wasn't working in teams_shots function 
# so created a function to loop through seasons 

fun_season <- function(.x) {
  return(teams_shots(teams = c("Golden State Warriors"), seasons = .x))
}

df_gs_team_shots <- map_df(.x = c(2010, 2021),
    .f = fun_season)

df_curry <- df_gs_team_shots %>% 
  filter(namePlayer == "Stephen Curry") 

facet_labels <- c("2010 <i style='font-size:18px'>(rookie year)</i>", "2021")
names(facet_labels) <- c("2010", "2021")

gg_curry <- ggplot(df_curry, aes(x = locationX / 10, y = locationY / 10 - 41.5)) + 
  geom_point(size = .5, color = "#fdb927", alpha = .3) + 
  facet_wrap(~ yearSeason, 
             strip.position = "bottom",
             labeller = labeller(yearSeason = facet_labels)) +
  coord_fixed() +
  scale_y_reverse() +
  theme_void(base_family = "Anton") + 
  labs(
    title = "Steph Curry Shot Chart",
    caption = "Data Viz: Bill Schmid @schmid_07 | Source: {nbastatR}"
  ) +
  theme(
    plot.margin = margin(t= 15, r = 15, b= 15, l = 15),
    strip.background = element_blank(),
    strip.text = element_textbox(
      size = 20, 
      color = "#fdb927",
      fill = "#006bb6",
      halign = .5,
      padding = margin(5, 0, 1, 0),
      # rounds corners
      r = unit(12, "pt"),
      width = unit(.95, "npc")),
    panel.spacing = unit(2, "lines"),
    plot.background = element_rect(fill = "grey18", color = NA),
    panel.background = element_rect(fill = "grey18", color = NA),
    plot.title = element_text(hjust = .5, 
                              color = "#fdb927", 
                              size = 35,
                              margin = margin(t = 15, b = 15)),
    plot.caption = element_text(hjust = .5,
                                size = 8,
                                margin = margin(t = 20),
                                color = "#fdb927")
  ) 

gg_curry %>% drawNBAcourt(size = 1, col = "#006bb6", full = TRUE)

ggsave(here::here("plots", "14", "curry.png"))
