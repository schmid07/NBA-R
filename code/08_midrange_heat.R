library(tidyverse)
library(rvest)
library(janitor)
library(showtext)
library(ggtext)
library(hoopR)
library(MetBrewer)


# load font ---------------------------------------------------------------


font_add_google(name = "Anton", family = "Anton")
font_add_google(name = "Outfit", family = "Outfit")
showtext_auto()

# pulling in team abbreviations ----------------------------------------------------------------

recode_key <- c("LA Clippers" = "Los Angeles Clippers")

df_team_logo_espn <- espn_nba_teams() %>%
  select(abbreviation, display_name, logo) %>%
  mutate(
    display_name = recode(display_name, !!!recode_key),
    display_name_2 = case_when(
      display_name == "New Orleans Pelicans" ~ "CHA/NO",
      display_name == "Charlotte Hornets" ~ "CHA-H",
      display_name == "Oklahoma City Thunder" ~ "SEA/OKC",
      display_name == "Brooklyn Nets" ~ "NJN/BKN",
      display_name == "Memphis Grizzlies" ~ "VAN/MEM",
      display_name == "Washington Wizards" ~ "WSH",
      TRUE ~ display_name
    )
  )

# basketball reference notes that shot type and location from the 1990s is inconsistent, thus excluded

seasons <- 2000:2022


# function to pull in data ------------------------------------------------

get_midrange_shots <- function(season) {
  url_data <- glue::glue("https://www.basketball-reference.com/leagues/NBA_{season}.html")

  css_selector_shots <- "#shooting-team"

  df_shots <- url_data %>%
    read_html() %>%
    html_element(css = css_selector_shots) %>%
    html_table(header = FALSE) %>%
    row_to_names(row_number = 2) %>%
    clean_names() %>%
    select(team, x16_3p) %>%
    mutate(across(c(x16_3p), as.numeric),
      team = str_remove_all(team, "\\*")
    )

  # there's no column for season so needed to add here
  df_shots %>%
    mutate(team_season = season)
}

# loop through seasons to get each season
df_shots_pull <- map_dfr(
  seasons, ~ get_midrange_shots(season = .)
)

# additional cleaning

df_shots <- df_shots_pull %>%
  mutate(team_2 = case_when(
    team == "Charlotte Hornets" & team_season < 2003 ~ "CHA/NO",
    team == "New Orleans Hornets" ~ "CHA/NO",
    team == "New Orleans Pelicans" ~ "CHA/NO",
    team == "New Orleans/Oklahoma City Hornets" ~ "CHA/NO",
    team == "Charlotte Bobcats" ~ "CHA-H",
    team == "Charlotte Hornets" & team_season > 2014 ~ "CHA-H",
    team == "Seattle SuperSonics" ~ "SEA/OKC",
    team == "Oklahoma City Thunder" ~ "SEA/OKC",
    team == "Brooklyn Nets" ~ "NJN/BKN",
    team == "New Jersey Nets" ~ "NJN/BKN",
    team == "Memphis Grizzlies" ~ "VAN/MEM",
    team == "Vancouver Grizzlies" ~ "VAN/MEM",
    team == "Washington Bullets" ~ "WSH",
    team == "Washington Wizards" ~ "WSH",
    TRUE ~ team
  )) %>%
  filter(team != "League Average") %>%
  left_join(df_team_logo_espn, by = c("team_2" = "display_name_2")) %>%
  mutate(abbreviation = case_when(
    abbreviation == "NO" ~ "CHA/NO",
    abbreviation == "CHA" ~ "CHA-H",
    abbreviation == "BKN" ~ "NJN/BKN",
    abbreviation == "OKC" ~ "SEA/OKC",
    abbreviation == "MEM" ~ "VAN/MEM",
    TRUE ~ abbreviation
  )) %>%
  mutate(rank = dense_rank(x16_3p)) %>%
  mutate(size = case_when(
    rank == 1 ~ 1.5,
    TRUE ~ .1
  )) %>%
  mutate(text_color = case_when(
    x16_3p > .2 ~ "#E0E0E0",
    TRUE ~ "#181818"
  ))


# plot --------------------------------------------------------------------

ggplot(data = df_shots, aes(
  x = team_season, y = abbreviation,
  fill = x16_3p,
  label = rank
)) +
  scale_fill_stepsn(
    colors = met.brewer("Tam"),
    n.breaks = 10,
    name = "% of Field Goal Attempts between 16 feet and 3-point line",
    breaks = c(.05, .1, .15, .2, .25, .3, .35),
    labels = c("5%", "10%", "15%", "20%", "25%", "30%", "35%"),
    guide = guide_coloursteps(
      frame.colour = "black",
      frame.linewidth = 1,
      title.position = "top",
      title.hjust = .5
    )
  ) +
  geom_tile(size = .1) +
  geom_tile(
    data = df_shots %>% filter(rank < 11),
    color = "black",
    fill = NA,
    size = .8
  ) +
  geom_text(size = 7, color = df_shots$text_color, family = "Outfit") +
  scale_x_continuous(
    expand = c(0, .2),
    breaks = c(2000, 2005, 2010, 2015, 2020, 2022)
  ) +
  labs(
    title = "The Decline of the Midrange Jumper",
    x = "", y = "",
    subtitle = "The below heat map shows the sharp decline of the midrange jumper as a percentage of
       all shots taken for a given team and season. The numbers in the tiles refer to each team's ranking,
       with '1' denoting the team with the lowest percentage of midrange shots for a given season across the 22 season
       sample. The ten lowest seasons are outlined in black, with all ten occurring between 2018 and 2022.",
    caption = "Data from www.basketball-reference.com | Graph by @Schmid07"
  ) +
  theme_void(base_family = "Outfit") +
  theme(
    axis.text.y = element_text(hjust = 1, vjust = 0.5),
    plot.background = element_rect(color = NA, fill = "grey90"),
    axis.text = element_text(size = 20, color = "#36454F"),
    panel.grid.major.y = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.text = element_text(size = 25, color = "#36454F"),
    legend.spacing.y = unit(.2, "cm"),
    legend.key.height = unit(.35, "cm"),
    legend.key.width = unit(1.8, "cm"),
    legend.title = element_text(
      size = 25, color = "#36454F",
      margin = margin(t = 0)
    ),
    legend.margin = margin(t = 5, b = 5),
    plot.title = element_text(
      family = "Anton", size = 55, margin = margin(b = 10),
      color = "#36454F"
    ),
    plot.subtitle = element_textbox_simple(
      size = 27, margin = margin(b = 15), lineheight = .4,
      color = "#36454F"
    ),
    plot.caption = element_text(size = 20),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  )

ggsave("midrange1.png", width = 10, height = 6)



# # This decline has been mostly offset by a large increase in 3-pt FGAs. In his book, the Midrange Theory,
# Seth Partnow notes that 66% of shots that were long twos in 2005 had been replaced by 3-pt FGA
# in 2020. Partnow notes that the trend from long twos to 3-pt FGA was precipitated by a decline in
# shots outside the paint but shorter than 16 ft, with the reduction mostly occurring in postup plays.
# He cites two primary factors: 1) the emergence of perimeter oriented big men like KG and Dirk Nowitzki
# and 2) rule changes implemented in 2001 and 2004 that allowed for zone
# defenses and removed hand-checking. With these two changes pushing shooters further away from the
# basket, the analytic revolution served as a final accelerant toward more 3-pt FGA and fewer midrange
# shots, with massive efficiencies resulting from shooting a 3-pt shot vs. a long two.
