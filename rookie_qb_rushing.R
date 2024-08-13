# Load packages
library(tidyverse)
library(nflreadr)
library(ggimage)
library(gt)
library(nflfastR)
library(ggplot2)
library(ggpmisc)

# Load play-by-play data from nflfastR
pbp <- load_pbp(2014:2023)

# Filter to only regular season
pbp_reg <- pbp |>
  filter(season_type == 'REG')

# Calculate player statistics and combine weeks of each year
rushing_stats <- calculate_player_stats(pbp_reg, weekly = T) |>
  group_by(player_id, season) |>
  summarize(
    games = n(),
    carries = sum(carries, na.rm  = T),
    yards = sum(rushing_yards, na.tm = T),
    fantasy_points = sum(fantasy_points, na.rm = T),
    team = last(recent_team),
    plot_name = last(player_name)
  ) |>
  mutate(fantasy_ppg = fantasy_points / games)

# Import rosters from same seasons
rosters <- load_rosters(2014:2023)

# Filter rosters to only have desired columns
rosters_use <- rosters |>
  select(season, full_name, position, gsis_id, rookie_year)

# Merge in roster data by year and id
merged <- merge(rushing_stats, rosters_use, by.x = c("player_id", "season"), by.y = c("gsis_id", "season"))

# Filter the df to only contain rookie QBs
rookie_qbs <- merged |>
  filter(position == 'QB') |>
  filter(season == rookie_year)

# Merge in team color data for the graph
with_colors <- rookie_qbs |>
  left_join(teams_colors_logos, by = c("team" = "team_abbr"))

# Generate plot
rookie_qb_plot <- with_colors |>
  filter(fantasy_points >= 50) |>
  filter(!is.na(plot_name)) |>
  ggplot(aes(x = carries, y = fantasy_points)) +
  geom_smooth(method = "glm", se = T, color='blue', fullrange = T) + 
  geom_point(aes(fill = team_color, color = team_color2, size = yards), 
             shape = 21, alpha = 0.9) +
  scale_color_identity(aesthetics = c("fill", "color")) +
  ggrepel::geom_text_repel(aes(label = plot_name)) +
  theme_bw() +
  geom_hline(yintercept = 269.55, linetype = "dashed", color='red') +
  annotate("text", x = Inf, y = 269.55, 
           label = "Average QB12 Finish", 
           vjust = 1.5, hjust = 1.1, color = "red", size = 4) +
  geom_vline(xintercept = mean(with_colors$carries), linetype = "dashed") +
  labs(x = "Rushing Attempts (includes scrambles)",
       y = "Fantasy Points",
       size = 'Rushing Yards',
       title = 'Rookie Quarterback Rushes vs Fantasy Production, 2014-2023',
       subtitle = "Only players with 50+ fantasy points included",
       caption = '@CoopFB | data from nflfastR') +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +
  theme(plot.title = element_text(size = 22, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 16, hjust = 0.5))

# Save the plot
ggsave('rookie_qb_rushing.png', rookie_qb_plot, dpi=300)
