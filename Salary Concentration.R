library(tidyverse)
library(ineq)
library(scales)


setwd("C:/Users/franc/Documents/HSG/Semester/HS 2025/Intr. to ML in R/ML Project")

# --- Load data ---
salaries <- read_csv("NBA Salaries(1990-2023).csv")
payroll  <- read_csv("NBA Payroll(1990-2023).csv")
team_records <- read_csv("Team_Records_fixed.csv")
player_stats <- read_csv("NBA Player Stats(1950 - 2022).csv")

# --- Clean salary column ---
salaries <- salaries %>%
  mutate(
    salary = gsub("[$,]", "", salary),
    salary = as.numeric(salary)
  )

# --- Merge team info from player stats (adds team column) ---
salaries <- salaries %>%
  left_join(
    player_stats %>%
      select(playerName = Player, seasonStartYear = Season, team = Tm),
    by = c("playerName", "seasonStartYear")
  )

# --- Check that merge worked ---
glimpse(salaries)

# --- Compute team-level Gini and Top-3 ratio ---
team_concentration <- salaries %>%
  group_by(team, seasonStartYear) %>%
  summarise(
    gini_salary = ineq(salary, type = "Gini"),
    top3_ratio = sum(sort(salary, decreasing = TRUE)[1:3], na.rm = TRUE) /
      sum(salary, na.rm = TRUE),
    .groups = "drop"
  )

# --- Clean team names for matching ---
team_concentration <- team_concentration %>%
  mutate(team = case_when(
    team == "ATL" ~ "Atlanta Hawks",
    team == "BOS" ~ "Boston Celtics",
    team == "BRK" | team == "NJN" ~ "Brooklyn Nets",
    team == "CHO" | team == "CHA" ~ "Charlotte Hornets",
    team == "CHI" ~ "Chicago Bulls",
    team == "CLE" ~ "Cleveland Cavaliers",
    team == "DAL" ~ "Dallas Mavericks",
    team == "DEN" ~ "Denver Nuggets",
    team == "DET" ~ "Detroit Pistons",
    team == "GSW" ~ "Golden State Warriors",
    team == "HOU" ~ "Houston Rockets",
    team == "IND" ~ "Indiana Pacers",
    team == "LAL" ~ "Los Angeles Lakers",
    team == "LAC" ~ "Los Angeles Clippers",
    team == "MEM" ~ "Memphis Grizzlies",
    team == "MIA" ~ "Miami Heat",
    team == "MIL" ~ "Milwaukee Bucks",
    team == "MIN" ~ "Minnesota Timberwolves",
    team == "NOP" | team == "NOH" ~ "New Orleans Pelicans",
    team == "NYK" ~ "New York Knicks",
    team == "OKC" | team == "SEA" ~ "Oklahoma City Thunder",
    team == "ORL" ~ "Orlando Magic",
    team == "PHI" ~ "Philadelphia 76ers",
    team == "PHO" ~ "Phoenix Suns",
    team == "POR" ~ "Portland Trail Blazers",
    team == "SAC" ~ "Sacramento Kings",
    team == "SAS" ~ "San Antonio Spurs",
    team == "TOR" ~ "Toronto Raptors",
    team == "UTA" ~ "Utah Jazz",
    team == "WAS" | team == "WSB" ~ "Washington Wizards",
    TRUE ~ team
  ))

# --- Prepare team record data ---
team_records <- team_records %>%
  mutate(Season = as.numeric(Season))

# --- Merge team concentration with team performance ---
team_data <- team_concentration %>%
  left_join(team_records, by = c("team" = "Team", "seasonStartYear" = "Season")) %>%
  mutate(win_pct = W / (W + L))

# --- Filter for available seasons (e.g., 2018–2021) ---
team_data_filtered <- team_data %>%
  filter(seasonStartYear >= 2018 & seasonStartYear <= 2021)

# --- Inspect data ---
glimpse(team_data_filtered)
head(team_data_filtered)

# --- Correlation check ---
cor(team_data_filtered$gini_salary, team_data_filtered$win_pct, use = "complete.obs")
cor(team_data_filtered$top3_ratio, team_data_filtered$win_pct, use = "complete.obs")

# --- Regression 1: Gini coefficient ---
model_gini <- lm(win_pct ~ gini_salary, data = team_data_filtered)
summary(model_gini)

# --- Regression 2: Top-3 salary ratio ---
model_top3 <- lm(win_pct ~ top3_ratio, data = team_data_filtered)
summary(model_top3)

# --- Plot 1: Gini coefficient vs Win% ---
ggplot(team_data_filtered, aes(x = gini_salary, y = win_pct)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Salary Concentration vs Team Success (2018–2021)",
    x = "Gini Coefficient (Salary Concentration)",
    y = "Win Percentage"
  )

# --- Plot 2: Top-3 ratio vs Win% ---
ggplot(team_data_filtered, aes(x = top3_ratio, y = win_pct)) +
  geom_point(color = "darkgreen") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "NBA Salary Concentration vs Team Success (2018–2021)",
    x = "Top 3 Salaries / Total Payroll",
    y = "Win Percentage"
  )