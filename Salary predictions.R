###############################################
# DATA MANAGEMENT PIPELINE — NBA ANALYSIS
# Final dataset identical to your working version
###############################################

library(tidyverse)
library(readxl)
library(janitor)
library(stringr)

###########################################################
# 1. LOAD PLAYER STATS AND FILTER THE PERIOD (1990–2022)
###########################################################

player_stats <- read_csv("/Users/jeremykundig/Desktop/ML/archive/NBA Player Stats(1950 - 2022).csv") %>%
  filter(Season >= 1990) %>% 
  # Replace all NA with 0.
  # In BBRef-style datasets, NAs mostly mean "no attempts / no events",
  # so zero is the correct interpretation for rate/attempt stats.
  mutate(across(everything(), ~ replace_na(.x, 0)))


###########################################################
# 2. STANDARDIZE TEAM NAMES (CONSISTENT THROUGH TIME)
###########################################################

team_names <- c(
  ATL = "Atlanta Hawks", BOS = "Boston Celtics", BRK = "Brooklyn Nets",
  NJN = "New Jersey Nets", CHI = "Chicago Bulls",
  CHO = "Charlotte Hornets", CHH = "Charlotte Hornets",
  CHA = "Charlotte Bobcats",
  CLE = "Cleveland Cavaliers", DAL = "Dallas Mavericks",
  DEN = "Denver Nuggets", DET = "Detroit Pistons",
  GSW = "Golden State Warriors", HOU = "Houston Rockets",
  IND = "Indiana Pacers", LAC = "Los Angeles Clippers",
  LAL = "Los Angeles Lakers", MEM = "Memphis Grizzlies",
  VAN = "Vancouver Grizzlies", MIA = "Miami Heat",
  MIL = "Milwaukee Bucks", MIN = "Minnesota Timberwolves",
  NOP = "New Orleans Pelicans", NOH = "New Orleans Hornets",
  NOK = "New Orleans/Oklahoma City Hornets", NYK = "New York Knicks",
  OKC = "Oklahoma City Thunder", SEA = "Seattle SuperSonics",
  ORL = "Orlando Magic", PHI = "Philadelphia 76ers",
  PHO = "Phoenix Suns", POR = "Portland Trail Blazers",
  SAC = "Sacramento Kings", SAS = "San Antonio Spurs",
  TOR = "Toronto Raptors", UTA = "Utah Jazz",
  WAS = "Washington Wizards", WSB = "Washington Bullets",
  TOT = "Multiple Teams"
)

player_stats <- player_stats %>%
  mutate(Tm = team_names[Tm])


###########################################################
# 3. HANDLE MULTI-TEAM SEASONS + COMPUTE TOTAL GAMES
###########################################################

player_stats <- player_stats %>%
  mutate(G = as.numeric(G)) %>%
  group_by(Player, Season) %>%
  # Total games = sum of games played for real teams (exclude "Multiple Teams")
  mutate(G_total_season = sum(G[Tm != "Multiple Teams"], na.rm = TRUE)) %>%
  ungroup() %>%
  # Keep only per-team rows (remove TOT line)
  filter(Tm != "Multiple Teams")


###########################################################
# 4. SPLIT TEAM NAME INTO CITY + TEAM (KEEPING 2-WORD CASES)
###########################################################

player_stats <- player_stats %>%
  mutate(
    team = case_when(
      str_detect(Tm, "Trail Blazers") ~ "Trail Blazers",
      TRUE ~ word(Tm, -1)
    ),
    city = str_trim(str_remove(Tm, team))
  )


###########################################################
# 5. LOAD SALARY DATA + MERGE TO PLAYER_STATS
###########################################################

salaries <- read_csv("/Users/jeremykundig/Desktop/ML/archive/NBA Salaries(1990-2023).csv") %>%
  mutate(
    salary_num = as.numeric(str_remove_all(salary, "[$,]")),
    inflationAdjSalary_num = as.numeric(str_remove_all(inflationAdjSalary, "[$,]"))
  )

player_stats <- player_stats %>%
  left_join(
    salaries %>% select(playerName, seasonStartYear, salary_num, inflationAdjSalary_num),
    by = c("Player" = "playerName", "Season" = "seasonStartYear"),
    relationship = "many-to-many"   # silences warning; does NOT change the join result
  ) %>%
  # Keep only players with documented salary
  filter(!is.na(salary_num)) %>%
  distinct()


###########################################################
# 6. COMPUTE EFFECTIVE PAYROLL + TEAM PAYROLL + SALARY SHARE
###########################################################

player_stats <- player_stats %>%
  mutate(
    # Effective salary for that specific team (split by games played)
    payroll_effective_team      = salary_num * (G / G_total_season),
    payroll_effective_team_infl = inflationAdjSalary_num * (G / G_total_season)
  ) %>%
  group_by(Season, Tm) %>%
  mutate(
    # Total payroll for each team-season
    team_payroll_season      = sum(payroll_effective_team, na.rm = TRUE),
    team_payroll_season_infl = sum(payroll_effective_team_infl, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    # Player share of team payroll
    salary_share      = payroll_effective_team / team_payroll_season,
    salary_share_infl = payroll_effective_team_infl / team_payroll_season_infl
  )


###########################################################
# 7. LOAD SALARY CAP DATA + MERGE + % OF CAP METRICS
###########################################################

salary_cap <- read_excel("/Users/jeremykundig/Desktop/ML/archive/salary_cap.xls") %>%
  clean_names()

year_col <- names(salary_cap)[str_detect(names(salary_cap), "season|year|start")]
cap_col  <- names(salary_cap)[str_detect(names(salary_cap), "cap")]

salary_cap <- salary_cap %>%
  rename(
    Season = all_of(year_col[1]),
    salary_cap_raw = all_of(cap_col[1])
  ) %>%
  mutate(
    salary_cap_num = as.numeric(str_remove_all(as.character(salary_cap_raw), "[$,]")),
    Season = parse_number(as.character(Season))
  )

player_stats <- player_stats %>%
  left_join(
    salary_cap %>% select(Season, salary_cap_num),
    by = "Season"
  ) %>%
  mutate(
    pct_team_payroll_cap  = team_payroll_season / salary_cap_num,
    pct_player_salary_cap = payroll_effective_team / salary_cap_num
  )


###########################################################
# 8. LOAD STANDINGS (1990–2021) + CONFERENCE RANKING
###########################################################

team_records <- read_csv("/Users/jeremykundig/Desktop/ML/archive/Team_Records_fixed.csv") %>%
  mutate(
    Team = str_remove_all(Team, "\\*") %>% str_trim(),
    Season = str_extract(as.character(Season), "\\d{4}") %>% as.numeric(),
    win_pct = as.numeric(`W/L%`),
    W = as.numeric(W),
    L = as.numeric(L)
  ) %>%
  filter(Season >= 1990, Season <= 2021) %>%
  group_by(Season, Conference) %>%
  arrange(desc(win_pct), .by_group = TRUE) %>%
  mutate(conf_rank = row_number()) %>%
  ungroup() %>%
  select(Season, Team, W, L, win_pct, conf_rank)

player_stats <- player_stats %>%
  left_join(team_records, by = c("Season", "Tm" = "Team"))


###########################################################
# 9. PLAYOFF QUALIFICATION INDICATOR
###########################################################

player_stats <- player_stats %>%
  mutate(
    playoff_team = case_when(
      !is.na(conf_rank) & conf_rank <= 8 ~ 1,
      TRUE ~ 0
    )
  )


###########################################################
# 10. REMOVE FIRST COLUMN (CSV INDEX CLEANUP)
###########################################################

# Your raw CSV creates an unwanted first index-like column.
# Removing column #1 keeps the final dataset consistent with your goal.
player_stats <- player_stats %>% select(-1)

###########################################################
# 11. SAVE FINAL DATASET TO ML FOLDER
###########################################################

write_csv(
  player_stats,
  "/Users/jeremykundig/Desktop/ML/player_stats_final1.csv"
)

###########################################################
###########################################################