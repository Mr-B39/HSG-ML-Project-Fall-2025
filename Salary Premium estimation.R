
#####################################################################
# =========================== H2 =============================
#####################################################################

# Load packages
library(readr)
library(dplyr)
library(broom)

# 1. Load data

# Adjust path if needed
player_df <- read_csv("Downloads/Rstudio/MBA group project/player_stats_final1.csv")

# 2. Basic cleaning / variable prep

player_df <- player_df %>%
  # Keep observations with valid salary and basic stats
  filter(!is.na(inflationAdjSalary_num),
         inflationAdjSalary_num > 0,
         !is.na(PTS),
         !is.na(MP),
         MP > 0,
         !is.na(win_pct)) %>%
  mutate(
    # Log salary to reduce skewness
    log_salary = log(inflationAdjSalary_num),
    Pos = as.factor(Pos),
    Season = as.factor(Season),
    Tm = as.factor(Tm)
  )

# 3. Player-level salary model (EXPECTED salary given performance)
#    IMPORTANT: this model should NOT include win_pct or team success vars.

salary_model <- lm(
  log_salary ~ PTS + MP + Age + G + Pos + Season,
  data = player_df
)

summary(salary_model)  # to inspect fit if you want


# 4. Compute residuals = "over/underpayment"

player_df <- player_df %>%
  mutate(
    salary_resid = resid(salary_model)  # positive => overpaid vs. model
  )


# 5. Aggregate to TEAM-SEASON level

team_df <- player_df %>%
  group_by(Season, Tm) %>%
  summarise(
    team_overpay = mean(salary_resid, na.rm = TRUE),  # avg residual
    win_pct = first(win_pct),
    team_payroll = first(team_payroll_season_infl),
    .groups = "drop"
  )


# 6. Test relationship: do winning teams overpay more?

# Team-level regression: overpay ~ win_pct (+ control for payroll)
overpay_model <- lm(
  team_overpay ~ win_pct + team_payroll,
  data = team_df
)

summary(overpay_model)

# Null:     H0: beta_win_pct = 0 (no relationship)
# Alt:      H1: beta_win_pct > 0 (winning teams overpay more)

# Extract coefficient and perform one-sided t-test on win_pct
coefs <- summary(overpay_model)$coefficients
beta_win <- coefs["win_pct", "Estimate"]
se_win   <- coefs["win_pct", "Std. Error"]
t_stat   <- (beta_win - 0) / se_win
df_res   <- overpay_model$df.residual

# One-sided p-value (beta_win > 0)
p_one_sided <- pt(t_stat, df = df_res, lower.tail = FALSE)

cat("One-sided test for H1: beta_win_pct > 0\n")
cat("  Estimate:", beta_win, "\n")
cat("  t-stat:  ", t_stat, "\n")
cat("  p-value:", p_one_sided, "\n\n")

alpha <- 0.05
if (p_one_sided < alpha) {
  cat("Decision at 5% level: REJECT H0.\n")
  cat("Conclusion: There is evidence that winning teams tend to overpay relative to player output.\n")
} else {
  cat("Decision at 5% level: FAIL TO REJECT H0.\n")
  cat("Conclusion: No statistically significant evidence that winning teams overpay more than others.\n")
}

