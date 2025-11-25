###############################################
# NBA PROJECT — FULL FINAL PIPELINE (NOMINAL)
# RAW -> FINAL DATASET -> ML
# Features = stats t + lags/deltas t-1,t-2
# ONLY FUTURE INFO allowed in X = new_contract_next_year
# Target = salary_num_lead1 (nominal)
# Training EXCLUDES new-contract years
# Evaluation = 75/25 Holdout + 5-fold CV on train
###############################################

##############################
# 0) LIBRARIES
##############################
library(tidyverse)
library(readxl)
library(janitor)
library(stringr)

# ML stack (no mlr3verse)
library(mlr3)
library(mlr3learners)
library(mlr3pipelines)
library(mlr3tuning)
library(paradox)

##############################
# 1) PATHS (adjust if needed)
##############################
PATH_PLAYER_STATS <- "/Users/jeremykundig/Desktop/ML/archive/NBA Player Stats(1950 - 2022).csv"
PATH_SALARIES     <- "/Users/jeremykundig/Desktop/ML/archive/NBA Salaries(1990-2023).csv"
PATH_CAP          <- "/Users/jeremykundig/Desktop/ML/archive/salary_cap.xls"
PATH_RECORDS      <- "/Users/jeremykundig/Desktop/ML/archive/Team_Records_fixed.csv"

PATH_FINAL_OUT    <- "/Users/jeremykundig/Desktop/ML/player_stats_final1.csv"
PATH_PRED_TEST_OUT<- "/Users/jeremykundig/Desktop/ML/pred_vs_actual_salary_test.csv"

##############################
# 2) HELPER: CLEAN NAMES FOR MLR3
# (do NOT overwrite janitor::clean_names)
##############################
clean_names_mlr3 <- function(nms) {
  nms <- gsub("%", "_pct", nms)
  nms <- gsub("\\+", "_plus", nms)
  nms <- gsub(":", "_", nms)
  nms <- gsub("-", "_minus", nms)
  nms <- gsub("\\.", "_", nms)
  nms <- gsub(" ", "_", nms)
  nms <- gsub("/", "_", nms)
  nms <- gsub("\\(", "", nms)
  nms <- gsub("\\)", "", nms)
  nms
}

#####################################################################
# ========================= PART A: RAW -> FINAL =====================
#####################################################################

###########################################################
# A1) LOAD PLAYER STATS (1990+)
###########################################################
player_stats <- read_csv(PATH_PLAYER_STATS, show_col_types = FALSE) %>%
  mutate(Season = as.numeric(Season)) %>%
  filter(Season >= 1990) %>%
  mutate(across(where(is.numeric), ~ replace_na(.x, 0)))  # NA->0 only numeric

###########################################################
# A2) STANDARDIZE TEAM NAMES
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
# A3) MULTI-TEAM SEASONS + TOTAL GAMES
###########################################################
player_stats <- player_stats %>%
  mutate(G = as.numeric(G)) %>%
  group_by(Player, Season) %>%
  mutate(G_total_season = sum(G[Tm != "Multiple Teams"], na.rm = TRUE)) %>%
  ungroup() %>%
  filter(Tm != "Multiple Teams")  # drop TOT rows

###########################################################
# A4) LOAD SALARIES + MERGE
###########################################################
salaries <- read_csv(PATH_SALARIES, show_col_types = FALSE) %>%
  mutate(
    seasonStartYear = as.numeric(seasonStartYear),
    salary_num = as.numeric(str_remove_all(salary, "[$,]")),
    inflationAdjSalary_num = as.numeric(str_remove_all(inflationAdjSalary, "[$,]"))
  )

player_stats <- player_stats %>%
  left_join(
    salaries %>% select(playerName, seasonStartYear, salary_num, inflationAdjSalary_num),
    by = c("Player" = "playerName", "Season" = "seasonStartYear")
  ) %>%
  filter(!is.na(salary_num)) %>%
  distinct()

###########################################################
# A5) TEAM PAYROLL + SALARY SHARE (keep for cap/team analysis)
###########################################################
player_stats <- player_stats %>%
  mutate(
    payroll_effective_team      = salary_num * (G / G_total_season),
    payroll_effective_team_infl = inflationAdjSalary_num * (G / G_total_season)
  ) %>%
  group_by(Season, Tm) %>%
  mutate(
    team_payroll_season      = sum(payroll_effective_team, na.rm = TRUE),
    team_payroll_season_infl = sum(payroll_effective_team_infl, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    salary_share      = payroll_effective_team / team_payroll_season,
    salary_share_infl = payroll_effective_team_infl / team_payroll_season_infl
  )

###########################################################
# A6) LOAD SALARY CAP (ROBUST DETECTION) + MERGE + %CAP
###########################################################
salary_cap <- read_excel(PATH_CAP) %>% janitor::clean_names()

cols <- names(salary_cap)
year_col <- cols[str_detect(cols, "(season|year|start|yr)")]
cap_col  <- cols[str_detect(cols, "(cap|salary_cap|team_cap|cap_value|league_cap|nba_cap)")]

detect_season_col <- function(df) {
  cn <- names(df)
  scores <- sapply(cn, function(col){
    vals <- suppressWarnings(readr::parse_number(df[[col]]))
    mean(vals >= 1950 & vals <= 2100, na.rm = TRUE)
  })
  cn[which.max(scores)]
}
detect_cap_col <- function(df, season_col) {
  cn <- setdiff(names(df), season_col)
  scores <- sapply(cn, function(col){
    vals <- suppressWarnings(readr::parse_number(df[[col]]))
    mean(vals > 5e6, na.rm = TRUE)
  })
  cn[which.max(scores)]
}

if (length(year_col) == 0) year_col <- detect_season_col(salary_cap)
if (length(cap_col) == 0)  cap_col  <- detect_cap_col(salary_cap, year_col[1])

message("Salary cap Season col used: ", year_col[1])
message("Salary cap Cap col used: ", cap_col[1])

salary_cap <- salary_cap %>%
  rename(
    Season = all_of(year_col[1]),
    salary_cap_raw = all_of(cap_col[1])
  ) %>%
  mutate(
    Season = suppressWarnings(readr::parse_number(as.character(Season))),
    salary_cap_num = suppressWarnings(readr::parse_number(as.character(salary_cap_raw)))
  ) %>%
  filter(!is.na(Season), !is.na(salary_cap_num))

player_stats <- player_stats %>%
  left_join(salary_cap %>% select(Season, salary_cap_num), by = "Season") %>%
  mutate(
    pct_team_payroll_cap  = team_payroll_season / salary_cap_num,
    pct_player_salary_cap = payroll_effective_team / salary_cap_num
  )

###########################################################
# A7) LOAD TEAM RECORDS + CONF RANK
###########################################################
team_records <- read_csv(PATH_RECORDS, show_col_types = FALSE) %>%
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
# A8) PLAYOFF INDICATOR + DROP USELESS COLS + SAVE FINAL
###########################################################
player_stats <- player_stats %>%
  mutate(playoff_team = if_else(!is.na(conf_rank) & conf_rank <= 8, 1, 0)) %>%
  select(-any_of(c("Unnamed: 0", "Unnamed__0", "X1", "...1")))

write_csv(player_stats, PATH_FINAL_OUT)
cat("\n✅ Final dataset saved to:", PATH_FINAL_OUT, "\n")





#####################################################################
# =========================== PART B: ML =============================
#####################################################################

###########################################################
# B1) LOAD FINAL FOR ML
###########################################################
df <- read_csv(PATH_FINAL_OUT, show_col_types = FALSE)
names(df) <- clean_names_mlr3(names(df))
df <- df %>% arrange(Player, Season)

###########################################################
# B2) FUTURE CONTRACT FLAG ONLY (allowed future info)
# Create new_contract_next_year from nominal salary jump t->t+1.
# Then DROP temporary future cols to avoid leakage.
###########################################################
df <- df %>%
  group_by(Player) %>%
  arrange(Season) %>%
  mutate(
    sal_lead1_tmp  = lead(salary_num, 1),
    sal_growth_tmp = (sal_lead1_tmp - salary_num) / salary_num,
    new_contract_next_year = case_when(
      is.na(sal_lead1_tmp) ~ 0,
      sal_growth_tmp > 0.20 ~ 1,
      sal_growth_tmp < -0.15 ~ 1,
      TRUE ~ 0
    )
  ) %>%
  ungroup() %>%
  select(-sal_lead1_tmp, -sal_growth_tmp)   # IMPORTANT: remove futures

###########################################################
# B3) TARGET = nominal salary at t+1
###########################################################
df <- df %>%
  group_by(Player) %>%
  arrange(Season) %>%
  mutate(salary_num_lead1 = lead(salary_num, 1)) %>%
  ungroup() %>%
  filter(!is.na(salary_num_lead1))

###########################################################
# B4) LAGS/DELTAS for ALL personal numeric stats
# (exclude salary/payroll/team context BEFORE lagging)
###########################################################
num_cols_all <- names(df)[sapply(df, is.numeric)]

exclude_from_stats <- num_cols_all[
  str_detect(num_cols_all,
             "(salary|payroll|share|pct_|inflation|cap|W$|L$|win_pct|conf_rank|playoff_team|G_total_season)")
]

personal_stats <- setdiff(num_cols_all, exclude_from_stats)

df_fa <- df %>%
  group_by(Player) %>%
  arrange(Season) %>%
  mutate(
    across(all_of(personal_stats), ~ lag(.x, 1), .names = "lag1_{col}"),
    across(all_of(personal_stats), ~ lag(.x, 2), .names = "lag2_{col}"),
    across(all_of(personal_stats), ~ .x - lag(.x, 1), .names = "d1_{col}"),
    across(all_of(personal_stats), ~ lag(.x, 1) - lag(.x, 2), .names = "d2_{col}")
  ) %>%
  ungroup()

###########################################################
# B5) MODEL RAW DATASET
###########################################################
df_model_raw <- df_fa %>%
  mutate(new_contract_next_year = as.numeric(new_contract_next_year)) %>%
  filter(!is.na(salary_num_lead1))

names(df_model_raw) <- clean_names_mlr3(names(df_model_raw))

###########################################################
# B6) FEATURE LIST (NO salary history, NO future except contract flag)
###########################################################
num_cols <- names(df_model_raw)[sapply(df_model_raw, is.numeric)]

salary_like_patterns <- c("salary_", "payroll", "team_payroll", "pct_", "share",
                          "inflation", "adj")

salary_like_cols <- num_cols[
  str_detect(num_cols, paste(salary_like_patterns, collapse="|"))
]

# keep salary_cap_num + contract flag only
salary_like_cols <- setdiff(
  salary_like_cols,
  c("salary_cap_num", "new_contract_next_year")
)

exclude_helper <- c("salary_num_lead1")

feature_cols <- setdiff(num_cols, c(salary_like_cols, exclude_helper))

forced_cols <- c("W","L","win_pct","conf_rank","playoff_team",
                 "salary_cap_num","new_contract_next_year")
forced_cols <- forced_cols[forced_cols %in% names(df_model_raw)]
feature_cols <- union(feature_cols, forced_cols)

# drop zero variance
feature_cols <- feature_cols[
  sapply(df_model_raw[feature_cols], function(x) sd(x, na.rm = TRUE) > 0)
]

df_model <- df_model_raw %>%
  select(all_of(feature_cols), salary_num_lead1) %>%
  na.omit()

cat("\n✅ Nb features utilisées =", length(feature_cols), "\n")

###########################################################
# B7) EXCLUDE contract renewal years from TRAINING SET
###########################################################
df_model_trainable <- df_model %>%
  filter(new_contract_next_year == 0)

###########################################################
##############################################################
# B8) TASK (trainable only)
##############################################################
task_salary <- TaskRegr$new(
  id = "nba_salary_nominal",
  backend = df_model_trainable,
  target = "salary_num_lead1"
)

##############################################################
# B9) TRAIN/TEST HOLDOUT (75/25) like Day 1
##############################################################
set.seed(123)
splits <- partition(task_salary, ratio = 0.75)

task_train <- task_salary$clone()$filter(splits$train)
task_test  <- task_salary$clone()$filter(splits$test)

##############################################################
# B10) PIPELINE = median imputation
##############################################################
imputer <- po("imputemedian")

baseline <- as_learner(imputer %>>% lrn("regr.featureless"))
lm_m     <- as_learner(imputer %>>% lrn("regr.lm"))
rf_m     <- as_learner(imputer %>>% lrn("regr.ranger", num.trees = 300))

###########################
# --- ELASTIC NET (glmnet) ---
###########################
# alpha = 0.5 → mélange LASSO + Ridge
# On ajoute un tuning de lambda (s)
enet_ps <- ps(
  s = p_dbl(lower = 0.0001, upper = 1)
)

enet_at <- AutoTuner$new(
  learner      = lrn("regr.glmnet", alpha = 0.5),
  resampling   = rsmp("cv", folds = 5),
  measure      = msr("regr.rmse"),
  search_space = enet_ps,
  tuner        = tnr("random_search"),
  terminator   = trm("evals", n_evals = 20)
)

enet_m <- as_learner(imputer %>>% enet_at)


mes <- msrs(c("regr.rsq","regr.rmse","regr.mse"))


##############################################################
# B11) 5-fold CV on TRAIN for fair comparison
##############################################################
rdesc <- rsmp("cv", folds = 5)

design <- benchmark_grid(
  tasks       = task_train,
  learners    = list(baseline, lm_m, enet_m, rf_m),
  resamplings = rdesc
)

bm <- benchmark(design)

cat("\n--- 5-Fold CV on TRAIN ---\n")
print(bm$aggregate(mes))


##############################################################
# B12) (Day 2) FAST tuning RF + nested CV on FULL trainable task
##############################################################
rf_ps <- ps(
  mtry = p_int(lower = 1, upper = min(20, length(task_salary$feature_names))),
  min.node.size = p_int(lower = 2, upper = 30)
)

res_inner <- rsmp("cv", folds = 3)
tuner <- tnr("random_search")
terminator <- trm("evals", n_evals = 25)

rf_at <- AutoTuner$new(
  learner      = lrn("regr.ranger", num.trees = 200),
  resampling   = res_inner,
  measure      = msr("regr.rmse"),
  search_space = rf_ps,
  terminator   = terminator,
  tuner        = tuner
)

rf_at_imp <- as_learner(imputer %>>% rf_at)

res_outer <- rsmp("cv", folds = 2)

cat("\n--- Nested CV (tuned RF) ---\n")
nested_res <- resample(task_salary, rf_at_imp, res_outer)
print(nested_res$aggregate(mes))

##############################################################
# B13) FINAL tuned RF on TRAIN + HOLDOUT TEST perf
##############################################################
rf_at_imp$train(task_train)
pred_test <- rf_at_imp$predict(task_test)

cat("\n--- HOLDOUT TEST PERFORMANCE ---\n")
print(pred_test$score(mes))

R2_test <- pred_test$score(msr("regr.rsq"))
cat("\n✅ R2 OUT-OF-SAMPLE (TEST) =", R2_test, "\n")



#####################################################################
# ================== PART D: MODEL COMPARISON =======================
#####################################################################

cat("\n================ MODEL COMPARISON (LM vs ENet vs RF) ================\n")

##############################
# D1) 5-FOLD CV COMPARISON
##############################

# On réutilise le benchmark bm si déjà calculé.
# Si tu veux être sûr qu'il existe, tu peux relancer le design + benchmark ici :

# rdesc <- rsmp("cv", folds = 5)
# design <- benchmark_grid(
#   tasks       = task_train,
#   learners    = list(baseline, lm_m, enet_m, rf_m),
#   resamplings = rdesc
# )
# bm <- benchmark(design)

cv_aggr <- bm$aggregate(mes)

cat("\n--- 5-Fold CV (Train only) ---\n")
print(cv_aggr)

##############################
# D2) HOLDOUT TEST COMPARISON
##############################

# 1) LM sur train + test
lm_m$train(task_train)
pred_lm_test <- lm_m$predict(task_test)

# 2) Elastic Net (AutoTuner) sur train + test
enet_m$train(task_train)
pred_enet_test <- enet_m$predict(task_test)

# 3) RF (déjà entraîné dans B13, mais on sécurise)
rf_m$train(task_train)
pred_rf_test <- rf_m$predict(task_test)

# 4) RF TUNED (AutoTuner rf_at_imp déjà entraîné en B13)
# rf_at_imp$train(task_train) # normalement déjà fait
pred_rf_tuned_test <- rf_at_imp$predict(task_test)

# 5) Regrouper les scores dans un data frame
get_scores <- function(pred, name){
  tibble(
    model = name,
    rsq   = pred$score(msr("regr.rsq")),
    rmse  = pred$score(msr("regr.rmse")),
    mse   = pred$score(msr("regr.mse"))
  )
}

test_results <- bind_rows(
  get_scores(pred_lm_test,        "LM (OLS)"),
  get_scores(pred_enet_test,      "Elastic Net (glmnet)"),
  get_scores(pred_rf_test,        "Random Forest (RF, untuned)"),
  get_scores(pred_rf_tuned_test,  "Random Forest (RF tuned)")
)

cat("\n--- HOLDOUT TEST COMPARISON (75/25 split) ---\n")
print(test_results)

# Optionnel : sauvegarder en CSV
write_csv(test_results, "model_comparison_holdout.csv")
write_csv(as_tibble(cv_aggr), "model_comparison_cv.csv")

cat("\n✅ Model comparison tables saved: model_comparison_cv.csv & model_comparison_holdout.csv\n")










#####################################################################
# ===================== PART C: REPORT GRAPHS =======================
#####################################################################

##############################
# C0) LIBRARIES FOR REPORT
##############################
library(ggplot2)
library(scales)

# PDPs via mlr3viz (install if missing)
if (!requireNamespace("mlr3viz", quietly = TRUE)) {
  install.packages("mlr3viz")
}
library(mlr3viz)

##############################
# C1) ENSURE FINAL MODEL + PRED
# (pred_test should already exist from B13,
# but re-run safely if not)
##############################
if (!exists("pred_test")) {
  rf_at_imp$train(task_train)
  pred_test <- rf_at_imp$predict(task_test)
}

pred_df <- tibble(
  actual = pred_test$truth,
  pred   = pred_test$response
) %>%
  mutate(
    resid   = actual - pred,
    abs_err = abs(resid)
  )

##############################
# C2) GRAPH: PREDICTED VS ACTUAL (HOLDOUT)
##############################
p_pred_vs_actual <- ggplot(pred_df, aes(x = actual, y = pred)) +
  geom_point(alpha = 0.30) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  scale_x_continuous(labels = dollar) +
  scale_y_continuous(labels = dollar) +
  labs(
    title = "Predicted vs Actual Salary (Holdout Test)",
    x = "Actual salary t+1",
    y = "Predicted salary t+1"
  ) +
  theme_minimal()

print(p_pred_vs_actual)
ggsave("01_pred_vs_actual_test.png", p_pred_vs_actual, width = 7, height = 5)

##############################
# C3) GRAPH: RESIDUAL DISTRIBUTION
##############################
p_resid_hist <- ggplot(pred_df, aes(x = resid)) +
  geom_histogram(bins = 60) +
  labs(
    title = "Residual Distribution (Holdout Test)",
    x = "Residual = actual - predicted",
    y = "Count"
  ) +
  theme_minimal()

print(p_resid_hist)
ggsave("02_residual_hist_test.png", p_resid_hist, width = 7, height = 5)

##############################
# C4) GRAPH: RESIDUALS VS PREDICTED
# (check heteroskedasticity / max-salary errors)
##############################
p_resid_vs_pred <- ggplot(pred_df, aes(x = pred, y = resid)) +
  geom_point(alpha = 0.30) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(labels = dollar) +
  labs(
    title = "Residuals vs Predicted (Holdout Test)",
    x = "Predicted salary t+1",
    y = "Residual"
  ) +
  theme_minimal()

print(p_resid_vs_pred)
ggsave("03_residuals_vs_pred_test.png", p_resid_vs_pred, width = 7, height = 5)

#####################################################################
# C5) FEATURE IMPORTANCE — SIMPLE & ROBUST (DIRECT ranger)
#####################################################################

library(ranger)
library(tidyverse)
library(ggplot2)
library(scales)

# 1) Récupérer les données train depuis mlr3
train_data <- task_train$data()

# 2) Imputation médiane MANUELLE (même logique que imputemedian)
num_cols <- names(train_data)[sapply(train_data, is.numeric)]
num_feats <- setdiff(num_cols, "salary_num_lead1")

for (c in num_feats) {
  med <- median(train_data[[c]], na.rm = TRUE)
  train_data[[c]][is.na(train_data[[c]])] <- med
}

# 3) Choisir des hyperparams "safe" si archive casse
p <- length(setdiff(names(train_data), "salary_num_lead1"))
best_mtry <- min(20, floor(sqrt(p)))  # règle standard RF
best_min_node <- 5

cat("\nParams used for importance refit:",
    "\nmtry =", best_mtry,
    "\nmin.node.size =", best_min_node, "\n")

# 4) Refit ranger DIRECT avec importance permutation ON (NO formula)

set.seed(123)

rf_direct <- ranger(
  dependent.variable.name = "salary_num_lead1",
  data = train_data,
  num.trees = 200,
  mtry = best_mtry,
  min.node.size = best_min_node,
  importance = "permutation"
)

# 5) Extraire importance
imp_raw <- rf_direct$variable.importance
if (is.null(imp_raw) || length(imp_raw) == 0) {
  stop("❌ variable.importance vide—vérifie installation ranger")
}

# 6) Construire top20
imp_df <- tibble(
  feature = names(imp_raw),
  importance = as.numeric(imp_raw)
) %>%
  arrange(desc(importance))

imp_top20 <- imp_df %>% slice_head(n = 20)

write_csv(imp_top20, "top20_feature_importance.csv")

# 7) Plot top20
p_imp_top20 <- ggplot(imp_top20,
                      aes(x = reorder(feature, importance),
                          y = importance)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 20 Most Important Features (Permutation Importance)",
    x = NULL,
    y = "Permutation importance"
  ) +
  theme_minimal()

print(p_imp_top20)
ggsave("04_top20_importance.png", p_imp_top20, width = 7, height = 6)

cat("\n✅ Top20 importance exported + plot saved.\n")


#####################################################################
# C6) PARTIAL DEPENDENCE PLOTS (TOP 4 FEATURES) — using pdp + ranger
#####################################################################

# install/load pdp if needed
if (!requireNamespace("pdp", quietly = TRUE)) {
  install.packages("pdp")
}
library(pdp)

# --- Safety: make names syntactic just for pdp, keep mapping ---
name_map <- tibble(
  original = names(train_data),
  safe     = make.names(names(train_data), unique = TRUE)
)

train_data_safe <- train_data
names(train_data_safe) <- name_map$safe

# target safe name
target_safe <- name_map$safe[name_map$original == "salary_num_lead1"]

# Refit rf_direct on safe data if needed (so pdp matches safe names)
set.seed(123)
rf_direct_safe <- ranger(
  dependent.variable.name = target_safe,
  data = train_data_safe,
  num.trees = 200,
  mtry = best_mtry,
  min.node.size = best_min_node,
  importance = "permutation"
)

# Top 4 features from importance (original names)
top_feats <- imp_top20$feature[1:4]

for (f in top_feats) {
  
  # map original -> safe name
  f_safe <- name_map$safe[match(f, name_map$original)]
  
  # compute PDP
  pd <- pdp::partial(
    object = rf_direct_safe,
    pred.var = f_safe,
    train = train_data_safe,
    grid.resolution = 20
  )
  
  # pdp returns column 'yhat' + feature column
  colnames(pd)[colnames(pd) == f_safe] <- "x"
  
  p_pdp <- ggplot(pd, aes(x = x, y = yhat)) +
    geom_line(linewidth = 1) +
    labs(
      title = paste("Partial Dependence Plot:", f),
      x = f,
      y = "Predicted salary t+1"
    ) +
    theme_minimal()
  
  print(p_pdp)
  ggsave(paste0("PDP_", f, ".png"), p_pdp, width = 7, height = 5)
}

cat("\n✅ PDPs exported for top 4 features using pdp + ranger.\n")


