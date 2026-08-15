# This script processes student survey data and state-level unemployment shocks
# to build a combined analysis dataset. 
#
# Outputs processed RDS files, printed DiD and robustness regression tables, and
# heterogeneity results comparing effects across student subgroups. It also exports
# high-resolution event study and subgroup plots as PNG figures.

# Load packages
library(haven)
library(dplyr)
library(readr)
library(tidyr)
library(tidyverse)
library(estimatr)
library(broom)

# Assign directories
HERI_DIR <- "/Users/aaronjoseph/slim"
OUT_DIR  <- "/Users/aaronjoseph/slim"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# Select 2003-2012 years, excluding 2008 as the crisis year
EVENT_STUDY_YEARS <- c(2003:2007, 2009:2012)

# File paths
DEMOGRAPHICS_SAV <- file.path(HERI_DIR, "extracted/demographics/DEMOGRAPHICS.SAV")
HIGHSCHOOL_SAV   <- file.path(HERI_DIR, "extracted/highschool/HIGHSCHOOL.SAV")
PLANS_SAV        <- file.path(HERI_DIR, "extracted/plans/PLANS.SAV")
FUNDS_SAV        <- file.path(HERI_DIR, "extracted/funds/FUNDS.SAV")
BLS_LAUS_PATH    <- file.path(HERI_DIR, "state_unemployment_monthly.csv")

# 1. Import DEMOGRAPHICS
read_sav(
  DEMOGRAPHICS_SAV, 
  col_select = any_of(c("SUBJID", "YEAR", "STATE", "SEX", "RACEGROUP", "INCOME", "FIRSTGEN"))
) %>%
  zap_labels() %>%
  rename(
    id         = SUBJID, 
    year       = YEAR, 
    home_state = STATE, 
    sex        = SEX,
    race       = RACEGROUP, 
    income     = any_of("INCOME"),
    firstgen   = any_of("FIRSTGEN")
  ) %>%
  filter(year %in% EVENT_STUDY_YEARS) %>%
  saveRDS(file.path(OUT_DIR, "demographics_slim.rds"))

gc()

# 2. Import FUNDS
read_sav(
  FUNDS_SAV, 
  col_select = any_of(c("SUBJID", "YEAR", "FINCON", "SELECTIVITY", "COMPGROUP2", "COMPGROUP3", "STUDWGT"))
) %>%
  zap_labels() %>%
  rename(
    id          = SUBJID, 
    year        = YEAR, 
    fin_concern = FINCON,
    selectivity = SELECTIVITY, 
    compgroup2  = COMPGROUP2,
    compgroup3  = COMPGROUP3, 
    studwgt     = STUDWGT
  ) %>%
  filter(year %in% EVENT_STUDY_YEARS) %>%
  saveRDS(file.path(OUT_DIR, "funds_slim.rds"))

gc()

# 3. Import PLANS
read_sav(
  PLANS_SAV, 
  col_select = any_of(c(
    "SUBJID", "subjid", "CASEID", "caseid", "ID", "id", 
    "YEAR", "year", "MAJORA", "majora", "MAJOR", "major", 
    "CARFIELD", "carfield", "FUTACT12", "futact12"
  ))
) %>%
  zap_labels() %>%
  select(
    id             = matches("^subjid$|^caseid$|^id$"), 
    year           = matches("^year$"),
    intended_major = matches("^majora$|major|^carfield"),
    work_pay       = matches("^FUTACT12$")
  ) %>%
  filter(year %in% EVENT_STUDY_YEARS) %>%
  saveRDS(file.path(OUT_DIR, "plans_slim.rds"))

gc()

# 4. Import HIGHSCHOOL
read_sav(
  HIGHSCHOOL_SAV, 
  col_select = any_of(c(
    "SUBJID", "subjid", "CASEID", "caseid", "ID", "id", 
    "YEAR", "year", "HSGPA", "hsgpa", "GPA", "gpa", "HS_GPA", "hs_gpa"
  ))
) %>%
  zap_labels() %>%
  select(
    id    = matches("^subjid$|^caseid$|^id$"), 
    year  = matches("^year$"),
    hsgpa = matches("^hsgpa$|^gpa$|hs_gpa")
  ) %>%
  filter(year %in% EVENT_STUDY_YEARS) %>%
  saveRDS(file.path(OUT_DIR, "highschool_slim.rds"))

gc()

# 5. Process BLS Unemployment Rates
read_csv(BLS_LAUS_PATH, show_col_types = FALSE) %>%
  rename_with(tolower) %>%
  rename(date = matches("date")) %>%
  mutate(year = as.integer(format(as.Date(date), "%Y"))) %>%
  filter(year %in% c(2007, 2009)) %>%
  pivot_longer(
    cols      = -c(date, year), 
    names_to  = "series_id", 
    values_to = "rate"
  ) %>%
  mutate(
    rate       = as.numeric(rate), 
    home_state = toupper(sub("UR$", "", series_id, ignore.case = TRUE))
  ) %>%
  filter(!is.na(rate), nchar(home_state) == 2) %>%
  group_by(home_state, year) %>%
  summarise(annual_rate = mean(rate, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from   = year, 
    values_from  = annual_rate, 
    names_prefix = "y"
  ) %>%
  mutate(state_shock = y2009 - y2007) %>%
  select(home_state, state_shock) %>%
  filter(!is.na(state_shock)) %>%
  saveRDS(file.path(OUT_DIR, "state_shock_slim.rds"))

# ===== DATA MERGING & FEATURE ENGINEERING =====

# Load cleaned files
demo_df        <- readRDS(file.path(OUT_DIR, "demographics_slim.rds"))
funds_df       <- readRDS(file.path(OUT_DIR, "funds_slim.rds"))
plans_df       <- readRDS(file.path(OUT_DIR, "plans_slim.rds"))
hs_df          <- readRDS(file.path(OUT_DIR, "highschool_slim.rds"))
state_shock_df <- readRDS(file.path(OUT_DIR, "state_shock_slim.rds"))

BASE_YEAR <- 2007

# Helper join function
join_by_id_year <- function(df, new_df) {
  keys   <- intersect(c("id", "year"), intersect(names(df), names(new_df)))
  new_df <- new_df %>% distinct(across(all_of(keys)), .keep_all = TRUE)
  left_join(df, new_df, by = keys)
}

# Construct primary dataset
tfs <- demo_df %>%
  join_by_id_year(funds_df) %>%
  join_by_id_year(plans_df) %>%
  join_by_id_year(hs_df) %>%
  mutate(
    year = coalesce(
      if ("year.x" %in% names(.)) year.x else year,
      if ("year.y" %in% names(.)) year.y else NA_real_
    )
  ) %>%
  select(-any_of(c("year.x", "year.y"))) %>%
  left_join(state_shock_df, by = "home_state") %>%
  filter(!is.na(state_shock)) %>%
  mutate(
    post               = as.integer(year >= 2009),
    major_fin_concern  = as.integer(fin_concern == 3),
    work_to_pay        = as.integer(work_pay %in% c(1, 2)),
    gender             = factor(sex),
    race               = factor(race),
    selectivity        = as.numeric(selectivity),
    income             = if ("income" %in% names(.)) factor(income) else NA,
    firstgen           = if ("firstgen" %in% names(.)) as.integer(firstgen == 1) else NA_integer_,
    year_f             = factor(year, levels = EVENT_STUDY_YEARS)
  )

# Define standard controls
controls <- paste(
  c("hsgpa", "gender", "race", "selectivity",
    if (!all(is.na(tfs$income))) "income",
    if (!all(is.na(tfs$firstgen))) "firstgen"),
  collapse = " + "
)

# Model fitting helper
fit <- function(outcome, data = tfs, extra = "") {
  f <- as.formula(paste(outcome, "~ state_shock * post", extra))
  lm_robust(f, data = data, clusters = home_state, se_type = "stata")
}

# ===== MAIN DiD MODELS =====

model_anxiety_nc <- fit("major_fin_concern")
model_anxiety    <- fit("major_fin_concern", extra = paste("+", controls))
model_work_nc    <- fit("work_to_pay")
model_work       <- fit("work_to_pay", extra = paste("+", controls))

did_table <- bind_rows(
  tidy(model_anxiety_nc) %>% mutate(outcome = "Anxiety [No Controls]"),
  tidy(model_anxiety)    %>% mutate(outcome = "Anxiety [Full Controls]"),
  tidy(model_work_nc)    %>% mutate(outcome = "Work [No Controls]"),
  tidy(model_work)       %>% mutate(outcome = "Work [Full Controls]")
) %>%
  filter(term %in% c("state_shock", "post", "state_shock:post")) %>%
  select(outcome, term, estimate, std.error, p.value, conf.low, conf.high)

print(as.data.frame(did_table))

# ===== EVENT STUDY MODELS =====

event_formula <- function(outcome) {
  as.formula(paste(
    outcome, 
    "~ state_shock * relevel(year_f, ref = as.character(BASE_YEAR)) +", 
    controls
  ))
}

event_model_anxiety <- lm_robust(
  event_formula("major_fin_concern"), 
  data     = tfs, 
  clusters = home_state, 
  se_type  = "stata"
)

event_model_work <- lm_robust(
  event_formula("work_to_pay"), 
  data     = tfs, 
  clusters = home_state, 
  se_type  = "stata"
)

extract_event_coefs <- function(model, label) {
  tidy(model) %>%
    filter(str_detect(term, "^state_shock:relevel")) %>%
    mutate(
      year    = as.integer(str_extract(term, "\\d{4}$")), 
      outcome = label
    ) %>%
    select(outcome, year, estimate, std.error, conf.low, conf.high)
}

event_coefs <- bind_rows(
  extract_event_coefs(event_model_anxiety, "Financial Anxiety"),
  extract_event_coefs(event_model_work, "Work to Pay"),
  tibble(
    outcome   = c("Financial Anxiety", "Work to Pay"), 
    year      = BASE_YEAR,
    estimate  = 0, 
    std.error = 0, 
    conf.low  = 0, 
    conf.high = 0
  )
) %>% 
  arrange(outcome, year)

# Plotting helper
plot_event_study <- function(outcome_label, title) {
  event_coefs %>%
    filter(outcome == outcome_label) %>%
    ggplot(aes(year, estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
    geom_vline(xintercept = 2008, linetype = "dotted", colour = "grey60") +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.15) +
    geom_point(size = 3, colour = "#C0392B") +
    geom_line(colour = "#C0392B", linewidth = 0.6) +
    scale_x_continuous(breaks = EVENT_STUDY_YEARS) +
    labs(
      title = title, 
      x     = "Survey Cohort Year", 
      y     = "Coefficient on state_shock"
    ) +
    theme_minimal(base_size = 13)
}

fig1_event <- plot_event_study("Financial Anxiety", "Event Study — Financial Anxiety")
fig2_event <- plot_event_study("Work to Pay", "Event Study — Work Expectation")

ggsave(file.path(OUT_DIR, "fig1_event_study_anxiety.png"), fig1_event, width = 8, height = 5, dpi = 300)
ggsave(file.path(OUT_DIR, "fig2_event_study_work.png"),    fig2_event, width = 8, height = 5, dpi = 300)

# ===== HETEROGENEITY ANALYSIS =====

het_by <- function(group_var) {
  tfs %>%
    filter(!is.na(.data[[group_var]])) %>%
    group_by(.data[[group_var]]) %>%
    group_modify(~ {
      m_a <- lm_robust(
        major_fin_concern ~ state_shock * post, 
        data     = .x, 
        clusters = home_state, 
        se_type  = "stata"
      )
      m_w <- lm_robust(
        work_to_pay ~ state_shock * post, 
        data     = .x, 
        clusters = home_state, 
        se_type  = "stata"
      )
      
      tibble(
        n            = nrow(.x),
        anxiety_coef = coef(m_a)["state_shock:post"], 
        anxiety_se   = m_a$std.error["state_shock:post"],
        work_coef    = coef(m_w)["state_shock:post"], 
        work_se      = m_w$std.error["state_shock:post"]
      )
    }) %>% 
    ungroup()
}

het_by_income   <- if (!all(is.na(tfs$income))) het_by("income") else NULL
het_by_firstgen <- if (!all(is.na(tfs$firstgen))) het_by("firstgen") else NULL

if (!is.null(het_by_income)) {
  fig3_het_income <- het_by_income %>%
    ggplot(aes(income, anxiety_coef)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
    geom_pointrange(
      aes(
        ymin = anxiety_coef - 1.96 * anxiety_se, 
        ymax = anxiety_coef + 1.96 * anxiety_se
      ),
      colour = "#2471A3"
    ) +
    coord_flip() +
    labs(
      title = "Effect of state_shock on Financial Anxiety, by Income Bracket",
      x     = "Parental Income Bracket", 
      y     = "Coefficient on state_shock x post"
    ) +
    theme_minimal(base_size = 12)
  
  ggsave(
    file.path(OUT_DIR, "fig3_heterogeneity_income.png"), 
    fig3_het_income, 
    width = 8, 
    height = 6, 
    dpi = 300
  )
}

# ===== ROBUSTNESS CHECKS =====

# Robustness 1: Survey Weights
rob1_a <- lm_robust(
  as.formula(paste("major_fin_concern ~ state_shock * post +", controls)),
  data     = tfs, 
  clusters = home_state, 
  se_type  = "stata", 
  weights  = studwgt
)

rob1_w <- lm_robust(
  as.formula(paste("work_to_pay ~ state_shock * post +", controls)),
  data     = tfs, 
  clusters = home_state, 
  se_type  = "stata", 
  weights  = studwgt
)

# Robustness 2: Symmetric Time Window (2007 vs 2009 only)
tfs_sym <- tfs %>% filter(year %in% c(BASE_YEAR, 2009))
rob2_a  <- fit("major_fin_concern", data = tfs_sym, extra = paste("+", controls))
rob2_w  <- fit("work_to_pay",        data = tfs_sym, extra = paste("+", controls))

rob_table <- bind_rows(
  tidy(model_anxiety) %>% filter(term == "state_shock:post") %>% mutate(spec = "Baseline",        outcome = "Anxiety"),
  tidy(rob1_a)        %>% filter(term == "state_shock:post") %>% mutate(spec = "Survey Weighted", outcome = "Anxiety"),
  tidy(rob2_a)        %>% filter(term == "state_shock:post") %>% mutate(spec = "Sym. Window",    outcome = "Anxiety"),
  tidy(model_work)   %>% filter(term == "state_shock:post") %>% mutate(spec = "Baseline",        outcome = "Work"),
  tidy(rob1_w)        %>% filter(term == "state_shock:post") %>% mutate(spec = "Survey Weighted", outcome = "Work"),
  tidy(rob2_w)        %>% filter(term == "state_shock:post") %>% mutate(spec = "Sym. Window",    outcome = "Work")
) %>% 
  select(outcome, spec, estimate, std.error, p.value, conf.low, conf.high)

print(as.data.frame(rob_table))

# Sample sizes
nobs(model_anxiety)
nobs(model_work)