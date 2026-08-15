# This script runs diff-in-diff and event study regressions to evaluate
# how economic shocks impact student financial anxiety and work expectations.

# Continuous-treatment DiD: state_shock x post, HERI Freshman Survey.
# Outcomes: financial anxiety (FINCON), work expectation (FUTACT12).

library(tidyverse)
library(estimatr)
library(broom)

demo_df        <- readRDS("/Users/aaronjoseph/slim/demographics_slim.rds")
funds_df       <- readRDS("/Users/aaronjoseph/slim/funds_slim.rds")
plans_df       <- readRDS("/Users/aaronjoseph/slim/plans_slim.rds")
hs_df          <- readRDS("/Users/aaronjoseph/slim/highschool_slim.rds")
state_shock_df <- readRDS("/Users/aaronjoseph/slim/state_shock_slim.rds")

EVENT_STUDY_YEARS <- sort(unique(demo_df$year))
BASE_YEAR <- 2007

join_by_id_year <- function(df, new_df) {
  keys <- intersect(c("id", "year"), intersect(names(df), names(new_df)))
  new_df <- new_df %>% distinct(across(all_of(keys)), .keep_all = TRUE)
  left_join(df, new_df, by = keys)
}

tfs <- demo_df %>%
  join_by_id_year(funds_df) %>%
  join_by_id_year(plans_df) %>%
  join_by_id_year(hs_df) %>%
  mutate(year = coalesce(if ("year.x" %in% names(.)) year.x else year,
                         if ("year.y" %in% names(.)) year.y else NA_real_)) %>%
  select(-any_of(c("year.x", "year.y"))) %>%
  left_join(state_shock_df, by = "home_state") %>%
  filter(!is.na(state_shock)) %>%
  mutate(
    post = as.integer(year >= 2009),
    major_fin_concern = as.integer(fin_concern == 3),
    work_to_pay = as.integer(work_pay %in% c(1, 2)),
    gender = factor(sex),
    race = factor(race),
    selectivity = as.numeric(selectivity),
    income = if ("income" %in% names(.)) factor(income) else NA,
    firstgen = if ("firstgen" %in% names(.)) as.integer(firstgen == 1) else NA_integer_,
    year_f = factor(year, levels = EVENT_STUDY_YEARS)
  )

controls <- paste(
  c("hsgpa", "gender", "race", "selectivity",
    if (!all(is.na(tfs$income)))   "income",
    if (!all(is.na(tfs$firstgen))) "firstgen"),
  collapse = " + "
)

fit <- function(outcome, data = tfs, extra = "") {
  f <- as.formula(paste(outcome, "~ state_shock * post", extra))
  lm_robust(f, data = data, clusters = home_state, se_type = "stata")
}

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

# Event study: state_shock x year, 2007 omitted
event_formula <- function(outcome) {
  as.formula(paste(outcome, "~ state_shock * relevel(year_f, ref = as.character(BASE_YEAR)) +", controls))
}
event_model_anxiety <- lm_robust(event_formula("major_fin_concern"), data = tfs, clusters = home_state, se_type = "stata")
event_model_work    <- lm_robust(event_formula("work_to_pay"),       data = tfs, clusters = home_state, se_type = "stata")

extract_event_coefs <- function(model, label) {
  tidy(model) %>%
    filter(str_detect(term, "^state_shock:relevel")) %>%
    mutate(year = as.integer(str_extract(term, "\\d{4}$")), outcome = label) %>%
    select(outcome, year, estimate, std.error, conf.low, conf.high)
}

event_coefs <- bind_rows(
  extract_event_coefs(event_model_anxiety, "Financial Anxiety"),
  extract_event_coefs(event_model_work, "Work to Pay"),
  tibble(outcome = c("Financial Anxiety", "Work to Pay"), year = BASE_YEAR,
         estimate = 0, std.error = 0, conf.low = 0, conf.high = 0)
) %>% arrange(outcome, year)

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
    labs(title = title, x = "Survey Cohort Year", y = "Coefficient on state_shock") +
    theme_minimal(base_size = 13)
}

fig1_event <- plot_event_study("Financial Anxiety", "Event Study — Financial Anxiety")
fig2_event <- plot_event_study("Work to Pay", "Event Study — Work Expectation")
ggsave("/Users/aaronjoseph/slim/fig1_event_study_anxiety.png", fig1_event, width = 8, height = 5, dpi = 300)
ggsave("/Users/aaronjoseph/slim/fig2_event_study_work.png", fig2_event, width = 8, height = 5, dpi = 300)

# Heterogeneity by income / first-gen
het_by <- function(group_var) {
  tfs %>%
    filter(!is.na(.data[[group_var]])) %>%
    group_by(.data[[group_var]]) %>%
    group_modify(~ {
      m_a <- lm_robust(major_fin_concern ~ state_shock * post, data = .x, clusters = home_state, se_type = "stata")
      m_w <- lm_robust(work_to_pay ~ state_shock * post, data = .x, clusters = home_state, se_type = "stata")
      tibble(n = nrow(.x),
             anxiety_coef = coef(m_a)["state_shock:post"], anxiety_se = m_a$std.error["state_shock:post"],
             work_coef = coef(m_w)["state_shock:post"], work_se = m_w$std.error["state_shock:post"])
    }) %>% ungroup()
}

het_by_income   <- if (!all(is.na(tfs$income)))   het_by("income")   else NULL
het_by_firstgen <- if (!all(is.na(tfs$firstgen))) het_by("firstgen") else NULL

if (!is.null(het_by_income)) {
  fig3_het_income <- het_by_income %>%
    ggplot(aes(income, anxiety_coef)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
    geom_pointrange(aes(ymin = anxiety_coef - 1.96 * anxiety_se, ymax = anxiety_coef + 1.96 * anxiety_se),
                    colour = "#2471A3") +
    coord_flip() +
    labs(title = "Effect of state_shock on Financial Anxiety, by Income Bracket",
         x = "Parental Income Bracket", y = "Coefficient on state_shock x post") +
    theme_minimal(base_size = 12)
  ggsave("/Users/aaronjoseph/slim/fig3_heterogeneity_income.png", fig3_het_income, width = 8, height = 6, dpi = 300)
}

# Robustness: survey-weighted, symmetric window
rob1_a <- lm_robust(as.formula(paste("major_fin_concern ~ state_shock * post +", controls)),
                    data = tfs, clusters = home_state, se_type = "stata", weights = studwgt)
rob1_w <- lm_robust(as.formula(paste("work_to_pay ~ state_shock * post +", controls)),
                    data = tfs, clusters = home_state, se_type = "stata", weights = studwgt)

tfs_sym <- tfs %>% filter(year %in% c(BASE_YEAR, 2009))
rob2_a <- fit("major_fin_concern", data = tfs_sym, extra = paste("+", controls))
rob2_w <- fit("work_to_pay", data = tfs_sym, extra = paste("+", controls))

rob_table <- bind_rows(
  tidy(model_anxiety) %>% filter(term == "state_shock:post") %>% mutate(spec = "Baseline", outcome = "Anxiety"),
  tidy(rob1_a) %>% filter(term == "state_shock:post") %>% mutate(spec = "Survey Weighted", outcome = "Anxiety"),
  tidy(rob2_a) %>% filter(term == "state_shock:post") %>% mutate(spec = "Sym. Window", outcome = "Anxiety"),
  tidy(model_work) %>% filter(term == "state_shock:post") %>% mutate(spec = "Baseline", outcome = "Work"),
  tidy(rob1_w) %>% filter(term == "state_shock:post") %>% mutate(spec = "Survey Weighted", outcome = "Work"),
  tidy(rob2_w) %>% filter(term == "state_shock:post") %>% mutate(spec = "Sym. Window", outcome = "Work")
) %>% select(outcome, spec, estimate, std.error, p.value, conf.low, conf.high)

print(as.data.frame(rob_table))

nobs(model_anxiety)
nobs(model_work)
