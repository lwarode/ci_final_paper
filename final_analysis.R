library(tidyverse)
library(legislatoR)
library(rdd)
library(rddtools)

data_all <- readRDS("data_processed/data_all.Rds")
source("theme_academia.R")
theme_set(theme_academia())

# -------------------------------------------------------------------------
# Descriptive Data --------------------------------------------------------
# -------------------------------------------------------------------------

get_traffic("deu") %>% 
  filter(date == min(date, na.rm = T) | date == max(date, na.rm = T)) %>% 
  distinct(date)

data_all %>% 
  group_by(election_won) %>% 
  dplyr::summarise(
    n = n()
  ) %>% 
  ungroup %>% 
  mutate(perc = n/sum(n))

# -------------------------------------------------------------------------
# Analysis ----------------------------------------------------------------
# -------------------------------------------------------------------------


# Linear Models -----------------------------------------------------------
# non-transformed
lm(trend_mean_six_months_before_after_traffic ~ mov, data = data_all) %>% summary
lm(trend_sum_six_months_before_after_traffic ~ mov, data = data_all) %>% summary
lm(trend_mean_six_months_before_after_traffic ~ mov + election_won, data = data_all) %>% summary
lm(trend_sum_six_months_before_after_traffic ~ mov + election_won, data = data_all) %>% summary

lm(trend_mean_six_months_before_after_traffic ~ mov, data = data_all %>% filter(mov >= -0.1, mov <= 0.1)) %>% summary
lm(trend_sum_six_months_before_after_traffic ~ mov, data = data_all %>% filter(mov >= -0.1, mov <= 0.1)) %>% summary



# ish
lm(ish_trend_mean_six_months_before_after_traffic ~ mov, data = data_all) %>% summary
lm(ish_trend_sum_six_months_before_after_traffic ~ mov, data = data_all) %>% summary
lm(ish_trend_mean_six_months_before_after_traffic ~ mov + election_won, data = data_all) %>% summary
lm(ish_trend_sum_six_months_before_after_traffic ~ mov + election_won, data = data_all) %>% summary

lm(ish_trend_mean_six_months_before_after_traffic ~ mov, data = data_all %>% filter(mov >= -0.1, mov <= 0.1)) %>% summary
lm(ish_trend_sum_six_months_before_after_traffic ~ mov, data = data_all %>% filter(mov >= -0.1, mov <= 0.1)) %>% summary



# rdrobust ----------------------------------------------------------------
library(rdrobust)
# non-transformed
# trend mean
rdrobust::rdrobust(
  data_all$mov,
  data_all$trend_mean_one_month_before_after_traffic
) %>% 
  summary

rdrobust::rdrobust(
  data_all$mov,
  data_all$trend_mean_three_months_before_after_traffic
) %>% 
  summary

rdrobust::rdrobust(
  data_all$mov,
  data_all$trend_mean_six_months_before_after_traffic
) %>% 
  summary

rdrobust::rdrobust(
  data_all$mov,
  data_all$trend_mean_twelve_months_before_after_traffic
) %>% 
  summary

# trend sum
rdrobust::rdrobust(
  data_all$mov,
  data_all$trend_sum_one_month_before_after_traffic
) %>% 
  summary

rdrobust::rdrobust(
  data_all$mov,
  data_all$trend_sum_three_months_before_after_traffic
) %>% 
  summary

rdrobust::rdrobust(
  data_all$mov,
  data_all$trend_sum_six_months_before_after_traffic
) %>% 
  summary

rdrobust::rdrobust(
  data_all$mov,
  data_all$trend_sum_twelve_months_before_after_traffic
) %>% 
  summary

# ish
# trend mean
rdrobust::rdrobust(
  data_all$mov,
  data_all$ish_trend_mean_one_month_before_after_traffic
) %>% 
  summary

rdrobust::rdrobust(
  data_all$mov,
  data_all$ish_trend_mean_three_months_before_after_traffic
) %>% 
  summary

rdrobust::rdrobust(
  data_all$mov,
  data_all$ish_trend_mean_six_months_before_after_traffic
) %>% 
  summary

rdrobust::rdrobust(
  data_all$mov,
  data_all$ish_trend_mean_twelve_months_before_after_traffic
) %>% 
  summary

# trend sum
rdrobust::rdrobust(
  data_all$mov,
  data_all$ish_trend_sum_one_month_before_after_traffic
) %>% 
  summary

rdrobust::rdrobust(
  data_all$mov,
  data_all$ish_trend_sum_three_months_before_after_traffic
) %>% 
  summary

rdrobust::rdrobust(
  data_all$mov,
  data_all$ish_trend_sum_six_months_before_after_traffic
) %>% 
  summary

rdrobust::rdrobust(
  data_all$mov,
  data_all$ish_trend_sum_twelve_months_before_after_traffic
) %>% 
  summary


# rdd ---------------------------------------------------------------------
rdd::IKbandwidth(
  data_all$mov,
  data_all$trend_mean_one_month_before_after_traffic
)

rdd::RDestimate(
  trend_mean_one_month_before_after_traffic ~ mov, data = data_all,
  cutpoint = 0,
  # bw = 0.102156
  # bw = 0.0851
) %>% 
  summary 


# rddtools ----------------------------------------------------------------
rddtools::rdd_bw_ik(
  rdd_data(data_all$trend_mean_one_month_before_after_traffic, data_all$mov, cutpoint = 0)
)

# separate slope ----------------------------------------------------------
# trend mean
rdd_data(
  data_all$trend_mean_one_month_before_after_traffic, 
  data_all$mov, 
  cutpoint = 0
) %>% 
  rdd_reg_lm(
    slope = "separate",
    bw = rdd_bw_ik(
      rdd_data(
        data_all$trend_mean_one_month_before_after_traffic,
        data_all$mov,
        cutpoint = 0
      )
    )
  ) %>% 
  summary

rdd_data(
  data_all$trend_mean_three_months_before_after_traffic, 
  data_all$mov, 
  cutpoint = 0
) %>% 
  rdd_reg_lm(
    slope = "separate",
    bw = rdd_bw_ik(
      rdd_data(
        data_all$trend_mean_three_months_before_after_traffic,
        data_all$mov,
        cutpoint = 0
      )
    )
  ) %>% 
  summary

rdd_data(
  data_all$trend_mean_six_months_before_after_traffic, 
  data_all$mov, 
  cutpoint = 0
) %>% 
  rdd_reg_lm(
    slope = "separate",
    bw = rdd_bw_ik(
      rdd_data(
        data_all$trend_mean_six_months_before_after_traffic,
        data_all$mov,
        cutpoint = 0
      )
    )
  ) %>% 
  summary

rdd_data(
  data_all$trend_mean_twelve_months_before_after_traffic, 
  data_all$mov, 
  cutpoint = 0
) %>% 
  rdd_reg_lm(
    slope = "separate",
    bw = rdd_bw_ik(
      rdd_data(
        data_all$trend_mean_twelve_months_before_after_traffic,
        data_all$mov,
        cutpoint = 0
      )
    )
  ) %>% 
  summary

# ish trend mean
rdd_data(
  data_all$ish_trend_mean_one_month_before_after_traffic, 
  data_all$mov, 
  cutpoint = 0
) %>% 
  rdd_reg_lm(
    slope = "separate",
    bw = rdd_bw_ik(
      rdd_data(
        data_all$ish_trend_mean_one_month_before_after_traffic,
        data_all$mov,
        cutpoint = 0
      )
    )
  ) %>% 
  summary

rdd_data(
  data_all$ish_trend_mean_three_months_before_after_traffic, 
  data_all$mov, 
  cutpoint = 0
) %>% 
  rdd_reg_lm(
    slope = "separate",
    bw = rdd_bw_ik(
      rdd_data(
        data_all$ish_trend_mean_three_months_before_after_traffic,
        data_all$mov,
        cutpoint = 0
      )
    )
  ) %>% 
  summary

rdd_data(
  data_all$ish_trend_mean_six_months_before_after_traffic, 
  data_all$mov, 
  cutpoint = 0
) %>% 
  rdd_reg_lm(
    slope = "separate",
    bw = rdd_bw_ik(
      rdd_data(
        data_all$ish_trend_mean_six_months_before_after_traffic,
        data_all$mov,
        cutpoint = 0
      )
    )
  ) %>% 
  summary

rdd_data(
  data_all$ish_trend_mean_twelve_months_before_after_traffic, 
  data_all$mov, 
  cutpoint = 0
) %>% 
  rdd_reg_lm(
    slope = "separate",
    bw = rdd_bw_ik(
      rdd_data(
        data_all$ish_trend_mean_twelve_months_before_after_traffic,
        data_all$mov,
        cutpoint = 0
      )
    )
  ) %>% 
  summary




# trend mean
rdd_data(
  data_all$trend_sum_one_month_before_after_traffic, 
  data_all$mov, 
  cutpoint = 0
) %>% 
  rdd_reg_lm(
    slope = "separate",
    bw = rdd_bw_ik(
      rdd_data(
        data_all$trend_sum_one_month_before_after_traffic,
        data_all$mov,
        cutpoint = 0
      )
    )
  ) %>% 
  summary

rdd_data(
  data_all$trend_sum_three_months_before_after_traffic, 
  data_all$mov, 
  cutpoint = 0
) %>% 
  rdd_reg_lm(
    slope = "separate",
    bw = rdd_bw_ik(
      rdd_data(
        data_all$trend_sum_three_months_before_after_traffic,
        data_all$mov,
        cutpoint = 0
      )
    )
  ) %>% 
  summary

rdd_data(
  data_all$trend_sum_six_months_before_after_traffic, 
  data_all$mov, 
  cutpoint = 0
) %>% 
  rdd_reg_lm(
    slope = "separate",
    bw = rdd_bw_ik(
      rdd_data(
        data_all$trend_sum_six_months_before_after_traffic,
        data_all$mov,
        cutpoint = 0
      )
    )
  ) %>% 
  summary

rdd_data(
  data_all$trend_sum_twelve_months_before_after_traffic, 
  data_all$mov, 
  cutpoint = 0
) %>% 
  rdd_reg_lm(
    slope = "separate",
    bw = rdd_bw_ik(
      rdd_data(
        data_all$trend_sum_twelve_months_before_after_traffic,
        data_all$mov,
        cutpoint = 0
      )
    )
  ) %>% 
  summary

# ish trend mean
rdd_data(
  data_all$ish_trend_sum_one_month_before_after_traffic, 
  data_all$mov, 
  cutpoint = 0
) %>% 
  rdd_reg_lm(
    slope = "separate",
    bw = rdd_bw_ik(
      rdd_data(
        data_all$ish_trend_sum_one_month_before_after_traffic,
        data_all$mov,
        cutpoint = 0
      )
    )
  ) %>% 
  summary

rdd_data(
  data_all$ish_trend_sum_three_months_before_after_traffic, 
  data_all$mov, 
  cutpoint = 0
) %>% 
  rdd_reg_lm(
    slope = "separate",
    bw = rdd_bw_ik(
      rdd_data(
        data_all$ish_trend_sum_three_months_before_after_traffic,
        data_all$mov,
        cutpoint = 0
      )
    )
  ) %>% 
  summary

rdd_data(
  data_all$ish_trend_sum_six_months_before_after_traffic, 
  data_all$mov, 
  cutpoint = 0
) %>% 
  rdd_reg_lm(
    slope = "separate",
    bw = rdd_bw_ik(
      rdd_data(
        data_all$ish_trend_sum_six_months_before_after_traffic,
        data_all$mov,
        cutpoint = 0
      )
    )
  ) %>% 
  summary

rdd_data(
  data_all$ish_trend_sum_twelve_months_before_after_traffic, 
  data_all$mov, 
  cutpoint = 0
) %>% 
  rdd_reg_lm(
    slope = "separate",
    bw = rdd_bw_ik(
      rdd_data(
        data_all$ish_trend_sum_twelve_months_before_after_traffic,
        data_all$mov,
        cutpoint = 0
      )
    )
  ) %>% 
  summary

# same slope --------------------------------------------------------------
# trend mean
rdd_data(
  data_all$trend_mean_one_month_before_after_traffic, 
  data_all$mov, 
  cutpoint = 0
) %>% 
  rdd_reg_lm(
    slope = "same",
    bw = rdd_bw_ik(
      rdd_data(
        data_all$trend_mean_one_month_before_after_traffic,
        data_all$mov,
        cutpoint = 0
      )
    )
  ) %>% 
  summary

rdd_data(
  data_all$trend_mean_three_months_before_after_traffic, 
  data_all$mov, 
  cutpoint = 0
) %>% 
  rdd_reg_lm(
    slope = "same",
    bw = rdd_bw_ik(
      rdd_data(
        data_all$trend_mean_three_months_before_after_traffic,
        data_all$mov,
        cutpoint = 0
      )
    )
  ) %>% 
  summary

rdd_data(
  data_all$trend_mean_six_months_before_after_traffic, 
  data_all$mov, 
  cutpoint = 0
) %>% 
  rdd_reg_lm(
    slope = "same",
    bw = rdd_bw_ik(
      rdd_data(
        data_all$trend_mean_six_months_before_after_traffic,
        data_all$mov,
        cutpoint = 0
      )
    )
  ) %>% 
  summary

rdd_data(
  data_all$trend_mean_twelve_months_before_after_traffic, 
  data_all$mov, 
  cutpoint = 0
) %>% 
  rdd_reg_lm(
    slope = "same",
    bw = rdd_bw_ik(
      rdd_data(
        data_all$trend_mean_twelve_months_before_after_traffic,
        data_all$mov,
        cutpoint = 0
      )
    )
  ) %>% 
  summary

# ish trend mean
rdd_data(
  data_all$ish_trend_mean_one_month_before_after_traffic, 
  data_all$mov, 
  cutpoint = 0
) %>% 
  rdd_reg_lm(
    slope = "same",
    bw = rdd_bw_ik(
      rdd_data(
        data_all$ish_trend_mean_one_month_before_after_traffic,
        data_all$mov,
        cutpoint = 0
      )
    )
  ) %>% 
  summary

rdd_data(
  data_all$ish_trend_mean_three_months_before_after_traffic, 
  data_all$mov, 
  cutpoint = 0
) %>% 
  rdd_reg_lm(
    slope = "same",
    bw = rdd_bw_ik(
      rdd_data(
        data_all$ish_trend_mean_three_months_before_after_traffic,
        data_all$mov,
        cutpoint = 0
      )
    )
  ) %>% 
  summary

rdd_data(
  data_all$ish_trend_mean_six_months_before_after_traffic, 
  data_all$mov, 
  cutpoint = 0
) %>% 
  rdd_reg_lm(
    slope = "same",
    bw = rdd_bw_ik(
      rdd_data(
        data_all$ish_trend_mean_six_months_before_after_traffic,
        data_all$mov,
        cutpoint = 0
      )
    )
  ) %>% 
  summary

rdd_data(
  data_all$ish_trend_mean_twelve_months_before_after_traffic, 
  data_all$mov, 
  cutpoint = 0
) %>% 
  rdd_reg_lm(
    slope = "same",
    bw = rdd_bw_ik(
      rdd_data(
        data_all$ish_trend_mean_twelve_months_before_after_traffic,
        data_all$mov,
        cutpoint = 0
      )
    )
  ) %>% 
  summary




# trend mean
rdd_data(
  data_all$trend_sum_one_month_before_after_traffic, 
  data_all$mov, 
  cutpoint = 0
) %>% 
  rdd_reg_lm(
    slope = "same",
    bw = rdd_bw_ik(
      rdd_data(
        data_all$trend_sum_one_month_before_after_traffic,
        data_all$mov,
        cutpoint = 0
      )
    )
  ) %>% 
  summary

rdd_data(
  data_all$trend_sum_three_months_before_after_traffic, 
  data_all$mov, 
  cutpoint = 0
) %>% 
  rdd_reg_lm(
    slope = "same",
    bw = rdd_bw_ik(
      rdd_data(
        data_all$trend_sum_three_months_before_after_traffic,
        data_all$mov,
        cutpoint = 0
      )
    )
  ) %>% 
  summary

rdd_data(
  data_all$trend_sum_six_months_before_after_traffic, 
  data_all$mov, 
  cutpoint = 0
) %>% 
  rdd_reg_lm(
    slope = "same",
    bw = rdd_bw_ik(
      rdd_data(
        data_all$trend_sum_six_months_before_after_traffic,
        data_all$mov,
        cutpoint = 0
      )
    )
  ) %>% 
  summary

rdd_data(
  data_all$trend_sum_twelve_months_before_after_traffic, 
  data_all$mov, 
  cutpoint = 0
) %>% 
  rdd_reg_lm(
    slope = "same",
    bw = rdd_bw_ik(
      rdd_data(
        data_all$trend_sum_twelve_months_before_after_traffic,
        data_all$mov,
        cutpoint = 0
      )
    )
  ) %>% 
  summary

# ish trend mean
rdd_data(
  data_all$ish_trend_sum_one_month_before_after_traffic, 
  data_all$mov, 
  cutpoint = 0
) %>% 
  rdd_reg_lm(
    slope = "same",
    bw = rdd_bw_ik(
      rdd_data(
        data_all$ish_trend_sum_one_month_before_after_traffic,
        data_all$mov,
        cutpoint = 0
      )
    )
  ) %>% 
  summary

rdd_data(
  data_all$ish_trend_sum_three_months_before_after_traffic, 
  data_all$mov, 
  cutpoint = 0
) %>% 
  rdd_reg_lm(
    slope = "same",
    bw = rdd_bw_ik(
      rdd_data(
        data_all$ish_trend_sum_three_months_before_after_traffic,
        data_all$mov,
        cutpoint = 0
      )
    )
  ) %>% 
  summary

rdd_data(
  data_all$ish_trend_sum_six_months_before_after_traffic, 
  data_all$mov, 
  cutpoint = 0
) %>% 
  rdd_reg_lm(
    slope = "same",
    bw = rdd_bw_ik(
      rdd_data(
        data_all$ish_trend_sum_six_months_before_after_traffic,
        data_all$mov,
        cutpoint = 0
      )
    )
  ) %>% 
  summary

rdd_data(
  data_all$ish_trend_sum_twelve_months_before_after_traffic, 
  data_all$mov, 
  cutpoint = 0
) %>% 
  rdd_reg_lm(
    slope = "same",
    bw = rdd_bw_ik(
      rdd_data(
        data_all$ish_trend_sum_twelve_months_before_after_traffic,
        data_all$mov,
        cutpoint = 0
      )
    )
  ) %>% 
  summary



# -------------------------------------------------------------------------
# PLOTS -------------------------------------------------------------------
# -------------------------------------------------------------------------

# no transformation -------------------------------------------------------
# trend mean
labels_trend_trend_mean <- c(
  `trend_mean_one_month_before_after_traffic` = "Trend Mean 1 Month",
  `trend_mean_three_months_before_after_traffic` = "Trend Mean 3 Months",
  `trend_mean_six_months_before_after_traffic` = "Trend Mean 6 Months",
  `trend_mean_twelve_months_before_after_traffic` = "Trend Mean 12 Months"
)

data_all %>% 
  select(mov, election_won, starts_with("trend_mean")) %>% 
  pivot_longer(cols = -c(mov, election_won), names_to = "var_name", values_to = "trend_mean") %>% 
  mutate(
    var_name = factor(
      var_name,
      levels = c(
        "trend_mean_one_month_before_after_traffic",
        "trend_mean_three_months_before_after_traffic",
        "trend_mean_six_months_before_after_traffic",
        "trend_mean_twelve_months_before_after_traffic"
      )
    )
  ) %>% 
  ggplot(aes(x = mov, y = trend_mean)) + 
  facet_wrap(~ var_name, labeller = as_labeller(labels_trend_trend_mean)) +
  geom_point(alpha = 0.5) +
  geom_smooth(aes(group = election_won), method = "lm") +
  labs(x = "Margin of Victory", y = "Trend Mean") +
  geom_vline(xintercept = 0, linetype = 2, alpha = 0.75)

# trend sum
labels_trend_trend_sum <- c(
  `trend_sum_one_month_before_after_traffic` = "Trend Sum 1 Month",
  `trend_sum_three_months_before_after_traffic` = "Trend Sum 3 Months",
  `trend_sum_six_months_before_after_traffic` = "Trend Sum 6 Months",
  `trend_sum_twelve_months_before_after_traffic` = "Trend Sum 12 Months"
)

data_all %>% 
  select(mov, election_won, starts_with("trend_sum")) %>% 
  pivot_longer(cols = -c(mov, election_won), names_to = "var_name", values_to = "trend_sum") %>% 
  mutate(
    var_name = factor(
      var_name,
      levels = c(
        "trend_sum_one_month_before_after_traffic",
        "trend_sum_three_months_before_after_traffic",
        "trend_sum_six_months_before_after_traffic",
        "trend_sum_twelve_months_before_after_traffic"
      )
    )
  ) %>% 
  ggplot(aes(x = mov, y = trend_sum)) + 
  facet_wrap(~ var_name, labeller = as_labeller(labels_trend_trend_sum)) +
  geom_point(alpha = 0.5) +
  geom_smooth(aes(group = election_won), method = "lm") +
  labs(x = "Margin of Victory", y = "Trend Sum") + 
  geom_vline(xintercept = 0, linetype = 2, alpha = 0.75)

# ish ---------------------------------------------------------------------
# ish trend mean
labels_trend_ish_trend_mean <- c(
  `ish_trend_mean_one_month_before_after_traffic` = "(ISH) Trend Mean 1 Month",
  `ish_trend_mean_three_months_before_after_traffic` = "(ISH) Trend Mean 3 Months",
  `ish_trend_mean_six_months_before_after_traffic` = "(ISH) Trend Mean 6 Months",
  `ish_trend_mean_twelve_months_before_after_traffic` = "(ISH) Trend Mean 12 Months"
)

data_all %>% 
  select(mov, election_won, starts_with("ish_trend_mean")) %>% 
  pivot_longer(cols = -c(mov, election_won), names_to = "var_name", values_to = "ish_trend_mean") %>% 
  mutate(
    var_name = factor(
      var_name,
      levels = c(
        "ish_trend_mean_one_month_before_after_traffic",
        "ish_trend_mean_three_months_before_after_traffic",
        "ish_trend_mean_six_months_before_after_traffic",
        "ish_trend_mean_twelve_months_before_after_traffic"
      )
    )
  ) %>% 
  ggplot(aes(x = mov, y = ish_trend_mean)) + 
  facet_wrap(~ var_name, labeller = as_labeller(labels_trend_ish_trend_mean)) +
  geom_point(alpha = 0.5) +
  geom_smooth(aes(group = election_won), method = "lm") +
  labs(x = "Margin of Victory", y = "(ISH) Trend Mean") +
  geom_vline(xintercept = 0, linetype = 2, alpha = 0.75)

# ish trend sum
labels_trend_ish_trend_sum <- c(
  `ish_trend_sum_one_month_before_after_traffic` = "(ISH) Trend Sum 1 Month",
  `ish_trend_sum_three_months_before_after_traffic` = "(ISH) Trend Sum 3 Months",
  `ish_trend_sum_six_months_before_after_traffic` = "(ISH) Trend Sum 6 Months",
  `ish_trend_sum_twelve_months_before_after_traffic` = "(ISH) Trend Sum 12 Months"
)

data_all %>% 
  select(mov, election_won, starts_with("ish_trend_sum")) %>% 
  pivot_longer(cols = -c(mov, election_won), names_to = "var_name", values_to = "ish_trend_sum") %>% 
  mutate(
    var_name = factor(
      var_name,
      levels = c(
        "ish_trend_sum_one_month_before_after_traffic",
        "ish_trend_sum_three_months_before_after_traffic",
        "ish_trend_sum_six_months_before_after_traffic",
        "ish_trend_sum_twelve_months_before_after_traffic"
      )
    )
  ) %>% 
  ggplot(aes(x = mov, y = ish_trend_sum)) + 
  facet_wrap(~ var_name, labeller = as_labeller(labels_trend_ish_trend_sum)) +
  geom_point(alpha = 0.5) +
  geom_smooth(aes(group = election_won), method = "lm") +
  labs(x = "Margin of Victory", y = "(ISH) Trend Sum") + 
  geom_vline(xintercept = 0, linetype = 2, alpha = 0.75)


# log ---------------------------------------------------------------------
# log trend mean
labels_trend_log_trend_mean <- c(
  `log_trend_mean_one_month_before_after_traffic` = "(Log) Trend Mean 1 Month",
  `log_trend_mean_three_months_before_after_traffic` = "(Log) Trend Mean 3 Months",
  `log_trend_mean_six_months_before_after_traffic` = "(Log) Trend Mean 6 Months",
  `log_trend_mean_twelve_months_before_after_traffic` = "(Log) Trend Mean 12 Months"
)

data_all %>% 
  select(mov, election_won, starts_with("log_trend_mean")) %>% 
  pivot_longer(cols = -c(mov, election_won), names_to = "var_name", values_to = "log_trend_mean") %>% 
  mutate(
    var_name = factor(
      var_name,
      levels = c(
        "log_trend_mean_one_month_before_after_traffic",
        "log_trend_mean_three_months_before_after_traffic",
        "log_trend_mean_six_months_before_after_traffic",
        "log_trend_mean_twelve_months_before_after_traffic"
      )
    )
  ) %>% 
  ggplot(aes(x = mov, y = log_trend_mean)) + 
  facet_wrap(~ var_name, labeller = as_labeller(labels_trend_log_trend_mean)) +
  geom_point(alpha = 0.5) +
  geom_smooth(aes(group = election_won), method = "lm") +
  labs(x = "Margin of Victory", y = "(Log) Trend Mean") +
  geom_vline(xintercept = 0, linetype = 2, alpha = 0.75)

# log trend sum
labels_trend_log_trend_sum <- c(
  `log_trend_sum_one_month_before_after_traffic` = "(Log) Trend Sum 1 Month",
  `log_trend_sum_three_months_before_after_traffic` = "(Log) Trend Sum 3 Months",
  `log_trend_sum_six_months_before_after_traffic` = "(Log) Trend Sum 6 Months",
  `log_trend_sum_twelve_months_before_after_traffic` = "(Log) Trend Sum 12 Months"
)

data_all %>% 
  select(mov, election_won, starts_with("log_trend_sum")) %>% 
  pivot_longer(cols = -c(mov, election_won), names_to = "var_name", values_to = "log_trend_sum") %>% 
  mutate(
    var_name = factor(
      var_name,
      levels = c(
        "log_trend_sum_one_month_before_after_traffic",
        "log_trend_sum_three_months_before_after_traffic",
        "log_trend_sum_six_months_before_after_traffic",
        "log_trend_sum_twelve_months_before_after_traffic"
      )
    )
  ) %>% 
  ggplot(aes(x = mov, y = log_trend_sum)) + 
  facet_wrap(~ var_name, labeller = as_labeller(labels_trend_log_trend_sum)) +
  geom_point(alpha = 0.5) +
  geom_smooth(aes(group = election_won), method = "lm") +
  labs(x = "Margin of Victory", y = "(Log) Trend Sum") + 
  geom_vline(xintercept = 0, linetype = 2, alpha = 0.75)


# reducing observations RDD window ----------------------------------------

data_all %>% 
  select(mov, election_won, starts_with("ish_trend_mean")) %>% 
  filter(mov <= 0.05, mov >= -0.05) %>% 
  pivot_longer(cols = -c(mov, election_won), names_to = "var_name", values_to = "ish_trend_mean") %>% 
  mutate(
    var_name = factor(
      var_name,
      levels = c(
        "ish_trend_mean_one_month_before_after_traffic",
        "ish_trend_mean_three_months_before_after_traffic",
        "ish_trend_mean_six_months_before_after_traffic",
        "ish_trend_mean_twelve_months_before_after_traffic"
      )
    )
  ) %>% 
  ggplot(aes(x = mov, y = ish_trend_mean)) + 
  facet_wrap(~ var_name, labeller = as_labeller(labels_trend_ish_trend_mean)) +
  geom_point(alpha = 0.5) +
  geom_smooth(aes(group = election_won), method = "lm") +
  labs(x = "Margin of Victory", y = "(ISH) Trend Mean") +
  geom_vline(xintercept = 0, linetype = 2, alpha = 0.75) #+
  # annotate("segment", x = 0, xend = 0, y = 1, yend = 2, color = "red")
