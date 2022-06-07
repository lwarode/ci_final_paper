library(tidyverse)
library(legislatoR)

deu_traffic <- get_traffic("deu")

# date of first wikipedia traffic observation
wiki_page_start_date <- deu_traffic %>% 
  group_by(pageid) %>% 
  filter(traffic != 0) %>% 
  dplyr::summarise(wiki_start_date = min(date)) %>% 
  ungroup

if (!file.exists("data_processed/wiki_page_start_date.Rds")) {
  if (!dir.exists("data_processed")) {dir.create("data_processed")}
  else (write_rds(wiki_page_start_date, "data_processed/wiki_page_start_date.Rds"))
} else (wiki_page_start_date <- readRDS("data_processed/wiki_page_start_date.Rds"))

wiki_traffic_data <- deu_traffic %>% 
  mutate(ym_date = format(date, "%Y-%m")) %>% 
  group_by(pageid, ym_date) %>% 
  dplyr::summarise(mean_traffic = mean(traffic, na.rm = T),
            sum_traffic = sum(traffic, na.rm = T)
  ) %>% 
  mutate(
    ym_date_proxy = paste0(ym_date, "-01") %>% as.Date,
    next_month = format(ym_date_proxy + months(1), "%Y-%m"),
    # 1 month
    one_month_before = format(ym_date_proxy - months(1), "%Y-%m"),
    one_month_after = format(ym_date_proxy + months(1), "%Y-%m"),
    # 3 months
    three_months_before = format(ym_date_proxy - months(3), "%Y-%m"),
    three_months_after = format(ym_date_proxy + months(3), "%Y-%m"),
    # 6 months
    six_months_before = format(ym_date_proxy - months(6), "%Y-%m"),
    six_months_after = format(ym_date_proxy + months(6), "%Y-%m"),
    # 12 months
    twelve_months_before = format(ym_date_proxy - months(12), "%Y-%m"),
    twelve_months_after = format(ym_date_proxy + months(12), "%Y-%m")
  ) %>% 
  arrange(pageid, ym_date) %>% 
  # sum
  mutate(
    sum_one_month_before_traffic = if_else(
      lag(ym_date, 1) == one_month_before,
      lag(sum_traffic, 1),
      NA_real_
    ),
    sum_one_month_after_traffic = if_else(
      lead(ym_date, 1) == one_month_after,
      lead(sum_traffic, 1),
      NA_real_
    )
  ) %>% 
  mutate(
    sum_three_months_before_traffic = if_else(
      lag(ym_date, 3) == three_months_before,
      lag(sum_traffic, 3),
      NA_real_
    ),
    sum_three_months_after_traffic = if_else(
      lead(ym_date, 3) == three_months_after,
      lead(sum_traffic, 3),
      NA_real_
    )
  ) %>% 
  mutate(
    sum_six_months_before_traffic = if_else(
      lag(ym_date, 6) == six_months_before,
      lag(sum_traffic, 6),
      NA_real_
    ),
    sum_six_months_after_traffic = if_else(
      lead(ym_date, 6) == six_months_after,
      lead(sum_traffic, 6),
      NA_real_
    )
  ) %>% 
  mutate(
    sum_twelve_months_before_traffic = if_else(
      lag(ym_date, 12) == twelve_months_before,
      lag(sum_traffic, 12),
      NA_real_
    ),
    sum_twelve_months_after_traffic = if_else(
      lead(ym_date, 12) == twelve_months_after,
      lead(sum_traffic, 12),
      NA_real_
    )
  ) %>% 
  # mean
  mutate(
    mean_one_month_before_traffic = if_else(
      lag(ym_date, 1) == one_month_before,
      lag(mean_traffic, 1),
      NA_real_
    ),
    mean_one_month_after_traffic = if_else(
      lead(ym_date, 1) == one_month_after,
      lead(mean_traffic, 1),
      NA_real_
    )
  ) %>% 
  mutate(
    mean_three_months_before_traffic = if_else(
      lag(ym_date, 3) == three_months_before,
      lag(mean_traffic, 3),
      NA_real_
    ),
    mean_three_months_after_traffic = if_else(
      lead(ym_date, 3) == three_months_after,
      lead(mean_traffic, 3),
      NA_real_
    )
  ) %>% 
  mutate(
    mean_six_months_before_traffic = if_else(
      lag(ym_date, 6) == six_months_before,
      lag(mean_traffic, 6),
      NA_real_
    ),
    mean_six_months_after_traffic = if_else(
      lead(ym_date, 6) == six_months_after,
      lead(mean_traffic, 6),
      NA_real_
    )
  ) %>% 
  mutate(
    mean_twelve_months_before_traffic = if_else(
      lag(ym_date, 12) == twelve_months_before,
      lag(mean_traffic, 12),
      NA_real_
    ),
    mean_twelve_months_after_traffic = if_else(
      lead(ym_date, 12) == twelve_months_after,
      lead(mean_traffic, 12),
      NA_real_
    )
  ) %>% 
  # compute trends
  mutate(
    # sum
    trend_sum_one_month_before_after_traffic = sum_one_month_after_traffic - sum_one_month_before_traffic,
    trend_sum_three_months_before_after_traffic = sum_three_months_after_traffic - sum_three_months_before_traffic,
    trend_sum_six_months_before_after_traffic = sum_six_months_after_traffic - sum_six_months_before_traffic,
    trend_sum_twelve_months_before_after_traffic = sum_twelve_months_after_traffic - sum_twelve_months_before_traffic,
    # mean
    trend_mean_one_month_before_after_traffic = mean_one_month_after_traffic - mean_one_month_before_traffic,
    trend_mean_three_months_before_after_traffic = mean_three_months_after_traffic - mean_three_months_before_traffic,
    trend_mean_six_months_before_after_traffic = mean_six_months_after_traffic - mean_six_months_before_traffic,
    trend_mean_twelve_months_before_after_traffic = mean_twelve_months_after_traffic - mean_twelve_months_before_traffic
  ) %>% 
  # add log values
  mutate(across(ends_with("_traffic"), ~ log(.x), .names = "log_{.col}")) %>% 
  arrange(pageid, ym_date) %>% 
  group_by(pageid) %>% 
  mutate(
    dif_sum_traffic = if_else(
      ym_date == lag(next_month),
      sum_traffic - lag(sum_traffic),
      NA_real_
    ),
    log_dif_sum_traffic = log(dif_sum_traffic)
  ) %>% 
  ungroup

if (!file.exists("data_processed/wiki_traffic_data.Rds")) {
  if (!dir.exists("data_processed")) {dir.create("data_processed")}
  else(write_rds(wiki_traffic_data, "data_processed/wiki_traffic_data.Rds"))
} else (wiki_traffic_data <- readRDS("data_processed/wiki_traffic_data.Rds"))


