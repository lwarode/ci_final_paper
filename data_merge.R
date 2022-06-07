library(tidyverse)
library(legislatoR)
library(lubridate)

source("btw_erststimmen_wk.R")

data_fun <- function(session_date, erststimmen_df) {
  # regex for wahlkreis constituency merge
  char_to_replace_for_join <- "-|–|[:blank:]|–|[.]"
  
  # CLD data
  deu_pol <- get_political("deu") %>% 
    left_join(get_core("deu") %>% select(pageid, name:religion))
  
  # mov for every direct candidate per election 
  mov_all <- erststimmen_df %>% 
    mutate(
      wkr_merge = str_replace_all(wahlkreis, char_to_replace_for_join, "") %>% 
        str_to_lower()
    ) %>% 
    group_by(wahlkreis) %>% 
    arrange(desc(vote_share_erststimme)) %>% 
    mutate(
      mov = if_else(
        vote_share_erststimme == max(vote_share_erststimme, na.rm = T),
        vote_share_erststimme - lead(vote_share_erststimme, na.rm = T),
        vote_share_erststimme - max(vote_share_erststimme, na.rm = T)
      )
    ) %>% 
    ungroup
  
  potential_district_losers <- deu_pol %>%
    filter(constituency != "Landesliste") %>%
    group_by(pageid, name, constituency) %>%
    # filter for MPs that are leaving office for given date
    filter(session_end == max(session_end) & session_end == session_date) %>%
    mutate(wkr_merge = str_replace_all(constituency, char_to_replace_for_join, "") %>% str_to_lower()) %>%
    left_join(
      erststimmen_df %>%
        mutate(wkr_merge = str_replace_all(wahlkreis, char_to_replace_for_join, "") %>% str_to_lower()),
      by = c("wkr_merge", "party")
    ) %>%
    ungroup
  
  incumbents <- deu_pol %>%
    filter(session_start == session_date, constituency != "Landesliste") %>%
    # match datum
    mutate(wkr_merge = str_replace_all(constituency, char_to_replace_for_join, "") %>% str_to_lower()) %>%
    left_join(erststimmen_df %>%
                mutate(wkr_merge = str_replace_all(wahlkreis, char_to_replace_for_join, "") %>% str_to_lower()),
              by = c("wkr_merge", "party"))
  
  data_merged <- potential_district_losers %>%
    bind_rows(incumbents) %>%
    # distinct(pageid, .keep_all = T) %>%
    mutate(
      ym_date = if_else(
        session_start == session_date,
        format(as.Date(session_start), "%Y-%m"),
        format(as.Date(session_end), "%Y-%m")
      )
    ) %>%
    ungroup %>%
    left_join(
      mov_all,
      # by = c("constituency_merge" = "wahlkreis_merge")
    ) %>% 
    filter(!is.na(mov))
  
  return(data_merged)
}

# inverse hyperbolic sine transformation
ish <- function(x) {
  log(x + sqrt(x^2 + 1))
}

data_09 <- data_fun(as.Date("2009-10-27"), btw2009_erststimmen)
data_13 <- data_fun(as.Date("2013-10-22"), btw2013_erststimmen)
data_17 <- data_fun(as.Date("2017-10-24"), btw2017_erststimmen)

wiki_traffic_data <- readRDS("data_processed/wiki_traffic_data.Rds")
wiki_page_start_date <- readRDS("data_processed/wiki_page_start_date.Rds")

data_all <- data_09 %>% 
  bind_rows(data_13) %>% 
  bind_rows(data_17) %>% 
  left_join(wiki_traffic_data) %>% 
  left_join(wiki_page_start_date) %>% 
  mutate(
    # ish transformation of trend variables
    across(starts_with("trend_"), ~ ish(.x), .names = "ish_{.col}"),
    election_won = ifelse(mov > 0, 1, 0)
  )


if (!file.exists("data_processed/data_all.Rds")) {
  if (!dir.exists("data_processed")) {dir.create("data_processed")}
    else (write_rds(data_all, "data_processed/data_all.Rds"))
} else (data_all <- readRDS("data_processed/data_all.Rds"))



