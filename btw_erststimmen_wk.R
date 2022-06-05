library(tidyverse)

# function for extracting erstimme vote shares from bundeswahlleiter datasets
erststimme_fun <- function(data_path, encoding, nr_lines_skip) {
  read_csv2(data_path, locale = encoding, skip = nr_lines_skip) %>% 
    mutate_all(~ as.character(.)) %>% 
    pivot_longer(cols = -c(1:3)) %>% 
    group_by(Gebiet) %>% 
    filter(
      !str_detect(name, "\\...")
    ) %>% 
    mutate(
      valid_votes_wk = if_else(
        name == "Gültige", 
        value,
        NA_character_
      )
    ) %>% 
    fill(
      valid_votes_wk, .direction = "downup"
    ) %>% 
    mutate(
      vote_share_erststimme = (value %>% as.numeric / valid_votes_wk %>% as.numeric) %>% round(5)
    ) %>% 
    filter(
      !name %in% c("Wahlberechtigte", "Wähler", "Gültige", "Ungültige", "gehört")
    ) %>% 
    rename(wahlkreis = Gebiet, party = name) %>% 
    ungroup %>% 
    filter(!is.na(wahlkreis))
}

# elections 2005, 2009, 2013, 2017
btw2005_erststimmen <- erststimme_fun(here::here("btw_kerg", "btw2005_kerg.csv"), locale(encoding = "latin1"), 5)
btw2009_erststimmen <- erststimme_fun(here::here("btw_kerg", "btw2009_kerg.csv"), locale(encoding = "latin1"), 5)
btw2013_erststimmen <- erststimme_fun(here::here("btw_kerg", "btw2013_kerg.csv"), locale(encoding = "latin1"), 5)
# btw2017_erststimmen <- erststimme_fun(here::here("btw_kerg", "btw2017_kerg.csv"), locale(encoding = "utf-8"), 5) %>% 
#   mutate(party = case_when(
#     party == "Christlich Demokratische Union Deutschlands" ~ "CDU",
#     party == "Sozialdemokratische Partei Deutschlands" ~ "SPD",
#     party == "BÜNDNIS 90/DIE GRÜNEN" ~ "GRÜNE",
#     party == "Freie Demokratische Partei" ~ "FDP",
#     party == "Alternative für Deutschland" ~ "AfD",
#     # party == "DIE LINKE" ~ "DIE LINKE",
#     T ~ party
#   ))
btw2017_erststimmen <- read_delim(
  "btw_kerg/btw2017_kerg2.csv", 
  delim = ";", 
  # escape_double = FALSE, 
  # trim_ws = TRUE, 
  skip = 9
) %>% 
  filter(
    Stimme == 1,
    Gruppenart == "Partei",
    Gebietsart == "Wahlkreis"
  ) %>% 
  mutate(vote_share_erststimme = str_replace(Prozent, "\\,", "\\.") %>% 
           as.numeric %>% `/`(100) %>% round(5)) %>% 
  select(
    wahlkreis = Gebietsname,
    Gebietsnummer,
    party = Gruppenname,
    vote_share_erststimme
  )

# election 2021
btw2021_erststimmen <- read_delim("btw_kerg/btw2021_kerg2.csv", 
                            delim = ";", escape_double = FALSE, trim_ws = TRUE, 
                            skip = 9) %>% 
  filter(UegGebietsart == "LAND", Gruppenart == "Partei") %>% 
  mutate(
    vote_share_erststimme = if_else(
      Stimme == 1,
      Prozent %>% str_replace(",", ".") %>% as.numeric  %>%  `/`(100) %>% round(5),
      NA_real_
    )
  ) %>% 
  filter(!is.na(vote_share_erststimme)) %>% 
  select(wahlkreis = Gebietsname, Gebietsnummer, party = Gruppenname, vote_share_erststimme)


# compute margin of victory
# btw2005_erststimmen %>% 
#   group_by(wahlkreis) %>% 
#   arrange(desc(vote_share_erststimme)) %>% 
#   mutate(
#     mov = if_else(
#       vote_share_erststimme == max(vote_share_erststimme, na.rm = T),
#       vote_share_erststimme - lead(vote_share_erststimme),
#       vote_share_erststimme - max(vote_share_erststimme, na.rm = T)
#     )
#   ) %>% 
#   arrange(wahlkreis, desc(mov)) 
  
