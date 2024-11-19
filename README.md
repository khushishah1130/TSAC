# TSAC


# Shane Data -- team data by year, including power ratings, team stats, stats filtered by game site (home, away, neutral site)
library(cbbplotR)
library(tidyverse)
library(cbbdata)

# to register for cbbdata (free)
cbbdata::cbd_create_account(username = 'xxx', email = 'xxx', password = 'xxx', confirm_password = 'xxx')

# then sign in with your username and password
cbbdata::cbd_login(username = 'xxx', password = 'xxx')

torvik_ratings <- cbd_torvik_ratings()

torvik_2018_present <- torvik_ratings |> 
  filter(year >= 2018)

split_by_location <- cbd_torvik_team_split(split = "location")

split_by_location_pivoted <- split_by_location |> 
  pivot_wider(names_from = location,
              values_from = min:games,
              names_sep = "_")

columns_to_drop <- grep("NA", colnames(split_by_location_pivoted), value = TRUE)

split_by_location_pivoted <- split_by_location_pivoted |> 
  select(-all_of(columns_to_drop)) |> 
  filter(year >= 2018)

team_by_year_stats <- torvik_2018_present |> 
  left_join(split_by_location_pivoted, by = c("team", "year"))

team_by_year_stats <- team_by_year_stats |> 
  mutate(team = recode(team, "N.C. State" = "North Carolina St.")) |> 
  relocate(team, year)

team_by_year_stats_power_5 <- team_by_year_stats |> 
  filter(conf %in% c("B12", "B10", "ACC", "P12", "SEC"))

power_5_teams <- c("North Carolina", "Duke", "Clemson", "Pittsburgh", "Wake Forest",
                   "Virginia", "Syracuse", "North Carolina St.", "Florida St.", "Virginia Tech",
                   "Boston College", "Georgia Tech", "Notre Dame", "Miami FL", "Louisville",
                   "Houston", "Iowa St.", "Baylor", "Texas Tech", "BYU",
                   "Kansas", "TCU", "Oklahoma", "Cincinnati", "Kansas St.",
                   "UCF", "Oklahoma St.", "West Virginia", "Texas",
                   "Illinois", "Indiana", "Iowa", "Maryland", "Michigan",
                   "Michigan St.", "Minnesota", "Nebraska", "Northwestern", "Ohio St.",
                   "Penn St.", "Purdue", "Rutgers", "Wisconsin",
                   "Alabama", "Arkansas", "Auburn", "Florida", "Georgia",
                   "Kentucky", "LSU", "Mississippi", "Mississippi St.", "Missouri",
                   "South Carolina", "Tennessee", "Texas A&M", "Vanderbilt",
                   "Arizona", "Arizona St.", "California", "Colorado", "Oregon",
                   "Oregon St.", "Stanford", "UCLA", "USC", "Utah",
                   "Washington", "Washington St.")

team_history_list <- list()

# this loop takes a while to run -- like 5ish minutes
for (team in power_5_teams) {
  
  team_data <- cbd_torvik_team_history(team)
  
  team_data <- team_data |> 
    filter(year >= 2018) |> 
    mutate(team = team)
  
  team_history_list[[team]] <- team_data
}

team_history_df <- bind_rows(team_history_list)

team_history_df <- team_history_df |> 
  relocate(team, year) |> 
  select(team, year, coach, finish, ov_rec, conf_rec, efg_o, efg_d, to_percent, tod_percent,
         or_percent, dr_percent, ftr, ftrd, ft_percent, two_pct, two_pct_d, three_pct)

all_stats_p5 <- team_by_year_stats_power_5 |> 
  left_join(team_history_df, by = c("team", "year"))

all_stats_p5 <- all_stats_p5 |> 
  relocate(team, year, conf, coach, ov_rec, conf_rec)
