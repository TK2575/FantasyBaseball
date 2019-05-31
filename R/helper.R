library(tidyverse)
library(janitor)
library(stringi)
source("R/z_scores.R")
source("R/yahoo_api.R")

#TODO review/resolve all player name joins (orphaned results, incorrect assignments)
#TODO ingest performance data from fangraphs
#TODO add position column compact options for load player data
#TODO add z_adj -> zar (sets z_score above replacement, based on median z_score of rostered players, grouped by type [batter/pitcher])
#TODO create a "get latest compiled roster" method

filename_today <- function(name) {
  paste0("data/", name, "_", gsub("-", "", Sys.Date()), ".Rds")
}

load_draft_results <- function() {
  read_tsv("data/draftResults.tsv") %>%
    clean_names() %>%
    mutate(player = stri_trans_general(player, "latin-ascii"),
           round = ceiling(pick / 12)) %>%
    select(team, player, pick, round) %>%
    rename(draft_pick = pick,
           draft_round = round,
           draft_team = team)
}

load_rosters <- function(force = FALSE) {
  file <- filename_today("rosters")
  
  if (force || !file %>% file.exists()) {
    get_team_rosters() %>%
      saveRDS(file)
  }
  readRDS(file)
}

load_players <- function(force = FALSE) {
  file <- filename_today("yahoo_players")
  
  if (force || !file %>% file.exists()) {
    get_players(2500) %>%
      clean_names() %>%
      mutate(type = case_when(
        position_type == "B" ~ "batter",
        position_type == "P" ~ "pitcher",
        TRUE ~ "other"
      )) %>% 
      select(-position_type) %>% 
      saveRDS(file)
  }
  readRDS(file)
}

load_projections <-
  function(force = FALSE,
           batter_file = NULL,
           pitcher_file = NULL) {
    file_start <- "data/depthChartProjections_ros_"
    date_string <- Sys.Date() %>% str_replace_all("-", "")
    batter_destination <-
      paste0(file_start, "batters_", date_string, ".csv")
    pitcher_destination <-
      paste0(file_start, "pitchers_", date_string, ".csv")
    
    if (force || !batter_destination %>% file.exists()) {
      if (batter_file %>% is_null() || !batter_file %>% file.exists()) {
        stop(
          "Could not find today's batter projections in expected folder, or replacement not specified"
        )
      } else {
        file.copy(batter_file, batter_destination)
      }
    }
    
    if (force || !pitcher_destination %>% file.exists()) {
      if (pitcher_file %>% is_null() || !pitcher_file %>% file.exists()) {
        stop(
          "Could not find today's pitcher projections in expected folder, or replacement not specified"
        )
      } else {
        file.copy(pitcher_file, pitcher_destination)
      }
    }
    
    compile_z_scores(batter_destination, pitcher_destination) %>% 
      rename(mlb_team = team)
    
  }

compile_z_scores <- function(batter_file, pitcher_file) {
  proj_batter <- add_z_scores(batter_file, "batter")
  proj_pitcher <- add_z_scores(pitcher_file, "pitcher")
  
  bind_rows(proj_batter, proj_pitcher) %>%
    clean_names() %>%
    adjust_names_for_join()
}

adjust_names_for_join <- function(df) {
  names_key_df <- read_csv("data/nameJoins.csv") %>%
    distinct()
    
    names_key <-
      setNames(as.character(names_key_df$name_yahoo),
               names_key_df$name_fangraphs)
    
    df %>%
      mutate(name = recode(name,!!!names_key))
}

load_players_with_projections <-
  function(force = FALSE,
           force_all = FALSE,
           batter_file = NULL,
           pitcher_file = NULL) {
    file <- filename_today("rosters_with_projections")
    
    if (force || force_all || !file %>% file.exists()) {
      load_rosters(force_all) %>%
        select(player_id, team, manager) %>%
        full_join(load_players(force_all)) %>%
        full_join(load_draft_results(), by = c("name_full" = "player")) %>%
        left_join(load_projections(force_all, batter_file, pitcher_file),
                  by = c("name_full" = "name", "type" = "type")) %>%
        mutate(team = if_else(is.na(team), "Free Agent", team),
               team = fct_reorder(team, z_sum)) %>%
        saveRDS(file)
    }
    
    readRDS(file)
  }