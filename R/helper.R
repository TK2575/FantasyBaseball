library(tidyverse)
library(janitor)
library(stringi)
source("R/z_scores.R")
source("R/yahoo_api.R")

#TODO review/resolve all player name joins (orphaned results, incorrect assignments)

filename <- function(name) {
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
  file <- filename("rosters")
  
  if (force || !file %>% exists()) {
    get_team_rosters() %>%
      saveRDS(file)
  }
  readRDS(file)
}

load_players <- function(force = FALSE) {
  file <- filename("yahoo_players")
  
  if (force || !file %>% exists()) {
    get_players(2500) %>%
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
    
    if (force || !batter_destination %>% exists()) {
      if (batter_file %>% is_null() || !batter_file %>% exists()) {
        stop(
          "Could not find today's batter projections in expected folder, or replacement not specified"
        )
      } else {
        file.copy(batter_file, batter_destination)
      }
    }
    
    if (force || !pitcher_destination %>% exists()) {
      if (pitcher_file %>% is_null() || !pitcher_file %>% exists()) {
        stop(
          "Could not find today's pitcher projections in expected folder, or replacement not specified"
        )
      } else {
        file.copy(pitcher_file, pitcher_destination)
      }
    }
    
    proj_batter <- add_z_scores_to_projections(batter_destination)
    proj_pitcher <- add_z_scores_to_projections(pitcher_destination)
    
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
           batter_file = NULL,
           pitcher_file = NULL) {
    file <- filename("rosters_with_projections")
    
    if (force || !file %>% exists()) {
      load_rosters(force) %>%
        select(player_id, team, manager) %>%
        full_join(load_players(force)) %>%
        clean_names() %>%
        mutate(name_full = stri_trans_general(name_full, "latin-ascii")) %>%
        full_join(load_draft_results(), by = c("name_full" = "player")) %>%
        full_join(load_projections(force, batter_file, pitcher_file),
                  by = c("name_full" = "Name")) %>%
        mutate(team = if_else(is.na(team), "Free Agent", team),
               team = fct_reorder(team, z_sum)) %>%
        saveRDS(file)
    }
    
    readRDS(file)
  }