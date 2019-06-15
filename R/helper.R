library(tidyverse)
library(janitor)
library(stringi)
source("R/z_scores.R")
source("R/yahoo_api.R")

#TODO ingest performance data from fangraphs
#TODO create a "get latest compiled roster" method
#TODO add overall/by type rankings
#TODO rank/zar by position

filename_today <- function(name) {
  paste0("data/", name, "_", gsub("-", "", Sys.Date()), ".Rds")
}

read_latest_data_file <- function(name) {
  list.files("./data",
             pattern ="yahoo_players",
             full.names = TRUE) %>% 
    tail(1) %>% 
    readRDS()
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
  file <- "rosters"
  
  if (force) {
    get_team_rosters() %>%
      saveRDS(filename_today(file))
  }
  read_latest_data_file(file)
}

load_players <- function(force = FALSE) {
  file <- "yahoo_players"
  
  if (force) {
    get_players(2500) %>%
      clean_names() %>%
      mutate(
        type = case_when(
          position_type == "B" ~ "batter",
          position_type == "P" ~ "pitcher",
          TRUE ~ "other"),
        # two-way players treated as two separate players, breaking join
        name_full = gsub(" (Pitcher)", "", name_full, fixed=TRUE),
        name_full = gsub(" (Batter)", "", name_full, fixed=TRUE)) %>% 
      select(-position_type) %>% 
      saveRDS(filename_today(file))
  }
  read_latest_data_file(file)
}

#TODO
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
        file.copy(batter_file, batter_destination, overwrite=TRUE)
      }
    }
    
    if (force || !pitcher_destination %>% file.exists()) {
      if (pitcher_file %>% is_null() || !pitcher_file %>% file.exists()) {
        stop(
          "Could not find today's pitcher projections in expected folder, or replacement not specified"
        )
      } else {
        file.copy(pitcher_file, pitcher_destination, overwrite=TRUE)
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
    file <- "rosters_with_projections"
    
    if (force || force_all || !file %>% file.exists()) {
      load_rosters(force_all) %>%
        select(player_id, team, manager) %>%
        full_join(load_players(force_all)) %>%
        full_join(load_draft_results(), by = c("name_full" = "player")) %>%
        inner_join(load_projections(force_all, batter_file, pitcher_file),
                  by = c("name_full" = "name", "type" = "type")) %>%
        mutate(team = if_else(is.na(team), "Free Agent", team),
               team = fct_reorder(team, z_sum)) %>%
        compact_positions() %>% 
        rescale_z_sum() %>% 
        saveRDS(filename_today(file))
    }
    
    read_latest_data_file(file)
  }

rescale_z_sum <- function(df) {
  df %>% 
    filter(type != 'NA', team != 'Free Agent') %>%
    group_by(type) %>% 
    summarize(adj = median(z_sum)) -> z_sum_adj
  
  df %>% 
    filter(!type == 'NA') %>% 
    mutate(zar = if_else(type=="batter",
                         z_sum-z_sum_adj$adj[z_sum_adj$type=="batter"],
                         z_sum-z_sum_adj$adj[z_sum_adj$type=="pitcher"]))
}

compact_positions <- function(df) {
  position_columns <- df %>%
    select(starts_with("eligible_positions")) %>%
    colnames()
  
  df %>%
    mutate(positions = paste(!!! syms(position_columns), sep = "," )) %>%
    mutate(positions = gsub('NA', '', positions)) %>%
    mutate(positions = gsub("^,*|(?<=,),|,*$", "", positions, perl=T)) %>%
    select(-(starts_with("eligible_positions"))) %>% 
    mutate(positions = if_else(positions == "",position,positions))
}

row_per_position <- function(df) {
  df <- df %>%
    mutate(C = grepl("C", display_position)) %>%
    mutate(`1B` = grepl("1B", positions)) %>%
    mutate(`2B` = grepl("2B", positions)) %>%
    mutate(SS = grepl("SS", positions)) %>%
    mutate(`3B` = grepl("3B", positions)) %>%
    mutate(LF = grepl("LF", positions)) %>% 
    mutate(CF = grepl("CF", positions)) %>% 
    mutate(RF = grepl("RF", positions)) %>% 
    mutate(SP = grepl("SP", positions)) %>% 
    mutate(RP = grepl("RP", positions))
  
  C <- df %>%
    filter(C) %>%
    mutate(position = "C")
  
  `1B` <- df %>%
    filter(`1B`) %>%
    mutate(position = "1B")
  
  `2B` <- df %>%
    filter(`2B`) %>%
    mutate(position = "2B")
  
  `3B` <- df %>%
    filter(`3B`) %>%
    mutate(position = "3B")
  
  SS <- df %>%
    filter(SS) %>%
    mutate(position = "SS")
  
  LF <- df %>%
    filter(LF) %>%
    mutate(position = "LF")
  
  CF <- df %>%
    filter(CF) %>%
    mutate(position = "CF")
  
  RF <- df %>%
    filter(RF) %>%
    mutate(position = "RF")
  
  SP <- df %>% 
    filter(SP) %>% 
    mutate(position = "SP")
  
  RP <- df %>% 
    filter(RP) %>% 
    mutate(position = "RP")
  
  C %>%
    bind_rows(`1B`) %>%
    bind_rows(`2B`) %>%
    bind_rows(`3B`) %>%
    bind_rows(SS) %>%
    bind_rows(LF) %>% 
    bind_rows(CF) %>% 
    bind_rows(RF) %>% 
    bind_rows(SP) %>% 
    bind_rows(RP) %>% 
    unique() %>%
    arrange(z_sum %>% desc) %>%
    mutate(z_rank = row_number(),
           position = fct_relevel(position, "C","1B", "2B","3B","SS","LF","CF","RF","SP","RP"))
}
