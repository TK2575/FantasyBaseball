library(tidyverse)
library(janitor)
library(stringi)
source("R/z_scores.R")
source("R/yahoo_api.R")

#TODO ingest performance data from fangraphs
#TODO add overall/by type rankings
#TODO move all read_csv references to read_latest_file_csv

filename_today <- function(name, extension) {
  paste0("data/", name, "_", gsub("-", "", Sys.Date()), extension)
}

filename_today_rds <- function(name) {
  filename_today(name, ".Rds")
}

filename_today_csv <- function(name) {
  filename_today(name, ".csv")
}

find_latest_data_file <- function(name) {
  #FIXME search by extension
  rslt <- list.files("./data",
             pattern = name,
             full.names = TRUE) %>% 
    tail(1)
  
  paste0("Found file = ", rslt) %>% print()
  
  rslt
}

read_latest_file_rds <- function(name) {
   find_latest_data_file(name) %>% 
    readRDS()
}

read_latest_file_csv <- function(name) {
  find_latest_data_file(name) %>% 
    read_csv() %>% 
    clean_names()
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
  file <- "team_rosters"
  
  if (force) {
    get_team_rosters() %>%
      saveRDS(filename_today_rds(file))
  }
  read_latest_file_rds(file)
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
      saveRDS(filename_today_rds(file))
  }
  read_latest_file_rds(file)
}

load_projections <- function(force = FALSE,
                             batter_projection_file = NULL,
                             pitcher_projection_file = NULL) {
  
  file_start <- "depthChartProjections_ros_"
  batter_file <- paste0(file_start, "batters") 
  pitcher_file <- paste0(file_start, "pitchers") 
  
  if (force) {
    if (batter_projection_file %>% is_null() || 
        !batter_projection_file %>% file.exists() ||
        pitcher_projection_file %>% is_null() ||
        !pitcher_projection_file %>% file.exists()) {
      stop(
        "Could not find today's batter or pitcher projections in expected folder, or replacement not specified"
      )
    } else {
      file.copy(batter_projection_file, filename_today_csv(batter_file), overwrite=TRUE)
      file.copy(pitcher_projection_file, filename_today_csv(pitcher_file), overwrite=TRUE)
    }
  }
  
  batter <- find_latest_data_file(batter_file)
  pitcher <- find_latest_data_file(pitcher_file)
  
  compile_z_scores(batter, pitcher) %>% 
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
  function(force_fetch = FALSE,
           force_join = FALSE,
           batter_file = NULL,
           pitcher_file = NULL) {
    file <- "rosters_with_projections"
    
    if (force_fetch) {
      force_join == TRUE
    }
    
    if (force_join) {
      result <- load_rosters(force_fetch) %>%
        select(player_id, team, manager) %>%
        full_join(load_players(force_fetch)) %>%
        full_join(load_draft_results(), by = c("name_full" = "player")) %>%
        inner_join(load_projections(force_fetch, batter_file, pitcher_file),
                  by = c("name_full" = "name", "type" = "type")) %>%
        mutate(team = if_else(is.na(team), "Free Agent", team),
               team = fct_reorder(team, z_sum)) %>%
        compact_positions() %>% 
        rescale_z_sum()
      
        if (force_fetch) { 
          result %>% 
            saveRDS(filename_today_rds(file))
        }
      result
    } else {
      read_latest_file_rds(file) 
    }
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
  
  rosters_by_position <- C %>%
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
           position = fct_relevel(position, "C","1B","2B","3B","SS","LF","CF","RF","SP","RP"))
  
  rosters_by_position %>% 
    filter(!is.na(manager)) %>% 
    group_by(position) %>% 
    count() %>% 
    ungroup() -> rostered_count_by_pos
  
  rosters_by_position %>% 
    group_by(position) %>% 
    arrange(zar %>% desc()) %>% 
    mutate(num = row_number()) %>% 
    full_join(rostered_count_by_pos, "position") %>% 
    filter(num <= n) %>% 
    summarize(median_zar = median(zar)) -> pos_median_zar
  
  rosters_by_position %>% 
    full_join(pos_median_zar) %>% 
    mutate(pos_zar = zar - median_zar)
}

optimal_lineup <- function(df) {
  positions <- c("C","1B","2B","3B","SS","LF","CF","RF","Util")
  
  result <- NULL
  
  df <- df %>% 
    filter(team != "Free Agent") %>% 
    droplevels()
  
  for (each in df$team %>% levels()) {
    team_result <- tibble(
      position = positions,
      player_id = NA
    )
    
    batters <- df %>%
      filter(type == "batter",
             team == each) %>% 
      select(team, player_id, name_full, position, pos_zar, zar) %>% 
      arrange(zar %>% desc(), pos_zar %>% desc()) 
    
    batter_ids <- batters$player_id %>% 
      as_factor() %>%
      levels()
    
    for (i in 1:length(batter_ids)) {
      batter_id <- batter_ids[i]
      
      batter <- batters %>% 
        filter(player_id == batter_id) 
      
      batters <- batters %>% 
        add_row(
          team = each,
          player_id = batter_id,
          name_full = batter$name_full[1],
          position = "Util",
          pos_zar = -100, # force to bottom of arrange 
          zar = batter$zar[1]
        ) %>% 
        arrange(zar %>% desc(), pos_zar %>% desc())
      
      for (j in 1:length(batter$position)) {
        if (team_result$player_id[team_result$position == batter$position[j]] %>% is.na()) {
          team_result$player_id[team_result$position == batter$position[j]] <- batter_id
          break
        }
      }
      
      if (!batter_id %in% (team_result$player_id %>% 
                           as_factor() %>% 
                           levels()) && 
          team_result$player_id[team_result$position == "Util"] %>% is.na()) {
        team_result$player_id[team_result$position == "Util"] <- batter_id
      }
    }
    
    team_result <- batters %>% 
      mutate(position = as.character(position)) %>% 
      inner_join(team_result, c("player_id","position")) 
    
    if (result %>% is.null()) {
      result <- team_result
    } else {
      result <- result %>% 
        bind_rows(team_result)
    }
  }
  
  result %>%
    mutate(pos_zar = if_else(position=="Util",zar,pos_zar))
} 
