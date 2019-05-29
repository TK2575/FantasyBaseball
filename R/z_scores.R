library(magrittr)
library(dplyr)
library(readr)

z_score_count <- function(df, stat, invert=FALSE, multiplier=1) {
  stat <- enquo(stat)
  col_nm <- paste0("z_", quo_name(stat))
  
  df <- df %>% 
    mutate(s_z = (!! stat - mean(!! stat)) / sd(!! stat)) 
  
  if (invert) {
    df %>%
      mutate(s_z = -s_z) -> df
  }
  
  if (multiplier != 1) {
    df %>%
      mutate(s_z = multiplier * s_z) -> df
  }
  
  df %>% 
    mutate(s_z = round(s_z,2)) %>%
    rename(!! col_nm := s_z)
} 

z_score_rate <- function(df, stat, quantity, invert=FALSE, multiplier = 1) {
  stat <- enquo(stat)
  quantity <- enquo(quantity)
  col_nm <- paste0("z_",quo_name(stat))
  
  df %>%
    mutate(s_q = (!! stat - mean(!! stat)) * !! quantity) %>%
    mutate(s_z = (s_q - mean(s_q)) / sd(s_q)) -> df
  
  if (invert) {
    df %>%
      mutate(s_z = -s_z) -> df
  }
  
  if (multiplier != 1) {
    df %>%
      mutate(s_z = multiplier * s_z) -> df
  }
  
  df %>%
    mutate(s_z = round(s_z, 2)) %>%
    rename(!! col_nm := s_z) %>%
    select(-s_q)
}

add_z_scores_to_projections <- function(file, mode) {
  add_z_scores(file, mode)
}

add_z_scores <- function(file, mode) {
  if (is.null(mode) | !mode %in% c("batter","pitcher")) {
    stop("mode must be either batter or pitcher")
  }
  
  raw <- read_csv(file)
  #TODO enforce expected columns by mode
  
  if (mode == "batter") {
    raw %>% 
      z_score_count(R) %>%
      z_score_count(H) %>%
      z_score_count(HR) %>%
      z_score_count(RBI) %>%
      z_score_count(SB) %>%
      z_score_count(BB) %>%
      z_score_count(SO, TRUE) %>%
      z_score_rate(OBP, PA) %>%
      z_score_rate(OPS, PA) %>%
      mutate(
        z_sum = pmap_dbl(
          select(., starts_with("z_")), 
          sum
        ),
        type = "batter")
  }
  
  else {
    raw %>% 
      z_score_count(SV) %>%
      z_score_count(SO) %>%
      z_score_count(HLD) %>%
      z_score_rate(ERA, IP, TRUE) %>%
      z_score_rate(WHIP, IP, TRUE) %>%
      mutate(
        z_sum = pmap_dbl(
          select(., starts_with("z_")), 
          sum
        ),
        type = "pitcher") 
  }
  
}