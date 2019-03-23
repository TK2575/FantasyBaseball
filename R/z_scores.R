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