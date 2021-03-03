library(rvest)
library(xml2)
library(janitor)
library(dplyr)
library(magrittr)
library(purrr)

#TODO enforce numeric columns (chr defaults)

scrape_table <- function(url) {
  url %>% 
    read_html() %>% 
    html_nodes("table") %>% 
    html_table() %>% 
    .[[1]] %>% 
    as_tibble() %>% 
    clean_names()
}

player_standard_batting <- function(year) {
  url <- paste0(
    "https://widgets.sports-reference.com/wg.fcgi?css=1&site=br&url=%2Fleagues%2FMLB%2F",
    year,
    "-standard-batting.shtml&div=div_players_standard_batting"
  )
  
  scrape_single_year_table(url, year)
}

player_standard_pitching <- function(year) {
  url <- paste0(
    "https://widgets.sports-reference.com/wg.fcgi?css=1&site=br&url=%2Fleagues%2FMLB%2F",
    year,
    "-standard-pitching.shtml&div=div_players_standard_pitching"
  )
  
  scrape_single_year_table(url, year)
}

player_starting_pitching <- function(year) {
  url <- paste0(
    "https://widgets.sports-reference.com/wg.fcgi?css=1&site=br&url=%2Fleagues%2FMLB%2F",
    year,
    "-starter-pitching.shtml&div=div_players_starter_pitching"
  )
  
  scrape_single_year_table(url, year)
}

player_relief_pitching <- function(year) {
  url <- paste0(
    "https://widgets.sports-reference.com/wg.fcgi?css=1&site=br&url=%2Fleagues%2FMLB%2F",
    year,
    "-reliever-pitching.shtml&div=div_players_reliever_pitching"
  )
  
  scrape_single_year_table(url, year)
}

standings_expanded <- function(year) {
  url <- paste0(
    "https://widgets.sports-reference.com/wg.fcgi?css=1&site=br&url=%2Fleagues%2FMLB%2F", 
    year,
    "-standings.shtml%3Fsr%26utm_source%3Ddirect%26utm_medium%3DShare%26utm_campaign%3DShareTool&div=div_expanded_standings_overall")
  
  scrape_single_year_table(url, year)
}

scrape_single_year_table <- function(url, year) {
  if (!is.numeric(year) || nchar(year) != 4) {
    stop("year must be a four digit number")
  }
  
  url %>% 
    scrape_table() %>% 
    mutate(year = year)
}

standings_expanded_years <- function(years) {
  years %>% 
    map(standings_expanded) %>% 
    bind_rows()
}

team_batting <- function(year) {
  url <- paste0("https://widgets.sports-reference.com/wg.fcgi?css=1&site=br&url=%2Fleagues%2FMLB%2F",
                year,
                ".shtml&div=div_teams_standard_batting")
  
  scrape_single_year_table(url, year) %>% 
    filter(tm != "Tm") %>% 
    mutate(tm = if_else(tm == "", "LgTot", tm))
}

team_batting_against <- function(year) {
  url <- paste0("https://widgets.sports-reference.com/wg.fcgi?css=1&site=br&url=%2Fleagues%2FMLB%2F",
                year,
              "-batting-pitching.shtml&div=div_teams_batting_pitching")
  
  scrape_single_year_table(url, year) %>% 
    filter(tm != "Tm") %>% 
    mutate(tm = if_else(tm == "", "LgTot", tm)) 
}

team_relief_pitching <- function(year) {
  url <- paste0("https://www.baseball-reference.com/leagues/MLB/",
                year,
                "-reliever-pitching.shtml")
  
  scrape_single_year_table(url, year) %>% 
    filter(tm != "Tm") %>% 
    mutate(tm = if_else(tm == "", "LgTot", tm)) 
}

team_relief_pitching_years <- function(years) {
  years %>% 
    map(team_relief_pitching) %>% 
    bind_rows()
}
