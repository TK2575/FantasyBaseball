library(here)
library(tidyverse)
library(listviewer)

here("R", "yahoo_api.R") %>% source()

path <- function () {
  paste0("league/",league_string())
}

team_path <- function(team_id) {
  paste0(path(),".t.",team_id)
}

path_scoreboard <- paste0(path(),"/scoreboard")
path_standings <- paste0(path(),"/standings")
path_settings <- paste0(path(),"/settings")

get_json_content <- function(path) {
  get_json(path) %>% 
    content()
}

scoreboard <- get_json_content(path_scoreboard)
standings <- get_json_content(path_standings)
settings <- get_json_content(path_settings)

listviewer::jsonedit(scoreboard)
listviewer::jsonedit(standings)
listviewer::jsonedit(settings)

scoreboard_week1 <- get_json_content(paste0(path_scoreboard,";week=1"))
listviewer::jsonedit(scoreboard_week1)

weeks <- c(1:23)
list <- list()
i <- 1
for (week in weeks) {
  list[[i]] <- 
    paste0(path_scoreboard,";week=",week) %>% 
    get_json_content()
  
  i <- i + 1
}

here("data", "scoreboard_wks_1-23.Rds") %>% 
  saveRDS(list, .)

here("data", "league_settings.Rds") %>% 
  saveRDS(settings, .)
