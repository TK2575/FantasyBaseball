library(here)
library(tidyverse)
library(listviewer)

here("R", "yahoo_api.R") %>% source()

path <- function () {
  paste0("league/",league_string())
}

team_path <- function(team_id) {
  paste0("team/",league_string(),".t.",team_id)
}

get_json_content <- function(path) {
  get_json(path) %>% 
    content()
}

team_day_perf <- 
  team_path(8) %>% 
  paste0("/stats;type=date;date=") %>% 
  get_json_content()

jsonedit(team_day_perf)

get_team_day_perf <- function(team, date) {
  team_path(team) %>% 
    paste0("/stats;type=date;date=",date) %>% 
    get_json_content()
}

days <- seq(
  from=as.Date("2019-03-20"),
  to=as.Date("2019-09-16"),
  by="1 day"
)

list <- list()
i <- 1
teams <- c(1:12)

for (team in teams) {
  sub_list <- list()
  j <- 1
  while (j <= length(days)) {
    sub_list[[j]] <- 
      get_team_day_perf(team, days[[j]])
    Sys.sleep(2)
    j <- j + 1
  }
  list[[i]] <- sub_list
  i <- i + 1
}

t <- 0
while (t < length(days)) {
  print(days[[t+1]])
  t <- t + 1
}

rslt <- get_team_day_perf(1, days[[1]])
