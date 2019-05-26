library(tidyverse)
library(janitor)
library(glue)
library(plotly)
library(stringi)
source("R/z_scores.R")
source("R/yahoo_api.R")

# get yahoo, fangraphs, draft data from disk or API, save to disk as needed
# point to pair of projection files, auto-rename and save to project directory
# clean/join data, create and save to disk one returned data set

#TODO check if file exists, maybe move all in this chunk to a script
filename <- function(name) {
  paste0("data/",name,"_",gsub("-","",Sys.Date()),".Rds")
}

get_draft_results <- function() {
read_tsv("data/draftResults.tsv") %>% 
  clean_names() %>% 
  mutate(player = stri_trans_general(player, "latin-ascii"),
           round = ceiling(pick/12)) %>% 
  select(team, player, pick, round) %>% 
  rename(draft_pick = pick,
         draft_round = round,
         draft_team = team)
}

load_rosters <- function(force=FALSE) {
	if ( force || !filename("rosters") %>% exists() ) {
		get_team_rosters() %>%
			saveRDS(filename("rosters"))
		}
	readRDS(filename("rosters"))
}

load_players <- function(force=FALSE) {
	if ( force || !filename("yahoo_players") %>% exists() ) {
		get_players(2500) %>%
			saveRDS(filename("yahoo_players"))
	}
	readRDS(filename("yahoo_players"))
}

all_players <- function(force=FALSE) {
	load_rosters(force) %>%
		select(player_id, team, manager) %>%
		full_join(load_players(force)) %>%
		clean_names() %>%
		mutate(name_full = stri_trans_general(name_full, "latin-ascii"))

	#TODO add projections, draft results
}
  

#TODO abstract projection filename
  
proj_batter <- add_z_scores_to_projections("data/depthChartProjections_ros_batters_20190513.csv","batter") 
proj_pitcher <- add_z_scores_to_projections("data/depthChartProjections_ros_pitchers_20190513.csv", "pitcher")
  
projections <- proj_batter %>% 
  bind_rows(proj_pitcher)


#TODO save these to some file
projections$Name[projections$Name == 'Enrique Hernandez'] <- "Kike Hernandez"
projections$Name[projections$Name == 'Peter Alonso'] <- "Pete Alonso"
projections$Name[projections$Name == 'Joshua James'] <- "Josh James"
projections$Name[projections$Name == 'Shin-Soo Choo'] <- "Shin-soo Choo"

#TODO test, include draft team
all_players %>% 
  full_join(draft_results, by = c("name_full" = "player")) %>% 
  full_join(projections, by = c("name_full" = "Name")) %>% 
  mutate(team = if_else(is.na(team),"Free Agent",team),
         team = fct_reorder(team, z_sum)) -> rosters_with_projections

rosters_with_projections %>% 
  saveRDS(filename("rosters_with_projections"))