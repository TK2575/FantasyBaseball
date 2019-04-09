library(httr)
library(httpuv)
library(jsonlite)
library(magrittr)
library(glue)
library(dplyr)
library(purrr)

app_keys <- function() {
  key <- Sys.getenv('YAHOO_KEY')
  secret <- Sys.getenv('YAHOO_SECRET')
  
  if (identical(key, "") || identical(secret, "")) {
    stop("Please set env var YAHOO_KEY to your Yahoo app key, and YAHOO_SECRET to your Yahoo app secret",
         call. = F)
  }
  
  c(key,secret)
}

authenticate <- function() {
  keys <- app_keys()
  
  myapp <- oauth_app("yahoo",
                     key = keys[1],
                     secret = keys[2]
  )
  
  token <- oauth2.0_token(oauth_endpoints("yahoo"), 
                                myapp,
                                use_oob = TRUE, 
                                oob_value = "oob"
  )
  
  config(token = token)
}

get_json <- function(path) {
  
  config <- authenticate()
  ua <- user_agent("http://github.com/TK2575/ff_data")
  
  base <- "https://fantasysports.yahooapis.com/fantasy/v2/"
  req_json <- "/?response=json"
  
  url <- paste0(base, path, req_json)
  
  print(url)
  
  resp <- GET(url = url, config = config, ua)
  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = F)
  }
  
  parsed <- fromJSON(content(resp, "text"),
                     simplifyVector = F)
  
  if (status_code(resp) != 200) {
    stop(
      sprintf(
        "Yahoo API request failed [%s]\n%s\n<%s>",
        status_code(resp),
        parsed$message,
        parsed$documentation_url
      ),
      call. = F
    )
  }
  
  resp
}

print.yahoo_api <- function(x, ...) {
  cat("<Yahoo ", x$path, ">\n", sep = "")
  str(x$content)
  invisible(x)
}

get_players <- function(rownum=25, start=0) {
  
  cursor <- start
  results <- tibble()
  pages <- (rownum / 25) %>% ceiling()
  
  for (i in 1:(pages)) {
    print(glue("Retrieving page {i} of {pages}"))  
    df <- get_players_page(cursor)
    
    results <- results %>%
      bind_rows(df)
    
    cursor <- cursor + 25
    if (df %>% nrow() < 25) {
      print(glue("No more players to retrieve, exiting"))
      break
    }
    Sys.sleep(5)
  }

  results
}

get_players_page <- function(start) {
  path <- paste0("users;use_login=1/games;game_keys=mlb/players;start=",start,";count=25")
  
  get_json(path) %>% 
    content() -> players_resp
  
  players_resp$fantasy_content$users$`0`$user[[2]]$games$`0`$game[[2]][[1]] -> players_filtered
  
  count <- players_filtered$count
  
  lapply(players_filtered[1:count], function(x) x[[1]][[1]] %>% 
           rlang::flatten() %>% 
           as.data.frame(stringsAsFactors = F)) %>% 
    bind_rows() %>% 
    as_tibble() %>%
    select(-(14:16))
}

league_string <- function(game_key=388) {
  league_key <- Sys.getenv("LEAGUE_KEY")
  if (identical("", league_key)) {
    stop("LEAGUE_KEY isn't set as R Environment variable")
  } else {
    paste0(game_key,".l.",Sys.getenv("LEAGUE_KEY"))
  }
}

get_team_roster <- function(team_id) {
  path <- paste0("team/",league_string(),".t.",team_id,"/roster/players")
  
  get_json(path) %>% 
    content() -> players_resp
  
  players_resp$fantasy_content$team[[1]][[3]]$name -> team_name
  
  players_resp$fantasy_content$team[[1]][[20]]$managers[[1]]$manager$nickname -> manager
  
  players_resp$fantasy_content$team[[2]]$roster$`0`$players -> players_filtered
  
  count <- players_filtered$count
  
  lapply(players_filtered[1:count], function(x) x[[1]][[1]] %>% 
           rlang::flatten() %>% 
           as.data.frame(stringsAsFactors = F)) %>% 
    bind_rows() %>% 
    as_tibble() %>% 
    mutate(team = team_name,
           manager = manager)
}

get_team_rosters <- function(teams=12) {
  1:teams %>% 
    map(get_team_roster) %>% 
    bind_rows() %>% 
    clean_names() %>% 
    mutate(name_full = chartr("áéóíñú", "aeoinu", name_full))
}

