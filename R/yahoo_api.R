library(httr)
library(httpuv)
library(jsonlite)
library(magrittr)

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

get_players <- function(rownum=400) {
  
  start <- 0
  results <- tibble()
  times <- (rownum / 25) %>% floor()
  
  for (i in 0:times) {
      df <- get_players_page(start)
    
    results <- results %>%
      bind_rows(df)
    
    start <- start + 25
    if (df %>% nrow() < 25) {
      break
    }
  }

  results
}

get_players_page <- function(start) {
  path <- paste0("users;use_login=1/games;game_keys=mlb/players;start=",start,";count=25")
  
  get_json(path) %>% 
    content() -> players_resp
  
  players_resp$fantasy_content$users$`0`$user[[2]]$games$`0`$game[[2]][[1]] -> players_filtered
  
  lapply(players_filtered[1:25], function(x) x[[1]][[1]] %>% 
           flatten() %>% 
           as.data.frame(stringsAsFactors = F)) %>% 
    bind_rows() %>% 
    as_tibble() %>%
    select(-(14:16))
}

