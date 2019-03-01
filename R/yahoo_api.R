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

parse_resp <- function(resp) {
  parsed <- fromJSON(content(resp, "text"),
                     simplifyVector = F)
  
  structure(
    list(
      content = parsed,
      path = path,
      response = resp
    ),
    class = "yahoo_api"
  )
  
}

yahoo_api <- function(path) {
  get_json() %>%
    parse_resp()    
}



