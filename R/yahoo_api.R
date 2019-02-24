# TODO refactor into single function call
library(httr)

keys <- app_keys()

myapp <- oauth_app("yahoo",
                   key = keys[1],
                   secret = keys[2]
)

yahoo_token <- oauth2.0_token(oauth_endpoints("yahoo"), 
                              myapp,
                              use_oob = TRUE, 
                              oob_value = "oob"
)


app_keys <- function() {
  key <- Sys.getenv('YAHOO_KEY')
  secret <- Sys.getenv('YAHOO_SECRET')
  
  if (identical(key, "") || identical(secret, "")) {
    stop("Please set env var YAHOO_KEY to your Yahoo app key, and YAHOO_SECRET to your Yahoo app secret",
         call. = F)
  }
  
  c(key,secret)
}