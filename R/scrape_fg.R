library(RSelenium)
library(here)

fprof <- makeFirefoxProfile(
  list(
    browser.download.dir = here("temp"), 
    browser.download.folderList = 2L, 
    browser.download.manager.showWhenStarting = FALSE, 
    browser.helperApps.neverAsk.saveToDisk = "text/csv"
    )
  )

url_projection_batter <- "https://www.fangraphs.com/projections.aspx?pos=all&stats=bat&type=rfangraphsdc&team=0&lg=all&players=0"
url_projection_pitcher <- "https://www.fangraphs.com/projections.aspx?pos=all&stats=pit&type=rfangraphsdc&team=0&lg=all&players=0"

url_perf_batter <- "https://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=0&type=c,4,6,5,7,11,12,13,21,14,16,-1,34,35,40,41,-1,23,37,38,39,50,61,-1,111,-1,203,199,58&season=2019&month=0&season1=2019&ind=0&team=&rost=&age=&filter=&players="
url_perf_pitcher <- "https://www.fangraphs.com/leaders.aspx?pos=all&stats=pit&lg=all&qual=0&type=c,4,5,11,114,7,8,13,24,-1,36,37,40,43,44,48,51,-1,6,42,45,62,-1,59&season=2019&month=0&season1=2019&ind=0&team=0&rost=0&age=0&filter=&players=0"

start_local_server <- function() {
  rsDriver(
    port = 2525L, 
    browser = "firefox", 
    extraCapabilities = fprof,
    verbose = FALSE
  )
}

download_fg_csv <- function(url, rd) {
  csv_xpath <- NULL
  
  if (grepl("/leaders", url)) {
    csv_xpath <- '//*[@id="LeaderBoard1_cmdCSV"]'
  } else if (grepl("/projections", url)) {
    csv_xpath <- '//*[@id="ProjectionBoard1_cmdCSV"]'
  }
  
  if (!is.null(csv_button)) {
    client <- rd$client
    client$navigate(url)
    
    # TODO avoid membership popup
    
    csv_button <- client$findElement(using = "xpath", value = csv_xpath)
    csv_button$clickElement
    client$close()
    
    # TODO rename downloaded file
    
  } else {
    stop("Unknown CSV download selector for url")
  }
}

# implementation
rd <- start_local_server()
download_fg_csv(url_projection_batter, rd)


