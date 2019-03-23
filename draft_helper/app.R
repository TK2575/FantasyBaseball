library(shiny)
library(plotly)
library(tidyverse)

main_df <- readRDS("../data/draft_dataset.Rds")
positions_df <- readRDS("../data/positions_ranked_with_keepers.Rds")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Tom's Draft Cheat Sheet"),
   
   # Sidebar
   sidebarLayout(
      sidebarPanel(
      ),
      
      # Main Panel
      mainPanel(
        tabsetPanel(
          tabPanel("Table", dataTableOutput("table")),
          tabPanel("Plot", plotlyOutput("plot"))
        )
      )
   )
)

# Server
server <- function(input, output) {
   
   output$plot <- renderPlotly({
      positions_df %>%
       group_by(round, position) %>%
       summarize(low = min(z_sum),
                 mid = median(z_sum) %>% round(2),
                 high = max(z_sum),
                 n = n()) %>%
       arrange(round) %>%
       ggplot(aes(y=mid, x=round, color=n)) +
       geom_pointrange(aes(ymax = high,ymin = low)) +
       labs(x="Round (ADP)", 
            y="Z_Score") +
       facet_wrap(~ position) -> rv
     
     ggplotly(rv)
   })
   
   output$table <- renderDataTable({
     main_df
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

