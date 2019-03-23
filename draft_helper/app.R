library(shiny)
library(plotly)
library(tidyverse)

source("chooser.R")

main_df <- readRDS("../data/draft_dataset.Rds")
positions_df <- main_df %>%
  mutate(C = grepl("C", display_position)) %>%
  mutate(`1B` = grepl("1B", positions)) %>%
  mutate(`2B` = grepl("2B", positions)) %>%
  mutate(SS = grepl("SS", positions)) %>%
  mutate(`3B` = grepl("3B", positions)) %>%
  mutate(LF = grepl("LF", positions)) %>% 
  mutate(CF = grepl("CF", positions)) %>% 
  mutate(RF = grepl("RF", positions)) %>%
  mutate(SP = grepl("SP", positions)) %>%
  mutate(RP = grepl("RP", positions)) 

C <- positions_df %>%
  filter(C) %>%
  mutate(position = "C")

`1B` <- positions_df %>%
  filter(`1B`) %>%
  mutate(position = "1B")

`2B` <- positions_df %>%
  filter(`2B`) %>%
  mutate(position = "2B")

`3B` <- positions_df %>%
  filter(`3B`) %>%
  mutate(position = "3B")

SS <- positions_df %>%
  filter(SS) %>%
  mutate(position = "SS")

LF <- positions_df %>%
  filter(LF) %>%
  mutate(position = "LF")

CF <- positions_df %>%
  filter(CF) %>%
  mutate(position = "CF")

RF <- positions_df %>%
  filter(RF) %>%
  mutate(position = "RF")

SP <- positions_df %>% 
  filter(SP) %>% 
  mutate(position = "SP")

RP <- positions_df %>% 
  filter(RP) %>% 
  mutate(position = "RP")

positions_df_ex <- C %>%
  bind_rows(`1B`) %>%
  bind_rows(`2B`) %>%
  bind_rows(`3B`) %>%
  bind_rows(SS) %>%
  bind_rows(LF) %>% 
  bind_rows(CF) %>% 
  bind_rows(RF) %>% 
  bind_rows(SP) %>%
  bind_rows(RP) %>%
  unique() %>%
  select(-(positions:RP))

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Tom's Draft Cheat Sheet"),
   
   # Sidebar
   sidebarLayout(
      sidebarPanel(
        # many thanks to rstudio/shiny-examples/036-custom-input-control/
        chooserInput("chooser", "Available Players", "Drafted Players",
                     main_df$Name, c(), size = 25, multiple = TRUE)
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
server <- function(input, output, session) {
  
  output$selection <- renderPrint(
    input$chooser
  )
  
  table_df <- reactive({
    main_df %>%
      select(-display_position) %>%
      filter(!Name %in% input$chooser$right)
  })
  
  plot_df <- reactive({
    positions_df_ex %>%
      filter(!Name %in% input$chooser$right)
  })
   
  output$plot <- renderPlotly({
      plot_df() %>%
       group_by(position, round) %>%
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
     table_df()
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

