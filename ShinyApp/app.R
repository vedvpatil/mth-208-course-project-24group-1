library(shiny)
library(shinyjs)


# Source all required files
source("utils.R")

# Define UI
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  theme = "bootstrap.vapor.css",
  navbarPage(
    title = "Football Colosseum",
    id = "navbar",
    collapsible = TRUE,
    inverse = TRUE,
    tabPanel("Squad Data Visualisation", page0_ui()),
    tabPanel("Player/Club Analysis", page2_ui()),
    tabPanel("Player Comparison", page1_ui()),
    tabPanel("Market Analysis", page3_ui())
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive value to track which page to show
  page <- reactiveVal("main")
  
  # Navigation actions
  observeEvent(input$go_to_page0, { page("page0") })
  observeEvent(input$go_to_page2, { page("page2") })
  observeEvent(input$go_to_page1, { page("page1") })
  observeEvent(input$go_to_page3, { page("page3") })
  
  # Dynamic UI rendering
  output$main_content <- renderUI({
    switch(
      page(),
      "page0" = page0_ui(),
      "page2" = page2_ui(),
      "page1" = page1_ui(),
      "page3" = page3_ui(),
      "main" = h2("Welcome to the Main Page")
    )
  })
  
  # Server logic for each page
  page0_server(input, output, session)
  page2_server(input, output, session)
  page1_server(input, output, session)
  page3_server(input, output, session)
}

# Run the app
shinyApp(ui = ui, server = server)
