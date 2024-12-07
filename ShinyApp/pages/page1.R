# Libraries from utils.R

get_dir <- function(category){
  file_path <- paste0("../Data/Player_Comparison_Data/", category, "/data.RData")
  return( file_path )
}

# Define UI
page1_ui <- function(){
  fluidPage(    
    sidebarLayout(
      sidebarPanel(
        id = "page2_sidebar",  # ID for sidebar panel

        # Dropdown with multiple choices for League selection
        selectInput(
          inputId = "selected_field",
          label = "Player Position :",
          choices = c("Advance", "Attack", "Defense", "GoalKeeping"),
          selected = c("Attack"),
          selectize = FALSE
        ),
        uiOutput("dynamic_input"),

        uiOutput("seasons"),
      ),
      
      mainPanel(
        id = "page1_main",  # ID for main panel
        plotOutput("radarPlot"),
        tableOutput("statsTable")
      )
    )
  )
}


# Define server logic
# Define server logic
page1_server <- function(input, output, session) {
  # Create a reactive value to store the data
  data <- reactive({
    req(input$selected_field)  # Ensure `selected_field` is not NULL
    
    # Define a new environment to load the data
    data_env <- new.env()
    
    # Get the file path and load the data
    data_path <- get_dir(input$selected_field)    
    load(data_path, envir = data_env)  # Load the data into `data_env`    
    dataset <- as.data.frame(data_env$data)
    
    return(dataset)  # Return the data frame
  })


  # Render the dynamic player selection inputs based on loaded data
  output$dynamic_input <- renderUI({
    req(data())  # Ensure `data` is loaded and accessible
    
    player_choices <- unique(data()$Player)  # Access the loaded data
    
    # Create a list of selectInputs for players
    tagList(
      selectInput(
        inputId = "player1",
        label = "Select Player 1:",
        choices = player_choices,
        selected = "Jude Bellingham"
      ),
      selectInput(
        inputId = "player2",
        label = "Select Player 2:",
        choices = player_choices,
        selected = "Harry Kane"
      ),
      selectInput(
        inputId = "player3",
        label = "Select Player 3:",
        choices = player_choices,
        selected = "Gerard Pique"
      )
    )
  })

  expanded_state <- reactiveVal(list())

  player_list <- reactive({
    req(input$player1, input$player2, input$player3)
    unique(c(input$player1, input$player2, input$player3))
  })

  output$seasons <- renderUI({
    req(player_list())
    
    checkbox_list <- lapply(player_list(), function(player){
      # Get unique seasons for each player
      seasonz <- unique(data() %>% filter(tolower(trimws(Player)) == tolower(trimws(player))) %>% pull(Season))
      
      # Toggle button and season selectInput
      toggle_button <- actionButton(
        inputId = paste0("toggle_", player),
        label = paste("Select season for", player),
        class = "btn btn-primary"
      )
      
      # Select input for season, using JavaScript to toggle visibility
      season_select <- conditionalPanel(
        condition = paste0("input.toggle_", player, " % 2 == 1"),
        class = "custom-select",
        selectInput(inputId = paste0("season_", player), label = NULL, choices = seasonz, selected = seasonz[1])
      )
      
      tagList(toggle_button, season_select)
    })
    
    do.call(tagList, checkbox_list)
  })
  
  # Reactive expression to filter data based on selections
  selected_data <- reactive({
    req(input$player1, input$player2, input$player3)
    
    # Access season inputs dynamically
    season1 <- input[[paste0("season_", input$player1)]]
    season2 <- input[[paste0("season_", input$player2)]]
    season3 <- input[[paste0("season_", input$player3)]]
    
    # Filter data for selected players and seasons
    data() %>% filter(
      (tolower(trimws(Player)) == tolower(trimws(input$player1)) & tolower(trimws(Season)) == tolower(trimws(season1))) |
      (tolower(trimws(Player)) == tolower(trimws(input$player2)) & tolower(trimws(Season)) == tolower(trimws(season2))) |
      (tolower(trimws(Player)) == tolower(trimws(input$player3)) & tolower(trimws(Season)) == tolower(trimws(season3)))
    ) %>% as.data.frame()
  })

  # Render Radar Plot
  output$radarPlot <- renderPlot(
    bg = rgb(1, 0.97, 1, 0.98),
    height = 350,
    {
      # Prepare data for radar chart
      radar_data <- selected_data() %>%
        select(-Player, -Season)  # Ensure only numerical stats remain

      # Add min and max rows for fmsb radar chart scaling
      radar_data <- rbind(rep(max(radar_data), ncol(radar_data)), rep(0, ncol(radar_data)), radar_data)
      
      # Colors for radar chart
      colors_border <- c("#40c9a252", "#a056b256", "#ffd16677")
      colors_in <- c(rgb(0.25, 0.78, 0.64, 0.45), rgb(0.63, 0.34, 0.7, 0.45), rgb(1, 0.87, 0.34, 0.5))
      
      # Plot radar chart
      radarchart(
        radar_data, axistype = 1, cex = 1,
        pcol = colors_border, pfcol = colors_in, plwd = 4, plty = 1,
        cglcol = "grey15", cglty = 1, axislabcol = "grey25",
        cglwd = 0.8,
        calcex = 0.9,
        palcex = 0.9,
        vlcex = 0.9
      )
      
      # Add legend
      legend_labels <- paste(selected_data()$Player, selected_data()$Season, sep = " - ")
      legend("right", legend = legend_labels, bty = "n", pch = 20, 
            col = colors_in, text.col = "grey20", cex = 1.2, pt.cex = 3)
    }
  )

  
  # Render stats table
  output$statsTable <- renderTable({
    selected_data()
  })
}