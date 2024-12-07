page0_ui <- function() {
  fluidPage(    
    useShinyjs(),
    sidebarLayout(
      sidebarPanel(
        id = "page2_sidebar",  # ID for sidebar panel

        # Dropdown with multiple choices for League selection
        selectInput(
          inputId = "selected_leagues",
          label = "Select Leagues",
          choices = c("Bundesliga", "LaLiga", "Ligue_1", "Premier_League", "Serie_A"),
          selected = c("Premier_League", "Serie_A"),
          multiple = TRUE,
          selectize = TRUE
        ),

        # Placeholder for dynamic, collapsible season checkboxes
        uiOutput("dynamic_season_checkboxes"),

        selectInput(
          inputId = "category",
          label = "Select Parameter Category",
          choices = c("Attack", "Advance", "Defense", "Fair_Play", "Goalkeeping", "win_loss"),
          selected = NULL,
        ),

        selectInput(
          inputId = "para1",
          label = "Select X - Axis",
          choices = NULL,
          selected = NULL
        ),

        selectInput(
          inputId = "para2",
          label = "Select Y - Axis",
          choices = NULL,
          selected = NULL
        ),
      ),

      mainPanel(
        id = "page2_main",  # ID for main panel
        plotOutput("plot1")
      )
    )
  )
}

page0_server <- function(input, output, session) {

  # disable("para1")
  # disable("para2")
  
  observeEvent(input$category, {
    req(input$category)  # Ensure input$category is not NULL
    # enable("para1")
    # enable("para2")
    # Define choices for each category
    category_choices <- list(
      Attack = c("Goal_Difference","Total_Goals_Scored","Total_Goals_Conceded","Home_Goals_Scored","Home_Goals_Conceded","Home_Goal_Difference","Away_Goals_Scored","Away_Goals_Conceded","Away_Goal_Difference","Assists","Penalty_Attempted","Penalty_Scored","Shots","Shots_on_Target","Shots_on_Target_Percentage","Goals_per_Shot","Goals_per_Shot_on_Target","Average_Shot_Distance","Free_Kick_Shots","Shot_Creating_Actions","Live_Pass_Shot_Creation","Other_Type_Shot_Creation","Goal_Creating_Actions","Live_Pass_Goal_Creation","Other_Type_Goal_Creation"),
      Advance = c("Player_Count","Progression_Carries","Progression_Passes","Total_Passes_Completed","Total_Passes_Attempted","Total_Pass_Percentage","Short_Passes_Completed","Short_Passes_Attempted","Short_Pass_Percentage","Long_Passes_Completed","Long_Passes_Attempted","Long_Pass_Percentage","Medium_Passes_Completed","Medium_Passes_Attempted","Medium_Pass_Percentage","Live_Pass","Dead_Pass","Free_Kicks","Through_Balls","Switches","Crosses","Throw_Ins","Corner_Kicks","Offside","Possession","Touches","Take_Ons_Attempted","Take_Ons_Wins","Take_On_Win_Percentage","Take_Ons_Tackled","Take_Ons_Tackled_Percentage"),
      Defense = c("Tackles","Tackles_Won","Tackles_Win_Percentage","Challenges","Challenges_Won","Challenges_Percentage","Total_Blocks","Shots_Blocked","Passes_Blocked","Interceptions","Clearances","Errors"),
      Fair_Play = c("Yellow_Card","Red_Card","Second_Yellow_Card","Total_Red_Cards","Fouls"),
      Goalkeeping = c("Goalkeepers_Used","Goals_Conceded","Saves","Saves_Percentage","Clean_Sheets","Clean_Sheets_Percentage","Penalty_Kicks_Saves","Penalty_Kicks_Save_Percentage"),
      win_loss = c("Rank","Wins","Draws","Losses","Pts","Home_Wins","Away_Wins","Home_Draws","Away_Draws","Home_Losses","Away_Losses","Home_Pts","Away_Pts")
    )
    
    # Get choices for the selected category
    selected_choices <- category_choices[[input$category]]
    
    # Update the choices in para1 and para2 based on selected category
    updateSelectInput(session, "para1",
                      choices = selected_choices,
                      selected = selected_choices[1]  # Set default selection to the first choice
    )
    updateSelectInput(session, "para2",
                      choices = selected_choices,
                      selected = selected_choices[2]  # Set default selection to the second choice, if available
    )
  })

  # Track the expanded/collapsed state of each league's checkbox
  expanded_state <- reactiveValues()
  
  output$dynamic_season_checkboxes <- renderUI({
    req(input$selected_leagues)  # Only proceed if leagues are selected

    seasons <- c("2020", "2021", "2022", "2023", "2024")
    
    # Create a collapsible list for each league
    checkbox_list <- lapply(input$selected_leagues, function(league) {
      # Ensure state exists for each league
      if (is.null(expanded_state[[league]])) {
        expanded_state[[league]] <- FALSE
      }
      
      # Toggle button for show/hide effect
      toggle_button <- actionButton(
        inputId = paste0("toggle_", league),
        label = paste("Select seasons for", league),
        class = "btn btn-primary"
      )
      
      # Conditional panel for the season checkboxes, controlled by expanded state
      checkboxes <- conditionalPanel(
        condition = sprintf("input['%s'] %% 2 == 1", paste0("toggle_", league)),
        class = "custom-checkbox",
        checkboxGroupInput(
          inputId = paste0("season_", league),
          label = NULL,
          choices = seasons,
          selected = c("2023", "2024")
        )
      )

      # Return the toggle button and checkboxes in a tagList
      tagList(toggle_button, checkboxes)
    })
    
    # Return the list of league checkboxes
    do.call(tagList, checkbox_list)
  })

  # Plot output based on selected leagues and seasons
    output$plot1 <- renderPlot({
    req(input$selected_leagues)  # Ensure leagues are selected
    
    # Initialize an empty data frame to store combined data
    Data <- data.frame()
    
    # Read and combine data files for each selected league and season
    lapply(input$selected_leagues, function(league) {
      selected_seasons <- as.numeric(input[[paste0("season_", league)]])
      if (is.null(selected_seasons)) return(NULL)  # Skip if no seasons selected

      lapply(selected_seasons, function(season) {
        season_short <- season - 2000  # Convert to short season format

        # Construct the file path
        file_path <- paste0("../Data/", league, "/", league, "_", season_short, "/Squad/", input$category , ".csv")
        
        # Check if file exists and read if available
        if (file.exists(file_path)) {
          dat <- read.csv(file_path)
          # ex : read.csv("Data/Bundesliga/Bundesliga_24/Squad/win_loss.csv")
          
          # Ensure the columns are numeric
          X <- as.numeric(dat[[input$para1]])
          Y <- as.numeric(dat[[input$para2]])
          data <- data.frame("X" = X, "Y" = Y, "Name" = c(paste0(league, "_", season_short)))
          
          # Append to main Data
          Data <<- rbind(Data, data)
        } else {
          warning(paste("File not found:", file_path))
          
        }
      })
    })
    
    # Ensure there is data to plot
    if (nrow(Data) > 0) {
      Data <- group_by(Data,Data$Name)
      unique_names <- unique(Data$Name)
      name_colors <- rainbow(length(unique_names))
      names(name_colors) <- unique_names
      
      # Create a color vector for plotting based on the Name of each row
      Data$color <- name_colors[Data$Name]
      
      # Plot Wins vs. Home Wins with custom colors and point types
      plot(
        Data$X, Data$Y,
        main = paste0(input$para1 , "  V/s  " , input$para2),
        xlab = gsub("_", " ", input$para1),
        ylab = gsub("_", " ", input$para2),
        pch = 19,  # Filled circles
        col = Data$color
      )
      
      # Add a legend in the top-right corner
      legend(
        "topright",
        legend = unique_names,
        col = name_colors,
        pch = 20,  # Same point type as plot
        title = "DataSet"
      )
    } else {
      plot.new()
      text(0.5, 0.5, "No data available for selected seasons", cex = 1.5)
    }
  })

}
