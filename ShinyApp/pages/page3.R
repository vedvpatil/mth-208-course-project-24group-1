# Load libraries from utils.R

# Load the dataset from dat.Rdata
load("../Data/dat.Rdata")

# Define a linear combination function for market value
calculate_market_value <- function(dat) {
  marketval<- dat
  marketval[6:38] <- lapply(marketval[6:38], function(x) x / max(x, na.rm = TRUE))
  
  marketval <- marketval %>%
    mutate(market_value = case_when(
      Pos == "FW" ~ 54.84 +                                 # Intercept
        marketval$Age * -32.05 +
        marketval$Total_Passes_Completed * 77.03 +
        marketval$Total_Pass_Percentage * -15.21 +
        marketval$Progression_Carries * 2.60 +
        marketval$Key_Passes * -32.03 +
        marketval$Passes_Into_Penalty_Area * -1.68 +
        marketval$Touches * -46.62 +
        marketval$Take_Ons_Attempted * 61.12 +
        marketval$Take_Ons_Wins * -32.46 +
        marketval$Goals.Assists * 11.14 +
        marketval$Shots * -14.85 +
        marketval$Shots_on_Target_Percentage * 0.83 +
        marketval$Goals_per_Shot * -37.2 +
        marketval$Goals_per_Shot_on_Target * 33.62 +
        marketval$Shot_Creating_Actions * -5.75 +
        marketval$Goal_Creating_Actions * 27.45,
      
      Pos == "MF" ~ 56.67083 +  # Intercept
        marketval$Total_Pass_Percentage * 12.39480 +
        marketval$Passes_Into_Penalty_Area * 10.36908 +
        marketval$Take_Ons_Wins * -9.18903 +
        marketval$Shots_on_Target_Percentage * 10.09618 +
        marketval$Shot_Creating_Actions * 36.49170 +
        marketval$Tackles_Win_Percentage * 1.71398 +
        marketval$Total_Blocks * 28.13582 +
        marketval$Fouls * -0.05927 +
        marketval$Age * -74.08313 +
        marketval$Progression_Carries * -33.24323 +
        marketval$Touches * 23.45878 +
        marketval$Goals.Assists * 63.62505 +
        marketval$Goals_per_Shot * -28.57321 +
        marketval$Goal_Creating_Actions * -27.10123 +
        marketval$Challenges * -54.04256 +
        marketval$Total_Passes_Completed * -44.67468 +
        marketval$Key_Passes * -25.51366 +
        marketval$Take_Ons_Attempted * 15.29182 +
        marketval$Shots * -7.91063 +
        marketval$Goals_per_Shot_on_Target * 21.08068 +
        marketval$Tackles * 56.78303 +
        marketval$Challenges_Percentage * 3.48653 +
        marketval$Interceptions * -0.89116 +
        marketval$Clearances * 22.13257,
      
      Pos == "DF" ~ 73.171 +                             
        marketval$Age * -37.749 +
        marketval$Total_Passes_Completed * 53.092 +
        marketval$Total_Pass_Percentage * -17.891 +
        marketval$Progression_Carries * -15.716 +
        marketval$Key_Passes * 11.389 +
        marketval$Touches * 7.107 +
        marketval$Tackles * 5.026 +
        marketval$Tackles_Win_Percentage * -13.213 +
        marketval$Total_Blocks * -18.162 +
        marketval$Challenges * -42.375 +
        marketval$Challenges_Percentage * -4.746 +
        marketval$Interceptions * 10.103 +
        marketval$Clearances * -16.683,
      
      Pos == "GK" ~ 26.0974 +
        marketval$Touches * 50.1457 +
        marketval$Saves * -0.2651 +
        marketval$Saves_Percentage * -46.76 +
        marketval$Clean_Sheets_Percentage * 3.12,
      
      TRUE ~ NA_real_  # default for any positions not covered above
    ))
  
  return(marketval)
}

# Apply the linear combination function
players_data <- calculate_market_value(dat)

# UI
page3_ui <- function(){
  fluidPage(
    titlePanel("Transfer Market Review"),
    
    sidebarLayout(
      sidebarPanel(
        h3("Please Select :"),
        id = "page1_sidebar",  # ID for sidebar panel
        # selectInput("selected_player", "Choose Player", 
        #             choices = unique(players_data$Player), 
        #             selected = unique(players_data$Player)[1]),
        selectInput(
          inputId = "selected_player",
          label = "Choose Player",
          choices = NULL,  # Leave choices NULL for server-side loading
          selected = NULL,
          selectize = 10
        ),
        width = 3  # Adjust sidebar width if needed
      ),
      
      mainPanel(
        h3(textOutput("market_value_text")),
        plotOutput("market_value_trend_plot"),
        plotOutput("performance_plot"),
        plotOutput("age_distribution_plot"),
        width = 9  # Adjust main panel width if needed
      )
    )
  )
}

# Server
page3_server <- function(input, output, session){
  updateSelectInput(
    session,
    inputId = "selected_player",
    choices = unique(players_data$Player),
    selected = "Cristiano Ronaldo"
  )
  
  # Reactive filter for selected player data
  selected_player_data <- reactive({
    players_data %>%
      filter(Player == input$selected_player)
  })
  
  selected_player_data1 <- reactive({
    players_data %>%
      filter(Player == input$selected_player, season==2024)
  })
  
 
  # Display player market value
  output$market_value_text <- renderText({
    paste("Market Value ($)", 
          formatC(selected_player_data1()$market_value, 
                  format = "f", big.mark = ","))
  })
  
 
  
  
  # Plot 1: Histogram of Market Value by Position
  output$performance_plot <- renderPlot({
    # Filter data for the 2024 season
    filtered_data <- players_data %>% filter(season == 2024)
    
    ggplot(filtered_data, aes(x = market_value, fill = Pos)) +
      geom_histogram(binwidth = 5, position = "dodge", color = "black") +
      labs(title = "Market Value by Position for 2024", 
           x = "Market Value", 
           y = "No. of Players") +
      scale_fill_manual(values = c("GK" = "#3498DB", "DF" = "#2C3E50", "MF" = "#18BC9C", "FW" = "#E74C3C")) +
      theme_minimal()
  })
  
  
  # Plot 2: Market Value Trend over Time
  output$market_value_trend_plot <- renderPlot({
    ggplot(selected_player_data(), aes(x = season, y = market_value)) +
      geom_line(color = "#E74C3C", size = 1) +
      geom_point(color = "#3498DB", size = 3) +
      labs(title = "Market Value Trend", x = "Season", y = "Market Value") +
      theme_minimal()
  })
  

  # Plot 3: Market Value by Nationality for the 2024 Season
  output$age_distribution_plot <- renderPlot({
    # Filter data for the 2024 season and specific nationalities
    filtered_data <- players_data %>%
      filter(season == 2024, Nation %in% c("ARG", "BRA", "CRO", "USA", "GER", "FRA", "ITA", "ENG", "ESP", "SUI", "BEL", "NED"))
    
    # Define custom colors for each nationality
    custom_colors <- c(
      "ARG" = "#F1C40F",  # Yellow
      "BRA" = "#2ECC71",  # Green
      "CRO" = "#E74C3C",  # Red
      "USA" = "#3498DB",  # Blue
      "GER" = "#9B59B6",  # Purple
      "FRA" = "#1ABC9C",  # Teal
      "ITA" = "#FF6347",  # Tomato red
      "ENG" = "#D35400",  # Orange
      "ESP" = "#F39C12",  # Golden Yellow
      "SUI" = "#2980B9",  # Blue
      "BEL" = "#8E44AD",  # Violet
      "NED" = "#E67E22"   # Orange
    )
    
    ggplot(filtered_data, aes(x = reorder(Nation, market_value), y = market_value, fill = Nation)) +
      geom_bar(stat = "identity") +
      labs(title = "Market Value by Nationality for the 2024 Season", 
           x = "Nationality", 
           y = "Market Value") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for readability
      scale_fill_manual(values = custom_colors)  # Use the defined custom colors
  })
}