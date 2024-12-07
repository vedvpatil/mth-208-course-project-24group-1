library(GGally)
library(viridis)
library(hrbrthemes)
library(plotly)
library(DT)

### Pre define functions for ease of exicution of the page : 

get_player_dir = function( league , season){
  season <- as.numeric(season) - 2000
  file_path <- paste0("../Data/", league, "/", league, "_", season, "/Player")
  return( file_path )
}

get_squad_dir = function( league , season){ 
  season <- as.numeric(season) - 2000
  file_path <- paste0("../Data/", league, "/", league, "_", season, "/Squad")
  return( file_path )
}

page2_ui <- function() {
  fluidPage(    
    
    sidebarLayout(
      sidebarPanel(
        id = "page2_sidebar",  # ID for sidebar panel
        
        # Dropdown with multiple choices for League selection
        selectInput(
          inputId = "league",
          label = "Select League:",
          choices = c("Bundesliga", "LaLiga", "Ligue_1", "Premier_League", "Serie_A"),
          selected = "Premier_League", 
        ),
        
        # Dropdown with multiple choices for Season
        selectInput(
          inputId = "season" , 
          label = "Select Season:" , 
          choices = 2020:2024, 
          selected = 2024
        ) , 
        
        # If the user wishes to have squad specific analysis 
        # or player specific analysis 
        selectInput(
          inputId = "level" , 
          label = "Level of Data:" , 
          choices = c( "Squad" , "Player" , "Player (Goalkeeper)") , 
          selected = "Squad"
        ) ,
        
        # Dynamic Dropdown depending upon the level chosen by the player 
        uiOutput("level_choise") , 
        width = 3
      ),
      
      mainPanel(
        id = "page1_main",  # ID for main panel
        # plotOutput("plot1") ,
        # textOutput('text') , 
        # tableOutput('tabel')
        uiOutput("main")
      )
    )
  )
}

page2_server <- function(input, output, session) {
  
  # Set the panels in the UI according to the, 
  # level chosen by the user above ! 
  output$level_choise = renderUI(
    {
      req( input$level )
      # For player specifc statistical analysis 
      # we further filter the data according to 
      # 1. The club 
      # 2. The player himself
      if( input$level == "Player" ){ 
        tagList(
          # Ask for the available clubs : 
          # Note: Options to be made 
          # available later in the script 
          selectInput(
            'squad' , 
            'Select Club:' , 
            choices = NULL , 
            selected = NULL 
          ) ,
          # Ask for the Player from the chosen 
          # squad. Again choises to be updated 
          # later in the script. 
          selectInput(
            'player' , 
            'Select Player:' , 
            choices = NULL  , 
            selected = NULL 
          ) ,
          # The kind of football specific analysis: 
          selectInput(
            'pkind' , 
            'Kind of Statistic:' , 
            choices = c( 'General' , 
                         'Attack' , 
                         'Advance' , 
                         'Defense' , 
                         'Fair_Play') , 
            selected = 'General' 
          )
        )
      }
      else if (input$level == 'Squad' ){ 
        tagList(
          # The kind of football specific analysis: 
          selectInput(
            'kind' , 
            'Kind of Statistic:' , 
            choices = c( 'win_loss' , 
                         'Attack' , 
                         'Advance' , 
                         'Defense' ,
                         'Goalkeeping',
                         'Fair_Play') , 
            selected = 'win_loss'
          )
        )
      }
      else{ 
        tagList(
          # Ask for the available clubs : 
          # Note: Options to be made 
          # available later in the script 
          selectInput(
            'squad' , 
            'Select Club:' , 
            choices = NULL , 
            selected = NULL 
          ) ,
          # Ask for the Player from the chosen 
          # squad. Again choices to be updated 
          # later in the script. 
          selectInput(
            'player_gk' , 
            'Select Player:' , 
            choices = NULL  , 
            selected = NULL 
          ) 
        )
      }
    }
  )
  
  # path to the player data folder for the given season 
  # and league
  squad_dir = reactive({
    get_squad_dir( input$league , input$season )
  })
  
  # path to the player data folder for the given season 
  # and league
  player_dir = reactive({
    get_player_dir( input$league , input$season ) 
  })
  
  
  observe({
    if( input$level == 'Player' || input$level == 'Player (Goalkeeper)'){
      # read a relevant csv file
      x = read.csv( paste0( squad_dir() , '/' , 'Attack.csv') )
      # Update the Available Squad Choises:
      updateSelectInput(
        inputId = 'squad' ,
        label = 'Select Club:' ,
        choices = x$Squad
      )
    }
  })
  
  observe({
    if( input$level == 'Player'){ 
      req(input$squad)
      # read the relevant CSV file
      x = read.csv(paste0(player_dir(), '/', 'General.csv'))
      x = x %>% filter( Squad == input$squad ) %>% 
        arrange( Player )
      # Update the Available Player Choises :
      updateSelectInput(
        inputId = 'player' ,
        label = 'Select Player:' ,
        choices = x$Player ,
        selected = NULL
      )
    }
    else if( input$level == 'Player (Goalkeeper)'){
      req( input$squad )
      # read the relevant CSV file
      x = read.csv(paste0(player_dir(), '/', 'GoalKeeping.csv'))
      x = x %>% filter( Squad == input$squad ) %>% 
        arrange( Player )
      # Update the Available Player Choises :
      updateSelectInput(
        inputId = 'player_gk' ,
        label = 'Select Player:' ,
        choices = x$Player ,
        selected = NULL
      ) 
    }
  })
  
  sfile = reactive({
    req( input$kind )
    read.csv( paste0( squad_dir() , '/' , input$kind , '.csv' ) )
  })
  
  pfile = reactive({
    req( input$pkind )
    read.csv( paste0( player_dir() , '/' , input$pkind , '.csv' ) )
  })
  
  output$main = renderUI({
    if (input$level == 'Squad') { 
      req(input$kind)
      if (input$kind == 'win_loss') {
        tagList(
          plotOutput('plot', height = "600px"),
          # verbatimTextOutput('summary'),
          DTOutput('table')
        )
      }
      else if (input$kind == 'Attack') {
        tagList(
          plotOutput('plot1', height = "600px"),
          plotOutput('plot2', height = "600px"),
          plotOutput('plot3', height = "600px"),
          # verbatimTextOutput('summary'),
          DTOutput('table')
        )
      }
      else if (input$kind == 'Advance') {
        tagList(
          plotOutput('plot1', height = "600px"),
          plotOutput('plot2', height = "600px"),
          # verbatimTextOutput('summary'),
          DTOutput('table')
        )
      }
      else if (input$kind == 'Defense') {
        tagList(
          plotOutput('plot1', height = "600px"),
          plotOutput('plot2', height = "600px"),
          # verbatimTextOutput('summary'),
          DTOutput('table')
        )
      }
      else if (input$kind == 'Goalkeeping') {
        tagList(
          plotOutput('plot1', height = "600px"),
          plotOutput('plot2', height = "600px"),
          plotOutput('plot3', height = "600px"),
          # verbatimTextOutput('summary'),
          DTOutput('table')
        ) 
      }
      else { 
        tagList(
          plotOutput('plot1', height = "600px"),
          plotOutput('plot2', height = "600px"),
          # verbatimTextOutput('summary'),
          DTOutput('table')
        ) 
      }
    }
    else if (input$level == 'Player') {
      req(input$pkind)
      if (input$pkind == 'General') {
        tagList(
          plotOutput('plot1', height = "600px"),
          DTOutput('table')
        )
      }
      else if (input$pkind == 'Attack') {
        tagList(
          DTOutput('table')
        )
      }
      else if (input$pkind == 'Defense') {
        tagList(
          DTOutput('table')
        )
      }
      else if (input$pkind == 'Fair_Play') {
        tagList(
          DTOutput('table')
        )
      }
      else {
        tagList(
          plotOutput('plot1', height = "600px"),
          DTOutput('table')
        )
      }
    }
    else { 
      tagList(
        plotOutput('plot1', height = "600px"),
        DTOutput('table')
      )
    }
  })
  
  observe({
    req( input$level)
    if( input$level == 'Squad' ){
      req(input$kind)
      if( input$kind == 'win_loss'){
        output$table = renderDT({ sfile() } )
        output$summary = renderPrint({ summary( sfile() ) } )
        # League standings plot ( default )
        output$plot = renderPlot({
          dat = sfile()
          teams = dim( dat )[1]
          dat$Squad =  factor( dat$Squad , levels = dat$Squad )
          dat$Relegated = factor(c( rep("NO" , teams - 3) , "Yes" , "Yes" , "Yes" ))
          ggplot( data = dat ) +
            geom_col( aes( x = Squad , y = Pts , fill = Relegated  ) ,
                      col = "black" , linewidth = 2 )  +
            ggtitle('Final League Standings') +
            ylab('Points') +
            geom_text( aes( x = Squad , y = Pts - 7 , label = paste0( rep("Pos" , teams ) , 1:teams)) , fontface = "bold"  ) +
            theme(axis.text.x = element_text(face = "bold" ,angle = 45 , size = 14 ) ,
                  plot.title = element_text(size = 30  , face = 'bold') ,
                  axis.title.x = element_text(size = 16 , face = 'bold' ) ,
                  axis.title.y = element_text(size = 16  , face = 'bold') )
        })      
      }
      else if( input$kind == "Attack" ){
        dat = sfile()
        teams = dim( dat )[1]
        output$table = renderDT({ dat } )
        output$summary = renderPrint({ summary( dat ) } )
        # plot 1 
        output$plot1 = renderPlot({
          foodf = dat %>% 
            mutate( Penalty_Scored_per_Penelty_Attempted = Penalty_Scored/Penalty_Attempted , 
                    Shots_on_Target_per_Shot = Shots_on_Target_Percentage/100 ) %>% 
            select(  Shots_on_Target_per_Shot , 
                     Goals_per_Shot , 
                     Goals_per_Shot_on_Target , 
                     Penalty_Scored_per_Penelty_Attempted ) %>% 
            rbind( rep( 1 , 4 ) , rep( 0 , 4  ) , . )
          op = par( mar = c( 1 , 1, 1, 1.2) )
          par( mfrow= c( 4 ,5 ))
          for( i in 1:teams ){ 
            radarchart( foodf[ c( 1 , 2 , i+2 ) , ] , 
                        axistype = 4  , pcol = "blue" , 
                        pfcol = scales::alpha( "blue" , 0.5 ) , 
                        cglwd = 0.5  , 
                        axislabcol = "gray" , 
                        vlcex = 0.8   , title = dat[ i , 1 ])  
          }
        })
        dat$Squad = factor( dat$Squad , levels=dat$Squad )
        foodf <- data.frame( Squad = rep( dat$Squad , 2 ) , 
                             Value = c( dat$Total_Goals_Scored  , dat$Total_Goals_Conceded) , 
                             Type = factor( c( rep( "Goals Scored" , teams ) , rep("Goals Conceded" , teams ) ) ) )
        # plot 2 
        output$plot2 = renderPlot({
          ggplot( data = foodf , aes( x = Squad , y = Value , fill = Type ) ) + 
            geom_col( stat = "identity",position = "dodge" ,col = "black") + 
            ggtitle("Club Goals Scored and Goals Conceded") + 
            theme(axis.text.x = element_text(face = "bold" ,angle = 45 , size = 14 ) , 
                  plot.title = element_text(size = 30  , face = 'bold') , 
                  axis.title.x = element_text(size = 16 , face = 'bold' ) , 
                  axis.title.y = element_text(size = 16  , face = 'bold') )
        })
        # plot 3 
        output$plot3 = renderPlot({
          ggparcoord( dat ,  
                      columns = c( 2, 4 , 11 , 15 , 22 , 25  ) , 
                      groupColumn =  1 , 
                      showPoints =T , 
                      boxplot =  T , 
                      title = "Attacking Analysis of Clubs") + 
            geom_line( linewidth = 2  , alpha = .5  ) + 
            geom_point( size = 3 ) + 
            geom_text(data = dat %>%
                        select(Squad) %>%
                        mutate(x = 1,
                               y = scale( dat$Total_Goals_Scored ) ),
                      aes(x = x, y = y, label = Squad , col = Squad),
                      inherit.aes = FALSE , 
                      hjust = 1.1 , 
                      size = 4  , 
                      fontface ="bold" , 
                      alpha = 0.7 ) + 
            theme(axis.text.x = element_text(face = "bold" , size = 12 ) , 
                  plot.title = element_text(size = 30  , face = 'bold') , 
                  axis.title.x = element_text(size = 16 , face = 'bold' ) , 
                  axis.title.y = element_text(size = 16  , face = 'bold') , 
                  legend.text =  element_text( size = 10 ) , 
                  legend.title = element_text( size = 12 , face = "bold"))
        })
      }
      else if( input$kind == "Advance" ){
        dat = sfile()
        teams = dim( dat )[1]
        output$table = renderDT({ dat } )
        output$summary = renderPrint({ summary( dat ) } )
        # plot 1 
        output$plot1 = renderPlot({
          foodf = dat %>% 
            select( Total_Pass_Percentage , 
                    Take_On_Win_Percentage , 
                    Take_Ons_Tackled_Percentage ) %>% 
            rbind( rep( 100 , 3 ) , rep( 0 , 3  ) , . )
          op = par( mar = c( 1 , 1, 1, 1.2) )
          par( mfrow= c( 4 ,5 ))
          for( i in 1:teams ){ 
            radarchart( foodf[ c( 1 , 2 , i+2 ) , ] , 
                        axistype = 4  , pcol = "blue" , 
                        pfcol = scales::alpha( "blue" , 0.5 ) , 
                        cglwd = 0.5  , 
                        axislabcol = "gray" , 
                        vlcex = 0.8   , title = dat[ i , 1 ])  
          }
        })
        output$plot2 = renderPlot({
          ggparcoord( dat  ,  
                      columns = c( 5 , 21 , 22 , 24 , 25 , 26 ) , 
                      groupColumn =  1 , 
                      showPoints =T , 
                      boxplot =  T , 
                      title = "Progression Analysis of Clubs") + 
            geom_line( size = 2 , alpha = .5  ) + 
            geom_point( size = 3 ) + 
            geom_text(data = dat %>%
                        select(Squad) %>%
                        mutate(x = 1,
                               y = scale( dat$Total_Passes_Completed ) ),
                      aes(x = x, y = y, label = Squad , col = Squad),
                      inherit.aes = FALSE , 
                      hjust = 1.1 , 
                      size = 4  , 
                      fontface ="bold" , 
                      alpha = 0.8 ) + 
            theme(axis.text.x = element_text(face = "bold" , size = 12 ) , 
                  plot.title = element_text(size = 30  , face = 'bold') , 
                  axis.title.x = element_text(size = 16 , face = 'bold' ) , 
                  axis.title.y = element_text(size = 16  , face = 'bold') , 
                  legend.text =  element_text( size = 10 ) , 
                  legend.title = element_text( size = 12 , face = "bold")) 
        })
        
      }
      else if( input$kind == 'Defense' ){
        dat = sfile()
        teams = dim( dat )[1]
        output$table = renderDT({ dat } )
        output$summary = renderPrint({ summary( dat ) } )
        output$plot1 = renderPlot({
          foodf <- data.frame( Squad = rep( dat$Squad , 2 ) , 
                               Value = c( dat$Tackles_Win_Percentage  , dat$Challenges_Percentage  ) , 
                               Type = factor( c( rep( "Tackel Win Percentage" , teams ) , rep("Challenge win Percentage" , teams) ) ) )
          ggplot( data = foodf , aes( x = Squad , y = Value , fill = Type ) ) + 
            geom_col( stat = "identity", position = "dodge" ,col = "black") + 
            ggtitle("Club Tackel Wins and Challenge Wins") + 
            theme(axis.text.x = element_text(face = "bold" ,angle = 45 , size = 14 ) , 
                  plot.title = element_text(size = 30  , face = 'bold') , 
                  axis.title.x = element_text(size = 16 , face = 'bold' ) , 
                  axis.title.y = element_text(size = 16  , face = 'bold') , 
                  legend.title = element_text( size = 16 , face = "bold") , 
                  legend.text = element_text( size = 12  ))
        })
        output$plot2 = renderPlot({
          ggparcoord(  dat,  
                       columns = c( 3, 5 , 8 , 11 , 12 ) , 
                       groupColumn =  1 , 
                       showPoints =T , 
                       boxplot =  T , 
                       title = "Defensive Analysis of Clubs") + 
            geom_line( size = 2 , alpha = .5  ) + 
            geom_point( size = 3 ) + 
            geom_text(data = dat %>%
                        select(Squad) %>%
                        mutate(x = 1,
                               y = scale( dat$Tackles_Won ) ),
                      aes(x = x, y = y, label = Squad , col = Squad),
                      inherit.aes = FALSE , 
                      hjust = 1.1 , 
                      size = 4.5  , 
                      fontface ="bold" , 
                      alpha = 0.8 ) + 
            theme(axis.text.x = element_text(face = "bold" , size = 12 ) , 
                  plot.title = element_text(size = 30  , face = 'bold') , 
                  axis.title.x = element_text(size = 16 , face = 'bold' ) , 
                  axis.title.y = element_text(size = 16  , face = 'bold') , 
                  legend.text =  element_text( size = 10 ) , 
                  legend.title = element_text( size = 12 , face = "bold"))
        })
      }
      else if( input$kind == 'Goalkeeping' ){
        dat = sfile()
        teams = dim( dat )[1]
        output$table = renderDT({ dat } )
        output$summary = renderPrint({ summary( dat ) } )
        output$plot1 = renderPlot({
          ggplot( data = dat ) + 
            geom_col( aes( x = Squad , 
                           y = Saves_Percentage , 
                           fill = Squad) , col = "black") + 
            geom_text( aes( x = Squad , 
                            y = Saves_Percentage-2   , 
                            label = Saves_Percentage ) , fontface = "bold"  ) + 
            ylab( "Percent of Shots Saved") + 
            theme(axis.text.x = element_text(face = "bold" ,angle = 45 , size = 14 ) , 
                  plot.title = element_text(size = 30  , face = 'bold') , 
                  axis.title.x = element_text(size = 16 , face = 'bold' ) , 
                  axis.title.y = element_text(size = 16  , face = 'bold') , 
                  legend.title = element_text( size = 16 , face = "bold") , 
                  legend.text = element_text( size = 12  )) + 
            theme( legend.position = "None")
        })
        output$plot2 = renderPlot({
          ggplot( data = dat ) + 
            geom_col( aes( x = Squad , 
                           y = Clean_Sheets_Percentage , 
                           fill = Squad) , col = "black") + 
            geom_text( aes( x = Squad , 
                            y = Clean_Sheets_Percentage-2   , 
                            label = Clean_Sheets_Percentage ) , fontface = "bold"  ) + 
            ylab( "Percentage of Clean Sheets Maintained") + 
            theme(axis.text.x = element_text(face = "bold" ,angle = 45 , size = 14 ) , 
                  plot.title = element_text(size = 30  , face = 'bold') , 
                  axis.title.x = element_text(size = 16 , face = 'bold' ) , 
                  axis.title.y = element_text(size = 16  , face = 'bold') , 
                  legend.title = element_text( size = 16 , face = "bold") , 
                  legend.text = element_text( size = 12  )) + 
            theme( legend.position = "None")
        })
        output$plot3 = renderPlot({
          ggplot( data = dat) + 
            geom_col( aes( x = Squad , 
                           y = Penalty_Kicks_Save_Percentage , 
                           fill = Squad) , col = "black") + 
            geom_text( aes( x = Squad , 
                            y = Penalty_Kicks_Save_Percentage+2   , 
                            label = Penalty_Kicks_Save_Percentage ) , fontface = "bold"  ) + 
            ylab( "Percentage of Penalty Kicks Saved") + 
            theme(axis.text.x = element_text(face = "bold" ,angle = 45 , size = 14 ) , 
                  plot.title = element_text(size = 30  , face = 'bold') , 
                  axis.title.x = element_text(size = 16 , face = 'bold' ) , 
                  axis.title.y = element_text(size = 16  , face = 'bold') , 
                  legend.title = element_text( size = 16 , face = "bold") , 
                  legend.text = element_text( size = 12  )) + 
            theme( legend.position = "None")
        })
      }
      else{ 
        dat = sfile()
        teams = dim( dat )[1]
        output$table = renderDT({ dat } )
        output$summary = renderPrint({ summary( dat ) } )
        output$plot1 = renderPlot({
          ggplot( data = dat ) + 
            geom_col( aes( x = Squad , 
                           y = Yellow_Card , 
                           fill = Squad) , col = "black") + 
            geom_text( aes( x = Squad , y = Yellow_Card - 3  , label = Yellow_Card ) , fontface = "bold"  ) + 
            ylab( "Number of Yellow Card Bookings") + 
            theme(axis.text.x = element_text(face = "bold" ,angle = 45 , size = 14 ) , 
                  plot.title = element_text(size = 30  , face = 'bold') , 
                  axis.title.x = element_text(size = 16 , face = 'bold' ) , 
                  axis.title.y = element_text(size = 16  , face = 'bold') , 
                  legend.title = element_text( size = 16 , face = "bold") , 
                  legend.text = element_text( size = 12  )) + 
            theme( legend.position = "None")
        })
        output$plot2 = renderPlot({
          
          ggplot( data = dat ) + 
            geom_col( aes( x = Squad , 
                           y = Fouls , 
                           fill = Squad) , col = "black") + 
            geom_text( aes( x = Squad , y = Fouls - 10  , label = Fouls ) , fontface = "bold"  ) + 
            ylab( "Total Number of Fouls Commited") + 
            theme(axis.text.x = element_text(face = "bold" ,angle = 45 , size = 14 ) , 
                  plot.title = element_text(size = 30  , face = 'bold') , 
                  axis.title.x = element_text(size = 16 , face = 'bold' ) , 
                  axis.title.y = element_text(size = 16  , face = 'bold') , 
                  legend.title = element_text( size = 16 , face = "bold") , 
                  legend.text = element_text( size = 12  )) + 
            theme( legend.position = "None")
        })
      }
    }
    else if ( input$level == 'Player'){ 
      req( input$pkind )
      req( input$player )
      dat = pfile()
      dat = dat[ dat$Player == input$player , ]
      output$table = renderDT({
        foo = as.data.frame( t( dat ) )
        colnames( foo ) = "Value"
        foo
      })
      if( input$pkind == 'General'){ 
        foo = dat %>% 
          select( Minutes_Per_Match_Played ,
                  Percent_of_Squad_Minutes_Played,
                  Matches_Started ) %>% 
          rbind(c( 90 , 100 , 38 ) , rep( 0 , 3 ) , . )
        output$plot1 = renderPlot({
          radarchart( foo, 
                      axistype = 4  , pcol = "blue" , 
                      pfcol = scales::alpha( "blue" , 0.5 ) , 
                      cglwd = 0.5  , 
                      axislabcol = "gray" , 
                      vlcex = 1.5   , title = input$player ) 
        })
      }
      else if( input$pkind == 'Advance' ){
        foo = dat %>% 
          select( Total_Pass_Percentage ,
                  Short_Pass_Percentage,
                  Long_Pass_Percentage,
                  Medium_Pass_Percentage , 
                  Take_On_Win_Percentage, 
                  Take_Ons_Tackled_Percentage) %>% 
          rbind( rep( 100 , 6) , rep( 0 , 6 ) , . ) 
        foo[is.na(foo)] = 0 
        output$plot1 = renderPlot({
          radarchart( foo, 
                      axistype = 4  , pcol = "blue" , 
                      pfcol = scales::alpha( "blue" , 0.5 ) , 
                      cglwd = 0.5  , 
                      axislabcol = "gray" , 
                      vlcex = 1.5   , title = input$player ) 
        })
      }
    }
    else{
      req( input$squad )
      req( input$player_gk )
      dat = read.csv( paste0( player_dir() , '/' , 'GoalKeeping.csv') )
      dat = dat[ dat$Player == input$player_gk , ]
      output$table = renderDT({
        foo = as.data.frame( t( dat ) )
        colnames( foo ) = "Value"
        foo
      })
      foo = dat %>% 
        select( Saves_Percentage ,
                Clean_Sheets_Percentage,
                Penalty_Kicks_Save_Percentage) %>% 
        rbind( rep( 100 , 3) , rep( 0 , 3) , . ) 
      foo[is.na(foo)] = 0 
      output$plot1 = renderPlot({
        radarchart( foo, 
                    axistype = 4  , pcol = "blue" , 
                    pfcol = scales::alpha( "blue" , 0.5 ) , 
                    cglwd = 0.5  , 
                    axislabcol = "gray" , 
                    vlcex = 1.5   , title = input$player_gk ) 
      })
    }
  })
}
