# select columns of interest from 20 files into 1 file

library(dplyr)

get_player_dir = function( league , season , filename){
  season <- as.numeric(season) - 2000
  file_path <- paste0(league, "/", league, "_", season, "/Player/" , filename)
  return( file_path )
}

leagues <-  c("Bundesliga", "LaLiga", "Ligue_1", "Premier_League", "Serie_A")

# GoalKeeping RData
goal <- c("Player", "Wins", "Saves_Percentage", "Clean_Sheets_Percentage", "Penalty_Kicks_Save_Percentage", "Season")
data <- data.frame(Player = character(), 
                          "Wins" = numeric(), 
                     "Saves_Percentage" = numeric(), 
                     "Clean_Sheets_Percentage" = numeric(), 
                     "Penalty_Kicks_Save_Percentage" = numeric(),
                     Season = character(),
                     stringsAsFactors = FALSE)

for (l in leagues){
  for (s in 2020 : 2024){
    dat <- read.csv(get_player_dir(l,s,"GoalKeeping.csv")) %>% select("Player", "Wins", "Saves_Percentage", "Clean_Sheets_Percentage", "Penalty_Kicks_Save_Percentage")
    # along every row , add season code
    seasonCode <- paste0(s," ",l)
    dat$Season <- seasonCode
    
    # remove null values
    dat <- na.omit(dat)
    
    data <- rbind(data , dat)
  }
}
save(data, file = "Player_Comparison_Data/GoalKeeping/data.RData")
