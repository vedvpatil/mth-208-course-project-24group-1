# select columns of interest from 20 files into 1 file

library(dplyr)

get_player_dir = function( league , season , filename){
  season <- as.numeric(season) - 2000
  file_path <- paste0(league, "/", league, "_", season, "/Player/" , filename)
  return( file_path )
}

leagues <-  c("Bundesliga", "LaLiga", "Ligue_1", "Premier_League", "Serie_A")

# Advance RData
adv <- c("Player", "Progression Passes Delivered" , "Total Pass Percentage" , "Key Passes" , "Take On Win Percentage", "Touches", "Season")
data <- data.frame(Player = character(), 
                      Progression_Passes_Delivered = numeric(), 
                      Total_Pass_Percentage = numeric(), 
                      Key_Passes = numeric(), 
                      Take_On_Win_Percentage = numeric(), 
                      Touches = numeric(), 
                      Season = character(),
                      stringsAsFactors = FALSE)

for (l in leagues){
  for (s in 2020 : 2024){
    dat <- read.csv(get_player_dir(l,s,"Advance.csv")) %>% select("Player" , "Progression_Passes_Delivered" , "Total_Pass_Percentage" , "Key_Passes" , "Take_On_Win_Percentage", "Touches")
    # along every row , add season code
    seasonCode <- paste0(s," ",l)
    dat$Season <- seasonCode
    
    # remove null values
    dat <- na.omit(dat)
    
    data <- rbind(data , dat)
  }
}
save(data, file = "Player_Comparison_Data/Advance/data.RData")
