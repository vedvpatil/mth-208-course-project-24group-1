# select columns of interest from 20 files into 1 file

library(dplyr)

get_player_dir = function( league , season , filename){
  season <- as.numeric(season) - 2000
  file_path <- paste0(league, "/", league, "_", season, "/Player/" , filename)
  return( file_path )
}

leagues <-  c("Bundesliga", "LaLiga", "Ligue_1", "Premier_League", "Serie_A")

# Attack RData
att <- c("Player", "Goals+Assists" , "Shots_on_Target_Percentage" , "Goals_per_Shot" , "Shot_Creating_Actions", "Live_Pass_Goal_Creation", "Season")
data <- data.frame(Player = character(), 
                     "Goals+Assists" = numeric(), 
                     "Shots on Target Percentage" = numeric(), 
                     "Goals per Shot" = numeric(), 
                     "Shot Creating Actions" = numeric(), 
                     "Live Pass Goal Creation" = numeric(), 
                     Season = character(),
                     stringsAsFactors = FALSE)

for (l in leagues){
  for (s in 2020 : 2024){
    dat <- read.csv(get_player_dir(l,s,"Attack.csv")) %>% select('Player', `Goals.Assists`, `Shots_on_Target_Percentage`, `Goals_per_Shot`, `Shot_Creating_Actions`, `Live_Pass_Goal_Creation`)
    # along every row , add season code
    seasonCode <- paste0(s," ",l)
    dat$Season <- seasonCode
    
    # remove null values
    dat <- na.omit(dat)
    
    data <- rbind(data , dat)
  }
}
save(data, file = "Player_Comparison_Data/Attack/data.RData")
