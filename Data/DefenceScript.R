# select columns of interest from 20 files into 1 file

library(dplyr)

get_player_dir = function( league , season , filename){
  season <- as.numeric(season) - 2000
  file_path <- paste0(league, "/", league, "_", season, "/Player/" , filename)
  return( file_path )
}

leagues <-  c("Bundesliga", "LaLiga", "Ligue_1", "Premier_League", "Serie_A")

# Defense RData
def <- c("Player", "Tackles_Win_Percentage" , "Challenges_Percentage" , "Total_Blocks" , "Clearances", "Season")
data <- data.frame(Player = character(), 
                     "Tackles_Win_Percentage" = numeric(), 
                     "Challenges_Percentage" = numeric(), 
                     "Total_Blocks" = numeric(), 
                     "Clearances" = numeric(),
                     Season = character(),
                     stringsAsFactors = FALSE)

for (l in leagues){
  for (s in 2020 : 2024){
    dat <- read.csv(get_player_dir(l,s,"Defense.csv")) %>% select("Player", "Tackles_Win_Percentage" , "Challenges_Percentage" , "Total_Blocks" , "Clearances")
    # along every row , add season code
    seasonCode <- paste0(s," ",l)
    dat$Season <- seasonCode
    
    # remove null values
    dat <- na.omit(dat)
    
    data <- rbind(data , dat)
  }
}
save(data, file = "Player_Comparison_Data/Defense/data.RData")
