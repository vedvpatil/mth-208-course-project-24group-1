################################################################################
################## Scraping Code for 5 major European Leagues ##################
################################################################################


# Load required packages 
library(tidyverse)
library(rvest)
library(dplyr)
library(httr)
library(curl)

Dir = "~/OneDrive - IIT Kanpur/Documents/IITKANPUR/SEM7/MTH208/Peoject"


extract_data = function( Dir , league , link ){ 
  
  mainDir = paste( Dir , "/Data/" , league , sep = "" , collapse = "" )
  season = 24 
  setwd( mainDir )
  
  while( season >= 20 ){
    
    subDir <- paste( League , season , sep = "_" )
    
    if (file.exists(file.path( mainDir , subDir ))){
      setwd(file.path(mainDir, subDir))
    } else{
      dir.create(file.path(mainDir, subDir))
      setwd(file.path(mainDir, subDir))
      
    }
    
    # read html
    # x <- GET(link, add_headers('user-agent' = 'Student data scraper ([[ashbathla06@gmail.com]])'))
    epl23 = link %>% read_html()
    Sys.sleep(20)
    # Extract all the tables from the web page
    tables = epl23  %>% html_table()
    
    num_table = length( tables ) # number of tables
    
    ### Data Cleaning Step :
    # Run a for Loop that cleans data stored in each of the 24 tables extracted above.
    # The following this are covered within the for loop:
    #   1. "Squad" columns in even indexed rows contain an addition "vs " in each of
    #       its values, which must be removed.
    #   2. All tables except for the first one, have two rows which specify the name of
    #       the column, the loop merges these two rows into one character row, seperated
    #       with "_".
    for( i in 2:num_table){
      
      cn  = colnames(tables[[i]]) # extract column names
      cn[ cn == "" ] = NA
      
      for( j in 1:length( cn ) ){
        
        if( !is.na(cn[j]) ){
          cn[j] = paste( cn[j] , as.character(tables[[i]][1 , j] ) , sep = '_' )
        }
        # if the first row for that column is not empty, merge the first and
        # second rows to make it a single unique column name .
        
        else{
          cn[j] = as.character( tables[[i]][ 1 , j ])
        }
        # if the first row name is empty, the column name is the second row element
        # for that column itself.
      }
      
      # replace spaces with "_".
      colnames(tables[[i]]) = sapply( cn , function(a) gsub( " " , "_" , a ) )
      
      # remove the first row and any column with "playing_time" data in it, as this
      # is not the data we are interested in.
      tables[[i]] = tables[[i]][ -1 , ] %>%
        as.data.frame() %>%
        select( -starts_with("Playing_Time"))
      
      # convert appropriate columns into numeric using as.numeric()
      for( j in 1:dim( tables[[i]] )[2]){
        if( cn[j] != "Squad")
          tables[[i]][ , j ] = tables[[i]][ , j ] %>% as.numeric( )
      }
      
      # remove "vs " from the squad column elements from even indexed tables
      # so as to make the column universal.
      # note that this column will be later used as a parameter to identify rows and
      # merge two tables together.
      if( i > 2 & i%%2 == 0 ){
        tables[[i]]$Squad = sapply( tables[[i]]$Squad , function(a) gsub("vs " , ""  , a) )
      }
      
    }
    
    
    if(file.exists(file.path( mainDir , subDir , "Squad" ))){
      setwd(file.path(mainDir, subDir ,"Squad"))
    } else{
      dir.create(file.path(mainDir, subDir , "Squad"))
      setwd(file.path(mainDir, subDir , "Squad"))
      
    }
    
    ###############################################################################
    ########################### 1. Wins Loss Statistics ###########################
    ###############################################################################
    #                                                                             #                                           #
    #   Columns to be extracted:                                                  #
    #     Tables[[1]] : Squad , Rank , Wins , Draws , Losses , Pts                #
    #     Tables[[2]] : Home_Wins , Home_Losses , Home_Draws , Home_Pts,          #
    #       Away_Wins , Away_Losses , Away_Draws , Away_Pts                       #
    #   Create one Data Frame:                                                    #
    #     Win_Loss_Statistic -> wl                                                #
    #                                                                             #
    ###############################################################################
    
    wl  = as.data.frame(tables[[1]])  %>%
      select(
        Squad , Rk , W , D , L , Pts
      ) %>%
      arrange( Rk )
    
    
    tb2 = tables[[2]] %>%
      select( Squad , Home_W , Away_W ,
              Home_D , Away_D ,
              Home_L , Away_L ,
              Home_Pts , Away_Pts )
    
    wl = merge( x = wl , y = tb2  ,
                by.x = "Squad" , by.y = "Squad" , all = T ) %>%
      arrange( Rk )
    
    wl = wl %>%
      rename(
        Rank = Rk
      )
    
    
    colnames( wl ) = colnames( wl )  %>%
      sapply( . , function(a)  gsub( 'D' , 'Draws' , a ) ) %>%
      sapply( . , function(a)  gsub( 'L' , 'Losses' , a ) ) %>%
      sapply( . , function(a)  gsub( 'W' , 'Wins' , a ) ) %>%
      as.character()
    
    head( wl )
    
    write.csv( wl , "win_loss.csv" , row.names =  F  )
    
    ###############################################################################
    ########### 2. Attacking Statistics / Opponent Attacking Statistics ###########
    ###############################################################################
    #                                                                             #
    #   Columns to be extracted:                                                  #
    #     Tables[[1]]  : GF, GA, GD                                               #
    #     Tables[[2]]  : Home_GF, Home_GA ..... , Away_GD                         #
    #     Tables[[3]]  : Gls, Ast, G+A, PKatt , PK                                #
    #     Tables[[4]]  : Gls_A , Ast_A , G+A_A , PKatt_A, PK_A                    #
    #     Tables[[9]]  : Sh , SoT , SoT% , G/Sh , G/SoT, Dist, FK                 #
    #     Tables[[10]] : Sh_A , SoT_A , SoT%_A , G/Sh_A , G/SoT_A , Dist_A, FK_A  #
    #     Tables[[15]] : SCA , GCA , PassLive , Others**                          #
    #     Tables[[16]] : SCA_A , GCA_A , PassLive_A , Others_A**                  #
    #       *fix squad name , **create these columns                              #
    #   Create two Data Frames:                                                   #
    #     Attacking_stats -> att                                                  #
    #     Opponent_Attacking_Stats -> oatt                                        #
    #                                                                             #
    ###############################################################################
    
    
    att = as.data.frame(tables[[1]]) %>%
      select( Squad , Total_Goals_Scored = GF  ,
              Total_Goals_Conceded = GA,
              Goal_Difference = GD  )
    
    temp = tables[[2]] %>%
      select( Squad , Home_Goals_Scored = Home_GF ,
              Home_Goals_Conceded = Home_GA ,
              Home_Goal_Difference = Home_GD ,
              Away_Goals_Scored = Away_GF ,
              Away_Goals_Conceded = Away_GA ,
              Away_Goal_Difference = Away_GD )
    
    # merge tables
    
    att = merge( x = att , y = temp ,
                 by.x = "Squad" , by.y = "Squad" , all = T )
    
    
    temp = tables[[3]] %>%
      select( Squad ,
              Assists = Performance_Ast ,
              "Goals+Assists" = "Performance_G+A" ,
              Penalty_Attempted = Performance_PKatt ,
              Penalty_Scored = Performance_PK )
    
    # merge tables
    
    att = merge( x = att , y = temp ,
                 by.x = "Squad" , by.y = "Squad" , all = T )
    
    oatt = att %>%
      select( Squad ,
              "Total_Goals_Scored" = Total_Goals_Conceded ,
              "Total_Goals_Conceded" = Total_Goals_Scored )
    
    temp = tables[[4]] %>%
      select( Squad ,
              Assists = Performance_Ast ,
              "Goals+Assists" = "Performance_G+A" ,
              Penalty_Attempts = Performance_PKatt ,
              Penalty_Scored = Performance_PK )
    
    # merge tables
    oatt = merge( x = oatt , y = temp ,
                  by.x = "Squad" , by.y = "Squad" , all = T )
    
    
    temp = tables[[9]] %>%
      select( Squad ,
              Shots = Standard_Sh ,
              Shots_on_Target = Standard_SoT ,
              Shots_on_Target_Percentage = 'Standard_SoT%' ,
              Goals_per_Shot = `Standard_G/Sh` ,
              Goals_per_Shot_on_Target = `Standard_G/SoT`  ,
              Average_Shot_Distance = Standard_Dist ,
              Free_Kick_Shots = Standard_FK )
    
    # merge tables
    att = merge( x = att , y = temp ,
                 by.x = "Squad" , by.y = "Squad" , all = T )
    
    
    
    temp = tables[[10]] %>%
      select( Squad ,
              Shots = Standard_Sh ,
              Shots_on_Target = Standard_SoT ,
              Shots_on_Target_Percentage = 'Standard_SoT%' ,
              Goals_per_Shot = `Standard_G/Sh` ,
              Goals_per_Shot_on_Target = `Standard_G/SoT`  ,
              Average_Shot_Distance = Standard_Dist ,
              Free_Kick_Shots = Standard_FK )
    
    # merge tables
    oatt = merge( x = oatt , y = temp ,
                  by.x = "Squad" , by.y = "Squad" , all = T )
    
    
    
    temp = tables[[15]] %>%
      mutate( Other_Type_Shot_Creation =
                SCA_Types_Def + SCA_Types_PassDead + SCA_Types_TO +
                SCA_Types_Sh + SCA_Types_Fld ,
              Other_Type_Goal_Creation =
                GCA_Types_PassDead + GCA_Types_TO + GCA_Types_Sh +
                GCA_Types_Fld + GCA_Types_Def
      ) %>%
      select( Squad ,
              Shot_Creating_Actions = SCA_SCA ,
              Live_Pass_Shot_Creation = SCA_Types_PassLive ,
              Other_Type_Shot_Creation ,
              Goal_Creating_Actions = GCA_GCA ,
              Live_Pass_Goal_Creation = GCA_Types_PassLive ,
              Other_Type_Goal_Creation)
    
    # merge tables
    att = merge( x = att , y = temp ,
                 by.x = "Squad" , by.y = "Squad" , all = T )
    
    
    
    temp = tables[[16]] %>%
      mutate( Other_Type_Shot_Creation =
                SCA_Types_Def + SCA_Types_PassDead + SCA_Types_TO + SCA_Types_Sh + SCA_Types_Fld ,
              Other_Type_Goal_Creation =
                GCA_Types_PassDead + GCA_Types_TO + GCA_Types_Sh + GCA_Types_Fld + GCA_Types_Def
      ) %>%
      select( Squad ,
              Shot_Creating_Actions = SCA_SCA ,
              Live_Pass_Shot_Creation = SCA_Types_PassLive ,
              Other_Type_Shot_Creation ,
              Goal_Creating_Actions = GCA_GCA ,
              Live_Pass_Goal_Creation = GCA_Types_PassLive ,
              Other_Type_Goal_Creation)
    
    # merge tables
    oatt = merge( x = oatt , y = temp ,
                  by.x = "Squad" , by.y = "Squad" , all = T )
    
    write.csv( att , "Attack.csv" , row.names =  F )
    write.csv( oatt , "Opponent_Attack.csv" , row.names =  F )
    
    ###############################################################################
    ########## 3. Goalkeeping Statistic / Opponent Goalkeeping Statistic ##########
    ###############################################################################
    #                                                                             #
    #   Columns to be extracted:                                                  #
    #     Tables[[5]] : #_PL , GA, Saves, Save%, CS, CS%  , PKA , PK_Save%        #
    #     Tables[[6]] : GA, Saves, Save%, CS, CS%  , PKA , PK_Save%               #
    #   Create two Data Frames:                                                   #
    #     Goalkeeping_stats -> gk                                                 #
    #     Opponent_Goalkeeping_Stats -> ogk                                       #
    #                                                                             #
    ###############################################################################
    
    
    gk = tables[[5]] %>%
      select( Squad ,
              Goalkeepers_Used = "#_Pl",
              Goals_Conceded = Performance_GA ,
              Saves = Performance_Saves ,
              Saves_Percentage = `Performance_Save%` ,
              Clean_Sheets = Performance_CS ,
              Clean_Sheets_Percentage = `Performance_CS%` ,
              Penalty_Kicks_Saves =  Penalty_Kicks_PKsv ,
              Penalty_Kicks_Save_Percentage =  `Penalty_Kicks_Save%`
      )
    
    ogk = tables[[6]] %>%
      select( Squad ,
              Goals_Conceded = Performance_GA ,
              Saves = Performance_Saves ,
              Saves_Percentage = `Performance_Save%` ,
              Clean_Sheets = Performance_CS ,
              Clean_Sheets_Percentage = `Performance_CS%` ,
              Penalty_Kicks_Saves =  Penalty_Kicks_PKsv ,
              Penalty_Kicks_Save_Percentage =  `Penalty_Kicks_Save%`
      )
    
    write.csv( gk , "Goalkeeping.csv" , row.names =  F )
    write.csv( ogk , "Opponent_Goalkeeping.csv" , row.names =  F )
    
    ###############################################################################
    ############# 4. Defense Statistics / Opponent Defense Statistics #############
    ###############################################################################
    #                                                                             #
    #   Columns to be extracted:                                                  #
    #     Tables[[17]] : Tkl , TklW , TklW% , DThk , DTklW, DTklW%, Blocks,       #
    #       Sh , Pass, Int, Tkl+Int, Clr, Err                                     #
    #     Tables[[18]] : Tkl , TklW , TklW% , DThk , DTklW, DTklW%, Blocks,       #
    #       Sh , Pass, Int, Tkl+Int, Clr, Err                                     #
    #   Create Two Data Frames:                                                   #
    #     Defense_Statistics -> df                                                #
    #     Opponent_Defense_Statistics -> odf                              #
    #                                                                             #
    ###############################################################################
    
    df = tables[[17]] %>%
      mutate( "Tklw%" = Tackles_TklW*100/Tackles_Tkl ) %>%
      select( Squad ,
              Tackles = Tackles_Tkl ,
              Tackles_Won = Tackles_TklW ,
              Tackles_Win_Percentage = "Tklw%" ,
              Challenges = Challenges_Att ,
              Challenges_Won = Challenges_Att ,
              Challenges_Percentage = `Challenges_Tkl%` ,
              Total_Blocks = Blocks_Blocks ,
              Shots_Blocked = Blocks_Sh ,
              Passes_Blocked = Blocks_Pass ,
              Interceptions = Int ,
              Clearances = Clr ,
              Errors = Err )
    
    odf = tables[[18]] %>%
      mutate( "Tklw%" = Tackles_TklW*100/Tackles_Tkl ) %>%
      select( Squad ,
              Tackles = Tackles_Tkl ,
              Tackles_Won = Tackles_TklW ,
              Tackles_Win_Percentage = "Tklw%" ,
              Challenges = Challenges_Att ,
              Challenges_Won = Challenges_Att ,
              Challenges_Percentage = `Challenges_Tkl%` ,
              Total_Blocks = Blocks_Blocks ,
              Shots_Blocked = Blocks_Sh ,
              Passes_Blocked = Blocks_Pass ,
              Interceptions = Int ,
              Clearances = Clr ,
              Errors = Err )
    
    write.csv( df , "Defense.csv" , row.names =  F )
    write.csv( odf , "Opponent_Defense.csv" , row.names =  F )
    
    ###############################################################################
    ########### 5. Holding and Advance -> Passes/Possession/Progression ###########
    ###############################################################################
    #                                                                             #
    #   Columns to be extracted:                                                  #
    #     Tables[[3]]  : #_Pl, PrgC, PrgP                                         #
    #     Tables[[4]]  : PrgC, PrgP                                               #
    #     Tables[[11]] : cmp, Att, cmp%, short_cmp, short_att , short_cmp%,       #
    #       ......... long_cmp%                                                   #
    #     Tables[[12]] : cmp, Att, cmp%, short_cmp, short_att , short_cmp%,       #
    #       ......... long_cmp%                                                   #
    #     Tables[[13]] : Live, Dead, FK, TB, Sw, Crs, TI, CK, Off                 #
    #     Tables[[14]] : Live, Dead, FK, TB, Sw, Crs, TI, CK, Off                 #
    #     Tables[[19]] : Poss, Touches, Att, Succ, Succ%, Tkld, Tkld%             #
    #     Tables[[20]] : Poss, Touches, Att, Succ, Succ%, Tkld, Tkld%             #
    #   Create Two Data Frames:                                                   #
    #     Squad_Advance -> advance                                                #
    #     Opponent_Advance -> oadvance                                            #
    #                                                                             #
    ###############################################################################
    
    
    
    advance = tables[[3]] %>%
      select( Squad ,
              Player_Count = "#_Pl" ,
              Progression_Carries = Progression_PrgC ,
              Progression_Passes = Progression_PrgP )
    
    
    oadvance = tables[[4]] %>%
      select( Squad ,
              Progression_Carries = Progression_PrgC ,
              Progression_Passes = Progression_PrgP )
    
    
    temp = tables[[11]] %>%
      select( Squad ,
              Total_Passes_Completed = Total_Cmp ,
              Total_Passes_Attempted = Total_Att ,
              Total_Pass_Percentage = `Total_Cmp%` ,
              Short_Passes_Completed = Short_Cmp ,
              Short_Passes_Attempted = Short_Att ,
              Short_Pass_Percentage = `Short_Cmp%` ,
              Long_Passes_Completed = Long_Cmp ,
              Long_Passes_Attempted = Long_Att ,
              Long_Pass_Percentage = `Long_Cmp%` ,
              Medium_Passes_Completed = Medium_Cmp ,
              Medium_Passes_Attempted = Medium_Att ,
              Medium_Pass_Percentage = "Medium_Cmp%" )
    
    advance = merge( x = advance , y = temp ,
                     by.x = "Squad" , by.y = "Squad" )
    
    
    
    temp = tables[[12]] %>%
      select( Squad ,
              Total_Passes_Completed = Total_Cmp ,
              Total_Passes_Attempted = Total_Att ,
              Total_Pass_Percentage = `Total_Cmp%` ,
              Short_Passes_Completed = Short_Cmp ,
              Short_Passes_Attempted = Short_Att ,
              Short_Pass_Percentage = `Short_Cmp%` ,
              Long_Passes_Completed = Long_Cmp ,
              Long_Passes_Attempted = Long_Att ,
              Long_Pass_Percentage = `Long_Cmp%` ,
              Medium_Passes_Completed = Medium_Cmp ,
              Medium_Passes_Attempted = Medium_Att ,
              Medium_Pass_Percentage = "Medium_Cmp%" )
    
    oadvance = merge( x = oadvance , y = temp ,
                      by.x = "Squad" , by.y = "Squad" )
    
    
    temp = tables[[13]] %>%
      select( Squad ,
              Live_Pass = Pass_Types_Live ,
              Dead_Pass = Pass_Types_Dead ,
              Free_Kicks = Pass_Types_FK ,
              Through_Balls = Pass_Types_TB ,
              Switches = Pass_Types_Sw ,
              Crosses = Pass_Types_Crs ,
              Throw_Ins = Pass_Types_TI ,
              Corner_Kicks = Pass_Types_CK ,
              Offside = Outcomes_Off )
    
    advance = merge( x = advance , y = temp ,
                     by.x = "Squad" , by.y = "Squad" )
    
    
    temp = tables[[14]] %>%
      select( Squad ,
              Live_Pass = Pass_Types_Live ,
              Dead_Pass = Pass_Types_Dead ,
              Free_Kicks = Pass_Types_FK ,
              Through_Balls = Pass_Types_TB ,
              Switches = Pass_Types_Sw ,
              Crosses = Pass_Types_Crs ,
              Throw_Ins = Pass_Types_TI ,
              Corner_Kicks = Pass_Types_CK ,
              Offside = Outcomes_Off )
    
    
    oadvance = merge( x = oadvance , y = temp ,
                      by.x = "Squad" , by.y = "Squad" )
    
    temp = tables[[19]] %>%
      select( Squad ,
              Possession = Poss ,
              Touches = Touches_Touches ,
              Take_Ons_Attempted = `Take-Ons_Att` ,
              Take_Ons_Wins = `Take-Ons_Succ`,
              Take_On_Win_Percentage = `Take-Ons_Succ%` ,
              Take_Ons_Tackled = `Take-Ons_Tkld` ,
              Take_Ons_Tackled_Percentage = `Take-Ons_Tkld%` )
    
    advance = merge( x = advance , y = temp ,
                     by.x = "Squad" , by.y = "Squad" )
    
    
    
    temp = tables[[20]] %>%
      select( Squad ,
              Possession = Poss ,
              Touches = Touches_Touches ,
              Take_Ons_Attempted = `Take-Ons_Att` ,
              Take_Ons_Wins = `Take-Ons_Succ`,
              Take_On_Win_Percentage = `Take-Ons_Succ%` ,
              Take_Ons_Tackled = `Take-Ons_Tkld` ,
              Take_Ons_Tackled_Percentage = `Take-Ons_Tkld%` )
    
    oadvance = merge( x = oadvance , y = temp ,
                      by.x = "Squad" , by.y = "Squad" )
    
    
    write.csv( advance , "Advance.csv" , row.names =  F )
    write.csv( oadvance , "Opponent_Advance.csv" , row.names =  F )
    
    ###############################################################################
    ################################# 6. Fair Play ################################
    ###############################################################################
    #                                                                             #
    #   Columns to be extracted:                                                  #
    #     Tables[[23]] : CrdY, CrdR, 2CrdY, Fls                                   #
    #     Tables[[24]] : CrdY, CrdR, 2CrdY, Fls                                   #
    #   Create Two Data Frames:                                                   #
    #     Fair_Play -> fp                                                         #
    #     Opponent_Fair_Play -> ofp                                               #
    #                                                                             #
    ###############################################################################
    
    fp = tables[[23]] %>%
      mutate( Total_Red_Cards = Performance_CrdR + Performance_2CrdY ) %>%
      select(
        Squad ,
        Yellow_Card = Performance_CrdY ,
        Red_Card = Performance_CrdR ,
        Second_Yellow_Card = Performance_2CrdY ,
        Total_Red_Cards ,
        Fouls = Performance_Fls
      )
    
    ofp = tables[[24]] %>%
      mutate( Total_Red_Cards = Performance_CrdR + Performance_2CrdY ) %>%
      select(
        Squad ,
        Yellow_Card = Performance_CrdY ,
        Red_Card = Performance_CrdR ,
        Second_Yellow_Card = Performance_2CrdY ,
        Total_Red_Cards ,
        Fouls = Performance_Fls
      )
    
    write.csv( fp , "Fair_Play.csv" , row.names =  F )
    write.csv( ofp , "Opponent_Fair_Play.csv" , row.names =  F )
    
    
    
    ###############################################################################
    ############################## Player Statistics ##############################
    ###############################################################################
    
    
    
    if (file.exists(file.path( mainDir , subDir , "Player"))){
      setwd(file.path(mainDir, subDir , "Player"))
    } else{
      dir.create(file.path(mainDir, subDir , "Player"))
      setwd(file.path(mainDir, subDir , "Player"))
    }
    
    foo = epl23 %>% html_elements("div[id = inner_nav] ul")
    player_table_links = foo[2] %>% html_elements("a") %>%
      html_attr("href")  %>%
      sapply( . , function(a) paste("https://fbref.com/" , a , sep = "" ) ) %>%
      as.character()
    
    
    nlinks = length( player_table_links )
    player_tables = vector( "list" , nlinks )
    
    
    for( i in 1:nlinks ){
      
      temp = player_table_links[i] %>%
        read_html()
      Sys.sleep(20)
      player_tables[[i]] =  temp %>%
        html_elements(".table_wrapper.setup_commented.commented") %>%
        html_elements(xpath = "comment()") %>%
        html_text() %>%
        read_html() %>%
        html_table() %>%
        as.data.frame()
      Sys.sleep(20)
    }
    
    
    second = function(a){
      
      if( length(a) == 1 ){
        return( NA )
      }
      else{
        return( a[2])
      }
      
    }
    
    
    for( i in 1:nlinks ){
      
      cn = colnames( player_tables[[i]] )
      foo = cn %>% grepl( "[0123456789]$" , . )
      cn[foo] = cn[foo] %>%
        strsplit( . , "." , fixed = T ) %>%
        lapply( . , function(a) paste(a[-length(a)] , collapse ="_" ) ) %>%
        unlist()
      indx = which( cn != "Var" )
      cn[-indx] = player_tables[[i]][ 1 , -indx ] %>% as.character()
      for( j in indx ){
        cn[j] = paste( cn[j] , player_tables[[i]][ 1 , j ] , sep = "_" , collapse = "_" )
      }
      colnames(player_tables[[i]]) = cn
      player_tables[[i]] = player_tables[[i]][ -1 , ]
      
      player_tables[[i]] = player_tables[[i]] %>% select( -1 , -length( cn ) )
      cn = colnames( player_tables[[i]] )
      for( j in 1:length(cn) ){
        if( cn[j] == "Playing_Time_Min" ){
          player_tables[[i]][ , j ] = strsplit(player_tables[[i]][ , j ] , ",") %>%
            lapply( . , function(a) paste(a , collapse = "")) %>%
            unlist()
        }
        if( cn[j] != "Player" & cn[j] != "Squad" & cn[j] != "Pos" & cn[j] != "Nation" ){
          player_tables[[i]][ , j ] = player_tables[[i]][ , j ] %>% as.numeric()
        }
      }
      
      player_tables[[i]] = player_tables[[i]] [complete.cases(player_tables[[i]]),]
      
      player_tables[[i]][ , 2 ] =
        str_extract_all( player_tables[[i]][ , 2 ] , "[A-Z]") %>%
        lapply( ., function(a) paste0(a , collapse =  "")) %>% unlist()
      
      Pos1 = player_tables[[i]][ , 3] %>%
        strsplit( . , "," ) %>%
        lapply( . , function(a) a[1] ) %>% unlist()
      
      Pos2 = player_tables[[i]][ , 3] %>%
        strsplit( . , ",") %>%
        lapply( .,second ) %>% unlist()
      
      player_tables[[i]]$Pos2 = Pos2
      player_tables[[i]]$Pos = Pos1
      
      player_tables[[i]] = player_tables[[i]] %>% select(
        Player , Squad , Nation ,
        Pos , Pos2 , everything() )
      
    }
    
    ###############################################################################
    ######################## 1. Player General Statistics  ########################
    ###############################################################################
    #                                                                             #
    #   Columns to be extracted:                                                  #
    #     Tables[[10]]  : Mp, Min, Min/MP, Min%, Starts, Mn/Start, Compl, Subs ,  #
    #       Mn/Subs, PPM, onG, onGA                                               #
    #   Create Data Frames:                                                       #
    #     Player_General_Statistics -> pgen                                       #
    #                                                                             #
    ###############################################################################
    
    
    pgen = player_tables[[10]] %>%
      select(
        Player , Squad , Nation ,
        Pos , Pos2 , Age , Born ,
        Total_Match_Played = Playing.Time_MP,
        Total_Playing_Time = Playing_Time_Min ,
        Minutes_Per_Match_Played = `Playing_Time_Mn/MP` ,
        Percent_of_Squad_Minutes_Played = `Playing_Time_Min%` ,
        Matches_Started = Starts_Starts ,
        Minutes_Per_Match_Start = `Starts_Mn/Start` ,
        Complete_Matches_PLayed = Starts_Compl ,
        Matches_Played_As_Substitute = Subs_Subs ,
        Minutes_Per_Match_Substitute = `Subs_Mn/Sub` ,
        Player_Points_Per_Match = Team.Success_PPM ,
        Player_On_Pitch_Squad_Goals = Team_Success_onG ,
        Player_On_Pitch_Goals_Allowed = Team_Success_onGA
        
      )
    
    foo = epl23 %>% html_elements("div[id = inner_nav] ul section p ")
    foo = foo[4] %>% html_element("a") %>% html_attr("href")
    foo = paste( "https://fbref.com" , foo , sep = "" , collapse =  "")
    
    temp = foo %>% read_html() %>% html_table()
    Sys.sleep(20)
    temp = temp[[2]]  %>% as.data.frame()
    
    temp = temp %>% select(
      Player ,
      Weekly_Wages = `Weekly Wages` ,
      Annual_Wages = `Annual Wages`
    )
    
    temp$Weekly_Wages = temp$Weekly_Wages %>%
      strsplit( . , '[()]' ) %>%
      lapply( . , function(a) gsub( ',| ' , "" , a[1]) ) %>%
      substring( first = 2 ) %>% as.numeric()
    
    temp$Annual_Wages = temp$Annual_Wages %>%
      strsplit( . , '[()]' ) %>%
      lapply( . , function(a) gsub( ',| ' , "" , a[1]) ) %>%
      substring( first = 2 ) %>% as.numeric()
    
    pgen = merge( x = pgen , y = temp ,
                  by.x = "Player" , by.y = "Player" )
    
    write.csv( pgen , "General.csv" , row.names =  F )
    
    ###############################################################################
    ########### 2. Attacking Statistics / Opponent Attacking Statistics ###########
    ###############################################################################
    #                                                                             #
    #   Columns to be extracted:                                                  #
    #     Tables[[1]]  : Gls, Ast, G+A, PKatt , PK                                #
    #     Tables[[4]]  : Sh , SoT , SoT% , G/Sh , G/SoT, Dist, FK                 #
    #     Tables[[7]] : SCA , GCA , PassLive , Others**                           #
    #   Create Data Frames:                                                       #
    #     Player_Attacking_stats -> patt                                          #
    #                                                                             #
    ###############################################################################
    
    
    
    
    patt = player_tables[[1]] %>%
      select( Player , Squad , Nation,
              Pos , Pos2 , Age ,
              Assists = Performance_Ast ,
              "Goals+Assists" = "Performance_G+A" ,
              Penalty_Attempted = Performance_PKatt ,
              Penalty_Scored = Performance_PK )
    
    
    temp = player_tables[[4]] %>%
      select( Player ,
              Shots = Standard_Sh ,
              Shots_on_Target = Standard_SoT ,
              Shots_on_Target_Percentage = 'Standard_SoT%' ,
              Goals_per_Shot = `Standard_G/Sh` ,
              Goals_per_Shot_on_Target = `Standard_G/SoT`  ,
              Average_Shot_Distance = Standard_Dist ,
              Free_Kick_Shots = Standard_FK )
    
    # merge tables
    patt = merge( x = patt , y = temp ,
                  by.x = "Player" , by.y = "Player" , all = T )
    
    
    
    temp = player_tables[[7]] %>%
      mutate( Other_Type_Shot_Creation =
                SCA_Types_Def + SCA_Types_PassDead + SCA_Types_TO +
                SCA_Types_Sh + SCA_Types_Fld ,
              Other_Type_Goal_Creation =
                GCA_Types_PassDead + GCA_Types_TO + GCA_Types_Sh + GCA_Types_Fld +
                GCA_Types_Def
      ) %>%
      select( Player ,
              Shot_Creating_Actions = SCA_SCA ,
              Live_Pass_Shot_Creation = SCA.Types_PassLive ,
              Other_Type_Shot_Creation ,
              Goal_Creating_Actions = GCA_GCA ,
              Live_Pass_Goal_Creation = GCA.Types_PassLive ,
              Other_Type_Goal_Creation)
    
    # merge tables
    patt = merge( x = patt , y = temp ,
                  by.x = "Player" , by.y = "Player" , all = T )
    
    
    write.csv( patt , "Attack.csv" , row.names =  F )
    
    ###############################################################################
    ########## 3. Goalkeeping Statistic / Opponent Goalkeeping Statistic ##########
    ###############################################################################
    #                                                                             #
    #   Columns to be extracted:                                                  #
    #     Player_Tables[[2]] : #_PL , GA, Saves, Save%, CS, CS%  , PKA , PK_Save% #
    #   Create two Data Frames:                                                   #
    #     Plyer_Goalkeeping_stats -> pgk                                          #
    #                                                                             #
    ###############################################################################
    
    
    pgk = player_tables[[2]] %>%
      select( Player , Squad , Nation ,
              Pos , Pos2 , Age ,
              Wins = Performance_W ,
              Draws = Performance_D ,
              Losses = Performance_L ,
              Goals_Conceded = Performance_GA ,
              Saves = Performance_Saves ,
              Saves_Percentage = `Performance_Save%` ,
              Clean_Sheets = Performance_CS ,
              Clean_Sheets_Percentage = `Performance_CS%` ,
              Penalty_Kicks_Saves =  Penalty_Kicks_PKsv ,
              Penalty_Kicks_Save_Percentage =  `Penalty_Kicks_Save%`
      )
    
    write.csv( pgk , "GoalKeeping.csv" , row.names =  F )
    
    ###############################################################################
    ############# 4. Defense Statistics / Opponent Defense Statistics #############
    ###############################################################################
    #                                                                             #
    #   Columns to be extracted:                                                  #
    #     Tables[[8]] : Tkl , TklW , TklW% , DThk , DTklW, DTklW%, Blocks,        #
    #       Sh , Pass, Int, Tkl+Int, Clr, Err                                     #
    #   Create Two Data Frames:                                                   #
    #     Player_Defense_Statistics -> pdf                                        #
    #                                                                             #
    ###############################################################################
    
    pdf_ = player_tables[[8]] %>%
      mutate( "Tklw%" = Tackles_TklW*100/Tackles_Tkl ) %>%
      select( Player , Squad , Nation ,
              Pos , Pos2 , Age ,
              Tackles = Tackles_Tkl ,
              Tackles_Won = Tackles_TklW ,
              Tackles_Win_Percentage = "Tklw%" ,
              Challenges = Challenges_Att ,
              Challenges_Won = Challenges_Att ,
              Challenges_Percentage = `Challenges_Tkl%` ,
              Total_Blocks = Blocks_Blocks ,
              Shots_Blocked = Blocks_Sh ,
              Passes_Blocked = Blocks_Pass ,
              Interceptions = Int ,
              Clearances = Clr ,
              Errors = Err )
    
    write.csv( pdf_ , "Defense.csv" , row.names =  F )
    
    ###############################################################################
    ########### 5. Holding and Advance -> Passes/Possession/Progression ###########
    ###############################################################################
    #                                                                             #
    #   Columns to be extracted:                                                  #
    #     Tables[[1]] : PrgC, PrgP                                                #
    #     Tables[[5]] : cmp, Att, cmp%, short_cmp, short_att , short_cmp%,        #
    #       ......... long_cmp%                                                   #
    #     Tables[[6]] : Live, Dead, FK, TB, Sw, Crs, TI, CK, Off                  #
    #     Tables[[9]] : Poss, Touches, Att, Succ, Succ%, Tkld, Tkld%              #
    #   Create Data Frames:                                                       #
    #     Player_Advance -> padv                                                  #
    #                                                                             #
    ###############################################################################
    
    
    
    padv = player_tables[[1]] %>%
      select( Player , Squad , Nation ,
              Pos , Pos2 , Age ,
              Progression_Carries = Progression_PrgC ,
              Progression_Passes_Delivered = Progression_PrgP ,
              Progression_Passes_Recived = Progression_PrgR)
    
    
    temp = player_tables[[5]] %>%
      select( Player ,
              Total_Passes_Completed = Total_Cmp ,
              Total_Passes_Attempted = Total_Att ,
              Total_Pass_Percentage = `Total_Cmp%` ,
              Short_Passes_Completed = Short_Cmp ,
              Short_Passes_Attempted = Short_Att ,
              Short_Pass_Percentage = `Short_Cmp%` ,
              Long_Passes_Completed = Long_Cmp ,
              Long_Passes_Attempted = Long_Att ,
              Long_Pass_Percentage = `Long_Cmp%` ,
              Medium_Passes_Completed = Medium_Cmp ,
              Medium_Passes_Attempted = Medium_Att ,
              Medium_Pass_Percentage = "Medium_Cmp%" ,
              Key_Passes = KP ,
              Passes_Into_Penalty_Area = PPA ,
              Crosses_Into_Penalty_Area = CrsPA )
    
    padv = merge( x = padv , y = temp ,
                  by.x = "Player" , by.y = "Player" )
    
    
    temp = player_tables[[6]] %>%
      select( Player ,
              Live_Pass = Pass.Types_Live ,
              Dead_Pass = Pass_Types_Dead ,
              Free_Kicks = Pass_Types_FK ,
              Through_Balls = Pass_Types_TB ,
              Switches = Pass_Types_Sw ,
              Crosses = Pass_Types_Crs ,
              Throw_Ins = Pass_Types_TI ,
              Corner_Kicks = Pass_Types_CK ,
              Offside = Outcomes_Off )
    
    padv = merge( x = padv , y = temp ,
                  by.x = "Player" , by.y = "Player" )
    
    
    temp = player_tables[[9]] %>%
      select( Player ,
              Touches = Touches_Touches ,
              Take_Ons_Attempted = `Take.Ons_Att` ,
              Take_Ons_Wins = `Take_Ons_Succ`,
              Take_On_Win_Percentage = `Take_Ons_Succ%` ,
              Take_Ons_Tackled = `Take_Ons_Tkld` ,
              Take_Ons_Tackled_Percentage = `Take_Ons_Tkld%` )
    
    padv = merge( x = padv , y = temp ,
                  by.x = "Player" , by.y = "Player" )
    
    
    write.csv( padv , "Advance.csv" , row.names =  F )
    ###############################################################################
    ################################# 6. Fair Play ################################
    ###############################################################################
    #                                                                             #
    #   Columns to be extracted:                                                  #
    #     Tables[[11]] : CrdY, CrdR, 2CrdY, Fls                                   #
    #   Create Data Frames:                                                       #
    #     player_Fair_Play -> pfp                                                 #
    #                                                                             #
    ###############################################################################
    
    pfp = player_tables[[11]] %>%
      mutate( Total_Red_Cards = Performance_CrdR + Performance_2CrdY ) %>%
      select(
        Player , Squad , Nation ,
        Pos , Pos2 , Age ,
        Yellow_Card = Performance_CrdY ,
        Red_Card = Performance_CrdR ,
        Second_Yellow_Card = Performance_2CrdY ,
        Total_Red_Cards ,
        Fouls = Performance_Fls
      )
    
    
    write.csv( pfp , "Fair_Play.csv" , row.names =  F )
    
    foo = epl23 %>%
      html_elements(".prevnext" ) %>%
      html_element(".button2.prev") %>%
      html_attr("href")
    
    link = paste( "https://fbref.com" , foo , sep = "" , collapse =  "")
    
    season = season - 1
    
  }
  
}


extract_data( Dir , 
        "Premier_League" , 
        "https://fbref.com/en/comps/9/2023-2024/2023-2024-Premier-League-Stats" )

extract_data( Dir , 
              "LaLiga" ,
              "https://fbref.com/en/comps/12/2023-2024/2023-2024-La-Liga-Stats" )

extract_data( Dir, 
              "Bundesliga",
              "https://fbref.com/en/comps/20/2023-2024/2023-2024-Bundesliga-Stats" )

extract_data( Dir , 
              "Serie_A", 
              "https://fbref.com/en/comps/11/2023-2024/2023-2024-Serie-A-Stats")

extract_data( Dir, 
              "Ligue_1",
              "https://fbref.com/en/comps/13/2023-2024/2023-2024-Ligue-1-Stats" )
