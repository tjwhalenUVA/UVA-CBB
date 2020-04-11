library(randomForest)

#Set up dataframe
tourney_games <- #randomly shuffle winners/losers so the inner is not always the same column
    dfs$MNCAATourneyCompactResults %>% 
    mutate(T_1_Win = rbinom(nrow(dfs$MNCAATourneyCompactResults), 1, 0.5), #Target variable
           #Set team 1 to WTeamID if target says T1 wins
           T_1_ID = if_else(T_1_Win == 1, WTeamID, LTeamID),
           #Set team 2 to LTeamID if target says T1 wins
           T_2_ID = if_else(T_1_Win == 1, LTeamID, WTeamID),
           #Feature indicating wether game went to OT or not
           OT = if_else(NumOT == 0, 0, 1)) %>% 
    select(T_1_Win, Season, T_1_ID, T_2_ID, OT)

#Remove region indicator from tournament seed before adding to feature df
tourney_seeds <- dfs$MNCAATourneySeeds %>% mutate(Seed = parse_number(Seed))
    
#Add team seeds to the feature df
tourney_games %<>% 
    left_join(.,
              tourney_seeds %>% rename(T_1_ID = TeamID, T_1_Seed = Seed),
              by = c("Season", "T_1_ID")) %>% 
    left_join(.,
              tourney_seeds %>% rename(T_2_ID = TeamID, T_2_Seed = Seed),
              by = c("Season", "T_2_ID"))

#Add season winning % and avg pts for and against
reg_szn_stats <-
    left_join(dfs$MRegularSeasonCompactResults %>% 
              group_by(Season, TeamID = WTeamID) %>% 
              summarise(Wins = n(), #calculate wins/losses/pts when team wins
                        PtsFor_W = sum(WScore),
                        PtsAgn_W = sum(LScore)),
          dfs$MRegularSeasonCompactResults %>% 
              group_by(Season, TeamID = LTeamID) %>% 
              summarise(Losses = n(), #calculate wins/losses/pts when team loses
                        PtsFor_L = sum(LScore),
                        PtsAgn_L = sum(WScore)),
          by = c("Season", "TeamID")) %>% 
    mutate(WinPct = Wins / (Wins + Losses),
           AvgPtsFor = (PtsFor_W + PtsFor_L) / (Wins + Losses),
           AvgPtsAgn = (PtsAgn_W + PtsAgn_L) / (Wins + Losses))

tourney_games %<>% 
    left_join(.,
              reg_szn_stats %>% 
                  select(T_1_ID = TeamID, T_1_WinPct = WinPct,
                         T_1_AvgPtsFor = AvgPtsFor, T_1_AvgPtsAgn = AvgPtsAgn),
              by = c("Season", "T_1_ID")) %>% 
    left_join(.,
              reg_szn_stats %>% 
                  select(T_2_ID = TeamID, T_2_WinPct = WinPct,
                         T_2_AvgPtsFor = AvgPtsFor, T_2_AvgPtsAgn = AvgPtsAgn),
              by = c("Season", "T_2_ID"))

