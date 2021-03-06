#Read in Data --------------------------------------------------------------
folder <- paste0(getwd(), '/data/MDataFiles_Stage1')
files <- list.files(folder)
dfs <- list()

for (f in files){
    df.name <- str_replace(f, '.csv', '')
    dfs[[df.name]] <- vroom(file.path(folder, f))
}

#Additional DFs --------------------------------------------------------------
#Create features from other DFs to add to the data for modeling
#Regular Season Statistics
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
              by = c("Season", "TeamID")) 

reg_szn_stats[is.na(reg_szn_stats)] = 0 #some teams never lost and others never won

reg_szn_stats %<>% 
    mutate(WinPct = Wins / (Wins + Losses), #season winning %
           AvgPtsFor = (PtsFor_W + PtsFor_L) / (Wins + Losses), #Average Score
           AvgPtsAgn = (PtsAgn_W + PtsAgn_L) / (Wins + Losses)) #Average Points against


#Feature Adding Functions ------------------------------------------------
#Add team seeds to the feature df
add_seeds <- function(df){
    tourney_seeds <- dfs$MNCAATourneySeeds %>% mutate(Seed = parse_number(Seed))
    df %<>% 
        left_join(., tourney_seeds %>% rename(T_1_ID = TeamID, T_1_Seed = Seed),
                  by = c("Season", "T_1_ID")) %>% 
        left_join(., tourney_seeds %>% rename(T_2_ID = TeamID, T_2_Seed = Seed),
                  by = c("Season", "T_2_ID"))
    return(df)
}

#Add reg season stats
add_RegSznStats <- function(df){
    df %<>% 
        left_join(., #Add stats for Team 1
                  reg_szn_stats %>% 
                      select(Season, T_1_ID = TeamID, T_1_WinPct = WinPct,
                             T_1_AvgPtsFor = AvgPtsFor, T_1_AvgPtsAgn = AvgPtsAgn),
                  by = c("Season", "T_1_ID")) %>% 
        left_join(., #Add stats for Team 2
                  reg_szn_stats %>% 
                      select(Season, T_2_ID = TeamID, T_2_WinPct = WinPct,
                             T_2_AvgPtsFor = AvgPtsFor, T_2_AvgPtsAgn = AvgPtsAgn),
                  by = c("Season", "T_2_ID"))
    return(df)
}

#Add team conferences
add_conference <- function(df){
    conferences <- dfs$MTeamConferences %>% mutate(ConfAbbrev = factor(ConfAbbrev))
    df %<>% 
        left_join(., #Add conference for Team 1
                  conferences %>% 
                      select(Season, T_1_ID = TeamID, T_1_Conf = ConfAbbrev),
                  by = c("Season", "T_1_ID")) %>% 
        left_join(., #Add stats for Team 2
                  conferences %>% 
                      select(Season, T_2_ID = TeamID, T_2_Conf = ConfAbbrev),
                  by = c("Season", "T_2_ID"))
    return(df)
}

#Previous Year Tournament Participant
add_PreSznTourn <- function(df){
    prev_szn_trn <- 
        dfs$MNCAATourneySeeds %>% 
        mutate(Season = Season + 1, MadeTourn = 1) %>% 
        select(Season, TeamID, MadeTourn)
    df %<>% 
        left_join(., #Add prev szn tourn participation for Team 1
                  prev_szn_trn %>% 
                      select(Season, T_1_ID = TeamID, T_1_MadeTourn = MadeTourn),
                  by = c("Season", "T_1_ID")) %>% 
        left_join(., #Add prev szn tourn participation for Team 2
                  prev_szn_trn %>% 
                      select(Season, T_2_ID = TeamID, T_2_MadeTourn = MadeTourn),
                  by = c("Season", "T_2_ID")) %>% 
        mutate(T_1_MadeTourn = if_else(is.na(T_1_MadeTourn), 0, T_1_MadeTourn),
               T_2_MadeTourn = if_else(is.na(T_2_MadeTourn), 0, T_1_MadeTourn))
}


#Convert specific columns to factors
convert_to_factor <- function(df){
    df %<>%
        mutate(Season = factor(Season), 
               T_1_Seed = factor(T_1_Seed),
               T_2_Seed = factor(T_2_Seed),
               T_1_Win = factor(T_1_Win),
               T_1_MadeTourn = factor(T_1_MadeTourn),
               T_2_MadeTourn = factor(T_2_MadeTourn))
    return(df)
}

#Train/Test Split
# train_test <- function(df, set = 'train', split.seed = 123){
#     set.seed(split.seed) #helps return same split each time
#     df %<>% mutate(split = sample.split(T_1_Win, SplitRatio=0.8))
#     
#     #Create 2 DFs for test and train
#     train <- filter(df, split == T) %>% select(-split, -Season, -T_1_ID, -T_2_ID)
#     test <- filter(df, split == F) %>% select(-split, -Season, -T_1_ID, -T_2_ID)
#     
#     if (set == 'train'){ return(train) }
#     else { return(test) }
# }

#Initial DF Creation -------------------------------------------------------
#Set up dataframe
tourney_games <- #randomly shuffle winners/losers so the inner is not always the same column
    dfs$MNCAATourneyCompactResults %>% 
    mutate(T_1_Win = rbinom(nrow(dfs$MNCAATourneyCompactResults), 1, 0.5), #Target variable
           #Set team 1 to WTeamID if target says T1 wins
           T_1_ID = ifelse(T_1_Win == 1, WTeamID, LTeamID),
           #Set team 2 to LTeamID if target says T1 wins
           T_2_ID = ifelse(T_1_Win == 1, LTeamID, WTeamID),
           #Feature indicating wether game went to OT or not
           OT = ifelse(NumOT == 0, 0, 1)) %>% 
    select(T_1_Win, Season, T_1_ID, T_2_ID, OT)

#Add new features to DF
tourney_games %<>% 
    add_seeds(.) %>% 
    add_RegSznStats(.) %>% 
    add_conference(.) %>%
    add_PreSznTourn(.) %>% 
    convert_to_factor(.)

#Seperate year to predict (test) from historical tournaments
train <- 
    tourney_games %>% filter(Season != 2019) %>% 
    select(-Season, -T_1_ID, -T_2_ID)
test <- 
    tourney_games %>% filter(Season == 2019) %>% 
    select(-Season, -T_1_ID, -T_2_ID)