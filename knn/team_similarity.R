#Setup----
library(tidyverse)
library(magrittr)
library(readr)
library(parallel)

#Get Data----
dataFolder <- paste(getwd(), '/data', sep='')
dataFiles <- list.files(dataFolder)
#Initiate Cluster
cl <- makeCluster(detectCores()-1)
#Give each core a connection to the database
clusterEvalQ(cl, {
    suppressMessages(library(readr))
    dataFolder <- paste(getwd(), '/data', sep='')
    dataFiles <- list.files(dataFolder)
    NULL
})
#Function for querying the database
readFiles <- function(fileName) { read.csv(file=paste(dataFolder, fileName, sep="/")) }
#Read all tables into result
cbb <- parSapply(cl, dataFiles, readFiles)
#shut down cluster
stopCluster(cl)

#Compute Stats----
rs.df <- cbb$RegularSeasonDetailedResults.csv
teams <- cbb$Teams.csv

metrics <- c("Season", "Numot",
             names(select(rs.df, contains("L", ignore.case = F))) %>% 
                 str_replace(., "L", ""),
             "Result")

split_result <- function(result){
    df <- 
        rs.df %>% 
        select(Season, Numot, starts_with(result)) %>%
        mutate(Result = result)
    if(result == "W"){ df %<>% select(-Wloc) }
    colnames(df) <- metrics
    return(df)
}

rs.df <- bind_rows(split_result("W"), split_result("L"))

#Prepare Data for ML Model----
library(mlr)
#Calculate stats for the season
df <- 
    rs.df %>%
    select(-Result) %>%
    group_by(Season, team) %>%
    summarise_all(mean) %>%
    left_join(.,
              rs.df %>%
                  select(Season, team, Result) %>%
                  group_by(Season, team, Result) %>%
                  count() %>%
                  spread(Result, n, fill=0), 
              by=c("Season", "team")) %>%
    ungroup()
#Normalize features
df.n <-
    normalizeFeatures(df, method = "range", 
                      cols = names(df %>% select(-Season, -team, -L, -W))) %>%
    mutate(pWin = W / (W + L)) %>%
    select(-W, -L)

like_teams <- function(tname, nbrs, szn = 2017){
    #Remove Current season data
    df.hist <- df.n %>% filter(Season != szn)
    df.17 <- df.n %>% filter(Season == szn)
    #Find team of interest for current season
    #tname <- "Virginia"
    tid <- filter(teams, Team_Name == tname)$Team_Id
    tstat.17 <- filter(df.17, team == tid)
    
    #Add team of interest to previous season data
    df.eval <- bind_rows(df.hist, tstat.17)
    #Calculate distance matrix
    dist.mat <-
        df.eval %>%
        select(-Season, -team) %>%
        dist(.) %>%
        as.matrix(.)
    #Sort distances to find nearest n teams
    tourneyWins <-
        cbb$TourneyCompactResults.csv %>%
        group_by(Season, Wteam) %>%
        count() %>%
        rename(team = Wteam, wins = n)
    
    df.sort <- 
        df.eval %>%
        mutate(distance = dist.mat[ncol(dist.mat),]) %>%
        select(Season, team, distance) %>%
        left_join(.,
                  tourneyWins,
                  by=c("Season", "team")) %>%
        filter(!is.na(wins))
    #Filter down to n neighbors
    #nbrs <- 3
    nbrs <- nbrs + 1
    kteams <- df.sort[order(df.sort$distance),][2:nbrs,]
    #Get team names
    kteams <-
        left_join(kteams, 
                  teams %>% rename(team = Team_Id), 
                  by="team")
    results <- list("wins" = mean(kteams$wins),
                    "similar" = kteams)
    return(results)
}

trn.17 <- filter(cbb$TourneySeeds.csv, Season == 2017)
wins <- c()
for(t in trn.17$Team){
    w <- like_teams(tname = teams[teams$Team_Id == t,]$Team_Name, 
                    nbrs = 7)
    print(paste(teams[teams$Team_Id == t,]$Team_Name, w$wins, sep=": "))
    wins <- c(wins, w$wins)
}

trn.17.pred <- 
    trn.17 %>%
    mutate(predW = wins) %>%
    left_join(., 
              teams %>% rename(Team = Team_Id), 
              by="Team")

trn.17.pred %>%
    mutate(region = str_sub(Seed, 1, 1),
           seedNum = str_replace(Seed, region, ""))

match_up <- function(seed){
    opp <- 16 - seed + 1
    print(paste(seed, opp, sep=" vs "))
}
