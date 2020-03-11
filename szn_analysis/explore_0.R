#Packages----
library(tidyverse)
library(magrittr)

#Read Data----
data.folder <- file.path(getwd(), 'data')
df.19 <- read.csv(file.path(data.folder, 'MEvents2019.csv'))
df.plyr <- read.csv(file.path(data.folder, 'MPlayers.csv'))
df.tms <- read.csv(file.path(data.folder, 'MDataFiles_Stage1', 'MTeams.csv'))

#Remove NCAAT games from dataframe so they don't 
df.19 %<>% 
    filter(DayNum < 134)

#Individual vs Distributed Scoring----
#Create a DF of player event counts
df.plyr.evnt <-
    df.19 %>%
    group_by(DayNum, WTeamID, PlayerID = EventPlayerID,
             TeamID = EventTeamID, EventType,
             WFinalScore, LFinalScore) %>% 
    count(.) %>% #gets the count of each event type by player
    ungroup() %>% 
    filter(PlayerID != 0) %>% #Remove events not associted to a player
    spread(EventType, n, fill = 0)

#DF of players contribution in pnts towards teams total
df.plyr.cntr <-
    df.plyr.evnt %>% 
    select(DayNum, WTeamID, PlayerID, TeamID,
           contains('made'), WFinalScore, LFinalScore) %>% 
    mutate(PlayerScore = made1 * 1 + made2 * 2 + made3 * 3,
           TeamScore = ifelse(WTeamID == TeamID, WFinalScore, LFinalScore),
           Result = ifelse(WTeamID == TeamID, 'Win', 'Lose'),
           Contribution = PlayerScore / TeamScore) %>% 
    select(DayNum, PlayerID, TeamID, PlayerScore,
           TeamScore, Result, Contribution) %>% 
    mutate(contr.bin = cut(Contribution, breaks = seq(0, 1, .1)),
           contr.bin.m = cut(Contribution,
                             breaks = seq(0, 1, .1),
                             labels = seq(0.05,  0.95, 0.1)))

#Number of Players who Scored
df.plyr.cntr %>% 
    filter(PlayerScore > 0) %>% 
    group_by(DayNum, TeamID, Result) %>% 
    count() %>% 
    ggplot() +
    geom_bar(aes(x=n, fill=Result), position = 'dodge') +
    labs(title = 'Number of Players Contributing to Scoring',
         x = 'No. Players Scored', y = 'Games') +
    theme_classic()

#Teams with 50% of scoring coming from <= 2 players
df.plyr.cntr %>% 
    group_by(DayNum, TeamID, TeamScore, Result) %>%
    mutate(rank = rank(desc(Contribution))) %>%
    filter(rank %in% c(1:2)) %>% 
    summarise(TwoPlayerContr = sum(Contribution)) %>% 
    ggplot() +
    geom_density(aes(x=TwoPlayerContr, fill=Result), alpha = 0.3)

df.plyr.cntr %>% 
    group_by(DayNum, TeamID, TeamScore, Result) %>%
    mutate(rank = rank(desc(Contribution))) %>%
    filter(rank %in% c(1:2)) %>% 
    summarise(TwoPlayerContr = sum(Contribution)) %>% 
    ggplot() +
    geom_point(aes(x=TeamScore, y=TwoPlayerContr,
                   color=Result), alpha = 0.3)

#Does the # of players contributing x% of the points
#make a diff in the result
df.50 <-
    df.plyr.cntr %>% 
    group_by(DayNum, TeamID, TeamScore, Result) %>%
    mutate(rank = rank(desc(Contribution))) %>% 
    arrange(rank) %>% 
    mutate(GroupContr = cumsum(Contribution)) %>% 
    filter(GroupContr >= 0.6) %>% 
    summarise(NumPlayers = floor(min(rank))) %>% 
    ungroup()

df.50 %>% 
    group_by(Result, NumPlayers) %>% 
    count() %>% 
    ggplot() +
    geom_bar(aes(x=NumPlayers, y=n, fill=Result),
             stat = 'identity', position = 'dodge')
