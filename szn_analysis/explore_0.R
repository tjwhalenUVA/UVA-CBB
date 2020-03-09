#Packages----
library(tidyverse)
library(magrittr)

#Read Data----
data.folder <- file.path(getwd(), 'data')
df.19 <- read.csv(file.path(data.folder, 'MEvents2019.csv'))
df.plyr <- read.csv(file.path(data.folder, 'MPlayers.csv'))
df.tms <- read.csv(file.path(data.folder, 'MDataFiles_Stage1', 'MTeams.csv'))

#Remove NCAAT games from dataframe so they don't 
df.19 %>% 
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


df.tms %>% filter(TeamID %in% c(1438, 1120))

df.plyr.cntr %>% 
    group_by(DayNum, TeamID, Result, contr.bin.m) %>% 
    count() %>% 
    filter(!is.na(contr.bin.m)) %>% 
    mutate(contr.weight = as.numeric(as.character(contr.bin.m)) * n) %>% 
    group_by(DayNum, TeamID, Result) %>% 
    summarise(contr.weight = sum(contr.weight)) %>% 
    ggplot() +
    geom_density(aes(x=contr.weight, fill=Result))
