#Packages----
library(tidyverse)
library(magrittr)

#Read Data----
data.folder <- file.path(getwd(), 'data', 'MDataFiles_Stage1')
df.mm <- read.csv(file.path(data.folder, 'MNCAATourneyDetailedResults.csv'))
df.rs <- read.csv(file.path(data.folder, 'MRegularSeasonDetailedResults.csv'))


#Clean up data so each record is a single teams game stats
clean_df <- function(df, outcome){
    tmp <- 
        df %>% select(-WLoc) %>% 
        select(Season, contains(outcome, ignore.case = F), NumOT) %>% 
        mutate(wins = ifelse(outcome == 'W', 1, 0))
    names(tmp) <- str_replace(names(tmp), outcome, '')
    return(tmp)
}

df.mm.c <- bind_rows(clean_df(df.mm, 'W'), clean_df(df.mm, 'L'))
df.rs.c <- bind_rows(clean_df(df.rs, 'W'), clean_df(df.rs, 'L'))

summarise_df <- function(df){
    df %>% group_by(Season, TeamID) %>% 
        summarise(Games = n(), Score = sum(Score), FGM = sum(FGM),
                  FGA = sum(FGA), FGM3 = sum(FGM3), FGA3 = sum(FGA3),
                  FTM = sum(FTM), FTA = sum(FTA), OR = sum(OR),
                  DR = sum(DR), Ast = sum(Ast), TO = sum(TO),
                  Stl = sum(Stl), Blk = sum(Blk), PF = sum(PF),
                  NumOT = sum(NumOT), wins = sum(wins)) %>% 
        ungroup()
}

df.rs.summ <- summarise_df(df.rs.c)
df.FF.teams <- summarise_df(df.mm.c) %>% filter(Games >= 5)
