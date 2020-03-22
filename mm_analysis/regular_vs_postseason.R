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

#Summarise each teams season into a single record
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
df.mm.summ <- summarise_df(df.mm.c)



df.rs.summ %>%
    left_join(df.mm.summ %>%
                  select(Season, TeamID) %>%
                  mutate(MM = 'yes'),
              by = c('Season', 'TeamID')) %>% 
    filter(MM == 'yes') %>% 
    select(-MM) %>% 
    mutate_at(vars(-Season, -TeamID, -Games), funs(. / Games))




#View AVGs vs Winning PCT
df.rs.summ %>%
    filter(Season == 2019) %>% 
    mutate_at(vars(-Season, -TeamID, -Games), funs(. / Games)) %>% 
    gather(metric, value, -Season, -TeamID, -Games, -wins) %>% 
    ggplot(aes(y = wins, x = value)) +
    geom_point() +
    facet_wrap(~metric, scales = 'free')
















#Best 3pt Shooting Teams [no apparent trend]
df.rs.summ %>% 
    filter(Season == 2019) %>% 
    mutate(F3vol = FGM3 / FGM,
           WPct = wins / Games) %>% 
    ggplot() +
    geom_point(aes(x=WPct, y=F3vol)) +
    scale_y_continuous(limits = c(0,1))

