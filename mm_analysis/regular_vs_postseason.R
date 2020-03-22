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
        ungroup() %>% 
        mutate_at(vars(-Season, -TeamID, -Games), funs(. / Games))
}

df.rs.summ <- summarise_df(df.rs.c)
df.mm.summ <- summarise_df(df.mm.c)



df.mm.comp <-
    df.rs.summ %>%
    gather(Metric, rs_value, -Season, -TeamID) %>% 
    left_join(., 
              df.mm.summ %>%
                  gather(Metric, mm_value, -Season, -TeamID),
              by = c('Season', 'TeamID', 'Metric')) %>% 
    filter(!is.na(mm_value),
           !(Metric %in% c('Games', 'NumOT', 'wins')))

team.set <- unique(df.mm.comp[c("Season", "TeamID")])

df.sim <- data.frame()

for (r in 1:nrow(team.set)) {
    szn <- team.set[r, "Season"][[1]]
    tid  <- team.set[r, "TeamID"][[1]]
    df.szn.tid <- filter(df.mm.comp, Season == szn, TeamID == tid)
    sim <- dist(rbind(df.szn.tid$rs_value, df.szn.tid$mm_value))
    df.sim <- bind_rows(df.sim,
                        data.frame(Season = szn, TeamID = tid,
                                   Similarity = as.numeric(sim)))
}

df.sim %>% 
    left_join(.,
              df.mm.summ %>% select(Season, TeamID, Games),
              by = c("Season", "TeamID")) %>% 
    mutate(Sweet16 = ifelse(Games >= 3, 'Yes', 'No'),
           FinalFour = ifelse(Games >= 5, 'Yes', 'No')) %>% 
    filter(Games > 1) %>% 
    ggplot(aes(x=Sweet16, y=Similarity, group=Sweet16)) +
    geom_boxplot()


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

