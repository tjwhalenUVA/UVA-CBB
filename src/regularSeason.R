regSznDet <- cbb$RegularSeasonDetailedResults.csv

teams <- cbb$Teams.csv

uva_id <- filter(teams, Team_Name == 'Virginia')$Team_Id


regSznDet %>%
    filter(Wteam == uva_id | Lteam == uva_id) %>%
    mutate(Virginia = if_else(Wteam == uva_id, 
                           'Win', 
                           'Lost')) %>%
    group_by(Season, Virginia) %>%
    summarise(Games = n()) %>%
    ggplot() +
    geom_bar(aes(x=Season, 
                 y=Games, 
                 fill=Virginia), 
             stat = 'identity', 
             position = 'dodge') +
    geom_vline(xintercept = 2008.5)


#Model Points Scored====
regSznDet %>%
    mutate(Season = as.character(Season)) %>%
    ggplot() +
    geom_density(aes(x=Wscore, 
                       fill=Season), 
                   alpha=.4)

regSznDet %>%
    mutate(Season = as.character(Season)) %>%
    ggplot() +
    geom_density(aes(x=Wscore, 
                     fill=Season), 
                 alpha=.4)

regSznDet %>%
    mutate(Season = as.character(Season)) %>%
    select(Wscore, Lscore, Season) %>%
    gather(Outcome, Score, -Season) %>%
    ggplot() +
    geom_density(aes(x=Score, 
                     fill=Outcome), 
                 alpha=.4) +
    facet_wrap(~Season)

df_names <- 
    regSznDet %>%
    select(contains('W', ignore.case=F)) %>%
    select(-Wloc) %>%
    names(.) %>%
    substring(., 2)

winners <-
    regSznDet %>%
    select(contains('W', ignore.case=F)) %>%
    select(-Wloc)
names(winners) <- df_names
winners %<>% mutate(Outcome = 'Win')

losers <-
    regSznDet %>%
    select(contains('L', ignore.case=F))
names(losers) <- df_names
losers %<>% mutate(Outcome = 'Lose')


winners %>%
    bind_rows(., 
              losers) %>%
    select(-team) %>%
    gather(Stat, Value, -Outcome) %>%
    ggplot() +
    geom_density(aes(x=Value, 
                     fill=Outcome), 
                 alpha=.4) +
    facet_wrap(~Stat, 
               scales = 'free')
