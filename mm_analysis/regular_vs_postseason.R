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
        mutate(result = outcome)
    names(tmp) <- str_replace(names(tmp), outcome, '')
    return(tmp)
}

df.mm.c <- bind_rows(clean_df(df.mm, 'W'), clean_df(df.mm, 'L'))
df.rs.c <- bind_rows(clean_df(df.rs, 'W'), clean_df(df.rs, 'L'))

