

##### Number of Championships ------------------------
mm.ff.teams %>% 
    group_by(Season, Champion) %>% 
    count() %>% 
    filter(Champion == 'Yes',
           n > 1)

