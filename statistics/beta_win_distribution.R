library(stringi)

gameList <- NULL
gameList$Result <- c('W', 'L')
gameList$szn.09 <- c(15, 16)
gameList$szn.10 <- c(16, 15)
gameList$szn.11 <- c(22, 10)
gameList$szn.12 <- c(23, 12)
gameList$szn.13 <- c(30, 7)
gameList$szn.14 <- c(30, 4)
gameList$szn.15 <- c(29, 8)
gameList$szn.16 <- c(23, 11)
gameList$szn.17 <- c(26, 2)

data.frame('winPercent' = seq(0.25, 1, by = 0.001)) %>%
    mutate(szn.09 = dbeta(winPercent, gameList$szn.09[1], gameList$szn.09[2]), 
           szn.10 = dbeta(winPercent, gameList$szn.10[1], gameList$szn.10[2]), 
           szn.11 = dbeta(winPercent, gameList$szn.11[1], gameList$szn.11[2]), 
           szn.12 = dbeta(winPercent, gameList$szn.12[1], gameList$szn.12[2]), 
           szn.13 = dbeta(winPercent, gameList$szn.13[1], gameList$szn.13[2]), 
           szn.14 = dbeta(winPercent, gameList$szn.14[1], gameList$szn.14[2]), 
           szn.15 = dbeta(winPercent, gameList$szn.15[1], gameList$szn.15[2]), 
           szn.16 = dbeta(winPercent, gameList$szn.16[1], gameList$szn.16[2]), 
           szn.17 = dbeta(winPercent, gameList$szn.17[1], gameList$szn.17[2])) %>%
    gather(Season, Probability, -winPercent) %>%
    ggplot(aes(x = winPercent, 
               y = Probability, 
               color = Season)) +
    geom_line(stat = 'identity')
