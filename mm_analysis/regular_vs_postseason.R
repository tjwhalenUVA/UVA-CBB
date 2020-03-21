#Packages----
library(tidyverse)
library(magrittr)

#Read Data----
data.folder <- file.path(getwd(), 'data', 'MDataFiles_Stage1')
df.mm <- read.csv(file.path(data.folder, 'MNCAATourneyDetailedResults.csv'))
df.rs <- read.csv(file.path(data.folder, 'MRegularSeasonDetailedResults.csv'))

