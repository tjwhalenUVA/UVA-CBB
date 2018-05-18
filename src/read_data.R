dataFolder <- paste(getwd(), '/data', sep='')
dataFiles <- list.files(dataFolder)

#Initiate Cluster
cl <- makeCluster(detectCores()-2)
#Give each core a connection to the database
clusterEvalQ(cl, {
    suppressMessages(library(readr))
    dataFolder <- paste(getwd(), '/data', sep='')
    dataFiles <- list.files(dataFolder)
    NULL
})
#Function for querying the database
readFiles <- function(fileName) {
    read.csv(file=paste(dataFolder, fileName, sep="/"))
}
#Read all tables into result
cbb <- parSapply(cl, dataFiles, readFiles)

#shut down cluster
stopCluster(cl)
