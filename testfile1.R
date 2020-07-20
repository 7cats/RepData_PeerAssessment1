for (i in 1:dim(filledData)[1]) {
    if(is.na(filledData[i,1]))
    {
        filledData[i,1] <- patDailyDF$patternDaily [which(patDailyDF$interv %in% filledData[i,2])]
    }
    
}
