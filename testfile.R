stepsFill <- apply(filledData, MARGIN = 1, function(x, refData = patDailyDF)
{
    if(is.na(x[1]))
    {
        x[1] <- refData$patternDaily [which(refData$interv %in% x[2])]
    }
    return(t(x))
}
)
