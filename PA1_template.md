---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
Loading the data and transform the date variable format; load package

```r
acti <- read.csv( unz("activity.zip", filename = "activity.csv") , header = TRUE, sep = ",")
acti$date <- as.Date(acti$date)

library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 4.0.2
```
## What is mean total number of steps taken per day?
Calculate the total steps take per day and its mean and median. Also the histogram of daily steps are plotted.

```r
stepDay <- tapply(acti$steps, acti$date, sum, na.rm = TRUE)
stepDayDF <- data.frame(stepTtlDay = stepDay, Day = unique(acti$date))

theme_update(plot.title = element_text(hjust = 0.5))
g <- ggplot(stepDayDF, aes(stepTtlDay))
g + geom_histogram(binwidth = 1000) + labs(x = "Total steps per day", 
                                           title = "Histogram of Total steps taken per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
stepDayAve <- mean(stepDay, na.rm = T)
stepDayMed <- median(stepDay, na.rm = T)
```
The average daily steps is 9354.2295082 and the median of daily steps is 10395.

## What is the average daily activity pattern?

```r
interval <- strptime(sprintf("%04d", acti$interval),format = "%H%M")
intervalNum <- interval$hour*60 + interval$min
patDaily <- tapply(acti$steps, acti$interval,mean,na.rm = TRUE)
patDailyDF <- data.frame(patternDaily = patDaily, interv = unique(intervalNum))

g <- ggplot(patDailyDF, aes(x = factor(interv), y = patternDaily, group = 1))
g + geom_line(size = 1) + labs(x = "Interval(hour)", y = "Steps", title = "Daily pattern of activity") +
  scale_x_discrete( breaks = as.character(seq(min(intervalNum),max(intervalNum+5),60*6)),
                    labels = as.character(seq(min(intervalNum)/60,max(intervalNum+5)/60,6)) )
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
maxStep <- max(patDaily)
maxStepInterval <- format(interval[which.max(patDaily)], format = "%H:%M")
```
The daily maximum steps took within one interval is 206.1698113 and the interval is 08:35

## Imputing missing values




## Are there differences in activity patterns between weekdays and weekends?
