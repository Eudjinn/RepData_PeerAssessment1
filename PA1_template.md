# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
activity <- read.csv(unz("activity.zip", "activity.csv"))
activity <- transform(activity, date = as.Date(date, "%Y-%m-%d"))
activity.woNA <- activity[!is.na(activity$steps), ]
```

## What is mean total number of steps taken per day?

```r
total.steps <- as.vector(tapply(activity.woNA$steps, activity.woNA$date, sum))
total.steps.df <- data.frame(total.steps)
mean.ts <- mean(total.steps)
median.ts <- median(total.steps)

ts.range = max(total.steps) - min(total.steps)

library(ggplot2)
ggplot(data = total.steps.df, aes(total.steps)) + 
    geom_histogram(binwidth = ts.range/10, col = "blue", aes(fill=..count..)) + 
    geom_vline(aes(xintercept = median.ts), color = "green", size = 1)
```

![](PA1_template_files/figure-html/totals-1.png) 

```r
print(paste("Mean of the total steps: ", mean.ts))
```

```
## [1] "Mean of the total steps:  10766.1886792453"
```

```r
print(paste("Median of the total steps: ", median.ts))
```

```
## [1] "Median of the total steps:  10765"
```

## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
