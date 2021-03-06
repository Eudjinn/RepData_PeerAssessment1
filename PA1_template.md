# Reproducible Research: Peer Assessment 1
Evgeniy Zabrodskiy  



## Loading and preprocessing the data
Unzip the file and read the data using read.csv, convert date from string to the Date format.


```r
activity <- read.csv(unz("activity.zip", "activity.csv"))
activity <- transform(activity, date = as.Date(date, "%Y-%m-%d"))

# make a subset without NA values
activity.woNA <- activity[!is.na(activity$steps), ]

# reset row index to make it sequential again
rownames(activity.woNA) <- NULL
```

Here are the first few rows of the original data:

```r
head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```
Here are a few rows with non-NA values:

```r
activity[1300:1306, ]
```

```
##      steps       date interval
## 1300   345 2012-10-05     1215
## 1301   345 2012-10-05     1220
## 1302    10 2012-10-05     1225
## 1303   485 2012-10-05     1230
## 1304   515 2012-10-05     1235
## 1305   168 2012-10-05     1240
## 1306     0 2012-10-05     1245
```

## What is mean total number of steps taken per day?

```r
# calculate total steps per date. tapply returns 1-dim array with date as 
# dimnames, we need to make a dataframe as it is a better format for plotting
total.steps <- as.vector(tapply(activity.woNA$steps, activity.woNA$date, sum))
total.steps.df <- data.frame(total.steps)
mean.ts <- as.integer(mean(total.steps))
median.ts <- as.integer(median(total.steps))
```

Rounded **mean** of the total steps per day: **10766**  
Rounded **median** of the total steps per day: **10765**    


```r
# calculate the range to set the binwidth of histogram 
ts.range = max(total.steps) - min(total.steps)

# median will be shown as a vertical green line on the histogram
ggplot(data = total.steps.df, aes(total.steps)) + 
    geom_histogram(binwidth = ts.range/10, col = "blue", aes(fill=..count..)) + 
    geom_vline(aes(xintercept = median.ts), color = "green", size = 1) +
    labs(title = "Total number of steps per day - histogram", 
         x = "Number of steps per day", 
         y = "Number of days")
```

![](PA1_template_files/figure-html/totals_hist-1.png) 

## What is the average daily activity pattern?

```r
# calculate average steps per interval. tapply returns 1-dim array with interval
# as dimnames, we need to make a dataframe as it is a better format for plotting
avg.steps <- tapply(activity.woNA$steps, activity.woNA$interval, mean)
avg.steps.df <- data.frame(interval = as.numeric(names(avg.steps)), 
                           average.steps = as.vector(avg.steps))

# This is needed to anser the question: Which 5-minute interval, on average 
# across all the days in the dataset, contains the maximum number of steps?
max.avg.steps.index <- which.max(avg.steps.df$average.steps)
max.avg.steps.interval <- avg.steps.df[max.avg.steps.index, "interval"]
max.avg.steps.value <- as.integer(avg.steps.df[max.avg.steps.index, "average.steps"])
```

Interval with the maximum number of steps on average across all days: **835**  
Rounded maximum number of steps on average in that interval: **206**  

Below is a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis). Maximum is shown as a red dot.


```r
ggplot(data = avg.steps.df, aes(interval, average.steps, group = 1)) +
    geom_line(aes(color = average.steps)) + 
    geom_point(data = NULL, 
               aes(max.avg.steps.interval, max.avg.steps.value), 
               color = "red", 
               size = 2) +
    labs(title = "Average number of steps per 5-minute interval across all days", 
         x = "5-minute interval", 
         y = "Average number of steps")
```

![](PA1_template_files/figure-html/averages_plot-1.png) 

## Imputing missing values

```r
# How many NA values are there?
total.NA = sum(is.na(activity$steps))
```

Total number of rows with NA values: **2304**

Imputing missing values will be done using mean steps per interval which were calculated earlier and stored in avg.steps.df.  
We take a subset of the original data where steps are NA and merge by interval with the dataframe containing mean steps per interval.  
Using the result of merging we create new dataframe with the same format as the original dataframe and add it to non-NA data from the original dataset. The result is a dataframe we wanted to make for further analysis.


```r
# Make a subset with NA values in order to create the same dataset with 
# corresponding mean values per inteval 
activity.NA <- activity[is.na(activity$steps), ]

# Mean values per intervals have been calculated previously and are stored in 
# avg.steps.df, it will be used for merging by interval
activity.imp <- activity.imp <- merge(activity.NA, 
                                      avg.steps.df, 
                                      by = "interval")

# Make new dataframe with mean values instead of NA values. 
# This dataframe will be added to activity.woNA and the result will be a 
# complete dataset with imputed values.
# Keep in mind that the order of dates and intervals is broken after 
# manipulations, it will be corrected later.
activity.imp <- data.frame(steps = activity.imp$average.steps, 
                           date = activity.imp$date, 
                           interval = activity.imp$interval)

# Create a new dataset that is equal to the original dataset but with the 
# missing data filled in. 
activity.new <- rbind(activity.woNA, activity.imp)

# Reorder all the rows by date and interval in order to fix the original order
activity.new <- activity.new[order(activity.new$date, activity.new$interval), ]

# reindex rows as a sequence since reordering of the rows messed it up
rownames(activity.new) <- NULL

# calculate total steps per date for the full dataset with imputed values. 
total.steps.full <- as.vector(tapply(activity.new$steps, activity.new$date, sum))
total.steps.full.df <- data.frame(total.steps.full)
mean.ts.full <- as.integer(mean(total.steps.full))
median.ts.full <- as.integer(median(total.steps.full))
```

Rounded **mean** of the total steps on a dataset with imputed values: **10766**  
Rounded **median** of the total steps on a dataset with imputed values: **10766**    

Below is a histogram of the total number of steps taken each day. 


```r
ggplot(data = total.steps.full.df, aes(total.steps.full)) + 
    geom_histogram(binwidth = ts.range/10, col = "blue", aes(fill=..count..)) + 
    # median will be shown as a vertical green line on the histogram.
    geom_vline(aes(xintercept = median.ts.full), color = "green", size = 1) +
    labs(title = "Total number of steps per day - histogram", 
         x = "Number of steps per day", 
         y = "Number of days")
```

![](PA1_template_files/figure-html/imputed_hist-1.png) 

These **mean** and **median** values for the dataset with imputed values are almost the same as for the dataset with NA values dropped but the shape of the histogram in the center looks different.

## Are there differences in activity patterns between weekdays and weekends?

```r
# Create a new factor variable in the dataset with two levels - "weekday" and 
# "weekend" indicating whether a given date is a weekday or weekend day
activity.new$day <- ifelse(weekdays(activity.new$date) == "Sunday" | 
                               weekdays(activity.new$date) == "Saturday",
                           "weekend",
                           "weekday")

# calculate average of steps per weekday per interval, as a result we get
# matrix with intervals as column names, weekday/weekend as rownames and
# corresponding averages as the values in the cells of the matrix
activity.new.day.avg <- tapply(activity.new$steps, 
                               list(activity.new$day, activity.new$interval),
                               mean)

# make a dataframe with weekday values
activity.avg.wd <- 
    data.frame(interval = as.integer(colnames(activity.new.day.avg)), 
               steps = as.vector(activity.new.day.avg["weekday", ]),
               day = "weekday")

# make a dataframe with weekend values
activity.avg.we <- 
    data.frame(interval = as.integer(colnames(activity.new.day.avg)), 
               steps = as.vector(activity.new.day.avg["weekend", ]),
               day = "weekend")

# combine two dataframes in order to use it in qplot with facets.
# all this transformation could be done using melt from reshape package but
# I wanted to use base R functionality in this assignment
activity.avg.day <- rbind(activity.avg.wd, activity.avg.we)
```

Below is a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
qplot(interval, 
      steps, 
      data = activity.avg.day, 
      geom = "line", 
      color = steps, 
      facets = day~.,
      main = "Average number of steps per 5-minute interval per day type",
      xlab = "5-munute interval",
      ylab = "Average number of steps")
```

![](PA1_template_files/figure-html/day_plot-1.png) 

