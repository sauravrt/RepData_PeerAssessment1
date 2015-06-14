# Reproducible Research: Peer Assessment 1
Saurav R Tuladhar  


## Loading and preprocessing the data


```r
data <- read.csv("activity.csv")  
data.clean <- subset(data, !is.na(data$steps))
```

## What is mean total number of steps taken per day?
* Make a histogram of total number of steps taken and compute mean, median

```r
library("ggplot2")
ggplot(data.clean, aes(x = steps)) + geom_histogram(binwidth = 10)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
steps.mean <- mean(data.clean$steps)
steps.median <- median(data.clean$steps)
print(steps.mean)
```

```
## [1] 37.3826
```

```r
print(steps.median)
```

```
## [1] 0
```


## What is the average daily activity pattern?
Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
data.interval <- split(data$steps, data$interval)
steps.interval.mean <- ((lapply(data.interval, function(x) mean(x, na.rm = TRUE))))
plot(levels(as.factor(data$interval)), unlist(steps.interval.mean), type = 'l', xlab = "interval", ylab = "avg. steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
max.mean <- max(unlist(steps.interval.mean))
levels(as.factor(data$interval))[unlist(steps.interval.mean) == max.mean]
```

```
## [1] "835"
```

## Imputing missing values
Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Create a new dataset that is equal to the original dataset but with the missing data filled in. 

Each entry of data.interval is number of steps on a particular interval over the two month time period.

Loop over all the intervals and replace NA's with mean

```r
interval.levels <- levels(as.factor(data$interval))
for(idx in interval.levels){
  data.interval.idx <- data.interval[[idx]]
  data.interval.idx[is.na(data.interval.idx)] = steps.interval.mean[[idx]]
  data.interval[[idx]] <- data.interval.idx
}
steps.filled <- unsplit(data.interval, data$interval)
data.new <- data.frame(steps.filled, date = data$date, interval = data$interval)
```



Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
ggplot(data.new, aes(x = steps.filled)) + geom_histogram(binwidth = 10)
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png) 

```r
steps.filled.mean <- mean(data.new$steps.filled)
steps.filled.median <- median(data.new$steps.filled)
print(steps.filled.mean)
```

```
## [1] 37.3826
```

```r
print(steps.filled.median)
```

```
## [1] 0
```


## Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
day.classify <- function(day){
  day.class <- "weekday"
  if(day == "Saturday" | day == "Sunday"){
    day.class <- "weekend"
    }
  day.class
  }

day.list <- lapply(X = as.list(weekdays(as.Date(data.new$date))), FUN = day.classify)
data.new$daytype <- as.factor(as.character(day.list))
```
Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was created using simulated data:


```r
data.new.interval <- split(data.new$steps.filled, data.new$interval)
steps.new.interval.mean <- ((lapply(data.new.interval, function(x) mean(x, na.rm = TRUE))))
```

