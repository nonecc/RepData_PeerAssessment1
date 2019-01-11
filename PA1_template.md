---
title: "Reproducible Research: Peer Assessment 1"
author: "nonecc"
date: "January 10, 2019"
output: 
  html_document:
    keep_md: true
---

## Loading and preprocessing the data
Show any code that is needed to:

1. Load the data (i.e. \color{red}{\verb|read.csv()|}read.csv())
2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
#load tidyverse
library(tidyverse)
```

```
## -- Attaching packages --------------------------------------------- tidyverse 1.2.1 --
```

```
## v ggplot2 3.1.0     v purrr   0.2.5
## v tibble  1.4.2     v dplyr   0.7.8
## v tidyr   0.8.2     v stringr 1.3.1
## v readr   1.3.1     v forcats 0.3.0
```

```
## -- Conflicts ------------------------------------------------ tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
#set working directory
setwd("C:/Users/u105068/Documents/Coursera/Reproducible Research/RepData_PeerAssessment1")

#unzip data and read into tibble/dataframe
activity_data <- read_csv(unz("activity.zip", "activity.csv"))
```

```
## Parsed with column specification:
## cols(
##   steps = col_double(),
##   date = col_date(format = ""),
##   interval = col_double()
## )
```


## What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day
2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
3. Calculate and report the mean and median of the total number of steps taken per day



```r
#1
daily_steps <- activity_data %>% 
                    group_by(date) %>% 
                    summarise(total_steps = sum(steps))

#2
ggplot(data=subset(daily_steps, !is.na(total_steps)), aes(x=total_steps)) + 
    geom_histogram(bins=20) +
    labs(title="Histogram of Total Steps", 
         y="", x="Total Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
#3
daily_steps %>%
    summarise(mean_steps=mean(total_steps, na.rm=TRUE),
              median_steps=median(total_steps, na.rm=TRUE))
```

```
## # A tibble: 1 x 2
##   mean_steps median_steps
##        <dbl>        <dbl>
## 1     10766.        10765
```


## What is the average daily activity pattern?
1. Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?



```r
#1
interval_steps <- activity_data %>%
                    group_by(interval) %>%
                    summarise(average_steps=mean(steps, na.rm=TRUE))
ggplot(data=interval_steps, aes(x=interval, y=average_steps)) +
    geom_line() + 
    labs(title="Average Number of Steps by Interval",
         x="Interval", 
         y="Average Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
#2
interval_steps %>% 
    summarise(max_interval=interval[which.max(average_steps)], max_steps=max(average_steps))
```

```
## # A tibble: 1 x 2
##   max_interval max_steps
##          <dbl>     <dbl>
## 1          835      206.
```

## Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as \color{red}{\verb|NA|}NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?



```r
#1
activity_data %>% 
    filter(is.na(steps)) %>% 
    summarise(sum(is.na(.)))
```

```
## # A tibble: 1 x 1
##   `sum(is.na(.))`
##             <int>
## 1            2304
```

```r
#2 & 3 filling by average of the interval
activity_data_filled <- activity_data %>% 
                            group_by(interval) %>% 
                            mutate(steps=ifelse(is.na(steps), mean(steps, na.rm=TRUE), steps))

#4
daily_steps_filled <- activity_data_filled %>%
                        group_by(date) %>% 
                        summarise(total_steps = sum(steps))

ggplot(daily_steps_filled, aes(total_steps)) + 
    geom_histogram(bins=20) +
    labs(title="Histogram of Total Steps", 
         x="", y="Total Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
daily_steps_filled %>%
    summarise(mean_steps=mean(total_steps, na.rm=TRUE),
              median_steps=median(total_steps, na.rm=TRUE))
```

```
## # A tibble: 1 x 2
##   mean_steps median_steps
##        <dbl>        <dbl>
## 1     10766.       10766.
```

## Are there differences in activity patterns between weekdays and weekends?
For this part the \color{red}{\verb|weekdays()|}weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
2. Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
#1
activity_data_filled <- activity_data_filled %>% 
                            mutate(weekday=parse_factor(ifelse(weekdays(date, abbreviate=TRUE) %in% 
                                        c("Mon", "Tue", "Wed", "Thu", "Fre"), 'weekday', 'weekend')))

#2
interval_steps_filled <- activity_data_filled %>%
                            group_by(weekday, interval) %>%
                            summarise(average_steps=mean(steps, na.rm=TRUE))

ggplot(data=interval_steps_filled, aes(x=interval, y=average_steps)) +
    geom_line() +
    facet_grid(weekday~.) +
    labs(title="Average Number of Steps by Interval between Weekend vs Weekday",
         x="Interval", y="Average Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
