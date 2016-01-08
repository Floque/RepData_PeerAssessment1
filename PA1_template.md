# PA1_template
Florian Schederecker  
8. Januar 2016  

This assignment will be described in multiple parts. You will need to write a report that answers the questions detailed below. Ultimately, you will need to complete the entire assignment in a single R markdown document that can be processed by knitr and be transformed into an HTML file.

Throughout your report make sure you always include the code that you used to generate the output you present. When writing code chunks in the R markdown document, always use echo = TRUE so that someone else will be able to read the code. This assignment will be evaluated via peer assessment so it is essential that your peer evaluators be able to review the code for your analysis.

For the plotting aspects of this assignment, feel free to use any plotting system in R (i.e., base, lattice, ggplot2)

Fork/clone the GitHub repository created for this assignment. You will submit this assignment by pushing your completed files into your forked repository on GitHub. The assignment submission will consist of the URL to your GitHub repository and the SHA-1 commit ID for your repository state.

NOTE: The GitHub repository also contains the dataset for the assignment so you do not have to download the data separately.

---

Loading and preprocessing the data

Show any code that is needed to

1.Load the data (i.e. read.csv())

opts_chunk$set(echo = TRUE)


```r
setwd("G:/Praktikum Helmholtz/R/Coursera/Reproducible Research/Reproducible Research Week 1/Assignment Course Project 1")
df <- read.csv("activity.csv")
```


2.Process/transform the data (if necessary) into a format suitable for your analysis

---

What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

1.Calculate the total number of steps taken per day



```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## Die folgenden Objekte sind maskiert von 'package:stats':
## 
##     filter, lag
```

```
## Die folgenden Objekte sind maskiert von 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(lubridate)
library(ggplot2)
str(df)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
df$date <- ymd(df$date)
df$steps <- as.numeric(df$steps)


steps <- df %>% filter(!is.na(steps)) %>% group_by(date) %>% summarize(steps=sum(steps))
steps
```

```
## Source: local data frame [53 x 2]
## 
##          date steps
##        (time) (dbl)
## 1  2012-10-02   126
## 2  2012-10-03 11352
## 3  2012-10-04 12116
## 4  2012-10-05 13294
## 5  2012-10-06 15420
## 6  2012-10-07 11015
## 7  2012-10-09 12811
## 8  2012-10-10  9900
## 9  2012-10-11 10304
## 10 2012-10-12 17382
## ..        ...   ...
```



2.If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day


```r
ggplot(steps, aes(x = steps)) +
  geom_histogram(fill="blue", binwidth = 1000) +
  labs(title = "Histogram: Steps per day", x = "Steps per day", y= "Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)\

```r
hist(steps$steps, breaks =20, col = "blue", xlab  = "Steps per day", main = "Histogram: Steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-2.png)\

3.Calculate and report the mean and median of the total number of steps taken per day


```r
mean(steps$steps)
```

```
## [1] 10766.19
```

```r
median(steps$steps)
```

```
## [1] 10765
```


Mean steps taken per day are 10770 and meadian steps are 10760.
---

What is the average daily activity pattern?

1.Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
interval <- df %>% filter(!is.na(steps)) %>%  group_by(interval) %>%
  summarize(steps = mean(steps))

plot(interval$interval, interval$steps, type="l", col="red")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)\

2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
interval[which.max(interval$steps),]
```

```
## Source: local data frame [1 x 2]
## 
##   interval    steps
##      (int)    (dbl)
## 1      835 206.1698
```

The Peak is at Interval 835 with 206.1698 steps.

---

Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(df))
```

```
## [1] 2304
```

The total number of missing values in the dataset are 2304.


2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


I going to fill in the missing nas with the mean number of steps in the same 5-min interval, and im going to use tapply for that procedure.


3.Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
df_full <- df
na <- is.na(df_full$steps)
table(na)
```

```
## na
## FALSE  TRUE 
## 15264  2304
```

```r
interval_mean <- tapply(df_full$steps, df_full$interval, mean, na.rm=TRUE, simplify=TRUE)

##comparing length of the vectors

length(df_full$steps[na]) 
```

```
## [1] 2304
```

```r
length(interval_mean[df_full$interval[na]])
```

```
## [1] 2296
```

```r
length(interval_mean[as.character(df_full$interval[na])])
```

```
## [1] 2304
```

```r
## seems to only work out with character vector

df_full$steps[na] <- interval_mean[as.character(df_full$interval[na])]



sum(is.na(df_full))
```

```
## [1] 0
```

```r
## imputing was succesful
```



4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
steps_full <- df_full %>% filter(!is.na(steps)) %>% group_by(date) %>% summarize(steps=sum(steps))
steps_full
```

```
## Source: local data frame [61 x 2]
## 
##          date    steps
##        (time)    (dbl)
## 1  2012-10-01 10766.19
## 2  2012-10-02   126.00
## 3  2012-10-03 11352.00
## 4  2012-10-04 12116.00
## 5  2012-10-05 13294.00
## 6  2012-10-06 15420.00
## 7  2012-10-07 11015.00
## 8  2012-10-08 10766.19
## 9  2012-10-09 12811.00
## 10 2012-10-10  9900.00
## ..        ...      ...
```

```r
hist(steps_full$steps, breaks=20, col="blue", xlab= "Steps per Day", main = "Histogram: Steps per Day with Imputation")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)\

```r
mean(steps_full$steps)
```

```
## [1] 10766.19
```

```r
median(steps_full$steps)
```

```
## [1] 10766.19
```


The the impact of imputing missing data on the estimates of the total daily number of steps is that the median and the mean are now the same with a value of 10766.19( while they differed on the old dataset with 10765 and 10766.19 respectivly) 


---

Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1.Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.



```r
daytype <- weekdays(df_full$date)
daytype <- ifelse(daytype == "Sunday"| daytype =="Saturday", "weekend", "weekday")

df_full$daytype <- daytype

str(df_full)
```

```
## 'data.frame':	17568 obs. of  4 variables:
##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  $ date    : POSIXct, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ daytype : chr  "weekday" "weekday" "weekday" "weekday" ...
```


2.Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
library(lattice)
library(ggplot2)
interval_full <- df_full  %>%  group_by(interval, daytype) %>%
  summarize(steps = mean(steps))

str(interval_full)
```

```
## Classes 'grouped_df', 'tbl_df', 'tbl' and 'data.frame':	288 obs. of  3 variables:
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ daytype : chr  "weekday" "weekday" "weekday" "weekday" ...
##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  - attr(*, "vars")=List of 1
##   ..$ : symbol interval
##  - attr(*, "drop")= logi TRUE
```

```r
par(mfrow=c(3,1))

with(subset(interval_full, daytype == "weekday"), plot(interval, steps, xlim =c(0,2500),ylim =c(0,250), col= "blue", type="l", main ="weekday"))

with(subset(interval_full, daytype == "weekend"), plot(interval, steps, xlim =c(0,2400),ylim =c(0,250), col ="red",type="l", main = "weekend"))
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)\




---
