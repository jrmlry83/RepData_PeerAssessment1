---
title: "Reproducible Research: Peer Assessment 1"
output: html_document
keep_md: true
---

This report is written for Reproducible Research, Peer Assessment 1

## Part 0: Loading of the required libraries

The libraries used for the code in this report are 'dplyr' and 'lubridate'.



## Part 1: Loading and preprocessing the data

The required data file must first be downloaded and placed in the working directory. This can be done by downloading the file from https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip and unzipping the resulting activity.csv file into the working directory.

The data is loaded with read.csv and saved to a variable called data. The date is converted to the appropriate date format using the lubridate package.


```r
data <- read.csv("activity.csv",stringsAsFactors=FALSE)
data$interval <- sprintf("%04d", data$interval)
data$date <- ymd(data$date)
```

## Part 2: What is mean total number of steps taken per day?

First, using the dplyr package, we group the data by date, before retrieving the sum of the steps recorded on each date, then plotting a histogram.


```r
by_date <- group_by(data,date)
steps_by_date <- summarise(by_date,sum(steps))
names(steps_by_date) <- c("date","sum")
hist(steps_by_date$sum,main="Histogram of total steps on a given day",xlab="Total steps")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

Subsequently, we use the mean and median functions to determine the mean and median of total number of steps taken each day, noting to ignore the missing values in the dataset.


```r
mean_steps <- (mean(steps_by_date$sum,na.rm=TRUE))
median_steps <- median(steps_by_date$sum,na.rm=TRUE)
mean_integer <- as.integer(mean_steps)
```

We can see that the mean (rounded off) is 10766 and the median is 10765.

## Part 3: What is the average daily activity pattern?

To do this, we create a new column, time. We insert a dummy date, today, and convert the interval into an hms (hours,minute,seconds) format. We then use the dplyr package to group the data frame by time and create a new data frame with each time interval and its accompanying average steps taken.

We then plot a time series plot of the two columns of the new table.


```r
data$time <- ymd_hms(paste(today(),data$interval,"00"))
by_time <- group_by(data,time)
steps_by_time <- summarise(by_time,mean(steps,na.rm=TRUE))
names(steps_by_time) <- c("time","mean")
plot(steps_by_time$time,steps_by_time$mean,type="l",main="Time series plot of average steps taken in a time interval",xlab="Time of day",ylab="Average steps")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

We then extract the time at which the average number of steps is at a maximum, using the which function to determine where the maximum occurs.


```r
steps_by_time[which(steps_by_time$mean==max(steps_by_time$mean)),][1]
```

```
## Source: local data frame [1 x 1]
## 
##                  time
## 1 2014-12-15 08:35:00
```

We find that this is at the time interval starting at 08:35:00

## Part 4: Imputing missing values

On inspection of the data, we notice that the missing values span 8 full days where no recordings were measured. Thus, we choose to replace these missing values with what forms an "average" day - using the average values for each 5-minute interval as determined in the previous part.

First, we check how many missing values there are.


```r
missing_values <- sum(is.na(data$steps))
```

The total number of missing values is 2304. As expected, this is a multiple of 288, the number of 5 minute-intervals in a day (8 x 288)

We create a new data frame in which we replace the values by the mean in the corresponding 5-minute interval with the following code:


```r
data2 <- data
data2$marker <- 1:288
for (i in 1:nrow(data2)) {
  if (is.na(data2[i,1])) {
    data2[i,1] <- steps_by_time[data2[i,5],2]
    }
  }
```

After replacing the data, we then create a histogram in a similar fashion to how it was done in Part 2. It is nearly the same histogram as that in Part 1, except with 8 more occurences between 10000 and 15000. This is because all 8 missing days have the same total number of steps (the mean steps per day)



```r
by_date2 <- group_by(data2,date)
steps_by_date2 <- summarise(by_date2,sum(steps))
names(steps_by_date2) <- c("date","sum")
hist(steps_by_date2$sum,main="Histogram of total steps on a given day, adjusted for NA values",xlab="Total steps")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png) 

We also use similar code to Part 2 to obtain the new mean and new median.


```r
mean_steps2 <- mean(steps_by_date2$sum,na.rm=TRUE)
median_steps2 <- median(steps_by_date2$sum,na.rm=TRUE)
mean_integer2 <- as.integer(mean_steps2)
median_integer2 <- as.integer(median_steps2)
```

We can see that the new mean is 10766 and the new median is 10766. The new mean is equal to the old mean, while the new median is the same value as the mean. The observation that the new mean is equal to the old mean is expected, as each day of missing values added up exactly to the mean. 

## Part 5: Are there differences in activity patterns between weekdays and weekends?

As directed, we first add in a new column of factor variables indicating if a given day is a weekday and weekend. 


```r
data2$weekday <- wday(data2$date)<7&wday(data2$date)>1 
data2$weekday <- factor(data2$weekday,labels=c("Weekend","Weekday"))
levels(data2$weekday) <- c("Weekend","Weekday")
```

To create the panel plots, we again use the dplyr package to group the data frame by time and the weekday variable, before creating two new data frames where the weekday and weekend observations have been filtered. We then create a panel plot comparing the values from these two data frames.


```r
by_time_weekday <- group_by(data2,time,weekday)
steps_by_time_weekday <- summarise(by_time_weekday,mean(steps))
names(steps_by_time_weekday) <- c("time","weekday","mean")
steps_weekend <- filter(steps_by_time_weekday,weekday=="Weekend")
steps_weekday <- filter(steps_by_time_weekday,weekday=="Weekday")
par(mfrow=c(2,1))
plot(steps_weekend$time,steps_weekend$mean,type="l",main="Weekend",xlab="Interval",ylab="Number of steps",ylim=c(0,250))
plot(steps_weekday$time,steps_weekday$mean,type="l",main="Weekday",xlab="Interval",ylab="Number of steps",ylim=c(0,250))
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png) 

We find that for weekend days, the total number of steps was higher and the number of steps was high from 08:00 to 22:00. For weekdays, the peak was highest from 08:00 to 09:00, and this peak exceeded the values seen over the weekend. However, the values for the rest of the day was low. This could be attributed to the bulk of steps taken happening while heading to work, with minimal steps taken for the rest of the day (a small peak appears again at 19:00, possibly at the end of the work day)
