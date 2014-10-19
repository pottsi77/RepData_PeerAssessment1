---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Loading and preprocessing the data
unzip(zipfile="activity.zip")
activity <- read.csv("activity.csv")


## What is mean total number of steps taken per day?
library(ggplot2)
total.steps <- tapply(activity$steps, activity$date, FUN=sum, na.rm=TRUE)
qplot(total.steps, binwidth=1000, xlab="total number of steps taken each day")

mean(total.steps, na.rm=TRUE)
median(total.steps, na.rm=TRUE)


## What is the average daily activity pattern?
library(ggplot2)
averages <- aggregate(x=list(steps=activity$steps), by=list(interval=activity$interval),
                      FUN=mean, na.rm=TRUE)
ggplot(data=averages, aes(x=interval, y=steps)) +
  geom_line() +
  xlab("5-minute interval") +
  ylab("average number of steps taken")

averages[which.max(averages$steps),]

## Imputing missing values
missing <- is.na(activity$steps)
table(missing)

impute_value <- function(steps, interval) {
  filled <- NA
  if (!is.na(steps))
    filled <- c(steps)
  else
    filled <- (averages[averages$interval==interval, "steps"])
  return(filled)
}
activity_imputed <- activity
activity_imputed$steps <- mapply(impute_value, activity_imputed$steps, activity_imputed$interval)

## Are there differences in activity patterns between weekdays and weekends?
library(ggplot2)
total.steps <- tapply(activity_imputed$steps, activity_imputed$date, FUN=sum, na.rm=TRUE)
qplot(total.steps, binwidth=1000, xlab="total number of steps taken each day (imputed)")

mean(total.steps, na.rm=TRUE)
median(total.steps, na.rm=TRUE)

activity_imputed$date <- as.Date(activity_imputed$date)
weekday_weekend <- function(date) {
  day <- weekdays(date)
  if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
    return("weekday")
  else if (day %in% c("Saturday", "Sunday"))
    return("weekend")
  else
    stop("invalid date")
}
activity_imputed$date <- as.Date(activity_imputed$date)
activity_imputed$daytype <- sapply(activity_imputed$date, FUN=weekday_weekend)

averages <- aggregate(steps ~ interval + daytype, data=activity_imputed, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(daytype ~ .) +
  xlab("5-minute interval") + ylab("Number of steps")
