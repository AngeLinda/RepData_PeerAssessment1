rm(list = ls())

library(ggplot2)
library(plyr)

# load and preprocess the data
activity <- read.csv("./activity.csv")
activity$date <- as.Date(activity$date, "%Y-%m-%d")

# mean total number of steps taken per day
activity_total_steps_daily <- ddply(activity, .(date), function(x) sum(x$steps))
colnames(activity_total_steps_daily)[2] <- "total steps per day"
hist(activity_total_steps_daily[,2], main = "Histogram of total number of steps taken per day", 
     xlab = "total number of steps taken per day", col = "lavender")

activity_total_steps_daily_mean   <- mean(activity_total_steps_daily$`total steps per day`, na.rm = TRUE)
activity_total_steps_daily_median <- median(activity_total_steps_daily$`total steps per day`, na.rm = TRUE)

# average daily activity
activity_mean_steps_daily <- ddply(activity, .(interval), function(x) mean(x$steps, na.rm = TRUE))
colnames(activity_mean_steps_daily)[2] <- "mean steps per interval"
plot(activity_mean_steps_daily$`mean steps per interval`, type = "l", main = "average number of steps taken in each 5-minute interval",
     xlab = "5-minute interval across all days", ylab = "average number of steps", lwd = 2)

activity_mean_steps_daily_max <- activity_mean_steps_daily[which.max(activity_mean_steps_daily$`mean steps per interval`),]

# imput missing values : replace NA by mean of steps per interval
na_number <- sum(!complete.cases(activity))
activity_imput_na <- activity
for(i in 1:nrow(activity_imput_na)) {
    int <- activity_imput_na$interval[i]
    activity_imput_na$steps[i][is.na(activity_imput_na$steps[i])] <- activity_mean_steps_daily$`mean steps per interval`[which(activity_mean_steps_daily$interval == int)] 
}

activity_total_steps_daily_na <- ddply(activity_imput_na, .(date), function(x) sum(x$steps))
colnames(activity_total_steps_daily_na)[2] <- "total steps per day"
hist(activity_total_steps_daily_na[,2], main = "Histogram of total number of steps taken per day", 
     xlab = "total number of steps taken per day", col = "lavender")

activity_total_steps_daily_na_mean   <- mean(activity_total_steps_daily_na$`total steps per day`, na.rm = TRUE)
activity_total_steps_daily_na_median <- median(activity_total_steps_daily_na$`total steps per day`, na.rm = TRUE)

# weekdays and weekend differences
activity_imput_na$day <- c("weekday")
wd <- weekdays(activity_imput_na$date)
for(i in 1:nrow(activity_imput_na)){
    if(wd[i] == "Saturday" || wd[i] == "Sunday"){
        activity_imput_na$day[i] <- "weekend"
    }
}

activity_total_steps_interval_na <- ddply(activity_imput_na, .(interval, day), function(x) mean(x$steps))
colnames(activity_total_steps_interval_na)[3] <- "mean steps per interval"

qplot(interval, `mean steps per interval`, data = activity_total_steps_interval_na, geom = c("line"), 
      xlab = "5-minute Interval", ylab = "Average number of steps") + facet_wrap(~day, ncol = 1)


