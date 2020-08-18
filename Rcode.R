library(readr)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(reshape2)


activity <- read.csv("activity.csv", sep = ",")


names(activity)
str(activity)



activity2 <- melt(activity[which(!is.na(activity$steps)), ], id.vars = c("date", "interval"))

steps_total <- activity2 %>% group_by(date) %>%  summarise(steps = sum(value))


summary(steps_total$steps) 


hist(steps_total$steps, main = "Histogram of total steps taken per day", 
     xlab = "Total steps per day", ylab = "Number of days", 
     breaks = 10, col = "red")
abline(v = mean(steps_total$steps), lty = 2, lwd = 2, col = "yellow")
abline(v = median(steps_total$steps), lty = 3, lwd = 2, col = "black")
legend(x = "topright", c("Mean", "Median"), col = c("yellow", "black"), 
       lty = c(1, 2), lwd = c(2, 2))


steps_means <- activity2 %>% group_by(interval) %>% summarise(steps = mean(value))


plot(steps_means$interval, steps_means$steps, ty = "l",
     xlab = "time interval", ylab = "Average steps", 
     main = "Average steps taken over all days vs time interval")

maxsteps <- steps_means$interval[which.max(steps_means$steps)]
maxsteps



## Imputing missing values
activity3 <- split(activity, activity$interval)
activity3 <- lapply(activity3, function(x) {
  x$steps[which(is.na(x$steps))] <- mean(x$steps, na.rm = TRUE)
  return(x)
})
activity3 <- do.call("rbind", activity3)
row.names(activity3) <- NULL

activity3 <- split(activity3, activity3$date)
df <- lapply(activity3, function(x) {
  x$steps[which(is.na(x$steps))] <- mean(x$steps, na.rm = TRUE)
  return(x)
})
activity3 <- do.call("rbind", activity3)
row.names(activity3) <- NULL
head(activity3)

library(reshape2)
activity4 <- melt(activity3, id.vars = c("date", "interval"))
steps_total <- dcast(activity4, date ~ variable, sum, na.rm = TRUE)
head(steps_total)

## Histogram of total number of steps taken with imputed missing values
hist(steps_total$steps, main = "Histogram of total steps taken per day", 
     xlab = "Total steps per day", ylab = "Number of days", 
     breaks = 10, col = "green")
abline(v = mean(steps_total$steps), lty = 1, lwd = 2, col = "yellow")
abline(v = median(steps_total$steps), lty = 2, lwd = 2, col = "black")
legend(x = "topright", c("Mean", "Median"), col = c("yellow", "black"), 
       lty = c(2, 1), lwd = c(2, 2))

## Number of rows with NA values
sum(is.na(activity$steps))
sum(is.na(activity$steps))*100/nrow(activity) # Percentage of rows



## Differences in activity patterns: Weekdays vs Weekends
library(lubridate)
weekends <- which(weekdays(as.Date(activity3$date)) == "Saturday" |
                    weekdays(as.Date(activity3$date)) == "Sunday")
weekdays <- which(weekdays(as.Date(activity3$date)) != "Saturday" &
                    weekdays(as.Date(activity3$date)) != "Sunday")
temp <- c(rep("a", length(activity3)))
temp[weekends] <- "weekend"
temp[weekdays] <- "weekday"
length(temp)
activity3 <- cbind(activity3, temp)
head(activity3)
names(activity3)[4] <- "day"

activity3split <- split(activity3, activity3$day)
steps_means <- lapply(activity3split, function(x) {
  temp <- aggregate(x$steps, list(x$interval), mean)
  names(temp) <- c("interval", "steps")
  return(temp)
})


steps_means <- do.call("rbind", steps_means)
weekdays <- grep("weekday" ,row.names(steps_means))
weekends <- grep("weekend" ,row.names(steps_means))
temp <- c(rep("a", length(steps_means$steps)))
temp[weekdays] <- "weekdays"
temp[weekends] <- "weekends"
names(temp) <- "day"
steps_means <- cbind(steps_means, temp)
row.names(steps_means) <- NULL

head(steps_means)

library(ggplot2)
ggplot(steps_means, aes(interval, steps)) + geom_line() + ggtitle("Weekdays") + theme_bw()
