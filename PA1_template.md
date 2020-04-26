# Loading the required libraries for the project

library(dplyr)
library(ggplot2)
library(knitr)
knit2html("PA1_template.Rmd", force_v1 = TRUE)

## Loading and preprocessing the data
'''{r simulation, echo = FALSE}
temp <- tempfile()
fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileURL, temp)
unzip(temp)

activity <- read.csv("./activity.csv")
'''

## What is mean total number of steps taken per day?
'''{r simulation, echo = FALSE}
steps.by.day <- activity %>% 
                group_by(date) %>% 
                summarize(total = sum(steps, na.rm = TRUE))
'''

## Make a histogram of the total number of steps taken each day
'''{r simulation, echo = FALSE}
hist(steps.by.day$total,
     main = "Total Number of Steps per Day",
     xlab = "Total Number of Steps",
     ylim = c(0,10),
     yaxt = 'n',
     xlim = c(0,25000),
     breaks = 24,
     col = "red")

axis(side = 2, at=seq(0, 10, by = 1), labels = seq(0, 10, by = 1))

# Calculate and report the mean and median of the total number of steps taken per day

mean.median <- steps.by.day %>% 
               summarize(mean = mean(total, na.rm = TRUE),
                         median = median(total, na.rm = TRUE)
                         )

# Results show 9354 (mean) and 10765 (median)

abline(v = mean.median$mean, lwd = 1, lty = 2, col = "blue")
abline(v = mean.median$median, lwd = 1, lty = 2, col = "blue")

# What is the average daily activity pattern?
# Time series plot of the average number of steps taken

interval <- activity %>%
            group_by(interval) %>%
            summarize(average = mean(steps, na.rm = TRUE))

plot(x = interval$interval, 
     y = interval$average, 
     type = "l",
     main = "Average Number of Steps per 5-minute Interval",
     ylab = "Intervals",
     xlab = "Average Number of Steps",
     col = "blue")

# Imputing missing values

sum(is.na(activity$steps)) # Answer is 2304

# Get a list of missing data

activity$day <- weekdays(as.Date(activity$date))

missing <- activity[is.na(activity$steps),]

interval.day <- activity %>%
                group_by(interval, day) %>%
                summarize(mean = mean(steps, na.rm = TRUE))

missing.values <- merge(missing, interval.day, by = c("day", "interval"))
missing.values$steps <- missing.values$mean
missing.values$mean <- NULL

exist <- activity[!is.na(activity$steps),]

new.data <- rbind(exist, missing.values)

new.steps.by.day <- new.data %>% 
                    group_by(date) %>% 
                    summarize(total = sum(steps, na.rm = FALSE))

mean.median.new <- new.steps.by.day %>% 
                   summarize(mean = mean(total, na.rm = TRUE),
                             median = median(total, na.rm = TRUE)
                             )
# New results: 10821 (mean) and 11015 (median)

hist(new.steps.by.day$total,
     main = "Total Number of Steps per Day",
     xlab = "Total Number of Steps",
     ylim = c(0,10),
     yaxt = 'n',
     xlim = c(0,25000),
     col = "blue",
     breaks = 24)
hist(steps.by.day$total,
     main = "Total Number of Steps per Day",
     xlab = "Total Number of Steps",
     ylim = c(0,10),
     yaxt = 'n',
     xlim = c(0,25000),
     col = "red",
     breaks = 24, add = TRUE)
axis(side = 2, at=seq(0, 10, by = 1), labels = seq(0, 10, by = 1))
legend("topright", c("Estimated Data", "Including NA Data"), fill = c("blue", "red"))

# Are there differences in activity patterns between weekdays and weekends?

library(lattice)

new.data$week[(new.data$day == "Saturday") | (new.data$day == "Sunday")] <- "Weekend"
new.data$week[(new.data$day == "Monday") | (new.data$day == "Tuesday") | (new.data$day == "Wednesday") |
              (new.data$day == "Thursday") | (new.data$day == "Friday")] <- "Weekday"

week.type <- new.data %>% 
             group_by(interval, week) %>% 
             summarize(mean = mean(steps, na.rm = FALSE))

xyplot(mean ~ interval | week, data = week.type, 
       layout = c(1,2),
       type = "l",
       main = "Average Number of Steps per Interval by Day Type",
       xlab = "Intervals",
       ylab = "Average Number of Steps"
       )

# The activity patterns appear to be different between weekdays and weekends as indicated in the plot.
