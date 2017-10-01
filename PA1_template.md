library(ggplot2)
library(dplyr)
library(lubridate)

installed.packages('rmarkdown')

setwd('./Data science cursus coursera/course5week2')
data <- read.csv('./activity.csv', quote="\"")

head(data)

activity <- na.omit(data)

activity.day <- summarize(group_by(activity, date), steps=sum(steps))

#       What is mean total number of steps taken per day?
## 1.   Calculate the total number of steps taken per day

Sumsteps <- aggregate(data$steps, by=list(data$date), sum)

names(Sumsteps)[1] ="Date"
names(Sumsteps)[2] ="Total"

head(Sumsteps,15)


## 2.   Make a histogram of the total number of steps taken each day

ggplot(Sumsteps, aes(x = Total)) + geom_histogram(fill = "blue",col="red", binwidth=1000, center=500) + labs(title = "Steps taken a day", x = "Steps a day", y = "Frequency")

## 3.   Calculate and report the mean and median of the total number of steps taken per day

summary(activity.day)


#       What is the average daily activity pattern?
##      1.      Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


activity.interval <- summarize(group_by(activity, interval), steps=mean(steps))


ggplot(activity.interval, aes(interval, steps)) + geom_line(colour="blue", size=0.3)


##      2.      Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

activity.interval[activity.interval$steps == max(activity.interval$steps), ]

#       Imputing missing values
##      1.      Calculate and report the total number of missing values in the dataset

nrow(data) - nrow(activity)

##      Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval.

names(activity.interval)[2] <- "mean"
activity.filling <- merge(data, activity.interval)

head(activity.filling)

##      3.Create a new dataset that is equal to the original dataset but with the missing data filled in.

activity.filling$steps[is.na(activity.filling$steps)] <- activity.filling$mean[is.na(activity.filling$steps)]

head(activity.filling)

##      4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.

SumData <- aggregate(data$steps, by=list(data$date), sum)

head(SumData,15)

names(SumData)[1] ="date"
names(SumData)[2] ="total"
head(SumData,15)


ggplot(SumData, aes(x = total)) + geom_histogram(fill = "blue", col="red", binwidth=1000, center=500) + labs(title = "Total Steps a day", x = "Steps", y = "Frequency")


summary(SumData, digits=10)
summary(Sumsteps, digits=10)

#       Are there differences in activity patterns between weekdays and weekends?

##      Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

activity$weekday <- weekdays(as.Date(activity$date))
activity$weekend <- as.factor(activity$weekday == "zaterdag" | activity$weekday == "zondag")
levels(activity$weekend) <- c("Weekday", "Weekend")

head(activity, 15)
tail(activity, 15)

## Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

activity.weekday <- activity[activity$weekend=="Weekday",]
activity.weekend <- activity[activity$weekend=="Weekend",]

activity.interval.weekday <- group_by(activity.weekday, interval)
activity.interval.weekday <- summarize(activity.interval.weekday, steps=mean(steps))

activity.interval.weekday$weekend <- "Weekday"

activity.interval.weekend <- group_by(activity.weekend, interval)
activity.interval.weekend <- summarize(activity.interval.weekend, steps=mean(steps))

activity.interval.weekend$weekend <- "Weekend"

activity.interval <- rbind(activity.interval.weekday, activity.interval.weekend)
activity.interval$weekend <- as.factor(activity.interval$weekend)
plot <- ggplot(activity.interval, aes(interval, steps, color=weekend)) + geom_line(size=0.5) + xlab("Interval") + ylab("Avarage steps")

plot + theme(panel.background = element_rect(fill = 'white'))
