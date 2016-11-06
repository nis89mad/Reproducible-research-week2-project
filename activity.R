Steps <- function() {
    library(dplyr)
    library(ggplot2)
    
    activity <- read.csv("activity.csv")
    
    activityGrouped <- group_by(activity, date)
    totalStepsPerDay <- summarise(activityGrouped, steps = sum(steps))
    hist(totalStepsPerDay$steps, xlab = "Total steps per day", main = "")
    mean(totalStepsPerDay$steps, na.rm = T)
    median(totalStepsPerDay$steps, na.rm = T)
    
    activityGrouped <- group_by(activity, interval)
    avgStepsPerInterval <- summarise(activityGrouped, Average.steps = mean(steps, na.rm = T))
    with(avgStepsPerInterval, plot(x=interval,y = Average.steps, type = "l"))
    indmax <- which(avgStepsPerInterval$Average.steps==max(avgStepsPerInterval$Average.steps, na.rm = T), arr.ind = F)
    avgStepsPerInterval$interval[indmax]
    
    
    sum(is.na(activity$steps))
    imputedActivity <- activity
    indMissing <- which(is.na(imputedActivity$steps))
    for (i in 1:length(indMissing)) {
        intrvl <- imputedActivity$interval[indMissing[i]]
        mean <- subset(avgStepsPerInterval, interval == intrvl)[,2]
        imputedActivity$steps[indMissing[i]] <- mean
    }
    ImactivityGrouped <- group_by(imputedActivity, date)
    ImactivityGrouped$steps  <- as.numeric(ImactivityGrouped$steps)
    imTotalStepsPerDay <- summarise(ImactivityGrouped, steps = sum(steps))
    hist(imTotalStepsPerDay$steps, xlab = "Total steps per day", main = "")
    mean(imTotalStepsPerDay$steps, na.rm = T)
    median(imTotalStepsPerDay$steps, na.rm = T)
    
    imputedActivity$date <- as.Date(imputedActivity$date, "%Y-%m-%d")
    weekend <- c("Saturday", "Sunday")
    weekendDays <- which(weekdays(imputedActivity$date) %in% weekend)
    imputedActivity$day <- "weekday"
    imputedActivity$day[weekendDays] <- "weekend"
    
    ImactivityGrouped <- group_by(imputedActivity, interval, day)
    ImactivityGrouped$steps  <- as.numeric(ImactivityGrouped$steps)
    ImavgStepsPerInterval <- summarise(ImactivityGrouped, Average.steps = mean(steps, na.rm = T))
    gg1 <- ggplot(ImavgStepsPerInterval, aes(interval,Average.steps))
    gg1+geom_line()+facet_grid(day~.)
    
}