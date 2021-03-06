# this script contains the code I'm going to copy into the rmd file

library(tidyverse)

# download and unzip the rawdata
rawdata_url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
if (!file.exists("rawdata//repdataactivity.zip")){
    download.file(url = rawdata_url, destfile = "rawdata//repdataactivity.zip")
}
unzip(zipfile = "rawdata//repdataactivity.zip",
      exdir = "rawdata//repdataactivity")

activity <- read.csv(file = "rawdata//repdataactivity//activity.csv",
                     header = TRUE)

# Q2
# activity0 is when you want to take missing step values as zero steps
activity0 <- activity
for (i in 1:nrow(activity0)){
    if (is.na(activity0[i, 1])){
        activity0[i, 1] <- 0L
    }
}
# calculate sum of steps for each day and replace the steps column with it
stepsinday <- tapply(activity0[ , 1], activity0$date, sum)
stepsinday <- cbind(distinct(activity0, date), stepsinday)
stepsinday[ , 1] <- as.Date(stepsinday[ , 1], format = "%Y-%m-%d")
# plot histogram with direct mapping of y-values to sum of steps per day
# this is done with stat parameter in geom_histogram
# this overrides default stat parameter of frequency count
stepsinday_plot <- ggplot(stepsinday, mapping = aes(date, stepsinday)) + 
  geom_histogram(stat = "identity")
print(stepsinday_plot)

# Q3
stepsinday_mean <- mean(stepsinday[ , 2])
stepsinday_median <- median(stepsinday[ , 2])

# Q4 calc the avg no. of steps across intervals
# remove date column since we are only concerned with intervals
activity4 <- activity0[ , -2]
# calculate avg steps per interval
avgstepsinday <- tapply(activity4[ , 1], activity4$interval, sum)
avgstepsinday <- cbind(distinct(activity4, interval), avgstepsinday)
# use geom_line to plot time series
avgstepsinday_plot <- ggplot2::ggplot(avgstepsinday, mapping = aes(interval, avgstepsinday)) + 
  geom_line()
print(avgstepsinday_plot)

# Q5
avgstepsinday_max <- avgstepsinday[which(avgstepsinday[ , 2] == max(avgstepsinday[ , 2])), ]
avgstepsinday_max <- avgstepsinday_max[ , 1]

# Q7 do Q2 but replaced NA values with overall avg no. of steps
activity7 <- activity
# replace NA values with avg no. of steps overall (imputting NA values)
for (i in 1:nrow(activity7)){
  if (is.na(activity7[i, 1])){
    activity7[i, 1] <- mean(activity7[ , 1], na.rm = TRUE)
  }
}
# get the sum of steps per day
stepsinday_NA <- tapply(activity7[ , 1], activity7$date, sum)
stepsinday_NA <- cbind(distinct(activity7, date), stepsinday_NA)
stepsinday_NA[ , 1] <- as.Date(stepsinday_NA[ , 1], format = "%Y-%m-%d")
# same as in Q2, plot sum of steps per day with direct mapping instead of
# default frequency count
stepsinday_NA_plot <- ggplot(stepsinday_NA, mapping = aes(date, stepsinday_NA)) + 
  geom_histogram(stat = "identity")
print(stepsinday_NA_plot)

# Q8
# get vectors with weekday and weekend names to filter through date column
weekday <- weekdays(stepsinday[ , 1])[1:5]
weekend <- weekdays(stepsinday[ , 1])[6:7]
activity9 <- activity
activity9[ , 2] <- as.Date(activity9[ , 2], format =  "%Y-%m-%d")
# remove NA values
activity9 <- na.omit(activity9)
# add new column to identify if value belongs to weekday or weekend
# this is so ggplot can easily define the facet grid later on
activity9$weekday <- rep("weekday", nrow(activity9))
for (i in 1:nrow(activity9)){
  if (weekdays(activity9[i, 2]) %in% weekend){
    activity9[i, 4] <- "weekend"
  }
}
# split data into weekday and weekend days
# doing this allows me to calculate values for intervals separated by type of day
activity9_weekday <- na.omit(activity9[weekdays(activity9$date) %in% weekday, ])
activity9_weekend <- na.omit(activity9[weekdays(activity9$date) %in% weekend, ])

# calculate the avg steps per interval
activity9_weekday_avg <- tapply(activity9_weekday[ , 1], activity9_weekday$interval, mean)
# replace steps column with avg steps per interval
activity9_weekday <- distinct(activity9_weekday, interval, .keep_all = TRUE)
activity9_weekday[ , 1] <- activity9_weekday_avg

activity9_weekend_avg <- tapply(activity9_weekend[ , 1], activity9_weekend$interval, mean)
activity9_weekend <- distinct(activity9_weekend, interval, .keep_all = TRUE)
activity9_weekend[ , 1] <- activity9_weekend_avg

# combine both weekday and weekend dataframes
activity9 <- rbind(activity9_weekday, activity9_weekend)
# rename avg steps column so ggplot automatically names it correctly
colnames(activity9)[1] <- "avg_steps_perinterval"

# plot the data with geom_line() to produce time series
# facet grid automatically filters data with weekday/weekend column
activity9_plot <- ggplot(activity9, mapping = aes(interval, avg_steps_perinterval, group = weekday)) + 
  facet_grid(. ~ weekday) + geom_line()
print(activity9_plot)