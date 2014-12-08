# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
# set variables
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
zipFile <- "activity.zip"
csvFile <- "activity.csv"

# download and unzip the data file
download.file(url,zipFile,method="curl")
unzip(zipFile, setTimes=TRUE)

# read the csv data file into a data.table
library(data.table)
ACTIVITY <- as.data.table(read.csv(csvFile))
```


## What is mean total number of steps taken per day?



## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?