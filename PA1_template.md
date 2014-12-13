# Reproducible Research: Peer Assessment 1


## (1) Loading and preprocessing the data

> The data for this assignment can be downloaded from the course web site:  
> • Dataset: Activity monitoring data [52K]  
  
> The variables included in this dataset are:  
> • steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)  
> • date: The date on which the measurement was taken in YYYY-MM-DD format  
> • interval: Identifier for the 5-minute interval in which measurement was taken  
 
> The dataset is stored in a comma-separated-value (CSV) file and there are a  
> total of 17,568 observations in this dataset.  
  


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
setkeyv(ACTIVITY, c('date','interval'))
tables()
```

```
##      NAME       NROW NCOL MB COLS                KEY          
## [1,] ACTIVITY 17,568    3  1 steps,date,interval date,interval
## Total: 1MB
```


## &nbsp;
## (2) What is mean total number of steps taken per day?

> For this part of the assignment, you can ignore the missing values in the dataset. 


```r
A_ign_na <- ACTIVITY[complete.cases(ACTIVITY), ]
```

     
> **1. Make a histogram of the total number of steps taken each day**


```r
hist(A_ign_na[, sum(steps), by=date]$V1,
     breaks=15,
     main="plot 2-1:\nHistogram of the total number of steps taken each day",
     xlab="Sum of steps per day")
```

![](PA1_template_files/figure-html/q2-1-1.png) 

> **2. Calculate and report the mean and median total number of steps taken per day**


```r
print(sprintf("%-15.15s: %.2f", "the mean is", mean(A_ign_na[, sum(steps), by=date]$V1)))
print(sprintf("%-15.15s: %d", "the median is", median(A_ign_na[, sum(steps), by=date]$V1)))
```

```
## [1] "the mean is    : 10766.19"
## [1] "the median is  : 10765"
```

## &nbsp;
## (3) What is the average daily activity pattern?

> **1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis)
and the average number of steps taken, averaged across all days (y-axis)**


```r
A <- A_ign_na[, mean(steps), by=interval]
plot(x=A$interval, y=A$V1,
     type="l",
     main="plot 3-1:\nAverage number of steps per 5-minute interval",
     xlab="# of 5-minute interval",
     ylab="mean(steps)")
```

![](PA1_template_files/figure-html/q3-1-1.png) 


> **2. Which 5-minute interval, on average across all the days in the dataset,
contains the maximum number of steps?**


```r
# maxInt <- A[which.max(A[,V1])]$interval
print(paste("it is the interval number", A[which.max(A[,V1])]$interval))
```

```
## [1] "it is the interval number 835"
```

## &nbsp;
## (4) Imputing missing values

> Note that there are a number of days/intervals where there are missing values
(coded as NA). The presence of missing days may introduce bias into some
calculations or summaries of the data.


> **1. Calculate and report the total number of missing values in the dataset
> (i.e. the total number of rows with NAs**


```r
print(paste("there are", dim(ACTIVITY[is.na(steps)])[1], "missing steps (rows with steps=NA)"))

# check also whether we have all dates and all intervals for each day
print(paste("are there missing dates/intervals?", dim(ACTIVITY)[1] != (31+30)*(24*12)))
```

```
## [1] "there are 2304 missing steps (rows with steps=NA)"
## [1] "are there missing dates/intervals? FALSE"
```

> **2. Devise a strategy for filling in all of the missing values in the dataset. The
> strategy does not need to be sophisticated. For example, you could use
> the mean/median for that day, or the mean for that 5-minute interval, etc**

As the plot 3-1 above shows a rather inequal  distribution, the imputed value 
should consider this average. In order to not jeopardize the
question about differences in the pattern for weekdays and weekends (5-1), the average per interval will be calculated seperatly for weekdays/weekends.

In anticipation of task 5-1, the variable `typeDay` with two levels – “weekday” and “weekend” will be inserted here into the dataset:


```r
# get English day names
Sys.setlocale(category = "LC_TIME", locale="C")

# insert column "typeDay" as "weekend" or "weekday" depending on date
ACTIVITY[, typeDay:=ifelse(weekdays(as.Date(date),abbreviate=TRUE) %in% c("Sat","Sun"),
                           "weekend", "weekday")]
```

> **3. Create a new dataset that is equal to the original dataset but with the
> missing data filled in.**


```r
# for comparison, copy steps column to naSteps (steps with NA values)
ACTIVITY[, naSteps:=steps]

# create an auxilary table with average steps per typeDay and interval
AUX <- ACTIVITY[!is.na(steps), as.integer(round(mean(steps))), keyby=.(typeDay,interval)]

# join the AUX into ACTIVITY by typeDay and interval
# and set steps = average from AUX where steps == NA
setkeyv(ACTIVITY, key(AUX))  # set key for join
ACTIVITY <- ACTIVITY[AUX][, steps:=ifelse(is.na(steps),V1,steps)][, V1:=NULL]
setkeyv(ACTIVITY, c('date','interval'))  # restore original key
```

```r
# show some example result lines
ACTIVITY[c(1,500,1100,9800,10000), ]
```

```
##    steps       date interval typeDay naSteps
## 1:     2 2012-10-01        0 weekday      NA
## 2:     0 2012-10-02     1735 weekday       0
## 3:    16 2012-10-04     1935 weekday      16
## 4:     0 2012-11-04       35 weekend      NA
## 5:   104 2012-11-04     1715 weekend      NA
```



> **4. Make a histogram of the total number of steps taken each day and Calculate
> and report the mean and median total number of steps taken per day.**  
> Do these values differ from the estimates from the first part of the assignment?  
> What is the impact of imputing missing data on the estimates of the total
> daily number of steps?


```r
hist(ACTIVITY[, sum(steps), by=date]$V1,
     breaks=15,
     main="plot 4-4:\nHistogram of the total number of steps taken each day\nimputed NA values",
     xlab="Sum of steps per day")
```

![](PA1_template_files/figure-html/q4-4-1.png) 

```r
print(sprintf("%-15.15s: %10.2f  --  without imputing it was: %.2f", 
              "the mean is", 
              mean(ACTIVITY[, sum(steps), by=date]$V1),
              mean(A_ign_na[, sum(steps), by=date]$V1)))
print(sprintf("%-15.15s: %10d  --  without imputing it was: %d", 
              "the median is", 
              median(ACTIVITY[, sum(steps), by=date]$V1),
              median(A_ign_na[, sum(steps), by=date]$V1)))
```

```
## [1] "the mean is    :   10761.90  --  without imputing it was: 10766.19"
## [1] "the median is  :      10571  --  without imputing it was: 10765"
```



## &nbsp;
## (5) Are there differences in activity patterns between weekdays and weekends?

> For this part the weekdays() function may be of some help here. Use the dataset
> with the filled-in missing values for this part.  

> **1. Create a new factor variable in the dataset with two levels – “weekday”
> and “weekend” indicating whether a given date is a weekday or weekend
> day**

This step has been realized under 4-2, above. Remove here the columns no longer needed and show some example lines from the dataset:


```r
# remove columns no longer needed
ACTIVITY <- ACTIVITY[,`:=`(vE=NULL,vD=NULL,naSteps=NULL)]
```

```
## Warning in `[.data.table`(ACTIVITY, , `:=`(vE = NULL, vD = NULL, naSteps =
## NULL)): Adding new column 'vE' then assigning NULL (deleting it).
```

```
## Warning in `[.data.table`(ACTIVITY, , `:=`(vE = NULL, vD = NULL, naSteps =
## NULL)): Adding new column 'vD' then assigning NULL (deleting it).
```

```r
# show some example result lines
ACTIVITY[c(1,500,1100,9800,10000), ]
```

```
##    steps       date interval typeDay
## 1:     2 2012-10-01        0 weekday
## 2:     0 2012-10-02     1735 weekday
## 3:    16 2012-10-04     1935 weekday
## 4:     0 2012-11-04       35 weekend
## 5:   104 2012-11-04     1715 weekend
```

**Note:**  
It is not recommended to use factors with data tables, see `vignette("datatable-faq")` Q 2.17  
&nbsp;  


> **2. Make a panel plot containing a time series plot (i.e. type = "l") of the
> 5-minute interval (x-axis) and the average number of steps taken, averaged
> across all weekday days or weekend days (y-axis).**


```r
WE <- ACTIVITY[typeDay=="weekend", mean(steps), by=interval]
WD <- ACTIVITY[typeDay=="weekday", mean(steps), by=interval]

par(mfrow=c(2,1)) 
plot(x=WD$interval, y=WD$V1,
     type="l",
     ylim = c(0,250),  # same y range on both plots for comparison
     main="plot 5-2:\nAverage number of steps per 5-minute interval\non weekdays:",
     xaxt="n", xlab=NULL,  # does not work
     ylab="mean(steps)")
plot(x=WE$interval, y=WE$V1,
     type="l",
     ylim = c(0,250),  # same y range on both plots for comparison
     main="on weekends:",
     xlab="# of 5-minute interval",
     ylab="mean(steps)")
```

![](PA1_template_files/figure-html/q5-2-1.png) 

