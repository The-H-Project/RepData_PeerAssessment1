# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data
This assignment was prepared using:  
* R version 3.2.3  
* data.table version 1.9.6

data.table is used to load the activity.csv dataset and:  
1. pad the interval data with zeroes we can convert it to time data (**Interval_f** = formatted interval)  
2. convert the date column to **date** data  
3. create a continuous date/time series with the date and time data (**Interval_dt**)  



```r
library(data.table)
dataset <- data.table(fread('activity.csv', na.strings='NA'))
dataset[,Interval_f := sprintf("%04d", as.numeric(interval))]
dataset[,date := as.Date(date, format='%Y-%m-%d')] 
dataset[,Interval_dt := as.POSIXct(paste(date,Interval_f), format='%Y-%m-%d %H%M')]  
```

## What is mean total number of steps taken per day?

data.table is used to create summary table 'totalset' that contains the total number steps per day. The 'totalset' table is then plotted using the hist function.


```r
totalset <- dataset[,sum(steps), by=date]
names(totalset)[2] <- 'TotalSteps'
mediansteps <- median(totalset$TotalSteps, na.rm = TRUE)
meansteps <- mean(totalset$TotalSteps, na.rm = TRUE)

hist(totalset$TotalSteps, main='Number of Steps Taken Per Day', xlab='Steps Taken Per Day',
     ylab='Number of Days (Frequency)', breaks=length(unique(totalset$TotalSteps)))  
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

The mean number of steps taken per day is: 10,766.19

The median number of steps taken per day is: 10,765

## What is the average daily activity pattern?

data.table is used to create summary table 'averageset', which contains the average number of steps per time interval across all days. The 'averageset' table is then plotted in time series plot.

Also, determine the interval that had the highest average number of steps.

```r
averageset <- dataset[,mean(steps, na.rm = TRUE), by=Interval_f]
names(averageset)[2] <- 'AverageSteps'

plot(x=averageset$Interval_f,y = averageset$AverageSteps, main='Average Daily Pattern',
     xlab = 'Time', ylab='Average Number of Steps', type='l')
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
whichinterval <- averageset[which.max(AverageSteps),Interval_f]
```

The 5-minute interval that on average contains the maximum number of steps: **0835**

## Imputing Missing Values

#### Missing Values in Dataset
We may determine the total number of missing values by counting the number of rows in the 'totalset' table that sum to NA for total number of steps.


```r
# Figure out and report the total number of rows with NAs. 
naset <- totalset[is.na(TotalSteps==TRUE)]

# Figure out the day number of each date, for later.
naset[,day := as.factor(format.Date(date, format='%w'))]
```

The number of dates missing values is (/ are): **8**


#### Calculating Average Values 

We're going to impute missing data values by calculating the average number of steps per interval per day number (0 to 6). However, we're going to round the average steps to the nearest whole digit, because people don't take fractional steps.

data.table is used to cast (widen and shorten) the data table by forcing each date to its own row, and the intervals to columns. (This is equivalent to creating a pivot table in Microsoft Excel, using Date as Row value Interval_f as the Column value, and steps as the data)


```r
datawide <- dcast.data.table(dataset, date ~ Interval_f, value.var = 'steps')

# Figure out the weekday number of each date.
datawide[,day := as.factor(format.Date(date, format='%w'))]

# Figure out the average values per interval on each given day, but round to the nearest digit because people don't take fractional steps. This will obviously skew the median and average steps.
#
# While averaging and rounding works should work as a chain, i.e.:
#
# dayaverages <- datawide[, lapply(.SD, mean, na.rm=TRUE), by=day][, lapply (.SD, round), .SDcols=2:ncol(dayaverages)]
#
# it does not seem to work with Knitr

# Determine the average number of steps per interval by day.
dayaverages <- datawide[, lapply(.SD, mean, na.rm=TRUE), by=day]

# Remove the date column from this table because it's not an averaged value.
dayaverages[,date := NULL]

# Round the number of steps to the nearest whole digit, because people don't take fractional steps. The first column is day, so it won't be averaged, but we still need it as an identifier column.
dayaverages <- cbind(dayaverages$day, dayaverages[, lapply (.SD, round), .SDcols=2:ncol(dayaverages)])
names(dayaverages)[1] <- 'day'
```

## Are there differences in activity patterns between weekdays and weekends?
