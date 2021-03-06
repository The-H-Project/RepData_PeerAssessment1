# Reproducible Research: Peer Assessment 1

This assignment was prepared using:   
* R version 3.2.3  
* data.table version 1.9.6

## Loading and preprocessing the data

1. Load required packages and the dataset.
2. pad the interval data with zeroes we can convert it to time data (**Interval_f** = formatted interval)  
3. convert the date column to **date** data  
4. create a continuous date/time series with the date and time data (**Interval_dt**)  


```r
library(data.table)
library(lattice)
dataset <- fread('activity.csv', na.strings='NA')
dataset[,Interval_f := sprintf("%04d", as.numeric(interval))]
dataset[,date := as.Date(date, format='%Y-%m-%d')] 
dataset[,Interval_dt := as.POSIXct(paste(date,Interval_f), format='%Y-%m-%d %H%M')]  
```

## What is mean total number of steps taken per day?

data.table is used to create summary table 'totalset' that contains the total number steps per date. The 'totalset' table is then plotted using the hist function.


```r
totalset <- dataset[,sum(steps), by=date]
names(totalset)[2] <- 'TotalSteps'
mediansteps <- median(totalset$TotalSteps, na.rm = TRUE)
meansteps <- mean(totalset$TotalSteps, na.rm = TRUE)

hist(totalset$TotalSteps, main='Number of Steps Taken Per Day', xlab='Steps Taken Per Day',
     ylab='Number of Days (Frequency)', breaks=length(unique(totalset$TotalSteps)))  
```

![](PA1_template_files/figure-html/mean_steps_per_day-1.png) 

The mean number of steps taken per day is: **10,766.19**

The median number of steps taken per day is: **10,765**

## What is the average daily activity pattern?

data.table is used to create summary table 'averageset', which contains the average number of steps per time interval for each day (day number). The 'averageset' table is then plotted in a time series plot.

Also, determine the interval that had the highest average number of steps.

```r
averageset <- dataset[,mean(steps, na.rm = TRUE), by=Interval_f]
names(averageset)[2] <- 'AverageSteps'

whichinterval <- averageset[which.max(AverageSteps),Interval_f]

plot(x=averageset$Interval_f,y = averageset$AverageSteps, main='Average Daily Pattern',
     xlab = 'Time', ylab='Average Number of Steps', type='l')
```

![](PA1_template_files/figure-html/average_daily_activity_pattern-1.png) 

The 5-minute interval that on average contains the maximum number of steps: **0835**

## Imputing Missing Values

#### Missing Values in Dataset
We may determine the total number of missing values by counting the number of rows in the 'totalset' table that sum to NA for total number of steps.


```r
# Figure out and report the total number of rows with NAs. 
nadataset <- is.na(dataset)
naset <- totalset[is.na(TotalSteps==TRUE)]

# Figure out the day number of each date, for later.
naset[,day := as.factor(format.Date(date, format='%w'))]
```

The number of dates missing values is: **8**

The total number of missing data points is: **2,304**

#### Calculating Average Values 

We're going to impute missing data values by calculating the average number of steps for each interval, for each day number (0 to 6): for example, the average number of steps for interval 0835 for all Mondays. However, we're going to round the average steps to the nearest whole digit, because people don't take fractional steps.

This conceptually takes four steps:  
1. We calculate the average values per interval for each day number. data.table is used to cast (widen and shorten) the data table by forcing each date to its own row, and the intervals to columns. This is equivalent to creating a pivot table in Microsoft Excel, using Date as Row value Interval_f as the Column value, and steps as the data.  
2. We get the dates that are missing data, and calculate their day numbers.  
3. We merge the two tables to create an ImputedValues dataset.  
4. We merge the ImputedValues dataset with the main dataset.  


```r
datawide <- dcast.data.table(dataset, date ~ Interval_f, value.var = 'steps')

# Figure out the weekday number of each date.
datawide[,day := as.factor(format.Date(date, format='%w'))]

# Figure out the average values per interval on each given day number, but round to the nearest digit because people don't take fractional steps. This will obviously skew the median and average steps.
#
# While averaging and rounding works should work as a chain, i.e.:
#
# dayaverages <- datawide[, lapply(.SD, mean, na.rm=TRUE), by=day][, lapply (.SD, round), .SDcols=2:ncol(dayaverages)]
#
# it does not seem to work with Knitr, so the steps are separated below

# Determine the average number of steps per interval by day number.
dayaverages <- datawide[, lapply(.SD, mean, na.rm=TRUE), by=day]

# Remove the date column from this table because it's not an averaged value.
dayaverages[,date := NULL]

# Round the number of steps to the nearest whole digit, because people don't take fractional steps. The first column is day, so it won't be averaged, but we still need it as an identifier column.
dayaverages <- cbind(dayaverages$day, dayaverages[, lapply (.SD, round), .SDcols=2:ncol(dayaverages)])
names(dayaverages)[1] <- 'day'

# Set the common key on tables in preparation for merge the two datasets
setkey(dayaverages,day)
setkey(naset, day)

# Create of table of imputed values for missing days in the datawide table.
# naset contains the dates for which we are missing values
# dayaverages contains the imputed interval values for each day number
ImputedValues <- merge(naset, dayaverages, all.x=TRUE)

# Set the common key on ImputedValues and datawide to date
setkey(ImputedValues, date)
setkey(datawide, date)

# Create the final dataset of all dates including those with imputed values. While the R data.table package is pretty strong with column manipulation, it is less so with row manipulation. There is no easy way to update rows. So we're going to do a SQL UPDATE in two steps:
# * getting rid of rows where there are dates with no steps
# * combine the original dataset with the imputed values
#
# 1. Merge the totalset (number of total steps per date) with datawide  
# 2. Get rid of rows where there are no steps, and combine the imputed values with the
# original dataset
# Index on the date, which will also sort the data by date ascending
ImputedDatawide <- datawide[totalset, on='date']
ImputedDatawide <- ImputedDatawide[!is.na(TotalSteps)]
ImputedDatawide <- rbind(ImputedDatawide, ImputedValues)
setkey(ImputedDatawide, date)

# Get rid of TotalSteps so we can recalculate it, and get rid of day because it gets in the way of TotalSteps being recalculated (specifically, the .SDCols parameter)
ImputedDatawide[, `:=` (TotalSteps = NULL, day = NULL)]

# Recalculate total steps per day
ImputedDatawide[ ,TotalSteps := rowSums(.SD), .SDcols = 2:ncol(ImputedDatawide)]

# Recalculate median and mean steps
Imputedmediansteps <- median(ImputedDatawide$TotalSteps)
Imputedmeansteps <- mean(ImputedDatawide$TotalSteps)

# Create the histogram
hist(ImputedDatawide$TotalSteps, main='Number of Steps Taken Per Day', xlab='Steps Taken Per Day', ylab='Number of Days (Frequency)', breaks=length(unique(ImputedDatawide$TotalSteps)))
mtext('(with imputed values for NA data)', side=3) 
```

![](PA1_template_files/figure-html/impute_missing_values-1.png) 

The mean number of steps taken per day is: **10,821.1**

The median number of steps taken per day is: **11,015**

The method used to impute missing data raised both the mean and median of the dataset. A different method of imputing data would have a different effect. For example, imputing missing data using mean values without rounding would keep the mean of the revised dataset, but change its variances.

## Are there differences in activity patterns between weekdays and weekends?

We're going to graph weekday vs weekend activity to show their differences. We must first determine which dates in the dataset are weekdays, and which are weekends. We're avoiding the weekdays function mentioned in the course assignment because it returns character string, but it's easier to use numbers to classify weekdays / weekends.


```r
# Determine the average number of steps per interval by day number.
# Recalculate the day number of each date, 0 to 6. 0 is Sunday, 6 is Saturday.
ImputedDatawide[,day := format.Date(date, format='%w')]

# Assign Weekday / Weekend to Imputed dataset, and get rid of the Date, Day, and TotalSteps variables, because these variables shouldn't be averaged.
ImputedDatawide[day %in% c(1:5), WeekDayOrEnd := 'Weekday']
ImputedDatawide[day %in% c(0,6), WeekDayOrEnd := 'Weekend']
ImputedDatawide[, `:=` (day = NULL, date = NULL, TotalSteps = NULL)]
ImputedDatawide$WeekDayOrEnd <- as.factor(ImputedDatawide$WeekDayOrEnd)

# Average the imputed data grouped by Weekday and Weekend
ImputedAverages <- ImputedDatawide[, lapply(.SD, mean), by=WeekDayOrEnd]

# Melt the data so it may be graphed.
ImputedLong <- melt(ImputedAverages, id.vars = 'WeekDayOrEnd', variable.name = 'Interval', value.name = 'AverageSteps')

# Use a lattice panel plot to graph averages steps for Weekends and Weekdays.
xyplot(AverageSteps ~ Interval | factor(WeekDayOrEnd), data=ImputedLong, groups=WeekDayOrEnd, type='l', layout=c(1,2))
```

![](PA1_template_files/figure-html/Weekdays_vs_Weekends-1.png) 
