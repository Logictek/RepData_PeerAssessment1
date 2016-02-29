#  Reproductible Research Assignment 1



## Synopsis

The purpose of this assignment is to practice:
* loading data using markdown
* processing and drawing conclusions from data
* learning mardown syntax

Also, set all code echo = TRUE


## Reading in data and processing



```r
unzip("activity.zip", exdir = ".")
activityData <- read.csv("activity.csv", colClasses = c("integer", "Date", "integer"))
```

## What is the mean total number of steps taken each day?


```r
library(ggplot2)

totalSteps <- aggregate(data=activityData, steps ~ date, na.rm = TRUE, sum)

summary(totalSteps)
```

```
##       date                steps      
##  Min.   :2012-10-02   Min.   :   41  
##  1st Qu.:2012-10-16   1st Qu.: 8841  
##  Median :2012-10-29   Median :10765  
##  Mean   :2012-10-30   Mean   :10766  
##  3rd Qu.:2012-11-16   3rd Qu.:13294  
##  Max.   :2012-11-29   Max.   :21194
```

```r
ggplot(data=totalSteps, aes(steps)) + geom_histogram(width = 1.5, bins = 20, binwidth = 1000, col = "black", fill = "turquoise") +
  
  labs(x = "Total steps per day") +
  labs(y = "Count") +
  labs(title = "Histogram of Steps per day")
```

![](PA1_template_files/figure-html/histogram-1.png)


to calculate the mean we can simply call the mean and median function on our processed data



```r
stepsmean <- round(mean(totalSteps$steps, na.rm = TRUE), digits = 5)
stepsmedian <- median(totalSteps$steps, na.rm = TRUE)
```

The mean = 1.0766189\times 10^{4} 
The median = 10765

##What is the average daily activity pattern?

We need to compute the average (mean) steps per interval first...then plot the daily average activity


```r
intervalSteps <- aggregate(data=activityData, steps ~ interval, na.rm = TRUE, mean)
intervalSteps$steps <- round(intervalSteps$steps, digits = 0)
```

###Plot the line chart of the average daily activity


```r
library(ggplot2)

ggplot(intervalSteps, aes(interval, steps)) +
    geom_line(color = "black", stat = "identity",
              size = 1, na.rm = TRUE) +
  
    labs(x = "Intervals") +
    labs(y = "Steps per Interval") +
    labs(title = "Time Series") 
```

![](PA1_template_files/figure-html/lineChar-1.png)

### Which interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
intervalSteps[which.max((intervalSteps$steps)),]
```

```
##     interval steps
## 104      835   206
```

##Imputing missing values

Total number of missing values (NAs)


```r
sum(is.na(activityData))
```

```
## [1] 2304
```

### Impute missing values

Using the mean for a given time interval, we can impute data irrespective of actual time. We need to loop over all steps in a given interval in order to aggregate the steps per day on the full data set, with imputed values. 


```r
index <- nrow(activityData)
for(n in 1:index) {
    if (is.na(activityData$steps[n])) {
        activityData$steps[n] <- intervalSteps[which(activityData$interval[n]
                                               == intervalSteps$interval), ]$steps
    }
}
```


```r
intervalStepsFilled <- aggregate(steps ~ date, activityData, sum)
```
### The mean and median are


```r
stepsmeanFilled <- round(mean(intervalStepsFilled$steps), digits = 0)
stepsmedianFilled <- median(intervalStepsFilled$steps)
```
Mean =  1.0766\times 10^{4} Median = 1.0762\times 10^{4}

This is what we would expect, since the imputation basis is the mean of the interval, the mean of the interval steps per day has not drastically changed (if by only a few hundred at most)

### Histogram of data file with imuputed values



```r
library(ggplot2)


ggplot(data=intervalStepsFilled, aes(steps)) + geom_histogram(width = 1.5, bins = 20, binwidth = 1000, col = "black", fill = "magenta") +
  
  labs(x = "Date") +
  labs(y = "Count") +
  labs(title = "Histogram of Steps per day, with imputed values")
```

![](PA1_template_files/figure-html/histogram2-1.png)

### Are there differences in activity patterns between weekdays and weekends?

First, find the day of the week for each measurement in the dataset.


```r
activityData$weekday <- with(activityData, weekdays(activityData$date, abbreviate=FALSE))

for (n in 1:index) {
  if (activityData$weekday[n] != "Saturday" &&
      activityData$weekday[n] != "Sunday" ) {
        activityData$weekday[n] <- "weekday"
        
  } else {
        activityData$weekday[n] <-"weekend"
      }
      
      
      
}
```

Aggregate the steps for interval for weekend and weekdays


```r
activityData$weekday <- as.factor(activityData$weekday)
intervalStepsFilled <- aggregate(steps ~ interval + weekday, activityData, mean)
intervalStepsFilled$steps <- round(intervalStepsFilled$steps, digits = 0)
```

### Plot the line chart in two panels


```r
library(ggplot2)

ggplot(intervalStepsFilled, aes(interval, steps, color = weekday)) +
  facet_wrap(~weekday, ncol = 1)+
  geom_line(stat = "identity", size = 1)+
  labs(x = "Intervals")+
  labs(y = "Steps per Interval")
```

![](PA1_template_files/figure-html/lineplot-1.png)
Fin
test
