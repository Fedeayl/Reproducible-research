### Download data

``` r
Url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(Url, destfile = "activity.zip", method = "curl")
unzip("activity.zip",exdir = "data")
```

### Load data and packages

## What is mean total number of steps taken per day?

1.  Calculate the total number of steps taken per day

We group the data by date and calculate the sum of steps for each day.

``` r
StepsDay <- summary_by(data = Data, formula = .~date, FUN=sum)
StepsDay <- StepsDay[,-3]
head(StepsDay, 10)
```

    ##          date steps.sum
    ## 1  2012-10-01        NA
    ## 2  2012-10-02       126
    ## 3  2012-10-03     11352
    ## 4  2012-10-04     12116
    ## 5  2012-10-05     13294
    ## 6  2012-10-06     15420
    ## 7  2012-10-07     11015
    ## 8  2012-10-08        NA
    ## 9  2012-10-09     12811
    ## 10 2012-10-10      9900

1.  Make a histogram of the total number of steps taken each day.

``` r
ggplot(StepsDay, aes(x = steps.sum)) +
    geom_histogram(fill = "darkblue", binwidth = 1000) +
    labs(title = "Steps per day", x = "Steps", y = "Frequency")
```

    ## Warning: Removed 8 rows containing non-finite values (stat_bin).

![](Project1_files/figure-markdown_github/plot%20total%20step-1.png)

1.  Calculate and report the mean and median of the total number of
    steps taken per day

**Step per day mean = ** 1.0766189^{4} **Step per day median = ** 10765

## What is the average daily activity pattern?

1.  Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = “𝚕”) of the 5-minute interval
    (x-axis) and the average number of steps taken, averaged across all
    days (y-axis)

``` r
Interval <- dplyr::group_by(Data, interval)
StepsInterval <- dplyr::summarise(Interval, steps = mean(steps, na.rm=T))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
ggplot(StepsInterval, 
       aes(x=interval, y=steps)) +
  geom_line(color="darkred") +
  xlab("Days' 5 min. Interval") + ylab("Steps") +
  ggtitle("Steps in 5 Minute Invervals Per Day")
```

![](Project1_files/figure-markdown_github/Steps%20per%20interval-1.png)

1.  Which 5-minute interval, on average across all the days in the
    dataset, contains the maximum number of steps?

The interval wich contains the maximum number of step is 835 . The
number of steps is 206.1698113

## Imputing missing values

1.  Calculate and report the total number of missing values in the
    dataset (i.e. the total number of rows with 𝙽𝙰s)

**Number of missing values:** 2304

1.  Devise a strategy for filling in all of the missing values in the
    dataset. The strategy does not need to be sophisticated. For
    example, you could use the mean/median for that day, or the mean for
    that 5-minute interval, etc.

We’ll use the mean for each 5 minute interval to fill the missing values
in the dataset.

``` r
Interval <- dplyr::group_by(Data, interval)
StepsInterval <- dplyr::summarise(Interval, steps = mean(steps, na.rm=T))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
ImputedData <- Data

for(x in 1:length(ImputedData$steps)) {
    if(is.na(ImputedData[x, 1])==TRUE) {
        ImputedData[x, 1] <- StepsInterval[StepsInterval$interval %in% ImputedData[x, 3], 2]
    }
}
```

1.  Create a new dataset that is equal to the original dataset but with
    the missing data filled in.

``` r
head(ImputedData, 10)
```

    ##        steps       date interval
    ## 1  1.7169811 2012-10-01        0
    ## 2  0.3396226 2012-10-01        5
    ## 3  0.1320755 2012-10-01       10
    ## 4  0.1509434 2012-10-01       15
    ## 5  0.0754717 2012-10-01       20
    ## 6  2.0943396 2012-10-01       25
    ## 7  0.5283019 2012-10-01       30
    ## 8  0.8679245 2012-10-01       35
    ## 9  0.0000000 2012-10-01       40
    ## 10 1.4716981 2012-10-01       45

1.  Make a histogram of the total number of steps taken each day and
    calculate and report the mean and median total number of steps taken
    per day. Do these values differ from the estimates from the first
    part of the assignment? What is the impact of imputing missing data
    on the estimates of the total daily number of steps?

``` r
ImputedStepsDay <- summary_by(data = ImputedData, formula = .~date, FUN=sum)

ggplot(ImputedStepsDay, aes(x = steps.sum)) +
    geom_histogram(fill = "darkblue", binwidth = 1000) +
    labs(title = "Steps per day", x = "Steps", y = "Frequency")
```

![](Project1_files/figure-markdown_github/histogram%20imputed-1.png)

**Step per day mean (Imputed Nas dataset) = ** 1.0766189^{4} **Step per
day median (Imputed Nas dataset) = ** 1.0766189^{4}

``` r
NonImputed <- c(mean(StepsDay$steps.sum, na.rm = TRUE), median(StepsDay$steps.sum, na.rm = TRUE))

Imputed <- c(mean(ImputedStepsDay$steps.sum, na.rm = TRUE), median(ImputedStepsDay$steps.sum, na.rm = TRUE))

Comp <- rbind (NonImputed, Imputed)
colnames(Comp) <- c("mean", "median")

Comp
```

    ##                mean   median
    ## NonImputed 10766.19 10765.00
    ## Imputed    10766.19 10766.19

Both distributions are very similar. The mean values are the same, at
1.0766189^{4} steps, while the median value is slightly larger for the
imputed data set, at 1.0766189^{4} steps, rather than 10765 steps.

## Are there differences in activity patterns between weekdays and weekends?

1.  Create a new factor variable in the dataset with two levels –
    “weekday” and “weekend” indicating whether a given date is a weekday
    or weekend day.

``` r
ImputedData$date <- as.Date(ImputedData$date)
ImputedData$Day <- weekdays(ImputedData$date)
ImputedData$Day <- as.factor(ImputedData$Day)

ImputedData$WeekDay <- car::recode(ImputedData$Day, "'Saturday'= 'Weekend'; 'Sunday'= 'Weekend'; else = 'Weekday' ")

head(ImputedData, 10)
```

    ##        steps       date interval    Day WeekDay
    ## 1  1.7169811 2012-10-01        0 Monday Weekday
    ## 2  0.3396226 2012-10-01        5 Monday Weekday
    ## 3  0.1320755 2012-10-01       10 Monday Weekday
    ## 4  0.1509434 2012-10-01       15 Monday Weekday
    ## 5  0.0754717 2012-10-01       20 Monday Weekday
    ## 6  2.0943396 2012-10-01       25 Monday Weekday
    ## 7  0.5283019 2012-10-01       30 Monday Weekday
    ## 8  0.8679245 2012-10-01       35 Monday Weekday
    ## 9  0.0000000 2012-10-01       40 Monday Weekday
    ## 10 1.4716981 2012-10-01       45 Monday Weekday

1.  Make a panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = “𝚕”) of
    the 5-minute interval (x-axis) and the average number of steps
    taken, averaged across all weekday days or weekend days (y-axis).
    See the README file in the GitHub repository to see an example of
    what this plot should look like using simulated data.

``` r
Weekday <- ImputedData[ImputedData$WeekDay =="Weekday", ]
Weekend <- ImputedData[ImputedData$WeekDay =="Weekend", ]

WeekdaySteps <- aggregate(steps ~ interval, Weekday, mean)
WeekendSteps <- aggregate(steps ~ interval, Weekend, mean)
```

``` r
par(mfrow=c(2, 1), mar=c(4, 4.1, 3, 2.1))
plot(WeekdaySteps$interval, WeekdaySteps$steps, type="l",
     main="Time Series Plot. Avrg. Steps per Interval. Weekdays",
     xlab="Intervals - 5 mins", ylab="Steps",
     col="darkred", lwd=1.5, ylim=c(0, 230))
plot(WeekdaySteps$interval, WeekdaySteps$steps, type="l",
     main="Time Series Plot. Avrg. Steps per Interval. Weekends",
     xlab="Intervals - 5 mins", ylab="Steps",
     col="darkblue", lwd=1.5, ylim=c(0, 230))
```

![](Project1_files/figure-markdown_github/panel%20plot%20weekdays%20vs%20weekends-1.png)
