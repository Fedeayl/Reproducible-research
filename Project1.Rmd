---
title: "project1"
author: "Federico Acosta y Lara"
date: "1/29/2021"
output: html_document
editor_options: 
  chunk_output_type: console
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

### Download data
```{r}
Url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(Url, destfile = "activity.zip", method = "curl")
unzip("activity.zip",exdir = "data")

```


### Load data and packages
```{r load data and packages, echo=FALSE, message=FALSE}
library(ggplot2)
library(doBy)

Data <- read.csv(here::here("data", "activity.csv"))

```


## What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day

We group the data by date and calculate the sum of steps for each day.
```{r sum step per day}

StepsDay <- summary_by(data = Data, formula = .~date, FUN=sum)
StepsDay <- StepsDay[,-3]
head(StepsDay, 10)
```

2. Make a histogram of the total number of steps taken each day. 

```{r plot total step}
ggplot(StepsDay, aes(x = steps.sum)) +
    geom_histogram(fill = "darkblue", binwidth = 1000) +
    labs(title = "Steps per day", x = "Steps", y = "Frequency")
```

3. Calculate and report the mean and median of the total number of steps taken per day

**Step per day mean = ** `r mean(StepsDay$steps.sum, na.rm = TRUE)`
**Step per day median = ** `r median(StepsDay$steps.sum, na.rm = TRUE)`


## What is the average daily activity pattern?

1. Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```{r Steps per interval}

Interval <- dplyr::group_by(Data, interval)
StepsInterval <- dplyr::summarise(Interval, steps = mean(steps, na.rm=T))

ggplot(StepsInterval, 
       aes(x=interval, y=steps)) +
  geom_line(color="darkred") +
  xlab("Days' 5 min. Interval") + ylab("Steps") +
  ggtitle("Steps in 5 Minute Invervals Per Day")
```

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

The interval wich contains the maximum number of step is `r StepsInterval[which.max(StepsInterval$steps),][[1]]` . The number of steps is `r StepsInterval[which.max(StepsInterval$steps),][[2]]`


## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 𝙽𝙰s)

**Number of missing values:** `r sum(is.na(Data$steps))`


2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

We'll use the mean for each 5 minute interval to fill the missing values in the dataset.

```{r filling NAs}

Interval <- dplyr::group_by(Data, interval)
StepsInterval <- dplyr::summarise(Interval, steps = mean(steps, na.rm=T))

ImputedData <- Data

for(x in 1:length(ImputedData$steps)) {
    if(is.na(ImputedData[x, 1])==TRUE) {
        ImputedData[x, 1] <- StepsInterval[StepsInterval$interval %in% ImputedData[x, 3], 2]
    }
}

```


3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```{r imputed dataset}
head(ImputedData, 10)
```


4. Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```{r histogram imputed}
ImputedStepsDay <- summary_by(data = ImputedData, formula = .~date, FUN=sum)

ggplot(ImputedStepsDay, aes(x = steps.sum)) +
    geom_histogram(fill = "darkblue", binwidth = 1000) +
    labs(title = "Steps per day", x = "Steps", y = "Frequency")


```


**Step per day mean (Imputed Nas dataset) = ** `r mean(ImputedStepsDay$steps.sum, na.rm = TRUE)`
**Step per day median  (Imputed Nas dataset) = ** `r median(ImputedStepsDay$steps.sum, na.rm = TRUE)`


```{r Non imputed vs imputed}
NonImputed <- c(mean(StepsDay$steps.sum, na.rm = TRUE), median(StepsDay$steps.sum, na.rm = TRUE))

Imputed <- c(mean(ImputedStepsDay$steps.sum, na.rm = TRUE), median(ImputedStepsDay$steps.sum, na.rm = TRUE))

Comp <- rbind (NonImputed, Imputed)
colnames(Comp) <- c("mean", "median")

Comp
```

Both distributions are very similar. The mean values are the same, at  `r mean(ImputedStepsDay$steps.sum, na.rm = TRUE)` steps, while the median value is slightly larger for the imputed data set, at `r median(ImputedStepsDay$steps.sum, na.rm = TRUE)` steps, rather than `r median(StepsDay$steps.sum, na.rm = TRUE)` steps.

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```{r}
ImputedData$date <- as.Date(ImputedData$date)
ImputedData$Day <- weekdays(ImputedData$date)
ImputedData$Day <- as.factor(ImputedData$Day)

ImputedData$WeekDay <- car::recode(ImputedData$Day, "'Saturday'= 'Weekend'; 'Sunday'= 'Weekend'; else = 'Weekday' ")

head(ImputedData, 10)
```

2. Make a panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```{r weekday_weekend_groups}

Weekday <- ImputedData[ImputedData$WeekDay =="Weekday", ]
Weekend <- ImputedData[ImputedData$WeekDay =="Weekend", ]

WeekdaySteps <- aggregate(steps ~ interval, Weekday, mean)
WeekendSteps <- aggregate(steps ~ interval, Weekend, mean)

```


```{r panel plot weekdays vs weekends}

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

