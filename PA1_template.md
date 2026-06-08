k---
title: "Reproducible Research: Peer Assessment 1"
author: "glenn"
date: "2026-06-08"
output: html_document
---

## Loading and preprocessing the data

``` r
knitr::opts_chunk$set(echo = TRUE)
library(ggplot2)
library(dplyr)
```


``` r
    data <- read.csv(unzip("activity.zip"), stringsAsFactors = FALSE)
    data$date <- as.Date(data$date)
```

---

## What is mean total number of steps taken per day?
### 1. Calculate total steps per day

``` r
    day_sum <- data %>%
        group_by(date) %>%
        summarise(sum_steps = sum(steps, na.rm = TRUE), .groups = "drop")
```

### 2. Histogram of total steps per day

``` r
    png("histogram_steps.png", width = 800, height = 600)
    
    histogram_steps <- ggplot(day_sum, aes(x = sum_steps)) + 
        geom_histogram(binwidth = 600, fill = "steelblue", color = "black") + 
        labs(
            title = "Histogram of Total Number of Steps Taken Each Day", 
            x = "Total Number of Steps", 
            y = "Frequency"
        )
```

### 3. Mean and median total steps per day

``` r
    day_summary <- data %>%
        group_by(date) %>%
        summarise(mean_steps = mean(steps, na.rm = TRUE), 
                  median_steps = median(steps, na.rm = TRUE),
                  .groups = "drop"
        )
    day_summary
```

```
## # A tibble: 61 × 3
##    date       mean_steps median_steps
##    <date>          <dbl>        <dbl>
##  1 2012-10-01    NaN               NA
##  2 2012-10-02      0.438            0
##  3 2012-10-03     39.4              0
##  4 2012-10-04     42.1              0
##  5 2012-10-05     46.2              0
##  6 2012-10-06     53.5              0
##  7 2012-10-07     38.2              0
##  8 2012-10-08    NaN               NA
##  9 2012-10-09     44.5              0
## 10 2012-10-10     34.4              0
## # ℹ 51 more rows
```
---

## What is the average daily activity pattern?

### 1. Time series plot of average steps

``` r
ggplot(day_summary, aes(x = date, y = mean_steps)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    labs(
        title = "Time Series Plot of Average Steps per Day",
        x = "Date",
        y = "Average Number of Steps"
    )
```

```
## Warning: Removed 2 rows containing missing values or values outside the scale range (`geom_line()`).
```

```
## Warning: Removed 8 rows containing missing values or values outside the scale range (`geom_point()`).
```

![plot of chunk time_series](figure/time_series-1.png)

### 2. Interval with maximum average steps


``` r
interval_avg <- data %>%
    group_by(interval) %>%
    summarise(mean_steps = mean(steps, na.rm = TRUE), .groups = "drop")

interval_max <- interval_avg %>%
    filter(mean_steps == max(mean_steps))

interval_max
```

```
## # A tibble: 1 × 2
##   interval mean_steps
##      <int>      <dbl>
## 1      835       206.
```


``` r
ggplot(interval_avg, aes(x = interval, y = mean_steps)) +
    geom_line(linewidth = 1.2) +
    geom_vline(
        xintercept = interval_max$interval,
        linetype = "dashed",
        color = "red"
    ) +
    labs(
        title = "Average Steps per 5-Minute Interval",
        x = "Interval",
        y = "Average Number of Steps"
    )
```

![plot of chunk max_plot](figure/max_plot-1.png)

---

## Imputing missing values

### 1. Total missing values


``` r
sum(is.na(data$steps))
```

```
## [1] 2304
```

### 2. Imputation strategy

Missing values are filled using the **mean number of steps for that 5‑minute interval**.

### 3. Create imputed dataset


``` r
data_imputed <- data %>%
    left_join(interval_avg, by = "interval") %>%
    mutate(steps = ifelse(is.na(steps), mean_steps, steps)) %>%
    select(-mean_steps)
```

### 4. Histogram after imputation


``` r
day_sum_imputed <- data_imputed %>%
    group_by(date) %>%
    summarise(sum_steps = sum(steps), .groups = "drop")

ggplot(day_sum_imputed, aes(x = sum_steps)) +
    geom_histogram(binwidth = 600, fill = "blue", color = "black") +
    labs(
        title = "Histogram After Imputing Missing Values",
        x = "Total Number of Steps",
        y = "Frequency"
    )
```

![plot of chunk histogram_imputed](figure/histogram_imputed-1.png)

---

## Are there differences in activity patterns between weekdays and weekends?

### 1. Create weekday/weekend factor


``` r
data_imputed_week <- data_imputed %>%
    mutate(
        day_type = ifelse(
            weekdays(date) %in% c("Saturday", "Sunday"),
            "weekend",
            "weekday"
        )
    ) %>%
    mutate(day_type = factor(day_type))
```

### 2. Panel plot


``` r
interval_weekday <- data_imputed_week %>%
    group_by(day_type, interval) %>%
    summarise(mean_steps = mean(steps), .groups = "drop")

ggplot(interval_weekday, aes(x = interval, y = mean_steps)) +
    geom_line() +
    facet_wrap(~ day_type, ncol = 1) +
    labs(
        title = "Average Steps per Interval: Weekdays vs Weekends",
        x = "Interval",
        y = "Average Number of Steps"
    )
```

![plot of chunk panel_plot](figure/panel_plot-1.png)

---

## Conclusion

This analysis demonstrates:

- Daily activity patterns
- Impact of missing data
- Differences between weekday and weekend behavior














