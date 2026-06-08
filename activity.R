    ## read the file
    library(ggplot2)
    library(knitr)
    library(dplyr)
    data <- read.csv(unzip("activity.zip"), stringsAsFactors = FALSE)
    
    ## transfer date type
    data$date <- as.Date(data$date)
    
    ## group the value of steps by days
    day_sum <- data %>%
        group_by(date) %>%
        summarise(sum_steps = sum(steps, na.rm = TRUE), .groups = "drop")
    
    ## plot histogram of the total number of steps taken each day
    png("histogram_steps.png", width = 800, height = 600)
    
    histogram_steps <- ggplot(day_sum, aes(x = sum_steps)) + 
        geom_histogram(binwidth = 600, fill = "steelblue", color = "black") + 
        labs(
            title = "Histogram of Total Number of Steps Taken Each Day", 
            x = "Total Number of Steps", 
            y = "Frequency"
        )
    print(histogram_steps)
    dev.off()
    
    ## Mean and median number of steps taken each day
    day_summary <- data %>%
        group_by(date) %>%
        summarise(mean_steps = mean(steps, na.rm = TRUE), 
                  median_steps = median(steps, na.rm = TRUE),
                  .groups = "drop"
        )
    
    ## Time series plot of the average number of steps taken
    png("time_series_steps.png", width = 800, height = 600)
    
    time_series_steps <- ggplot(day_summary, aes(x = date, y = mean_steps)) + 
        geom_line(linewidth = 1.2) + 
        geom_point(size = 3) + 
        labs(
            title = "Time Series Plot of the Average Number of Steps Taken per Day", 
            x = "Date", 
            y = "Average Number of Steps"
        )
    print(time_series_steps)
    dev.off()
    
    ## The 5-minute interval that, on average, contains the maximum number of steps
    interval_avg <- data %>%
        group_by(interval) %>%
        summarise(mean_steps = mean(steps, na.rm = TRUE), 
                  .groups = "drop")
    
    interval_max <- interval_avg %>%
        filter(mean_steps == max(mean_steps))
    
    # plot the figure of 5 mins interval and average number of steps 
    png("avg_steps_per_interval.png", width = 800, height = 600)
    
    avg_steps_per_interval <- 
        ggplot(interval_avg, aes(interval, mean_steps)) + 
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
    print(avg_steps_per_interval)
    dev.off()

    ## Code to describe and show a strategy for imputing missing data
    # Calculate and report the total number of missing values
    na_step_sum <- sum(is.na(data$steps))
    
    # filling missing values with mean value of that day and 
    # create a new dataset with missing data filled in
    data_imputed <- data %>%
        left_join(interval_avg, by = "interval") %>%
        mutate(steps = ifelse(is.na(steps), mean_steps, steps)) %>%
        select(-mean_steps)
    
    day_sum_imputed <- data_imputed %>%
        group_by(date) %>%
        summarise(sum_steps = sum(steps, na.rm = TRUE), .groups = "drop")
    
    # Calculate and report the mean and median total number of steps taken per day
    day_summary_imputed <- data_imputed %>%
        group_by(date) %>%
        summarise(mean_steps = mean(steps), 
                  median_steps = median(steps),
                  .groups = "drop"
        )
        
    ## Histogram of the total number of steps taken each day after missing values are imputed
    png("hist_imputed.png", width = 800, height = 600)
    
    hist_imputed <- ggplot(day_sum_imputed, aes(x = sum_steps)) + 
        geom_histogram(binwidth = 600, fill = "blue", color = "black")
    
    print(hist_imputed)
    dev.off()
    
    ## Panel plot comparing the average number of steps taken per 5-minute 
    ## interval across weekdays and weekends
    # Create a new factor variable in the dataset with two levels – “weekday”
    # and “weekend”
    data_imputed_week <- data_imputed %>%
        mutate(
            day_type = ifelse(
                weekdays(date) &in& c("Saturday", "Sunday"), 
                "weekend",
                "weekday"
            ) 
        ) %>%
        mutate(day_type = factor(day_type))
    
    # Make a panel plot containing a time series plot of the 5-minute interval 
    # and the average number of steps taken, averaged across all weekday days 
    # or weekend days. 
    interval_weekday <- data_imputed_week %>%
        group_by(day_type, interval) %>%
        summarise(mean_steps = mean(steps), .groups = "drop")
    
    png("weekday_weekend.png", width = 800, height = 600)
    
    ggplot(interval_weekday, aes(x = interval, y = mean_steps)) + 
        geom_line() + 
        facet_wrap(~ day_type, ncol = 1) + 
        labs(
            title = "Average Steps per Interval: Weekdays vs Weekends",
            x = "Interval",
            y = "Average Number of Steps"
        )
    print()
    dev.off()
        
   
    