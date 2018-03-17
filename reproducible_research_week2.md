Reproducible Research Week 2 Project
====================================

Load and Process Data
---------------------

``` r
library(tidyverse)
```

    ## -- Attaching packages ------------------------------------------------------------------------------------------ tidyverse 1.2.1 --

    ## v ggplot2 2.2.1     v purrr   0.2.4
    ## v tibble  1.4.2     v dplyr   0.7.4
    ## v tidyr   0.8.0     v stringr 1.2.0
    ## v readr   1.1.1     v forcats 0.2.0

    ## -- Conflicts --------------------------------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
df <- read.csv("activity.csv", sep = ",", header = TRUE)
df$date <- as.Date(as.character(df$date, "%Y%m%d"))
```

Total Number of Steps Taken each day
====================================

Histogram of the total number of steps taken each day.
======================================================

``` r
df[!is.na(df$steps) == TRUE,] %>% 
  group_by(date) %>% 
  summarise(total_steps = sum(steps)) %>% 
  ggplot() +
  geom_histogram(aes(x=total_steps), binwidth = 1000)
```

![](reproducible_research_week2_files/figure-markdown_github/unnamed-chunk-2-1.png)

Mean and Median of steps taken per day
======================================

``` r
df_date <- df[!is.na(df$steps) == TRUE,] %>% 
  group_by(date) %>% 
  summarise(total_steps = sum(steps))  
mean(df_date$total_steps)
```

    ## [1] 10766.19

``` r
median(df_date$total_steps)
```

    ## [1] 10765

Time series plot of the average number of steps taken
=====================================================

``` r
x <- unique(df$interval)
y <- tapply(df$steps, df$interval, mean, na.rm=T)
plot(x, y, type = "l",xlab="Interval", ylab="Steps", main = "Average Number of Steps taken in 5-min Intervals")
```

![](reproducible_research_week2_files/figure-markdown_github/unnamed-chunk-4-1.png)

The 5-minute interval that, on average, contains the maximum number of steps
============================================================================

``` r
df_int <- df[!is.na(df$steps) == TRUE,] %>% 
  group_by(interval) %>% 
  summarise(steps =mean(steps))
df_int[df_int$steps == max(df_int$steps),]
```

    ## # A tibble: 1 x 2
    ##   interval steps
    ##      <int> <dbl>
    ## 1      835   206

Number of Rows with Missing Values
==================================

``` r
nrow(df[is.na(df$steps) == TRUE,])
```

    ## [1] 2304

Impute Missing Values
=====================

``` r
df2 <- df
int_avg <- as.data.frame(tapply(df2$steps, df2$interval, mean, na.rm=T))
int_avg$interval <- rownames(int_avg)
int_avg$interval <- as.integer(int_avg$interval)
names(int_avg) <- c("avg","interval")
df2 <- left_join(df2,int_avg, by = "interval")
df2$steps <- ifelse(is.na(df2$steps) == TRUE, df2$avg, df2$steps)
df2 <- df2[ ,-c(4)]
```

Histogram of the total number of steps taken each day after missing values are imputed
======================================================================================

``` r
  df2%>% 
  group_by(date) %>% 
  summarise(total_steps = sum(steps)) %>% 
  ggplot() +
  geom_histogram(aes(x=total_steps), binwidth = 1000)
```

![](reproducible_research_week2_files/figure-markdown_github/unnamed-chunk-8-1.png)

Mean and Median of Total Steps by date
======================================

``` r
df2_date <- df2%>% 
            group_by(date) %>% 
            summarise(total_steps = sum(steps)) 
mean(df2_date$total_steps)
```

    ## [1] 10766.19

``` r
median(df2_date$total_steps)
```

    ## [1] 10766.19

Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
=========================================================================================================

``` r
df2$weekday_end <- ifelse(weekdays(df2$date) %in% c("Saturday","Sunday"), "weekend", "weekday")
df2%>% 
  group_by(interval, weekday_end) %>% 
  summarise(avg_steps = mean(steps)) %>% 
  ggplot() +
  geom_line(aes(x=interval, y=avg_steps)) +
  facet_grid(weekday_end ~.) +
  labs(x="Interval", y="Number of Steps", title="Comparison - Average Number of Steps in each Interval")
```

![](reproducible_research_week2_files/figure-markdown_github/unnamed-chunk-10-1.png)
