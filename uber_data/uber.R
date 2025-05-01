library(shiny)
library(ggplot2)
library(DT)
library(readxl)
library(RCurl)
library(bslib)
library(dplyr)
library(tidyr)
library(janitor)
library(lubridate)
rm(list = ls())

setwd('/Users/sashabotsul/Downloads/rstudio/uber_data')

data1<- read.csv('uber-raw-data-apr14.csv')
data2<- read.csv('uber-raw-data-aug14.csv')
data3<- read.csv('uber-raw-data-jul14.csv')
data4<- read.csv('uber-raw-data-jun14.csv')
data5<- read.csv('uber-raw-data-may14.csv')
data6<- read.csv('uber-raw-data-sep14.csv')

#combine data
combined_data <-bind_rows(data1, data2, data3, data4, data5, data6)
#clean names
combined_data <- combined_data %>%
  clean_names()

#convert time to hour and am/pm
combined_data$time_strip <- as.POSIXct(combined_data$date_time, format = "%m/%d/%Y %H:%M:%S", tz = "UTC")

combined_data$hour <- hour(combined_data$time_strip) %% 12

combined_data$hour[combined_data$hour == 0] <- 12

combined_data$am_pm <- ifelse(hour(combined_data$time_strip) < 12, "AM", "PM")

combined_data$hour_am_pm <- paste(combined_data$hour, combined_data$am_pm)

combined_data <- combined_data %>%
  separate(date_time, into = c('date', 'time'), sep = ' ')

combined_data$time <- ifelse(nchar(combined_data$time) == 7,
                                   paste0("0", combined_data$time),
                                   combined_data$time)

#clean up combined data to show only columns needed
combined_data_clean <- combined_data %>%
  select(c(1, 2, 3, 4, 5, 9))

#write csv
write.csv(combined_data_clean, 'clean_uber.csv')

