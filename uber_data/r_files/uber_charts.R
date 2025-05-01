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
library(viridisLite)
library(viridis)


setwd('/Users/sashabotsul/Downloads/rstudio/uber_data')

data<- read.csv('clean_uber.csv')

#getting month and weekday names
data<- data %>%
  mutate(
    date_parsed = mdy(date),                  
    month_name = month(date_parsed, label = TRUE, abbr = FALSE),  
    weekday_name = wday(date_parsed, label = TRUE, abbr = FALSE)  
  )

# Chronological order for 12-hour AM/PM
ordered_hours <- c(
  "12 AM", "1 AM", "2 AM", "3 AM", "4 AM", "5 AM",
  "6 AM", "7 AM", "8 AM", "9 AM", "10 AM", "11 AM",
  "12 PM", "1 PM", "2 PM", "3 PM", "4 PM", "5 PM",
  "6 PM", "7 PM", "8 PM", "9 PM", "10 PM", "11 PM"
)


#pivot table and chart of trips by hour
trips_by_hour<- data %>%
  group_by(hour_am_pm)%>%
  summarize(count= n())


trips_by_hour$hour_am_pm <- factor(trips_by_hour$hour_am_pm, levels = ordered_hours)

hour_colors <- viridis(24, option = 'D')

ggplot(trips_by_hour, aes(x=hour_am_pm, y=count, fill=hour_am_pm))+
  geom_bar(stat='identity')+
  scale_x_discrete(limits = ordered_hours) +
  labs(title= 'Trips by Hour',
       x = 'Hour',
       y= "Number of Trips")+
  scale_y_continuous(labels = scales::label_number(scale = 1e-3, suffix = "K"))+
  scale_fill_manual(values = hour_colors)+
  theme(legend.position = "none")


#pivot table and chart of trips every hour and month
trips_by_hour_month <- data %>%
  group_by(month_name, hour_am_pm)%>%
  summarize(count= n())

trips_by_hour_month$hour_am_pm <- factor(trips_by_hour_month$hour_am_pm, levels = ordered_hours)


ggplot(trips_by_hour_month, aes(x=month_name, y=count, fill=hour_am_pm))+
  geom_bar(stat= 'identity',  position = 'dodge')+
  labs(fill ='Hour', 
       title= 'Trips by Hour and Month',
       x = 'Month',
       y= "Number of Trips")+
  scale_fill_manual(values = hour_colors)

#pivot table of trips every day of the month 
data$day <- day(data$date_parsed)

trips_per_day <- data %>%
  group_by(month_name, day)%>%
  summarize(count = n(), .groups = 'drop')%>%
  pivot_wider(
    names_from = month_name,
    values_from = count,
    values_fill = 0
  )

#pivot and chart of trips by day and month
trips_by_day_month <- data %>%
  group_by(month_name, weekday_name)%>%
  summarize(count = n())

ggplot(trips_by_day_month, aes(x=month_name, y=count, fill=weekday_name))+
  geom_bar(stat= 'identity',  position = 'dodge')+
  labs(fill ='Day', 
       title= 'Trips by Day and Month',
       x = 'Month',
       y= "Number of Trips")+
  scale_fill_brewer(palette = 'Blues')

#pivot and chart of trips by bases and month
trips_by_bases <- data %>%
  group_by(base, month_name)%>%
  summarize(count =n())

ggplot(trips_by_bases, aes(x=base, y=count, fill=month_name))+
  geom_bar(stat= 'identity',  position = 'dodge')+
  labs(fill ='Month', 
       title= 'Trips by Bases and Month',
       x = 'Base',
       y= "Number of Trips")+
  scale_y_continuous(labels = scales::label_number(scale = 1e-3, suffix = "K"))+
  scale_fill_brewer(palette = 'Blues')

#heat map by hour and day
heat_hour_day <- data %>%
  group_by(hour_am_pm, day)%>%
  summarize(count = n())

heat_hour_day$hour_am_pm <- factor(heat_hour_day$hour_am_pm, levels = ordered_hours)


ggplot(heat_hour_day, aes(x=day, y=hour_am_pm, fill=count))+
  geom_tile(color = 'white')+
  scale_fill_viridis(direction = -1) +  
  labs(title= 'Heatmap by Hour and Day',
       x = 'Day',
       y= "Hour",
       fill = '# Trips')+
  theme_minimal()

#heat map by month and day
heat_month_day<- data %>%
  group_by(month_name, day)%>%
  summarize(count = n())


ggplot(heat_month_day, aes(x=month_name, y=day, fill=count))+
  geom_tile(color = 'white')+
  scale_fill_viridis(direction = -1) + 
  scale_y_continuous(breaks = seq(1, max(heat_month_day$day), by = 1)) +  
  labs(title= 'Heatmap by Month and Day',
       x = 'Month',
       y= "Day",
       fill = '# Trips')+
  theme_minimal()

#heat map by month and week


#get the week
data <- data %>%
  mutate(
    week_start = floor_date(data$date_parsed, unit = "week"),
    start_of_month = floor_date(data$date_parsed, unit = "month"),
    week_of_month = as.integer((day(data$date_parsed) - 1) / 7) + 1 
    )

heat_month_week<- data%>%
  group_by(month_name, week_of_month)%>%
  summarize(count = n())

ggplot(heat_month_week, aes(x=month_name, y=week_of_month, fill=count))+
  geom_tile(color = 'white')+
  scale_fill_viridis(direction = -1) + 
  labs(title= 'Heatmap by Month and Week',
       x = 'Month',
       y= "Week",
       fill = '# Trips')+
  theme_minimal()

#heat map by bases and day of week
heat_bases_day <- data%>%
  group_by(base, weekday_name)%>%
  summarize(count = n())

ggplot(heat_bases_day, aes(x=base, y=weekday_name, fill=count))+
  geom_tile(color = 'white')+
  scale_fill_viridis(direction = -1) + 
  labs(title= 'Heatmap by Bases and Weekday',
       x = 'Base',
       y= "Weekday",
       fill = '# Trips')+
  theme_minimal()


write.csv(data, 'uber_data_final.csv')

saveRDS(data, file = 'uber_data_finals.rds')
