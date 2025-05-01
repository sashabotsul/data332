# Uber Data 
<p> by: Sasha Botsul </p>

---

### This page shows snippets of my code during the process of cleaning the uber data, creating charts, and the shiny app.

---
## Data Cleaning
1. Time Format
- Converting time format to just display hour and am/pm
  ```
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
  ```
2. Date
- Get the Months and Weekdays
  ```
  data<- data %>%
  mutate(
  date_parsed = mdy(date),
  month_name = month(date_parsed, label = TRUE, abbr = FALSE),
  weekday_name = wday(date_parsed, label = TRUE, abbr = FALSE))
  ```
- Get the Days
  ```
  data$day <- day(data$date_parsed)
  ```
- Get the Weeks of the Month
  ```
  data <- data %>%
  mutate(
    week_start = floor_date(data$date_parsed, unit = "week"),
    start_of_month = floor_date(data$date_parsed, unit = "month"),
    week_of_month = as.integer((day(data$date_parsed) - 1) / 7) + 1 
    )
  ```
3. Ordering Hours for Pivot Tables
   ```
   ordered_hours <- c(
    "12 AM", "1 AM", "2 AM", "3 AM", "4 AM", "5 AM",
   "6 AM", "7 AM", "8 AM", "9 AM", "10 AM", "11 AM",
   "12 PM", "1 PM", "2 PM", "3 PM", "4 PM", "5 PM",
   "6 PM", "7 PM", "8 PM", "9 PM", "10 PM", "11 PM")
   ```
4. Set the Colors for Hours
   ```
   hour_colors <- viridis(24, option = 'D')
   ```
---
## Chart Code
1. Trips by Hour
- Pivot
```
trips_by_hour<- data %>%
group_by(hour_am_pm)%>%
summarize(count= n())
trips_by_hour$hour_am_pm <- factor(trips_by_hour$hour_am_pm, levels ordered_hours)
```
- Chart
```
ggplot(trips_by_hour, aes(x=hour_am_pm, y=count, fill=hour_am_pm))+
geom_bar(stat='identity')+
scale_x_discrete(limits = ordered_hours) +
labs(title= 'Trips by Hour',
  x = 'Hour',
 y= "Number of Trips")+
scale_y_continuous(labels = scales::label_number(scale = 1e-3, suffix = "K"))+
scale_fill_manual(values = hour_colors)+
theme(legend.position = "none")
```
2. Trips by Hour and Month
- Pivot
```
trips_by_hour_month <- data %>%
  group_by(month_name, hour_am_pm)%>%
  summarize(count= n())
trips_by_hour_month$hour_am_pm <- factor(trips_by_hour_month$hour_am_pm, levels = ordered_hours)
```
- Chart
```
ggplot(trips_by_hour_month, aes(x=month_name, y=count, fill=hour_am_pm))+
  geom_bar(stat= 'identity',  position = 'dodge')+
  labs(fill ='Hour', 
       title= 'Trips by Hour and Month',
       x = 'Month',
       y= "Number of Trips")+
  scale_fill_manual(values = hour_colors)
```
3. Trips by Day of the Month
- Pivot
```
trips_per_day <- data %>%
  group_by(month_name, day)%>%
  summarize(count = n(), .groups = 'drop')%>%
  pivot_wider(
    names_from = month_name,
    values_from = count,
    values_fill = 0
  )
```
4. Trips by Day and Month
- Pivot
```
trips_by_day_month <- data %>%
  group_by(month_name, weekday_name)%>%
  summarize(count = n())
```
- Chart
```
ggplot(trips_by_day_month, aes(x=month_name, y=count, fill=weekday_name))+
  geom_bar(stat= 'identity',  position = 'dodge')+
  labs(fill ='Day', 
       title= 'Trips by Day and Month',
       x = 'Month',
       y= "Number of Trips")+
  scale_fill_brewer(palette = 'Blues')
```
5. Trips by Bases and Month
- Pivot
```
trips_by_bases <- data %>%
  group_by(base, month_name)%>%
  summarize(count =n())
```
- Chart
```
ggplot(trips_by_bases, aes(x=base, y=count, fill=month_name))+
  geom_bar(stat= 'identity',  position = 'dodge')+
  labs(fill ='Month', 
       title= 'Trips by Bases and Month',
       x = 'Base',
       y= "Number of Trips")+
  scale_y_continuous(labels = scales::label_number(scale = 1e-3, suffix = "K"))+
  scale_fill_brewer(palette = 'Blues')
```
## Heat Map Code
1. Heat Map by Hour and Day
- Pivot
```
heat_hour_day <- data %>%
  group_by(hour_am_pm, day)%>%
  summarize(count = n())

heat_hour_day$hour_am_pm <- factor(heat_hour_day$hour_am_pm, levels = ordered_hours)
```
- Heat Map
```
ggplot(heat_hour_day, aes(x=day, y=hour_am_pm, fill=count))+
  geom_tile(color = 'white')+
  scale_fill_viridis(direction = -1) +  
  labs(title= 'Heatmap by Hour and Day',
       x = 'Day',
       y= "Hour",
       fill = '# Trips')+
  theme_minimal()
```
2. Heat Map by Month and Day
- Pivot
```
heat_month_day<- data %>%
  group_by(month_name, day)%>%
  summarize(count = n())
```
- Heat Map
```
ggplot(heat_month_day, aes(x=month_name, y=day, fill=count))+
  geom_tile(color = 'white')+
  scale_fill_viridis(direction = -1) + 
  scale_y_continuous(breaks = seq(1, max(heat_month_day$day), by = 1)) +  
  labs(title= 'Heatmap by Month and Day',
       x = 'Month',
       y= "Day",
       fill = '# Trips')+
  theme_minimal()
```
3. Heat Map by Month and Week
- Pivot
```
heat_month_week<- data%>%
  group_by(month_name, week_of_month)%>%
  summarize(count = n())
```
- Heat Map
```
ggplot(heat_month_week, aes(x=month_name, y=week_of_month, fill=count))+
  geom_tile(color = 'white')+
  scale_fill_viridis(direction = -1) + 
  labs(title= 'Heatmap by Month and Week',
       x = 'Month',
       y= "Week",
       fill = '# Trips')+
  theme_minimal()
```
4. Heat Map by Bases and Weekday
- Pivot
```
heat_bases_day <- data%>%
  group_by(base, weekday_name)%>%
  summarize(count = n())
```
- Heat Map
```
ggplot(heat_bases_day, aes(x=base, y=weekday_name, fill=count))+
  geom_tile(color = 'white')+
  scale_fill_viridis(direction = -1) + 
  labs(title= 'Heatmap by Bases and Weekday',
       x = 'Base',
       y= "Weekday",
       fill = '# Trips')+
  theme_minimal()
```
## Leaflet


     
