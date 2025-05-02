library(shiny)
library(ggplot2)
library(DT)
library(readxl)
library(RCurl)
library(bslib)
library(dplyr)
library(tidyr)
library(hms)
library(lubridate)
library(viridis)
library(leaflet)
library(leaflet.extras)
rm(list=ls())

dataset <- read.csv('uber_data_final.csv')

column_names<-colnames(dataset)

# Chronological order for 12-hour AM/PM
ordered_hours <- c(
  "12 AM", "1 AM", "2 AM", "3 AM", "4 AM", "5 AM",
  "6 AM", "7 AM", "8 AM", "9 AM", "10 AM", "11 AM",
  "12 PM", "1 PM", "2 PM", "3 PM", "4 PM", "5 PM",
  "6 PM", "7 PM", "8 PM", "9 PM", "10 PM", "11 PM"
)

ordered_months <- c('April', 'May', 'June', 'July', 'August', 'September')

ordered_days <- c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday')


#pivot table trips by hour
trips_by_hour<- dataset %>%
  group_by(hour_am_pm)%>%
  summarize(count= n())
trips_by_hour$hour_am_pm <- factor(trips_by_hour$hour_am_pm, levels = ordered_hours)
hour_colors <- viridis(24, option = 'D')

#pivot table trips by hour and month
trips_by_hour_month <- dataset %>%
  group_by(month_name, hour_am_pm)%>%
  summarize(count= n())
trips_by_hour_month$month_name <- factor(trips_by_hour_month$month_name, levels = ordered_months)
trips_by_hour_month$hour_am_pm <- factor(trips_by_hour_month$hour_am_pm, levels = ordered_hours)

#pivot table trips by day and month
trips_by_day_month <- dataset %>%
  group_by(month_name, weekday_name)%>%
  summarize(count = n())

trips_by_day_month$month_name <- factor(trips_by_day_month$month_name, levels = ordered_months)
trips_by_day_month$weekday_name <- factor(trips_by_day_month$weekday_name, levels = ordered_days)


#pivot table trips by base and month
trips_by_bases <- dataset %>%
  group_by(base, month_name)%>%
  summarize(count =n())

trips_by_bases$month_name <- factor(trips_by_bases$month_name, levels = ordered_months)


#pivot table heat map trips by hour and day
heat_hour_day <- dataset %>%
  group_by(hour_am_pm, day)%>%
  summarize(count = n())
heat_hour_day$hour_am_pm <- factor(heat_hour_day$hour_am_pm, levels = ordered_hours)

#pivot table heat map trips by month and day
heat_month_day<- dataset %>%
  group_by(month_name, day)%>%
  summarize(count = n())

heat_month_day$month_name <- factor(heat_month_day$month_name, levels = ordered_months)


#pivot table heat map trips by month and week
heat_month_week<- dataset %>%
  group_by(month_name, week_of_month)%>%
  summarize(count = n())

heat_month_week$month_name <- factor(heat_month_week$month_name, levels = ordered_months)

#pivot table heat map trips by base and weekday
heat_bases_day <- dataset%>%
  group_by(base, weekday_name)%>%
  summarize(count = n())

heat_bases_day$weekday_name <- factor(heat_bases_day$weekday_name, levels = ordered_days)


#pivot table of trips every day of the month 

dataset$month_name <- factor(dataset$month_name, levels = ordered_months)

trips_per_day <- dataset %>%
  group_by(month_name, day)%>%
  summarize(count = n(), .groups = 'drop')%>%
  pivot_wider(
    names_from = month_name,
    values_from = count,
    values_fill = 0
  )



ui <- fluidPage( 
  theme = shinythemes::shinytheme("slate"),
  titlePanel(title = "Uber Data"),
  
  p('This application visualizes Uber pickup patterns in New York City over several months using barcharts, heat maps, and maps to explore trends.'),
  
  navset_card_underline(
    
    
    nav_panel("Charts",
              h4('Trips by Hour'),
              plotOutput("trip_hour"),
              h5("This chart shows the number of trips occuring every hour.
                 The largest peak happens at 5pm."),
              
              h4("Trips by Hour and Month"),
              plotOutput("trip_hour_month"),
              h5("This chart shows the number of trips occuring every month by the hour. 
                 September has the largrest number of trips. Peaks in every month occur from 5pm-6pm."),
              
              h4("Trips by Day and Month"),
              plotOutput("trip_day_month"),
              h5("This chart shows the number of trips every month by the weekday. 
                 Sundays are the slowest days."),
              
              h4("Trips by Bases and Months"), 
              plotOutput("trip_base_month"),
              h5("This chart shows the number of trips at every base by month. 
                 Base B02598 seems to be the most consistent.")
              ),
              
    nav_panel("Heat Maps",
              h4("Trips by Hour and Day"), 
              plotOutput("heat_hour_day"),
              h5("Each tile in this heat map represents an hour of a given day.
                 The fill of each tile is the number of trips taken at that hour."),
              
              h4("Trips by Month and Day"), 
              plotOutput("heat_month_day"),
              h5("Each tile in this heat map represents one day of a given month.
                 The fill of each tile is the number of trips taken at that day."),
              
              h4("Trips by Month and Week"), 
              plotOutput("heat_month_week"),
              h5("Each tile in this heat map represents a week of a given month.
                  The fill of each tile is the number of trips taken at that week."),
              
              h4("Trips by Base and Weekday"), 
              plotOutput("heat_base_weekday"),
              h5("Each tile in this heat map represents a weekday for a given base.
                  The fill of each tile is the number of trips taken at that base.")
              ),
    nav_panel("Tables",
              h4("Trips Every Day of the Month"), 
              tableOutput("table"),
              
              h4("Trips By Hour"),
              tableOutput("table2"),
              ),
    nav_panel('Leaflet',
              h4('Leaflet'),
              leafletOutput('map'),
              h5("This leaflet map displayed the density of trips. Considering it is mostly yellow,
                 the density is very high throughout New York.")
              )
    )
)




server <- function(input, output) {
  
  
  #Chart of trips by hour
  output$trip_hour <- renderPlot({
    ggplot(trips_by_hour, aes(x=hour_am_pm, y=count, fill=hour_am_pm))+
      geom_bar(stat='identity')+
      scale_x_discrete(limits = ordered_hours) +
      labs(x = 'Hour',
           y= "Number of Trips")+
      scale_y_continuous(labels = scales::label_number(scale = 1e-3, suffix = "K"))+
      scale_fill_manual(values = hour_colors)+
      theme(legend.position = "none")
  })
  
  #chart of trips by hour and month
  output$trip_hour_month <- renderPlot({
  ggplot(trips_by_hour_month, aes(x=month_name, y=count, fill=hour_am_pm))+
    geom_bar(stat= 'identity',  position = 'dodge')+
    labs(fill ='Hour', 
         x = 'Month',
         y= "Number of Trips")+
    scale_fill_manual(values = hour_colors)
  })
  
  #chart of trips by day and month
  output$trip_day_month <- renderPlot({
    ggplot(trips_by_day_month, aes(x=month_name, y=count, fill=weekday_name))+
      geom_bar(stat= 'identity',  position = 'dodge')+
      labs(fill ='Day', 
           x = 'Month',
           y= "Number of Trips")+
      scale_fill_brewer(palette = 'Blues')
  })
  
  #chart of trips by bases and months
  output$trip_base_month <- renderPlot({
  ggplot(trips_by_bases, aes(x=base, y=count, fill=month_name))+
    geom_bar(stat= 'identity',  position = 'dodge')+
    labs(fill ='Month', 
         x = 'Base',
         y= "Number of Trips")+
    scale_y_continuous(labels = scales::label_number(scale = 1e-3, suffix = "K"))+
    scale_fill_brewer(palette = 'Blues')
  })
  
  #heat map of trips by hour and day
  output$heat_hour_day<- renderPlot({
  ggplot(heat_hour_day, aes(x=day, y=hour_am_pm, fill=count))+
    geom_tile(color = 'white')+
    scale_fill_viridis(direction = -1) +  
    labs(title= 'Heatmap by Hour and Day',
         x = 'Day',
         y= "Hour",
         fill = '# Trips')+
    theme_minimal()
  })
  
  #heat map of trips by month and day
  output$heat_month_day <- renderPlot({
    ggplot(heat_month_day, aes(x=month_name, y=day, fill=count))+
      geom_tile(color = 'white')+
      scale_fill_viridis(direction = -1) + 
      scale_y_continuous(breaks = seq(1, max(heat_month_day$day), by = 1)) +  
      labs(title= 'Heatmap by Month and Day',
           x = 'Month',
           y= "Day",
           fill = '# Trips')+
      theme_minimal()
  })
    
  #heat map of trips by month and week
  output$heat_month_week <- renderPlot({
    ggplot(heat_month_week, aes(x=month_name, y=week_of_month, fill=count))+
      geom_tile(color = 'white')+
      scale_fill_viridis(direction = -1) + 
      labs(title= 'Heatmap by Month and Week',
           x = 'Month',
           y= "Week",
           fill = '# Trips')+
      theme_minimal()
  })
  
  #heat map of trips by bases and weekday
  output$heat_base_weekday <- renderPlot({
  ggplot(heat_bases_day, aes(x=base, y=weekday_name, fill=count))+
    geom_tile(color = 'white')+
    scale_fill_viridis(direction = -1) + 
    labs(title= 'Heatmap by Bases and Weekday',
         x = 'Base',
         y= "Weekday",
         fill = '# Trips')+
    theme_minimal()
  })
  
  #table of trips every day of the month
  output$table <- renderTable({
    trips_per_day%>%
      rename(Day = day)
    
  })
  
  #table of trips by hour
  output$table2 <- renderTable({
    trips_by_hour %>%
      mutate(hour_am_pm = factor(hour_am_pm, levels = ordered_hours)) %>%
      arrange(hour_am_pm)%>%
      rename(Hour = hour_am_pm, `Number of Trips` = count)
  })
  
  #leaflet
  output$map <- renderLeaflet({
    leaflet(data = dataset) %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      addHeatmap(
        lng= ~lon,
        lat = ~lat,
        blur = 20,
        max = 0.05,
        radius = 15,
        gradient = viridis(256)
      ) %>%
      addLegend(
        position = 'bottomright',
        title = 'Trip Density',
        colors = viridis::viridis(4),
        labels = c('Low', 'Medium', 'High', 'Very High'),
        opacity = 0.5
      )
  })
}


shinyApp(ui=ui, server=server)

