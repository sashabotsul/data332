library(shiny)
library(ggplot2)
library(DT)
library(readxl)
library(RCurl)
library(bslib)
library(dplyr)
library(tidyr)
library(janitor)
library(tools)
library(readr)  

setwd('/Users/sashabotsul/Downloads/rstudio/counting_cars/csv_files')

data1<- read.csv('Car Data Collection.csv')
data2<- read.csv('combined_cars.csv')
data3<- read.csv('counting_cars_final.csv')
data4<- read.csv('Counting_Cars.csv')
data5<- read.csv('Data_Counting_Cars.csv')
data6<- read.csv('speed_counting_cars1.csv')
data7<- read.csv('carTracker.csv')

car_types <- c(
  "1" = "Emergency",
  "2" = "Hatchback",
  "3" = "Sedan",
  "4" = "SUV",
  "5" = "Van",
  "6" = "Minivan",
  "7" = "Motorcycle",
  "8" = "Coupe",
  "9" = "Truck",
  "10" = "Pickup Truck"
)
data4$Type_of_Car <- car_types[as.character(data4$Type_of_Car)]


combined_data <-bind_rows(data1, data2, data3, data4, data5, data6, data7)
combined_data <- clean_names(combined_data)
combined_data <- combined_data %>%
  select(-c(1, 4, 6, 7, 9,14, 15,18,21, 22, 29, 30, 31, 32, 33, 34, 37, 42,43, 44, 45, 47, 49, 50))

names(combined_data)

#combine columns
combined_data <- combined_data %>%
  mutate(
    vehicle = coalesce(as.character(type_of_car), 
                       as.character(type_of_car_2), 
                       as.character(vehicle_style), 
                       as.character(vehicle_type), 
                       as.character(body_style)
    ),
    initial = coalesce(as.numeric(initial_speed), 
                       as.numeric(initial_read), 
                       as.numeric(init_speed)
    ),
    final = coalesce(as.numeric(final_speed), 
                     as.numeric(final_read), 
                     as.numeric(final_speed_2), 
                     as.numeric(mph), 
                     as.numeric(speed), 
                     as.numeric(speed_mph),
                     as.numeric(mph_2)
    ),
    times = coalesce(as.character(time_of_the_day), 
                     as.character(hr_min), 
                     as.character(time_recorded), 
                     as.character(time),
                     as.character(time_tracked)
    ),
    dates = coalesce(as.character(date), 
                     as.character(date_2), 
                     as.character(date_recorded)
    ),
    differences = coalesce(as.numeric(difference), 
                           as.numeric(difference_in_readings), 
                           as.numeric(speed_change)
    )
  )
#keep only combined
combined_data <- combined_data %>%
  select(vehicle, initial, final, times, dates, differences)

#duplicate dates column to avoid permanent changes to date
combined_data$date<-combined_data$dates

combined_data$date <- ifelse(
  grepl("^[A-Za-z]+, ", combined_data$date), 
  sub("^[A-Za-z]+, ", "", combined_data$date), 
  combined_data$date
)

# Only process dates that match the "Month DD, YYYY" format
combined_data$date <- ifelse(
  grepl("^[A-Za-z]+ [0-9]{2}, [0-9]{4}$", combined_data$date),
  format(as.Date(combined_data$date, format = "%B %d, %Y"), "%m/%d/%Y"),
  combined_data$date
)

combined_data$date <- ifelse(
  grepl("^\\d{2}/\\d{2}/\\d{4}$", combined_data$date),
  # Extract month, day, and year as separate components
  sapply(strsplit(combined_data$date, "/"), function(x) paste(as.numeric(x[1]), as.numeric(x[2]), x[3], sep = "/")),
  combined_data$date
)

combined_data$date <- ifelse(
  grepl("^\\d{1,2}/\\d{1,2}/\\d{2}$", combined_data$date),
  # If the year has 2 digits, add '20' to make it a 4-digit year
  gsub("(\\d{1,2}/\\d{1,2})/(\\d{2})", "\\1/20\\2", combined_data$date),
  combined_data$date
)

#apply changes to official dates column
combined_data$dates<- combined_data$date
#remove additional dates column
combined_data <- combined_data %>%
  select(-c(7))

#duplicate times column to avoid permanent changes to time
combined_data$time<- combined_data$times

combined_data$time <- ifelse(
  grepl("AM|PM", combined_data$time),
  format(strptime(combined_data$time, format = "%I:%M: %p"), format = "%H:%M"),  # Convert to 24-hour format
  combined_data$time 
)

combined_data$time <- ifelse(grepl(":", combined_data$time) & grepl("\\d{2}:\\d{2}:\\d{2}", combined_data$time),
                             format(strptime(combined_data$time, format = "%H:%M:%S"), format = "%H:%M"),
                             ifelse(grepl("\\d{1}:\\d{2}:\\d{2}", combined_data$time),
                                    format(strptime(combined_data$time, format = "%H:%M:%S"), format = "%H:%M"),
                                    combined_data$time))


#apply changes to official dates column
combined_data$times<- combined_data$time
#remove additional time column
combined_data <- combined_data %>%
  select(-c(7))

#duplicate vehicles column to avoid permanent changes to vehicles
combined_data$vehicles <- combined_data$vehicle

combined_data$vehicles <- gsub("_", " ", combined_data$vehicles)

combined_data$vehicles<- toTitleCase(combined_data$vehicles)

combined_data <- combined_data %>%
  mutate(vehicles = ifelse(vehicles == "Suv", "SUV", vehicles))


#apply changes to official dates column
combined_data$vehicle<- combined_data$vehicles
#remove additional vehicle column
combined_data <- combined_data %>%
  select(-c(7))

write_csv(combined_data,"clean_cars.csv" )


