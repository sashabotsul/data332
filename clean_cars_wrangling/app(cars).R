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

data_url <- getURL("https://raw.githubusercontent.com/sashabotsul/data332/refs/heads/main/clean_cars_wrangling/clean_cars.csv")
dataset <- read.csv(text = data_url)



column_names<-colnames(dataset) #for input selections

dataset <- dataset %>%
  mutate(Speeding_Classification = case_when(
    differences <= 0 ~ "not_speeding",
    differences > 0 & differences <= 5 ~ "speeding_by_5_or_less",
    differences > 5 ~ "speeding_by_more_than_5"
  ))

car_type_speeding_pivot_table <- dataset %>%
  group_by(vehicle, Speeding_Classification) %>%
  summarize(Count = n())


dataset <- dataset %>%
  mutate(Time_Category = case_when(
    times >= "00:00" & times < "12:00" ~ "morning",
    times >= "12:00" & times < "17:00" ~ "midday",
    times >= "17:00" & times <= "23:59" ~ "afternoon"
  ))

time_of_day_speeding_pivot_table <- dataset %>%
  group_by(vehicle, Time_Category, Speeding_Classification) %>%
  summarize(Count = n())

ui <- fluidPage( 
  
  titlePanel(title = "Explore Counting Car Dataset"),
  
  
  navset_card_underline(
    
    nav_panel("Difference in Speed Box Plots",
              plotOutput("difference_in_speed_box_plot"),
              h5("Summary Statistics for Difference in Readings"),
              tableOutput("diff_summary"),
              h5("This box & whisker plot displays the difference in speed from a driver's initial .vs. their final read. 
                 The mean and median both being at or near zero respectively show that the 
                 presence of a speed sign may not deter drivers from lowering there speed.")
    ),
    
    nav_panel("Final Speed Box Plots",
              plotOutput("final_speed_box_plot"),
              h5("Summary Statistics for Final Read"),
              tableOutput("final_summary"),
              h5("The median is just over the speed limit of 30. The interquartile range is from 28-34, meaning 50% of the speeds fall within this range. 
                  A large portion of the IQR is above 30 mph, meaning most of the drivers were speeding.
                  The right whisker ends at 41, further portraying that the data is skewed above the speed limit. 
                  Based on this, we can assume that the presence of a speed sign may not lead to drivers decreasing their speed.")
    ),
    
    nav_panel("Speeding by Car Type",
              plotOutput("speeding_by_car_type"),
              h5("In this chart the cars are classified by their type and by how much if at all they were speeding by.
                 The red indicates more the 5 mph over the speed limit, yellow is under 5 over the speed limit, and green is not speeding.
                 Our analysis shows that the majority of cars in this area are Sedans and SUVs. 
                 Between these two majority groups the Sedans show a higher probablity of the driver speeding than the SUV drivers.")
    ),
    
    nav_panel("Speeding by Time of Day", 
              plotOutput("speeding_by_time_of_day"),
              h5("This charts showcases the three different time frames the car data was collected. 
                 This graph shows that more people speed during the mornings. 
                 This could be because of people trying to get to work or school. 
                 A drop off of speeders over 5 mph during the mid is seen but they return once again in the afternoon after they get off work.")),
    
    nav_panel("Scatter Plots", fluidRow(
      column(2,
             selectInput('X', 'Choose X', column_names, column_names[1]),
             selectInput('Y', 'Choose Y', column_names, column_names[3]),
             selectInput('Splitby', 'Split By', column_names, column_names[3])
      ),
      column(4, plotOutput('scatter_plot')),
      column(6,
             DT::dataTableOutput("table_01", width = "100%"),
             h5("Summary Statistics for Selected Columns"),
             tableOutput("scatter_summary"),
             h5("analysis here")
      )
    ))
  )
)


server <- function(input, output) {
  
  # Scatter plot
  output$scatter_plot <- renderPlot({
    ggplot(dataset, aes_string(x = input$X, y = input$Y, colour = input$Splitby)) + 
      geom_point()
  })
  
  # Data Table
  output$table_01 <- DT::renderDataTable({
    dataset[, c(input$X, input$Y, input$Splitby)]
  }, options = list(pageLength = 4))
  
  # Summary for Scatter Plot
  output$scatter_summary <- renderTable({
    df <- dataset[, c(input$X, input$Y)]
    data.frame(
      Variable = names(df),
      Min = sapply(df, min, na.rm = TRUE),
      Mean = round(sapply(df, mean, na.rm = TRUE), 2),
      Median = sapply(df, median, na.rm = TRUE),
      Max = sapply(df, max, na.rm = TRUE),
      SD = round(sapply(df, sd, na.rm = TRUE), 2)
    )
  })
  
  # Boxplot: Difference in Speed
  output$difference_in_speed_box_plot <- renderPlot({
    boxplot(dataset$differences, col = "#69b3a2", xlab = "Difference in Readings", horizontal = TRUE)
  })
  
  # Summary for Difference in Speed
  output$diff_summary <- renderTable({
    diff_data <- dataset$differences
    data.frame(
      Min = min(diff_data, na.rm = TRUE),
      Mean = round(mean(diff_data, na.rm = TRUE), 2),
      Median = median(diff_data, na.rm = TRUE),
      Max = max(diff_data, na.rm = TRUE),
      SD = round(sd(diff_data, na.rm = TRUE), 2)
    )
  })
  
  # Boxplot: Final Speed
  output$final_speed_box_plot <- renderPlot({
    boxplot(dataset$final, col = "#69b3a2", xlab = "Final Read", horizontal = TRUE)
  })
  
  # Summary for Final Read
  output$final_summary <- renderTable({
    final_data <- dataset$final
    data.frame(
      Min = min(final_data, na.rm = TRUE),
      Mean = round(mean(final_data, na.rm = TRUE), 2),
      Median = median(final_data, na.rm = TRUE),
      Max = max(final_data, na.rm = TRUE),
      SD = round(sd(final_data, na.rm = TRUE), 2)
    )
  })
  
  # Bar Chart for Speeding by Car Type
  output$speeding_by_car_type <- renderPlot({
    ggplot(car_type_speeding_pivot_table, aes(fill=Speeding_Classification, y=Count, x=vehicle)) + 
      geom_bar(position="stack", stat="identity") +
      scale_fill_manual(values = c("#2dc937", "#e7b416", "#cc3232")) +
      xlab("Type of Car")
  })

  #Bar Chart for time of day
  output$speeding_by_time_of_day <- renderPlot({
    ggplot(time_of_day_speeding_pivot_table, aes(fill=Speeding_Classification, y=Count, x=Time_Category)) + 
      geom_bar(position="stack", stat="identity") +
      scale_fill_manual(values = c("#2dc937", "#e7b416", "#cc3232")) +
      xlab("Time of Day")
  })
}


shinyApp(ui=ui, server=server)
