library(readxl)
library(dplyr)
library(readr)  
rm(list=ls())

setwd('/Users/sashabotsul/Downloads/rstudio/counting_cars')
file <- "cars_count.xlsx"

sheet1 <- read_excel(file, sheet= 'Aashish')
sheet2 <- read_excel(file, sheet= 'Abhib')
sheet3 <- read_excel(file, sheet= 'Kritan')

combined_data <- bind_rows(sheet1, sheet2, sheet3)
head(combined_data)

write_csv(combined_data,"combined_cars.csv" )
