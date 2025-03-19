library(ggplot2)
library(dplyr)
library(tidyverse)
library(readxl)
library(tidyr)
rm(list=ls())

setwd('/Users/sashabotsul/Downloads/rstudio/text_analysis')

#read file and standardize names
df_complaints <- read_csv("Consumer_Complaints.csv", name_repair='universal')

#proper date format
df_complaints$Date.received <- mdy(df_complaints$Date.received)
df_complaints$Date.sent.to.company <- mdy(df_complaints$Date.sent.to.company)

#standardize text columns
text_columns <- c("Product", "Sub.product", "Issue", "Sub.issue", "Company", "State", "Tags",
                  "Consumer.consent.provided.", "Submitted.via", "Company.response.to.consumer",
                  "Timely.response.", "Consumer.disputed.")
df_complaints[text_columns] <- lapply(df_complaints[text_columns], function(x) ifelse(is.na(x), "Unknown", x))

#replace NA with "unknown"
categorical_cols <- c("Consumer.complaint.narrative", "Company.public.response") 
df_complaints[categorical_cols] <- lapply(df_complaints[categorical_cols], function(x) ifelse(is.na(x), "Unknown", x))
df_complaints$Consumer.consent.provided.[df_complaints$Consumer.consent.provided. %in% c(NA, "N/A")] <- "Unknown"

#remove zipcode column due to inconsistency
df_complaints <- subset(df_complaints, select= -c(10))

#remove duplicates
df_complaints<- distinct(df_complaints)

#check structure and summarize data
str(df_complaints)
summary(df_complaints)

#export clean file
write.csv(df_complaints,'/Users/sashabotsul/Downloads/rstudio/text_analysis/clean_data.csv',  row.names= FALSE)


