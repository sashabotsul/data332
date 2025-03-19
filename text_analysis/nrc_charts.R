library(ggplot2)
library(dplyr)
library(tidyverse)
library(readxl)
library(tidyr)
library(tidytext)
library(scales)
rm(list=ls())

setwd('/Users/sashabotsul/Downloads/rstudio/text_analysis')

df_complaints <- read_csv("clean_data.csv")

#cleaning complaint narrative
df_clean <- df_complaints %>%
  select(Consumer.complaint.narrative) %>%
  filter(Consumer.complaint.narrative != 'Unknown')%>%
  rename(text = 'Consumer.complaint.narrative')

df_token <- df_clean %>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words, by = 'word')

head(df_token)

#sentiment analysis using nrc
nrc_sentiment <- df_token %>%
  inner_join(get_sentiments('nrc'))%>%
  count(sentiment, sort=TRUE)

ggplot(nrc_sentiment, aes(x= reorder(sentiment, -n), y=n, fill=sentiment))+
  geom_bar(stat='identity')+
  coord_flip()+
  theme_minimal()+
  labs(title= 'NRC Sentiment Analysis of Consumer Complaints',
       x='Emotion',
       y= 'Count')

#---nrc emotions by months
#cleaning the data again to add in date
df_clean2 <- df_complaints %>%
  select(Consumer.complaint.narrative, Date.sent.to.company) %>%
  filter(Consumer.complaint.narrative != 'Unknown')%>%
  rename(text = 'Consumer.complaint.narrative')

df_token2 <- df_clean2 %>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words, by = 'word')

df_token2[c('year', 'month', 'day')]<-
  str_split_fixed(df_token2$Date.sent.to.company, '-', 3)

df_emotions <- df_token2%>%
  inner_join(get_sentiments('nrc'))%>%
  count(month, sentiment)

ggplot(df_emotions, aes(x= month, y=n, fill= sentiment))+
  geom_bar(stat= 'identity')+
  theme_minimal()+
  labs(title= 'Trends of Emotion by Month',
       x = 'Month',
       y= 'Count')+
  scale_y_continuous(labels = comma_format()) +
  geom_text(aes(label = n), position = position_stack(vjust = .5), size = 1.75, color = "black") +
  scale_fill_brewer(palette= 'Paired')
  





  
