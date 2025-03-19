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

#USING BING

#cleaning complaint narrative
df_clean <- df_complaints %>%
  select(Consumer.complaint.narrative) %>%
  filter(Consumer.complaint.narrative != 'Unknown')%>%
  rename(text = 'Consumer.complaint.narrative')

df_token <- df_clean %>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words, by = 'word')

head(df_token)

#sentiment analysis using bing
bing_sentiment <- df_token %>%
  inner_join(get_sentiments('bing'), by = 'word')%>%
  count(sentiment, sort= TRUE)

#plotting sentiment analysis  
ggplot(bing_sentiment, aes(x= sentiment, y= n, fill = sentiment))+
  geom_bar(stat = 'identity')+
  scale_y_continuous(labels = comma_format()) +
  scale_fill_manual(values = c("negative" = "#C44E52", "positive" = "lightgreen")) +
  theme_minimal()+
  labs(title = "Bing Sentiment Analysis of Consumer Complaints",
       x= 'Sentiment',
       y= 'Count')

#common positive and negative words using bing

bing_words <- df_token%>%
  inner_join(get_sentiments('bing'))%>%
  count(word,sentiment, sort=TRUE)%>%
  ungroup()
bing_words %>%
  group_by(sentiment)%>%
  slice_max(n, n=10)%>%
  ungroup() %>%
  mutate(word = reorder(word, n))%>%
  ggplot(aes(x=n, y=word, fill=sentiment))+
  geom_col(show.legend = FALSE)+
  scale_fill_manual(values = c("negative" = "#C44E52", "positive" = "lightgreen"))+
  facet_wrap(~sentiment, scales = 'free_y')+
  labs(title = 'Common Positive and Negative Words',
       x= 'Contribution to Sentiment',
       y=NULL)

#positive and negative sentiments by company
#adjusting clean data to include company
df_clean3 <- df_complaints %>%
  select(Consumer.complaint.narrative, Company) %>%
  filter(Consumer.complaint.narrative != 'Unknown')%>%
  rename(text = 'Consumer.complaint.narrative')

df_token3 <- df_clean3 %>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words, by = 'word')

df_company <-df_token3%>%
  inner_join(get_sentiments('bing'))%>%
  count(Company, sentiment,sort=TRUE)%>%
  ungroup()
df_company%>%
  group_by(sentiment)%>%
  slice_max(n, n=10)%>%
  ungroup() %>%
  ggplot(aes(x=Company, y=n, fill=sentiment))+
  geom_bar(stat= 'identity')+
  theme_minimal()+
  labs(title= 'Positive and Negative Sentiments by Company',
       x = 'Company',
       y= 'Count')+
  scale_fill_manual(values = c("negative" = "#C44E52", "positive" = "lightgreen"))+
  theme(axis.text = element_text(angle=30, vjust= .5, hjust=1))

  
