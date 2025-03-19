# Text Analysis of Consumer Complaints
<p> by: Sasha Botsul </p>

#### This page analyzes the consumer complaint narratives/messages to the companies using sentiment analyses, bing and NRC.

---
## Data Cleaning
1. Date Format
- Convert the Date Received and Date sent to Compant to a proper format
```
df_complaints$Date.received <- mdy(df_complaints$Date.received)
df_complaints$Date.sent.to.company <- mdy(df_complaints$Date.sent.to.company)
```
2. Text Columns
- Convert N/A values into "Unknown"
```
text_columns <- c("Product", "Sub.product", "Issue", "Sub.issue", "Company", "State", "Tags",
                  "Consumer.consent.provided.", "Submitted.via", "Company.response.to.consumer",
                  "Timely.response.", "Consumer.disputed.")
df_complaints[text_columns] <- lapply(df_complaints[text_columns], function(x) ifelse(is.na(x), "Unknown", x))
```
3. Categorical Columns
- Convert N/A values into "Unknown"
```
categorical_cols <- c("Consumer.complaint.narrative", "Company.public.response") 
df_complaints[categorical_cols] <- lapply(df_complaints[categorical_cols], function(x) ifelse(is.na(x), "Unknown", x))
df_complaints$Consumer.consent.provided.[df_complaints$Consumer.consent.provided. %in% c(NA, "N/A")] <- "Unknown"
```
4. Remove Zipcode Column
- Removed the Zipcode column due to inconsistency in data and unnecessary data.
```
df_complaints <- subset(df_complaints, select= -c(10))
```
5. Ensure Structure
- Remove any potential duplicates
- Check structure and summarize data
```
df_complaints<- distinct(df_complaints)
str(df_complaints)
summary(df_complaints)
```
6. Export Clean File
```
write.csv(df_complaints,'/Users/sashabotsul/Downloads/rstudio/text_analysis/clean_data.csv',  row.names= FALSE)
```
#### Cleaning Complaint Narrative
- Although the file itself is clean, the text needed to be cleaned before using for sentiment analysis.
- The text is broken up into individual tokens, removing stop words as well.
```
df_clean <- df_complaints %>%
  select(Consumer.complaint.narrative) %>%
  filter(Consumer.complaint.narrative != 'Unknown')%>%
  rename(text = 'Consumer.complaint.narrative')

df_token <- df_clean %>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words, by = 'word')
```
---
## Text Analysis Using BING
1. Sentiment Analysis
<img src= 'bing_count.png'>
- This bar chart shows the proportion of negative and positive sentiments.
- Considering the data is consumer complaints, the results are not shocking.
- The negative sentiment count is 543,382. The positive sentiment count is 188,536.
- This chart is valuable as it shows the overall sentiments of consumers among banks.

2. Common Positive and Negative Words
<img src = 'positive/negative.png'>




