library(ggplot2)
library(dplyr)
library(tidyverse)
library(readxl)
library(tidyr)
library(here)
library(kableExtra)
library(viridisLite)
library(scales)
rm(list=ls())

setwd('/Users/sashabotsul/Downloads/rstudio/patient_billing')
df_billing <- read_excel("Billing.xlsx")
df_patient <- read_excel("Patient.xlsx")
df_visit <- read_excel("Visit.xlsx")

df_join1 <- left_join(df_billing, df_visit, by= c('VisitID'))
df_join2 <- left_join(df_join1, df_patient, by = c('PatientID'))

#chart on reason for visit by month of the year

unique_visit <- df_join2 %>%
  distinct(PatientID, VisitDate, .keep_all = TRUE)
  
print(unique_visit)

unique_visit[c('visit_year', 'visit_month', 'visit_day')]<-
  str_split_fixed(unique_visit$VisitDate, '-', 3)

reason_by_month <- unique_visit %>%
  group_by(visit_month, Reason) %>%
  summarize(count = n())

head(reason_by_month)

ggplot(reason_by_month, aes(x=Reason, y=count, fill=visit_month))+
  geom_bar(stat='identity')+
  labs(title= 'Reason for visit by month',
       x = 'Reason',
       y= "Number of Visits")+
  geom_text(aes(label = count), position = position_stack(vjust = .5), size = 3, color = "black") +
  theme(axis.text = element_text(angle=80, vjust= .5, hjust=1))


#chart on reason for visit based on walk in or not

reason_by_walkin <- unique_visit %>%
  group_by(Reason, WalkIn) %>%
  summarize(count = n())

head(reason_by_walkin)

ggplot(reason_by_walkin, aes(x=Reason, y=count, fill=WalkIn))+
  geom_bar(stat='identity')+
  labs(title= 'Reason for visit by walk in',
       x= "Reason",
       y= 'Number of Visits')+
  scale_fill_manual(values = c("lightblue", "lightgreen"))+
  geom_text(aes(label = count), position = position_stack(vjust = .5), size = 3, color = "black") +
  theme(axis.text = element_text(angle=80, vjust= .5, hjust=1))

#chart on reason for visit based on city

reason_by_city <- unique_visit %>%
  group_by(Reason, City) %>%
  summarize(count = n())

colors <-hue_pal()(21)

ggplot(reason_by_city, aes(x= Reason, y=count, fill = City))+
  geom_bar(stat = 'identity')+
  labs(title= "Reason for visit by City",
       x='Reason',
       y='Number of Visits')+
  geom_text(aes(label = count), position = position_stack(vjust = .5), size = 3, color = "black") +
  theme(axis.text = element_text(angle=80, vjust= .5, hjust=1))+
  scale_fill_manual(values = colors)
  
#chart on total invoice by reason and if paid

total_invoice <- df_join2 %>%
  group_by(Reason, InvoicePaid)%>%
  summarize(sum = sum(InvoiceAmt))

head(total_invoice)

ggplot(total_invoice, aes(x=Reason, y=sum, fill=InvoicePaid))+
  geom_bar(stat='identity')+
  labs(title= 'Total Invoice based on reason for visit',
       x='Reason',
       y= 'Total Invoice')+
  scale_fill_manual(values = c("lightpink", "lightgreen"))+
  geom_text(aes(label = sum), position = position_stack(vjust = .5), size = 3, color = "black") +
  theme(axis.text = element_text(angle=80, vjust= .5, hjust=1))
  
#chart on invoice type and if it is paid or not

invoice_type <- df_join2 %>%
  group_by(InvoiceItem, InvoicePaid) %>%
  summarize(count = n())

head(invoice_type)

ggplot(invoice_type, aes(x=InvoiceItem, y=count, fill=InvoicePaid))+
  geom_bar(stat='identity')+
  labs(title= 'Types of invoices and payment status',
       x = 'Invoice Item',
       y= "Number of Invoices")+
  scale_fill_manual(values = c("lightblue", "lightgreen"))+
  geom_text(aes(label = count), position = position_stack(vjust = .5), size = 3, color = "black") +
  theme(axis.text = element_text(angle=80, vjust= .5, hjust=1))

