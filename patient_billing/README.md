# Analysis of Patient Billing
<p> by: Sasha Botsul </p>

#### This page analyzes the patient, visit, and billing data.

## Summary of Process
<p>The data first needed to be read and joined, which was done using a left join.</p>

```
df_billing <- read_excel("Billing.xlsx")
df_patient <- read_excel("Patient.xlsx")
df_visit <- read_excel("Visit.xlsx")

df_join1 <- left_join(df_billing, df_visit, by= c('VisitID'))
df_join2 <- left_join(df_join1, df_patient, by = c('PatientID'))
```
<p>In order to get unique data for the graphs showing reasons for visit, the data needed to be distinct.</p>

```
unique_visit <- df_join2 %>%
  distinct(PatientID, VisitDate, .keep_all = TRUE)
```
## Reasons for Visit By Month
