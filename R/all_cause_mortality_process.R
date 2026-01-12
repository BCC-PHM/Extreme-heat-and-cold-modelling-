setwd("C:/Users/chung/Documents/R_project/Extreme heat and cold/data")

library(tidyverse)
library(readxl)

all_cause_mortality_2022_2024 = read_excel("raw/all_cause-mortality_2022_2024.xlsx")


# check duplicates
duplicated_ID = all_cause_mortality_2022_2024 %>% 
  mutate(PatientId = as.character(PatientId)) %>% 
  count(PatientId) %>% 
  filter(n>1) 

#aggregate to daily death count 
daily_death_age_group = all_cause_mortality_2022_2024 %>% 
  select(REG_DATE,DEC_AGEC,LSOA_OF_RESIDENCE_CODE) %>% 
  mutate(Age_group = case_when(
    DEC_AGEC >= 0 & DEC_AGEC <=64 ~"0-64",
    DEC_AGEC >= 65 & DEC_AGEC <=74 ~"65-74",
    DEC_AGEC >= 75 & DEC_AGEC <=84 ~"75-84",
    DEC_AGEC >= 85 ~ "85+",
    TRUE ~ NA_character_),
         REG_DATE = as.Date(REG_DATE)) %>% 
  group_by(REG_DATE,LSOA_OF_RESIDENCE_CODE,Age_group) %>% 
  summarise(count = n())

ggplot(data=daily_death_age_group,aes(x=REG_DATE,y=count) )+
geom_point(position = position_jitter(width = 0.2))

# Plot time series of daily deaths by age group
daily_death_age_group %>%
  group_by(REG_DATE) %>%
  summarise(count = sum(count), .groups = "drop") %>%
  ggplot(aes(x = REG_DATE, y = count)) +
  geom_line() +
  scale_x_date(
  date_labels = "%b %Y",
  date_breaks = "3 months",
  limits = as.Date(c("2022-01-01", "2025-01-01")),
  expand = c(0, 0)
)+
  labs(title = "Daily All-Cause Mortality by Age Group",
       x = "Date",
       y = "Daily Death Count") +
  theme_minimal() +
  theme(
    base_size = 14,
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1))
