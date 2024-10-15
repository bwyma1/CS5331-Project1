#' This document and all rights to this document belong to the Brock Wyma
#' The data provided for use in this document was collected by Google about the spread of the COVID- 19 virus. The data is available on the Google Cloud Platform: https://console.cloud.google.com/marketplace/browse?filter=solution-%20type:dataset&filter=category:covid19
#' The data is provided in three files. 
#' 
#' 

# 'Libraries to be used in the document
library("tidyverse")
library("ggplot2")
library("ggrepel")
library("ggcorrplot")
library("DT")
library(dplyr)

# Reading in the data
census <- read_csv("COVID-19/COVID-19_cases_plus_census.csv")
census <- census[census$state == 'TX',] #Subset census to only Texas

mobility <- read_csv("COVID-19/Global_Mobility_Report.csv")
mobility <- mobility[mobility$sub_region_1 == 'Texas',]
mobility <- mobility %>% drop_na(country_region_code) #Subset mobility report to only Texas and drop empty rows

cases_TX <- read_csv("COVID-19/COVID-19_cases_TX.csv")
cases_TX <- cases_TX[cases_TX$date >= "2020-03-05",] #March 5th 2020 is the First Case 
str(cases_TX)

counties <- read_csv("COVID-19/TX_Counties.csv")

# 'Cleaning cases data
cases <- as_tibble(cases)
selected_columns <- c('county_name', 'confirmed_cases', 'deaths', 'total_pop')

cases_cleaned <- cases %>% select(all_of(selected_columns))
cases_cleaned <- cases_cleaned %>%
  rename(population = total_pop) %>%
  rename(county = county_name) %>%
  rename(cases = confirmed_cases) %>%
  mutate(county = as.factor(county)) 
age_under_21 <- cases %>% mutate(age_under_21 = rowSums(across(
  c('male_under_5':'male_21', 'female_under_5':'female_21')))) %>%
  select(age_under_21)
age_22_to_34 <- cases %>% mutate(age_22_to_34 = rowSums(across(
  c('male_22_to_24':'male_30_to_34', 'female_22_to_24':'female_30_to_34')))) %>%
  select(age_22_to_34)
age_35_to_64 <- cases %>% mutate(age_35_to_64 = rowSums(across(
  c('male_35_to_39':'male_62_64', 'female_35_to_39':'female_62_to_64')))) %>%
  select(age_35_to_64)
age_85_and_over <- cases %>% mutate(age_85_and_over = rowSums(across(
  c('male_65_to_66':'male_85_and_over', 'female_65_to_66':'female_85_and_over')))) %>%
  select(age_85_and_over)
cases_cleaned <- cases_cleaned %>% 
  mutate(age_under_21 = age_under_21$age_under_21) %>%
  mutate(age_22_to_34 = age_22_to_34$age_22_to_34) %>% 
  mutate(age_35_to_64 = age_35_to_64$age_35_to_64) %>%
  mutate(age_85_and_over = age_85_and_over$age_85_and_over)

cases_cleaned <- cases_cleaned %>% 
  mutate(age_under_21 = age_under_21/population) %>%
  mutate(age_22_to_34 = age_22_to_34/population) %>% 
  mutate(age_35_to_64 = age_35_to_64/population) %>%
  mutate(age_85_and_over = age_85_and_over/population)

# 'Cleaning Texas cases data
cases_tx <- as_tibble(cases_tx)
selected_columns <- c('county_name', 'date', 'confirmed_cases', 'deaths')
cases_tx <- cases_tx %>% select(all_of(selected_columns))


# 'Cleaning mobility report
mobility <- as_tibble(mobility)
selected_columns <- c('sub_region_2', 'date', 
                      'retail_and_recreation_percent_change_from_baseline', 
                      'grocery_and_pharmacy_percent_change_from_baseline',
                      'parks_percent_change_from_baseline',
                      'transit_stations_percent_change_from_baseline',
                      'workplaces_percent_change_from_baseline',
                      'residential_percent_change_from_baseline')
mobility <- mobility %>% select(all_of(selected_columns))
mobility <- mobility %>% 
  rename(county = sub_region_2)

# ' 
# Checking out the mobility report for the Texas Counties
mobility <- mobility %>% drop_na(county)

# Removing Weekends since people don't normally work on weekends 
mobility <-  mobility %>%
  filter(!(weekdays(date) %in% c("Saturday", "Sunday")))

# Plotting line chart for entire US with date and 
ggplot(mobility) +
  geom_line(aes(x = date, y = workplaces_percent_change_from_baseline, color = "Workplace")) +
  geom_smooth(aes(x = date, y = workplaces_percent_change_from_baseline, color = "Workplace"), se = FALSE) +
  geom_line(aes(x = date, y = parks_percent_change_from_baseline, color = "Parks")) +
  geom_smooth(aes(x = date, y = parks_percent_change_from_baseline, color = "Parks"), se = FALSE) +
  geom_line(aes(x = date, y = grocery_and_pharmacy_percent_change_from_baseline, color = "Grocery/Pharmacy")) +
  geom_smooth(aes(x = date, y = grocery_and_pharmacy_percent_change_from_baseline, color = "Grocery/Pharmacy"), se = FALSE) +
  labs(title = "Percent Change From Baseline", x = "Date", y = "% Change", color = "Legend") +
  theme_minimal()
  
=======

