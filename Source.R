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
library(stringr)
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

facility <- read_csv("COVID-19/2020_MedicalFacilities.csv")
# Acute care 
acute_facility <- facility %>%
  filter(`License Type` == "Acute") %>%
  select(-`License Type`, -Psych)

# check for missing and duplication 
colSums(is.na(acute_facility))
sum(duplicated(acute_facility))


# 'Cleaning census data and adding variables
census <- as_tibble(census)
selected_columns <- c('county_name', 'confirmed_cases', 'deaths', 'total_pop')

census_cleaned <- census %>% select(all_of(selected_columns))
census_cleaned <- census_cleaned %>%
  rename(population = total_pop) %>%
  rename(county = county_name) %>%
  rename(cases = confirmed_cases) %>%
  mutate(percent_dead = deaths/cases,
         county = as.factor(county),
         cases_per_1000 = cases/population*1000,
         deaths_per_1000 = deaths/population*1000) 

age_under_21 <- census %>% mutate(age_under_21 = rowSums(across(
  c('male_under_5':'male_21', 'female_under_5':'female_21')))) %>%
  select(age_under_21)
age_22_to_64 <- census %>% mutate(age_22_to_64 = rowSums(across(
  c('male_22_to_24':'male_62_64', 'female_22_to_24':'female_62_to_64')))) %>%
  select(age_22_to_64)
age_65_and_over <- census %>% mutate(age_65_and_over = rowSums(across(
  c('male_65_to_66':'male_85_and_over', 'female_65_to_66':'female_85_and_over')))) %>%
  select(age_65_and_over)
census_cleaned <- census_cleaned %>% 
  mutate(age_under_21 = age_under_21$age_under_21) %>%
  mutate(age_22_to_64 = age_22_to_64$age_22_to_64) %>% 
  mutate(age_65_and_over = age_65_and_over$age_65_and_over)

commute_public_transportation <- census %>% mutate(public_transportation = rowSums(across(
  c('commuters_by_bus':'commuters_by_subway_or_elevated')))) %>%
  select(public_transportation)
census_cleaned <- census_cleaned %>%
  mutate(commute_public_transportation = commute_public_transportation$public_transportation,
         commute_alone = census$commuters_drove_alone)

# 'Looking into covid cases and deaths variables for cleaning

# Check for duplicates or missing values
colSums(is.na(census_cleaned))
sum(duplicated(census_cleaned))

# Create the box and whisker plot for deaths and cases
data_long <- census_cleaned %>%
  pivot_longer(cols = c('cases_per_1000','deaths_per_1000'), names_to = "Column", values_to = "Value")

ggplot(data_long, aes(x = Column, y = Value)) +
  geom_boxplot() +
  facet_wrap(~ Column, scales = "free") +
  labs(title = '',x = "Variable Name", y = "Values") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        strip.text = element_blank())

# 'Taking a look into age



# 'Cleaning Texas cases data
cases_TX <- as_tibble(cases_TX)
selected_columns <- c('county_name', 'date', 'confirmed_cases', 'deaths')
cases_TX_cleaned <- cases_TX %>% select(all_of(selected_columns))

cases_TX_cleaned <- cases_TX_cleaned %>%
  rename(county = county_name,
         cases = confirmed_cases) %>%
  filter(county != 'Statewide Unallocated') %>%
  mutate(county = as.factor(county)) 


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
mobility_cleaned <- mobility %>% 
  rename(county = sub_region_2) %>%
  mutate(county = as.factor(county))
mobility_cleaned <- mobility_cleaned %>% drop_na(county)


# 'Cleaning acute_facility data
acute_facility <- as_tibble(acute_facility)
selected_columns <- c('County', 'Facility', 'Ownership', 'Acute')
acute_facility <- acute_facility %>% select(all_of(selected_columns))
acute_facility_cleaned <- acute_facility %>%
  rename(county = County,
         facility = Facility,
         ownership = Ownership,
         beds = Acute) %>%
  mutate(county = str_to_title(county),
         county = str_c(county, " County"),
         county = as.factor(county),
         facility = as.factor(facility),
         ownership = as.factor(ownership))


# 'Removing unwanted environment variables
rm(age_22_to_64, age_65_and_over, age_under_21, data_long, facility, commute_public_transportation)

# '
# '
# 'Data Exploration

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