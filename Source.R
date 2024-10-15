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

# 'Reading in the data
cases <- read_csv("COVID-19/COVID-19_cases_plus_census.csv")
str(cases)
cases_tx <- read_csv("COVID-19/COVID-19_cases_TX.csv")
str(cases_tx)
mobility_report <- read_csv("COVID-19/Global_Mobility_Report.csv")
str(mobility_report)

# 'Cleaning cases data
cases <- as_tibble(cases)
selected_columns <- c('poverty', 'walked_to_work', 'worked_at_home', 'total_pop', 
                      'median_age', 'median_income', 'income_per_capita',
                      'family_households', 'households', 'nonfamily_households', 
                      'confirmed_cases', 'deaths', 'state', 'county_name')
cases <- cases %>% select(all_of(selected_columns))

# 'Cleaning Texas cases data
cases_tx <- as_tibble(cases_tx)
selected_columns <- c('county_name', 'date', 'confirmed_cases', 'deaths')
cases_tx <- cases_tx %>% select(all_of(selected_columns))

# 'Cleaning mobility report
mobility_report <- as_tibble(mobility_report)
selected_columns <- c('country_region', 'date', 
                      'retail_and_recreation_percent_change_from_baseline', 
                      'grocery_and_pharmacy_percent_change_from_baseline',
                      'parks_percent_change_from_baseline',
                      'transit_stations_percent_change_from_baseline',
                      'workplace_percent_change_from_baseline',
                      'residential_percent_change_from_baseline')
mobility_report <- mobility_report %>% select(all_of(selected_columns))

# ' 
# Checking out the mobility report for the United States
mobility_report_us <- mobility_report %>% filter(country_region_code == "US")
mobility_report_us_no_region <- mobility_report_us %>% filter(is.na(sub_region_1))

# Removing Weekends since people don't normally work on weekends 
mobility_report_us_no_region <-  mobility_report_us_no_region %>%
  filter(!(weekdays(date) %in% c("Saturday", "Sunday")))

# Plotting line chart for entire US with date and 
ggplot(mobility_report_us_no_region) +
  geom_line(aes(x = date, y = workplaces_percent_change_from_baseline, color = "Workplace")) +
  geom_smooth(aes(x = date, y = workplaces_percent_change_from_baseline, color = "Workplace"), se = FALSE) +
  geom_line(aes(x = date, y = parks_percent_change_from_baseline, color = "Parks")) +
  geom_smooth(aes(x = date, y = parks_percent_change_from_baseline, color = "Parks"), se = FALSE) +
  geom_line(aes(x = date, y = grocery_and_pharmacy_percent_change_from_baseline, color = "Grocery/Pharmacy")) +
  geom_smooth(aes(x = date, y = grocery_and_pharmacy_percent_change_from_baseline, color = "Grocery/Pharmacy"), se = FALSE) +
  labs(title = "Percent Change From Baseline", x = "Date", y = "% Change", color = "Legend") +
  theme_minimal()
  

