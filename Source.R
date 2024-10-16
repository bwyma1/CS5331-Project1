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


# Carson's 
census_select <- census_cleaned %>% select(county, cases_per_1000, deaths_per_1000, commute_public_transportation, cases, deaths, population)
cor_TX <- cor(census_select[,-1])
ggcorrplot(cor_TX, p.mat = cor_pmat(census_select[,-1]), insig = "blank", hc.order = TRUE)

ggplot(census_cleaned, mapping = aes(x = age_65_and_over / population, y = deaths_per_1000, size = population)) + 
  geom_point() +
  geom_smooth(method = lm) +
  theme(legend.position="none") +
  labs(x = "Percentage of People 65 and Over", y = "Deaths per 1000", size = "total population")

ggplot(census_cleaned, mapping = aes(x = age_22_to_64 / population, y = deaths_per_1000, size = population)) + 
  geom_point() +
  geom_smooth(method = lm) +
  theme(legend.position="none") +
  labs(x = "Percentage of People Between 22 and 64", y = "Deaths per 1000", size = "total population")

ggplot(census_cleaned, mapping = aes(x = age_under_21 / population, y = deaths_per_1000, size = population)) + 
  geom_point() +
  geom_smooth(method = lm) +
  theme(legend.position="none") +
  labs(x = "Percentage of People Under 21", y = "Deaths per 1000", size = "total population")



# David's stuff DO NOT CHANGE______________
# Load required libraries
library(ggplot2)
library(dplyr)
library(maps)

# Get Texas state and county map data
states <- map_data("state")
tx_df <- subset(states, region == "texas")

counties <- map_data("county")
tx_county_df <- subset(counties, region == "texas")

# Ensure county names match between map data and your cleaned data
# Extract just the county name from the "subregion" column in the map data
tx_county_df$county <- tolower(tx_county_df$subregion)  # Standardize names

acute_facility_cleaned$county <- tolower(acute_facility_cleaned$county)  # Convert to lowercase
acute_facility_cleaned$county <- gsub(" county$", "", acute_facility_cleaned$county)  # Remove " county" at the end

# 1. Facility count per county
facility_per_county <- acute_facility_cleaned %>%
  group_by(county) %>%
  summarise(Facility_Count = n())

# 2. Total beds per county
bed_count_per_county <- acute_facility_cleaned %>%
  group_by(county) %>%
  summarise(Total_Beds = sum(beds, na.rm = TRUE))

# 2.5 Average beds per facility per county
avg_beds_per_facility <- acute_facility_cleaned %>%
  group_by(county) %>%
  summarise(Avg_Beds_Per_Facility = mean(beds, na.rm = TRUE))

# 3. Merge the map data with facility and bed counts
tx_facility_map <- tx_county_df %>%
  left_join(facility_per_county, by = "county")

tx_bed_map <- tx_county_df %>%
  left_join(avg_beds_per_facility, by = "county")

# 4. Plot Heatmap for Facility Count
ggplot(tx_facility_map, aes(x = long, y = lat, group = group, fill = Facility_Count)) +
  geom_polygon(color = "grey", size = 0.2) +  # Lighter color and thinner border
  theme_void() +  # Minimal theme
  scale_fill_gradientn(colors = c("yellow", "blue"), na.value = "lightgrey", name = "Acute Facility Count") +  # Smooth color transition
  labs(title = "Texas: Acute Facility per County",
       subtitle = "Year: 2020", 
       caption = "Source: Texas Health Data:healthdata.dshs.texas.gov"
       )

# 5. Plot Heatmap for Average Beds per Facility per county
ggplot(tx_bed_map, aes(x = long, y = lat, group = group, fill = Avg_Beds_Per_Facility)) +
  geom_polygon(color = "grey", size = 0.2) +  # Lighter color and thinner border
  theme_void() +  # Minimal theme
  scale_fill_gradientn(colors = c("blue", "yellow"), na.value = "lightgrey", name = "Available Bed Space") +  # Smooth color transition
  labs(title = "Texas: Average Bedspace per Facility per County",
       subtitle = "Year: 2020", 
       caption = "Source: Texas Health Data:healthdata.dshs.texas.gov")


# standardized for county map
census_cleaned$county <- tolower(census_cleaned$county) # Convert to lowercase
census_cleaned$county <- gsub(" county$", "", census_cleaned$county)

death_per_county <- census_cleaned %>% group_by(county) %>% select(deaths_per_1000)
death_map <- tx_county_df %>% left_join(death_per_county, by = "county")

ggplot(death_map, aes(x = long, y = lat, group = group, fill = deaths_per_1000)) +
  geom_polygon(color = "white", size = 0.2) +  # Lighter color and thinner border
  theme_void() +  # Minimal theme
  scale_fill_gradientn(
    colors = c("blue","yellow"), 
    na.value = "lightgrey", 
    name = "Death per 1,000"
  ) + 
  labs(
    title = "Texas: Deaths per 1,000 by County", 
    subtitle = "Year: 2020", 
    caption = "Source: Census Data"
  )

population_density <- census_cleaned %>%
  group_by(county) %>%select(population)

population_density_map <- tx_county_df %>%
  left_join(population_density, by = "county")

ggplot(population_density_map, aes(x = long, y = lat, group = group, fill = population)) +
  geom_polygon(color = "grey", size = 0.2) +  # Lighter color and thinner border
  theme_void() +  # Minimal theme
  scale_fill_gradientn(colours = rev(rainbow(7)),
                       breaks = c(100, 1,000, 10,000,100,000, 1,000,000, 10,000,000),
                       trans = "log10",
                       name = "Population",
                       labels = function(x) format(x, big.mark = ",", scientific = FALSE))+ 
  labs(title = "Texas: Population by County",
       subtitle = "Year: 2020",
       caption = "Source: Census"
       )

# time series graph____

mobility_cleaned$county <- tolower(mobility_cleaned$county) # Convert to lowercase
mobility_cleaned$county <- gsub(" county$", "", mobility_cleaned$county)

# Replace NA values with 0
mobility_cleaned <- mobility_cleaned %>%
  mutate(retail_and_recreation_percent_change_from_baseline = 
           replace_na(retail_and_recreation_percent_change_from_baseline, 0))

# Summarize the data by date
mobility_time <- mobility_cleaned %>%
  group_by(date) %>%
  summarize(
    retail_and_recreation_avg = mean(retail_and_recreation_percent_change_from_baseline, na.rm = TRUE),
    grocery_and_pharmacy_avg = mean(grocery_and_pharmacy_percent_change_from_baseline, na.rm = TRUE),
    parks_avg = mean(parks_percent_change_from_baseline, na.rm = TRUE),
    transit_stations_avg = mean(transit_stations_percent_change_from_baseline, na.rm = TRUE),
    workplaces_avg = mean(workplaces_percent_change_from_baseline, na.rm = TRUE),
    residential_avg = mean(residential_percent_change_from_baseline, na.rm = TRUE)
  )

# Define a plotting function for individual time series
plot_mobility <- function(data, y, title) {
  ggplot(data, aes(x = date, y = !!sym(y))) +  # Use !!sym() for dynamic column name
    geom_line(color = "blue") +
    geom_smooth(color = "lightpink", size = 0.75, alpha = 0.5)+
    labs(title = title, x = "Date", y = "% Change from Baseline") +
    theme_minimal()
}

# Generate the individual plots
p1 <- plot_mobility(mobility_time, "retail_and_recreation_avg", "Retail & Recreation: % Change Over Time")
p2 <- plot_mobility(mobility_time, "grocery_and_pharmacy_avg", "Grocery & Pharmacy: % Change Over Time")
p3 <- plot_mobility(mobility_time, "parks_avg", "Parks: % Change Over Time")
p4 <- plot_mobility(mobility_time, "transit_stations_avg", "Transit Stations: % Change Over Time")
p5 <- plot_mobility(mobility_time, "workplaces_avg", "Workplaces: % Change Over Time")
p6 <- plot_mobility(mobility_time, "residential_avg", "Residential: % Change Over Time")

print(p1)
print(p2)
print(p3)
print(p4)
print(p5)
print(p6)

# cases time series 
cases_time <- cases_TX_cleaned %>%
  group_by(date) %>%
  summarize(
    total_case = sum(cases, na.rm = TRUE),
    total_death = sum(deaths, na.rm = TRUE)
  )

ggplot(cases_time, aes(x = date)) +
  geom_line(aes(y = total_case, color = "Total Cases"), size = 1) +
  labs(
    title = "Daily Cases and Deaths in Texas",
    x = "Date", y = "Count",
    color = "Metric"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

ggplot(cases_time, aes(x = date, y = total_case)) +
  geom_line(color = "red", size = 1) +
  geom_smooth(color = "blue", size = 0.75, alpha = 0.5) +
  labs(
    title = "Daily Cases in Texas",
    x = "Date", y = "Total Cases"
  ) +
  theme_minimal()

ggplot(cases_time, aes(x = date, y = total_death)) +
  geom_line(color = "red", size = 1) +
  geom_smooth(color = "blue", size = 0.75, alpha = 0.5) +
  labs(
    title = "Daily Deaths in Texas",
    x = "Date", y = "Total Deaths"
  ) +
  theme_minimal()



