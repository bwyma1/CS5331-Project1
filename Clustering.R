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

# Get Texas state and county map data
states <- map_data("state")
tx_df <- subset(states, region == "texas")

counties <- map_data("county")
tx_county_df <- subset(counties, region == "texas") %>%
  mutate(
    subregion = str_replace_all(subregion, "de witt", "dewitt")
  )
 
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
  group_by(county, ownership) %>%
  summarise(Avg_Beds_Per_Facility = mean(beds, na.rm = TRUE)) 

# 3. Merge the map data with facility and bed counts
tx_facility_map <- tx_county_df %>%
  left_join(facility_per_county, by = "county")

tx_bed_map <- tx_county_df %>%
  left_join(avg_beds_per_facility, by = "county")

# standardized for county map
census_cleaned$county <- tolower(census_cleaned$county) # Convert to lowercase
census_cleaned$county <- gsub(" county$", "", census_cleaned$county)

death_per_county <- census_cleaned %>% group_by(county) %>% select(deaths_per_1000)
death_map <- tx_county_df %>% left_join(death_per_county, by = "county")

population_density <- census_cleaned %>%
  group_by(county) %>%select(population)


population_density_map <- tx_county_df %>%
  left_join(population_density, by = "county") 

county_area <- read_csv("COVID-19/TX_Counties.csv")
county_area$NAME <- tolower(county_area$NAME) # Convert to lowercase
county_area$NAME <- gsub(" county$", "", county_area$NAME)
# Rename the "NAME" column to "county"
county_area <- county_area %>%
  rename(county = NAME) %>%
  rename(area = ALAND_SQMI) %>%
  select(county, area)  

population_density_map <- population_density_map %>%
  left_join(county_area, by = "county")

population_density_map <- population_density_map %>%
  mutate(population_density = population / area)


#'
#'
#' Clustering Analysis
#' 
#'

#'
#'
#' Creating Clustering Groups
#'
#'

## Scale function between (-3,3) mostly
scale_numeric <- function(x) {
  x |> mutate(across(where(is.numeric), 
                     function(y) (y - mean(y, na.rm = TRUE)) / sd(y, na.rm = TRUE)))
}


cluster_group_1 <- census_cleaned %>%
  left_join(population_density_map, by = 'county') %>%
  distinct(county, .keep_all = TRUE) %>%
  select(age_under_21, age_22_to_64, age_65_and_over, population_density) |>
  scale_numeric()

cluster_group_2 <- census_cleaned %>%
  left_join(population_density_map, by = 'county') %>%
  distinct(county, .keep_all = TRUE) %>%
  select(commute_public_transportation, commute_alone, population_density) |>
  scale_numeric()

cluster_group_3 <- acute_facility_cleaned %>% 
  mutate(
    county = str_replace_all(county, "de witt", "dewitt")
  ) %>%
  left_join(population_density_map, by = 'county') %>%
  select(ownership, beds, population_density) %>% 
  distinct() |>
  scale_numeric()
  
cluster_group_4 <- census_cleaned %>%
  left_join(avg_beds_per_facility, by = 'county') %>%
  left_join(facility_per_county, by = 'county') %>%
  select(age_65_and_over, Avg_Beds_Per_Facility, Facility_Count) %>%
  mutate_all(~ replace_na(., 0)) |>
  scale_numeric()

#'
#'
#' Creating clusters using k-means, k-modes, hierarchical clustering
#'
#'

pkgs <- c("cluster", "dbscan", "e1071", "factoextra", "fpc", 
          "GGally", "kernlab", "mclust", "mlbench", "scatterpie", 
          "seriation", "tidyverse")

pkgs_install <- pkgs[!(pkgs %in% installed.packages()[,"Package"])]
if(length(pkgs_install)) install.packages(pkgs_install)

# K-Mean
cluster_1 <- do.call(rbind, cluster_group_1)
sum(is.na(cluster_1))
sum(is.nan(cluster_1))
sum(is.infinite(cluster_1))
which(is.na(cluster_1))


cluster_2 <- do.call(rbind, cluster_group_2)
sum(is.na(cluster_2))
sum(is.nan(cluster_2))
sum(is.infinite(cluster_2))
which(is.na(cluster_2))


cluster_3 <- do.call(rbind, cluster_group_3)
sum(is.na(cluster_3))
sum(is.nan(cluster_3))
sum(is.infinite(cluster_3))
which(is.na(cluster_3))
summary(cluster_group_3)

cluster_4 <- do.call(rbind, cluster_group_4)
sum(is.na(cluster_4))
sum(is.nan(cluster_4))
sum(is.infinite(cluster_4))
which(is.na(cluster_4))



km_cluster_1 <- kmeans(cluster_1, centers = 3, nstart = 10)
km_cluster_1

ggplot(cluster_group_1, aes(x = age_under_21, y = age_22_to_64, color = factor(km_cluster_1$cluster))) +
  geom_point() +
  geom_text_repel(aes(label = county), size = 2) +
  labs(title = "K-Means Clustering of Age Groups", x = "Age Under 21", y = "Age 22 to 64") +
  theme_minimal()




