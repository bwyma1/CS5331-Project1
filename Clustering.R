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
          "seriation", "tidyverse", "plotly")

pkgs_install <- pkgs[!(pkgs %in% installed.packages()[,"Package"])]
if(length(pkgs_install)) install.packages(pkgs_install)

# Pre Cluster Visualization 

library(plotly)

## Cluster 1
# Create 3D scatter plot with updated axes
fig1 <- plot_ly(cluster_group_1, 
               x = ~age_under_21, 
               y = ~age_65_and_over,  # Switch population_density with age_65_and_over
               z = ~age_22_to_64, 
               color = ~population_density,  # Color points based on population_density
               size = ~population_density,  # Size points based on population_density
               sizes = c(10, 50))  # Adjust the size range of points

# Customize axes labels
fig1 <- fig1 %>% layout(scene = list(
  xaxis = list(title = 'Age Under 21'),
  yaxis = list(title = 'Age 65 and Over'),
  zaxis = list(title = 'Age 22-64')
))

fig1 # Display the plot

## Cluster 2
fig2 <- plot_ly(cluster_group_2,
                x = ~commute_public_transportation,
                y = ~commute_alone, 
                z = ~population_density,
                color = ~population_density,  # Color points based on population_density
                size = ~population_density,   # Size points based on population_density
                sizes = c(10, 50))             # Adjust the size range of points

# Customize axes labels and set the range between -1 and 1
fig2 <- fig2 %>% layout(scene = list(
  xaxis = list(title = 'Commute Public Transportation'),  # Set x-axis range
  yaxis = list(title = 'Commute Alone'),  # Set y-axis range
  zaxis = list(title = 'Population Density')  # Set z-axis range
))

fig2

## Cluster 3
# Since ownership is categorical 
unique_ownership <- unique(cluster_group_3$ownership)# Grab unique values from the 'ownership' column
print(unique_ownership)

ggplot(cluster_group_3, aes(x = beds, y = population_density, color = ownership)) +
  geom_point() +  # Add points to the plot
  labs(title = "Beds vs Population Density by Ownership",
       x = "Beds",
       y = "Population Density") +
  theme_minimal()  # Optional: a clean theme for the plot


## Cluster 4
fig4 <- plot_ly(cluster_group_4, 
                x = ~age_65_and_over, 
                y = ~Avg_Beds_Per_Facility, 
                z = ~Facility_Count,
                color = ~Facility_Count,  # Color points based on Facility_Count
                size = ~Facility_Count,   # Size points based on Facility_Count
                sizes = c(10, 50))        # Adjust the size range of points

# Customize axes labels and set the range between -1 and 1
fig4 <- fig4 %>% layout(scene = list(
  xaxis = list(title = 'Age 65 and Over' ),  # Set x-axis range
  yaxis = list(title = 'Avg Beds Per Facility'),  # Set y-axis range
  zaxis = list(title = 'Facility Count')  # Set z-axis range
))

# Display the plot
fig4


#'
#'
#' k-means
#'
#'
#'

# Perform K-means cluster 1
#_____
# Initialize lists to store averaged clustering models and statistics
avg_wss_values <- numeric(9)  # To store average WSS for each k from 2 to 10
avg_silhouette_values <- numeric(9)  # To store average silhouette width for each k from 2 to 10

# Loop through cluster counts from 2 to 10
for (k in 2:10) {
  # Initialize vectors to store WSS and silhouette values for each of the 10 runs
  wss_runs <- numeric(10)
  silhouette_runs <- numeric(10)
  
  for (i in 1:10) {
    # Run k-means clustering with k clusters
    km_model <- kmeans(cluster_group_1, centers = k, nstart = 10)
    
    # Calculate the distance matrix
    d <- dist(cluster_group_1)
    
    # Calculate clustering statistics using the fpc package without loading it
    stats <- fpc::cluster.stats(d, as.integer(km_model$cluster))
    
    # Store the WSS and silhouette width for this run
    wss_runs[i] <- km_model$tot.withinss
    silhouette_runs[i] <- stats$avg.silwidth
  }
  
  # Calculate and store the average WSS and silhouette width for this k
  avg_wss_values[k - 1] <- mean(wss_runs)
  avg_silhouette_values[k - 1] <- mean(silhouette_runs)
}

# Plot the average WSS to find the elbow point
plot(2:10, avg_wss_values, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (k)", ylab = "Average Total WSS",
     main = "Elbow Method for Optimal k (Averaged over 10 runs)")

# Plot the average silhouette width to find the optimal k
plot(2:10, avg_silhouette_values, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (k)", ylab = "Average Silhouette Width",
     main = "Silhouette Method for Optimal k (Averaged over 10 runs)")
#____








km_cluster_1 <- kmeans(cluster_group_1, centers = 3, nstart = 10)
km_cluster_1

# Perform K-means cluster 2

km_cluster_2 <- kmeans(cluster_group_2, centers = 2, nstart = 10)
km_cluster_2

cluster_2_df <- as.data.frame(cluster_group_2)  # Convert to data frame for easy plotting
cluster_2_df$Cluster <- factor(km_cluster_2$cluster)  # Add cluster assignments as a factor

# Perform K-means cluster 3
cluster_3 <- do.call(rbind, cluster_group_3)
sum(is.na(cluster_3))
sum(is.nan(cluster_3))
sum(is.infinite(cluster_3))
which(is.na(cluster_3))
summary(cluster_group_3)

km_cluster_3 <- kmeans(cluster_3, centers = 2, nstart = 10)
km_cluster_3

cluster_3_df <- as.data.frame(cluster_3)  # Convert to data frame for easy plotting
cluster_3_df$Cluster <- factor(km_cluster_3$cluster)  # Add cluster assignments as a factor


# Perform K-means cluster 4

km_cluster_4 <- kmeans(cluster_group_4, centers = 2, nstart = 10)
km_cluster_4

cluster_4_df <- as.data.frame(cluster_group_4)  # Convert to data frame for easy plotting
cluster_4_df$Cluster <- factor(km_cluster_4$cluster)  # Add cluster assignments as a factor

#'
#'
#' k-mode
#'
#'
#'
# Install and load necessary packages
if (!require(klaR)) install.packages("klaR")
library(klaR)

# Perform K-mode clustering on cluster group 1
# Ensure that categorical variables are factored appropriately
cluster_1_mode <- cluster_group_1 %>%
  mutate(across(where(is.factor), as.character))  # Convert factors to characters

# Now apply k-mode clustering (centers = number of clusters)
km_mode_1 <- kmodes(cluster_1_mode, modes = 3)

# Add cluster assignments to the data
cluster_1_mode_df <- as.data.frame(cluster_1_mode)  # Convert to data frame for easy plotting
cluster_1_mode_df$Cluster <- factor(km_mode_1$cluster)  # Add cluster assignments as a factor

# Perform K-mode clustering on cluster group 2
cluster_2_mode <- cluster_group_2 %>%
  mutate(across(where(is.factor), as.character))  # Convert factors to characters

km_mode_2 <- kmodes(cluster_2_mode, modes = 2)

# Add cluster assignments to the data
cluster_2_mode_df <- as.data.frame(cluster_2_mode)
cluster_2_mode_df$Cluster <- factor(km_mode_2$cluster)

# Perform K-mode clustering on cluster group 3
cluster_3_mode <- cluster_group_3 %>%
  mutate(across(where(is.factor), as.character))  # Convert factors to characters

km_mode_3 <- kmodes(cluster_3_mode, modes = 2)

# Add cluster assignments to the data
cluster_3_mode_df <- as.data.frame(cluster_3_mode)
cluster_3_mode_df$Cluster <- factor(km_mode_3$cluster)

# Perform K-mode clustering on cluster group 4
cluster_4_mode <- cluster_group_4 %>%
  mutate(across(where(is.factor), as.character))  # Convert factors to characters

km_mode_4 <- kmodes(cluster_4_mode, modes = 2)

# Add cluster assignments to the data
cluster_4_mode_df <- as.data.frame(cluster_4_mode)
cluster_4_mode_df$Cluster <- factor(km_mode_4$cluster)

# View the k-mode clustering results
list(
  cluster_1_mode_df = cluster_1_mode_df,
  cluster_2_mode_df = cluster_2_mode_df,
  cluster_3_mode_df = cluster_3_mode_df,
  cluster_4_mode_df = cluster_4_mode_df
)

#'
#'
#' hierarchical clustering
#'
#'
#'
# Load necessary library
if (!require(dendextend)) install.packages("dendextend")
library(dendextend)

# Hierarchical Clustering on cluster group 1
# We can use scaled data for this step (as before, scale if necessary)
cluster_1_scaled <- scale_numeric(cluster_group_1)

# Compute the distance matrix using Euclidean distance
dist_cluster_1 <- dist(cluster_1_scaled)

# Perform hierarchical clustering using complete linkage method
hclust_cluster_1 <- hclust(dist_cluster_1, method = "complete")

# Plot the dendrogram
plot(hclust_cluster_1, main = "Hierarchical Clustering - Cluster Group 1", xlab = "", sub = "", cex = 0.8)

# Cut the dendrogram to form clusters (e.g., 3 clusters)
clusters_1 <- cutree(hclust_cluster_1, k = 3)

# Add cluster assignments to the data
cluster_1_hclust_df <- as.data.frame(cluster_1_scaled)  # Convert to data frame for easy plotting
cluster_1_hclust_df$Cluster <- factor(clusters_1)  # Add cluster assignments

# Perform Hierarchical Clustering on cluster group 2
cluster_2_scaled <- scale_numeric(cluster_group_2)
dist_cluster_2 <- dist(cluster_2_scaled)
hclust_cluster_2 <- hclust(dist_cluster_2, method = "complete")
plot(hclust_cluster_2, main = "Hierarchical Clustering - Cluster Group 2", xlab = "", sub = "", cex = 0.8)

clusters_2 <- cutree(hclust_cluster_2, k = 2)
cluster_2_hclust_df <- as.data.frame(cluster_2_scaled)
cluster_2_hclust_df$Cluster <- factor(clusters_2)

# Perform Hierarchical Clustering on cluster group 3
cluster_3_scaled <- scale_numeric(cluster_group_3)
dist_cluster_3 <- dist(cluster_3_scaled)
hclust_cluster_3 <- hclust(dist_cluster_3, method = "complete")
plot(hclust_cluster_3, main = "Hierarchical Clustering - Cluster Group 3", xlab = "", sub = "", cex = 0.8)

clusters_3 <- cutree(hclust_cluster_3, k = 2)
cluster_3_hclust_df <- as.data.frame(cluster_3_scaled)
cluster_3_hclust_df$Cluster <- factor(clusters_3)

# Perform Hierarchical Clustering on cluster group 4
cluster_4_scaled <- scale_numeric(cluster_group_4)
dist_cluster_4 <- dist(cluster_4_scaled)
hclust_cluster_4 <- hclust(dist_cluster_4, method = "complete")
plot(hclust_cluster_4, main = "Hierarchical Clustering - Cluster Group 4", xlab = "", sub = "", cex = 0.8)

clusters_4 <- cutree(hclust_cluster_4, k = 2)
cluster_4_hclust_df <- as.data.frame(cluster_4_scaled)
cluster_4_hclust_df$Cluster <- factor(clusters_4)

# View the hierarchical clustering results
list(
  cluster_1_hclust_df = cluster_1_hclust_df,
  cluster_2_hclust_df = cluster_2_hclust_df,
  cluster_3_hclust_df = cluster_3_hclust_df,
  cluster_4_hclust_df = cluster_4_hclust_df
)










