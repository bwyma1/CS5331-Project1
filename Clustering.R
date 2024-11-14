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

#_______________________________
# Assuming you've already executed the following to prepare cluster_group_1:
cluster_group_1 <- census_cleaned %>%
  left_join(population_density_map, by = 'county') %>%
  distinct(county, .keep_all = TRUE) %>%
  select(age_under_21, age_22_to_64, age_65_and_over, population_density) %>%
  scale_numeric()

# Step 1: Standardize the data (calculate Z-scores)
z_scores <- scale(cluster_group_1)

# Step 2: Identify outliers (Z-scores greater than 3 or less than -3)
outliers_z <- abs(z_scores) > 3  # Create a logical matrix for outliers

# Step 3: Find the row indices of the outliers
outlier_indices <- which(rowSums(outliers_z) > 0)  # Rows where there are any outliers

# Step 4: Select the 4 rows with the highest Z-scores (highest outliers)
outliers_z_scores <- z_scores[outlier_indices, ]
outlier_max_z_scores <- apply(outliers_z_scores, 1, function(x) max(abs(x)))  # Max Z-score in each row
top_4_outliers_idx <- order(outlier_max_z_scores, decreasing = TRUE)[1:4]  # Get the indices of the 4 highest outliers

# Step 5: Extract the 4 outliers
top_4_outliers <- cluster_group_1[outlier_indices[top_4_outliers_idx], ]

# Step 6: Remove the top 4 outliers from the dataset
cluster_group_1_no_outliers <- cluster_group_1[-outlier_indices[top_4_outliers_idx], ]

# Display the new dataset without the top 4 outliers
cluster_group_1 <-cluster_group_1_no_outliers

#______________
# Assuming you've already executed the following to prepare cluster_group_2:
cluster_group_2 <- census_cleaned %>%
  left_join(population_density_map, by = 'county') %>%
  distinct(county, .keep_all = TRUE) %>%
  select(commute_public_transportation, commute_alone, population_density) %>%
  scale_numeric()

# Step 1: Standardize the data (calculate Z-scores) for cluster_group_2
z_scores_2 <- scale(cluster_group_2)

# Step 2: Identify outliers (Z-scores greater than 3 or less than -3)
outliers_z_2 <- abs(z_scores_2) > 3  # Create a logical matrix for outliers

# Step 3: Find the row indices of the outliers
outlier_indices_2 <- which(rowSums(outliers_z_2) > 0)  # Rows where there are any outliers

# Step 4: Select the 4 rows with the highest Z-scores (highest outliers)
outliers_z_scores_2 <- z_scores_2[outlier_indices_2, ]
outlier_max_z_scores_2 <- apply(outliers_z_scores_2, 1, function(x) max(abs(x)))  # Max Z-score in each row
top_4_outliers_idx_2 <- order(outlier_max_z_scores_2, decreasing = TRUE)[1:4]  # Get the indices of the 4 highest outliers

# Step 5: Extract the 4 outliers
top_4_outliers_2 <- cluster_group_2[outlier_indices_2[top_4_outliers_idx_2], ]

# Step 6: Remove the top 4 outliers from the dataset
cluster_group_2_no_outliers <- cluster_group_2[-outlier_indices_2[top_4_outliers_idx_2], ]

# Display the new dataset without the top 4 outliers
cluster_group_2 <- cluster_group_2_no_outliers



cluster_group_3 <- acute_facility_cleaned %>% 
  mutate(
    county = str_replace_all(county, "de witt", "dewitt")
  ) %>%
  left_join(population_density_map, by = 'county') %>%
  select(ownership, beds, population_density) %>% 
  distinct() |>
  scale_numeric()

#_________________________
# Prepare cluster_group_4 by joining data and selecting relevant columns
cluster_group_4 <- census_cleaned %>%
  left_join(avg_beds_per_facility, by = 'county') %>%
  left_join(facility_per_county, by = 'county') %>%
  select(age_65_and_over, Avg_Beds_Per_Facility, Facility_Count) %>%
  scale_numeric() %>%
  mutate(across(everything(), ~ replace_na(., 0)))  # Replace NA values with 0

# Step 1: Standardize the data (calculate Z-scores) for cluster_group_4
z_scores_4 <- scale(cluster_group_4)

# Step 2: Identify outliers (Z-scores greater than 3 or less than -3)
outliers_z_4 <- abs(z_scores_4) > 3  # Create a logical matrix for outliers

# Step 3: Find the row indices of the outliers
outlier_indices_4 <- which(rowSums(outliers_z_4) > 0)  # Rows where there are any outliers

# Step 4: Select the 4 rows with the highest Z-scores (highest outliers)
outliers_z_scores_4 <- z_scores_4[outlier_indices_4, ]
outlier_max_z_scores_4 <- apply(outliers_z_scores_4, 1, function(x) max(abs(x)))  # Max Z-score in each row
top_4_outliers_idx_4 <- order(outlier_max_z_scores_4, decreasing = TRUE)[1:4]  # Get the indices of the 4 highest outliers

# Step 5: Extract the 4 outliers
top_4_outliers_4 <- cluster_group_4[outlier_indices_4[top_4_outliers_idx_4], ]

# Step 6: Remove the top 4 outliers from the dataset
cluster_group_4_no_outliers <- cluster_group_4[-outlier_indices_4[top_4_outliers_idx_4], ]

# Display the new dataset without the top 4 outliers
cluster_group_4 <- cluster_group_4_no_outliers


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
#
# Initialize lists to store averaged clustering models and statistics for cluster group 1
avg_wss_values_1 <- numeric(14)  # To store average WSS for each k from 2 to 15
avg_silhouette_values_1 <- numeric(14)  # To store average silhouette width for each k from 2 to 15
avg_dunn_values_1 <- numeric(14)  # To store average Dunn Index for each k from 2 to 15

# Loop through cluster counts from 2 to 15 for cluster group 1
for (k in 2:15) {
  # Initialize vectors to store WSS, silhouette, and Dunn Index values for each of the 10 runs
  wss_runs <- numeric(10)
  silhouette_runs <- numeric(10)
  dunn_runs <- numeric(10)  # Store Dunn Index for each run
  
  for (i in 1:10) {
    # Run k-means clustering with k clusters for cluster group 1
    km_model <- kmeans(cluster_group_1, centers = k, nstart = 10)
    
    # Calculate the distance matrix
    d <- dist(cluster_group_1)
    
    # Calculate clustering statistics using fpc package without loading it
    stats <- fpc::cluster.stats(d, as.integer(km_model$cluster))
    
    # Store the WSS, silhouette width, and Dunn Index for this run
    wss_runs[i] <- km_model$tot.withinss
    silhouette_runs[i] <- stats$avg.silwidth
    dunn_runs[i] <- stats$dunn
  }
  
  # Calculate and store the average WSS, silhouette width, and Dunn Index for this k
  avg_wss_values_1[k - 1] <- mean(wss_runs)
  avg_silhouette_values_1[k - 1] <- mean(silhouette_runs)
  avg_dunn_values_1[k - 1] <- mean(dunn_runs)
}

# Plot the average WSS to find the elbow point for cluster group 1
plot(2:15, avg_wss_values_1, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (k) Group 1", ylab = "Average Total WSS",
     main = "WSS for Optimal k (Averaged over 10 runs)")

# Plot the average silhouette width to find the optimal k for cluster group 1
plot(2:15, avg_silhouette_values_1, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (k) Group 1", ylab = "Average Silhouette Width",
     main = "Silhouette Method for Optimal k (Averaged over 10 runs)")

# Plot the average Dunn Index to assess cluster separation for optimal k for cluster group 1
plot(2:15, avg_dunn_values_1, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (k) Group 1", ylab = "Average Dunn Index",
     main = "Dunn Index for Optimal k (Averaged over 10 runs)")


#
# Perform K-means cluster 2
#
#
# Initialize lists to store averaged clustering models and statistics for cluster group 2
avg_wss_values_2 <- numeric(14)  # To store average WSS for each k from 2 to 15
avg_silhouette_values_2 <- numeric(14)  # To store average silhouette width for each k from 2 to 15
avg_dunn_values_2 <- numeric(14)  # To store average Dunn Index for each k from 2 to 15

# Loop through cluster counts from 2 to 15 for cluster group 2
for (k in 2:15) {
  # Initialize vectors to store WSS, silhouette, and Dunn Index values for each of the 10 runs
  wss_runs <- numeric(10)
  silhouette_runs <- numeric(10)
  dunn_runs <- numeric(10)  # Store Dunn Index for each run
  
  for (i in 1:10) {
    # Run k-means clustering with k clusters for cluster group 2
    km_model <- kmeans(cluster_group_2, centers = k, nstart = 10)
    
    # Calculate the distance matrix
    d <- dist(cluster_group_2)
    
    # Calculate clustering statistics using fpc package without loading it
    stats <- fpc::cluster.stats(d, as.integer(km_model$cluster))
    
    # Store the WSS, silhouette width, and Dunn Index for this run
    wss_runs[i] <- km_model$tot.withinss
    silhouette_runs[i] <- stats$avg.silwidth
    dunn_runs[i] <- stats$dunn
  }
  
  # Calculate and store the average WSS, silhouette width, and Dunn Index for this k
  avg_wss_values_2[k - 1] <- mean(wss_runs)
  avg_silhouette_values_2[k - 1] <- mean(silhouette_runs)
  avg_dunn_values_2[k - 1] <- mean(dunn_runs)
}

# Plot the average WSS to find the elbow point for cluster group 2
plot(2:15, avg_wss_values_2, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (k) Group 2", ylab = "Average Total WSS",
     main = "WSS for Optimal k (Averaged over 10 runs)")

# Plot the average silhouette width to find the optimal k for cluster group 2
plot(2:15, avg_silhouette_values_2, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (k) Group 2", ylab = "Average Silhouette Width",
     main = "Silhouette Method for Optimal k (Averaged over 10 runs)")

# Plot the average Dunn Index to assess cluster separation for optimal k for cluster group 2
plot(2:15, avg_dunn_values_2, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (k) Group 2", ylab = "Average Dunn Index",
     main = "Dunn Index for Optimal k (Averaged over 10 runs)")



# Perform K-means cluster 3
# Oh, it's Nothing !!!!!
# Because there is categorical data

#
# Perform K-means cluster 4
#
#
# Initialize lists to store averaged clustering models and statistics for cluster group 4
avg_wss_values_4 <- numeric(14)  # To store average WSS for each k from 2 to 15
avg_silhouette_values_4 <- numeric(14)  # To store average silhouette width for each k from 2 to 15
avg_dunn_values_4 <- numeric(14)  # To store average Dunn Index for each k from 2 to 15

# Loop through cluster counts from 2 to 15 for cluster group 4
for (k in 2:15) {
  # Initialize vectors to store WSS, silhouette, and Dunn Index values for each of the 10 runs
  wss_runs <- numeric(10)
  silhouette_runs <- numeric(10)
  dunn_runs <- numeric(10)  # Store Dunn Index for each run
  
  for (i in 1:10) {
    # Run k-means clustering with k clusters for cluster group 4
    km_model <- kmeans(cluster_group_4, centers = k, nstart = 10)
    
    # Calculate the distance matrix
    d <- dist(cluster_group_4)
    
    # Calculate clustering statistics using fpc package without loading it
    stats <- fpc::cluster.stats(d, as.integer(km_model$cluster))
    
    # Store the WSS, silhouette width, and Dunn Index for this run
    wss_runs[i] <- km_model$tot.withinss
    silhouette_runs[i] <- stats$avg.silwidth
    dunn_runs[i] <- stats$dunn
  }
  
  # Calculate and store the average WSS, silhouette width, and Dunn Index for this k
  avg_wss_values_4[k - 1] <- mean(wss_runs)
  avg_silhouette_values_4[k - 1] <- mean(silhouette_runs)
  avg_dunn_values_4[k - 1] <- mean(dunn_runs)
}

# Plot the average WSS to find the elbow point for cluster group 4
plot(2:15, avg_wss_values_4, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (k) Group 4", ylab = "Average Total WSS",
     main = "WSS for Optimal k (Averaged over 10 runs)")

# Plot the average silhouette width to find the optimal k for cluster group 4
plot(2:15, avg_silhouette_values_4, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (k) Group 4", ylab = "Average Silhouette Width",
     main = "Silhouette Method for Optimal k (Averaged over 10 runs)")

# Plot the average Dunn Index to assess cluster separation for optimal k for cluster group 4
plot(2:15, avg_dunn_values_4, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (k) Group 4", ylab = "Average Dunn Index",
     main = "Dunn Index for Optimal k (Averaged over 10 runs)")

# Plotting the optimized K-mean for 1, 2, and 4

km_model_1 <- kmeans(cluster_group_1, centers = 4, nstart = 10)

d <- dist(cluster_group_1)
# Calculate clustering statistics using fpc package without loading it
stats <- fpc::cluster.stats(d, as.integer(km_model_1$cluster))

# Convert the cluster centers to a tibble and reshape for plotting
cluster_centers <- as_tibble(km_model_1$centers, rownames = "cluster") %>%
  pivot_longer(cols = colnames(km_model_1$centers), names_to = "feature", values_to = "z_score")

# Plot the cluster profiles
ggplot(cluster_centers, aes(y = feature, x = z_score, fill = cluster)) +
  geom_bar(stat = "identity") +
  facet_grid(cols = vars(cluster)) +
  labs(y = "Feature", x = "Z-scores", title = "Cluster Profiles for km_model_1") + 
  guides(fill = "none")

# Apply k-means clustering to cluster_group_2 
km_model_2 <- kmeans(cluster_group_2, centers = 4, nstart = 10)

d2 <- dist(cluster_group_2)
# Calculate clustering statistics using fpc package without loading it
stats2 <- fpc::cluster.stats(d, as.integer(km_model_2$cluster))

# Convert cluster centers for km_model_2 to tibble and reshape for plotting
cluster_centers_2 <- as_tibble(km_model_2$centers, rownames = "cluster") %>%
  pivot_longer(cols = colnames(km_model_2$centers), names_to = "feature", values_to = "z_score")

# Plot for km_model_2
ggplot(cluster_centers_2, aes(y = feature, x = z_score, fill = cluster)) +
  geom_bar(stat = "identity") +
  facet_grid(cols = vars(cluster)) +
  labs(y = "Feature", x = "Z-scores", title = "Cluster Profiles for km_model_2") + 
  guides(fill = "none")

# Apply k-means clustering to cluster_group_4
km_model_4 <- kmeans(cluster_group_4, centers = 4, nstart = 10)
d4 <- dist(cluster_group_4)  # Compute distance matrix for cluster_group_4

# Calculate clustering statistics for km_model_4 using fpc package
stats4 <- fpc::cluster.stats(d4, as.integer(km_model_4$cluster))

# Convert cluster centers for km_model_4 to tibble and reshape for plotting
cluster_centers_4 <- as_tibble(km_model_4$centers, rownames = "cluster") %>%
  pivot_longer(cols = colnames(km_model_4$centers), names_to = "feature", values_to = "z_score")

# Plot for km_model_4
ggplot(cluster_centers_4, aes(y = feature, x = z_score, fill = cluster)) +
  geom_bar(stat = "identity") +
  facet_grid(cols = vars(cluster)) +
  labs(y = "Feature", x = "Z-scores", title = "Cluster Profiles for km_model_4") + 
  guides(fill = "none")



#'
#'
#' k-mode
#'
#'
#'
# Load necessary libraries

# Ensure 'ownership' column is a factor (categorical data)
cluster_group_3_categorical <- cluster_group_3
cluster_group_3_categorical$ownership <- as.factor(cluster_group_3_categorical$ownership)

# Initialize lists to store clustering models and Dunn Index values
avg_dunn_index_values_3 <- numeric(14)  # To store average Dunn index for each k from 2 to 15

# Loop through cluster counts from 2 to 15 for cluster group 3
for (k in 2:15) {
  # Initialize vector to store Dunn index values for each of the 10 runs
  dunn_index_runs <- numeric(10)
  
  for (i in 1:10) {
    # Run K-modes clustering with k clusters
    km_model <- kmodes(cluster_group_3_categorical[, "ownership", drop = FALSE], modes = k, iter.max = 10)
    
    # Calculate the Dunn index directly from the cluster assignments
    dist_matrix <- dist(cluster_group_3_categorical[, "ownership", drop = FALSE])  # Calculate pairwise distance matrix
    dunn_index_runs[i] <- cluster.stats(dist_matrix, as.integer(km_model$cluster))$dunn
  }
  
  # Calculate and store the average Dunn index for this k
  avg_dunn_index_values_3[k - 1] <- mean(dunn_index_runs)
}

# Plot the average Dunn index to find the optimal k for cluster group 3
plot(2:15, avg_dunn_index_values_3, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (k) Group 3", ylab = "Average Dunn Index",
     main = "Dunn Index for Optimal k (Averaged over 10 runs for K-modes)")




#'
#'
#' hierarchical clustering
#'
#'
#'
install.packages("factoextra")
library(factoextra)

dist_model_1<- dist(cluster_group_1)
hc_model_1 <- hclust(dist_model_1, method = "complete")
fviz_dend(hc_model_1 )

dist_model_2<- dist(cluster_group_2)
hc_model_2 <- hclust(dist_model_2, method = "complete")
fviz_dend(hc_model_2)

dist_model_4<- dist(cluster_group_4)
hc_model_4 <- hclust(dist_model_4, method = "complete")
fviz_dend(hc_model_4)












