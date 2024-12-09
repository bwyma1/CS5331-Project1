# 'Libraries to be used in the document
library("tidyverse")
library("ggplot2")
library("ggrepel")
library("ggcorrplot")
library("DT")
library(stringr)
library(dplyr)

cases_tx <- read_csv("COVID-19/COVID-19_cases_TX.csv")

#'
#'
#' Reading Hospital Data
#'
#'

hospital_data <- read_csv("COVID-19/2020_MedicalFacilities.csv")

hospital_data <- hospital_data %>%
  rename(county_name = County,
         hospital_beds = Acute) %>%
  mutate(county_name = str_to_title(county_name),
         county_name = str_c(county_name, " County"),
         county_name = as.factor(county_name)) %>%
  select(county_name, hospital_beds) %>%
  filter(!is.na(hospital_beds)) %>%
  group_by(county_name) %>%
  summarise(hospital_beds = sum(hospital_beds))

#'
#'
#' Reading Mobility Data
#'
#'

mobility <- read_csv("COVID-19/Global_Mobility_Report.csv")

mobility <- mobility %>%
  mutate_if(is.character, factor) %>%
  rename(state = sub_region_1,
         county_name = sub_region_2,
         retail_recreation = retail_and_recreation_percent_change_from_baseline,
         grocery_pharmacy = grocery_and_pharmacy_percent_change_from_baseline,
         parks = parks_percent_change_from_baseline,
         transit_stations = transit_stations_percent_change_from_baseline,
         workplace = workplaces_percent_change_from_baseline,
         residential = residential_percent_change_from_baseline) %>%
  filter(state == 'Texas') %>%
  select(state, county_name, retail_recreation, grocery_pharmacy, parks, transit_stations, workplace, residential)

mobility_avg <- mobility %>%
  group_by(county_name) %>%
  summarise(retail_recreation_avg = mean(retail_recreation, na.rm = TRUE),
            grocery_pharmacy_avg = mean(grocery_pharmacy, na.rm = TRUE),
            parks_avg = mean(parks, na.rm = TRUE),
            transit_stations_avg = mean(transit_stations, na.rm = TRUE),
            workplace_avg = mean(workplace, na.rm = TRUE),
            residential_avg = mean(residential, na.rm = TRUE)) %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .)))

#'
#'
#' Reading Census Cases Data
#'
#'

cases <- read_csv("COVID-19/COVID-19_cases_plus_census.csv")
cases <- cases %>% mutate_if(is.character, factor)

cases <- cases %>% filter(confirmed_cases > 0) 

cases <- cases %>%
  filter(state == 'TX')

cases <- cases %>% 
  arrange(desc(confirmed_cases)) #%>%    
#select(county_name, state, confirmed_cases, deaths, total_pop, median_income, median_age)
cases <- cases %>% mutate(
  cases_per_10000 = confirmed_cases/total_pop*10000, 
  deaths_per_10000 = deaths/total_pop*10000)

age_under_21 <- cases %>% mutate(age_under_21 = rowSums(across(
  c('male_under_5':'male_21', 'female_under_5':'female_21')))) %>%
  select(age_under_21)
age_22_to_64 <- cases %>% mutate(age_22_to_64 = rowSums(across(
  c('male_22_to_24':'male_62_64', 'female_22_to_24':'female_62_to_64')))) %>%
  select(age_22_to_64)
age_65_and_over <- cases %>% mutate(age_65_and_over = rowSums(across(
  c('male_65_to_66':'male_85_and_over', 'female_65_to_66':'female_85_and_over')))) %>%
  select(age_65_and_over)
cases <- cases %>% 
  mutate(age_under_21 = age_under_21$age_under_21) %>%
  mutate(age_22_to_64 = age_22_to_64$age_22_to_64) %>% 
  mutate(age_65_and_over = age_65_and_over$age_65_and_over)

commute_public_transportation <- cases %>% mutate(public_transportation = rowSums(across(
  c('commuters_by_bus':'commuters_by_subway_or_elevated')))) %>%
  select(public_transportation)
cases <- cases %>%
  mutate(commute_public_transportation = commute_public_transportation$public_transportation,
         commute_alone = cases$commuters_drove_alone)

cases_sel <- cases %>% select(county_name, total_pop,
                              commute_public_transportation,
                              commute_alone, age_under_21, age_22_to_64,
                              age_65_and_over,
                              cases_per_10000, deaths_per_10000)

# normalize by population 
cases_sel <- cases_sel %>% mutate(
  commute_public_transportation = commute_public_transportation / total_pop, 
  commute_alone = commute_alone / total_pop,
  age_under_21 = age_under_21 / total_pop, 
  age_22_to_64 = age_22_to_64 / total_pop, 
  age_65_and_over = age_65_and_over / total_pop, 
)

library(seriation)
cm <- cor(cases_sel %>% select_if(is.numeric) %>% na.omit)
hmap(cm, margins = c(14,14))

#'
#'
#' Creating Classes
#'
#'

cases_sel <- cases_sel %>% mutate(bad_deaths = as.factor(deaths_per_10000 > 10))
cases_sel %>% pull(bad_deaths) %>% table()

cases_sel <- cases_sel %>% mutate(bad_cases = as.factor(cases_per_10000 > 750))
cases_sel %>% pull(bad_cases) %>% table()


#'
#'
#' Combining Data into One Data Set
#'
#'

cases_sel <- full_join(cases_sel, mobility_avg, by = "county_name") %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0))) 

cases_sel <- cases_sel %>%
  filter(!is.na(county_name)) 

cases_sel <- full_join(cases_sel, hospital_data, by = "county_name") %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0)),
         hospital_beds = hospital_beds / total_pop *10000) 

cases_sel <- cases_sel %>%
  filter(total_pop != 0)

cases_sel <- cases_sel %>%
  select(-cases_per_10000, -deaths_per_10000)

summary(cases_sel)

#'
#'
#' Splitting into training and testing data sets
#'
#'
pkgs <- c("basemodels", "C50", "caret", "e1071", "klaR", 
          "lattice", "MASS", "mlbench", "nnet", "palmerpenguins", 
          "randomForest", "rpart", "RWeka", "scales", "tidyverse", 
          "xgboost", "rpart.plot", "sampling")

pkgs_install <- pkgs[!(pkgs %in% installed.packages()[,"Package"])]
if(length(pkgs_install)) install.packages(pkgs_install)

library(caret)

cases_sel_without_deaths <- cases_sel %>%
  select(-bad_deaths)

inTrain <- createDataPartition(y = cases_sel_without_deaths$bad_cases, p = 0.8, list = FALSE)
training_cases <- cases_sel_without_deaths[inTrain, ]
testing_cases <- cases_sel_without_deaths[-inTrain, ]

cases_sel_without_cases <- cases_sel %>%
  select(-bad_cases)

inTrain <- createDataPartition(y = cases_sel_without_cases$bad_deaths, p = 0.8, list = FALSE)
training_deaths <- cases_sel_without_cases[inTrain, ]
testing_deaths <- cases_sel_without_cases[-inTrain, ]


# Map TRUE/FALSE in bad_deaths to 1/0
cases_class <- cases_sel %>%
  mutate(
    bad_deaths = ifelse(bad_deaths == TRUE, 1, 0),
    bad_cases = ifelse(bad_cases == TRUE, 1, 0)
  )


cases_class_without_deaths <- cases_class %>%
  select(-bad_deaths)

inTrain <- createDataPartition(y = cases_class_without_deaths$bad_cases, p = 0.8, list = FALSE)
training_cases_ohe <- cases_class_without_deaths[inTrain, ]
testing_cases_ohe <- cases_class_without_deaths[-inTrain, ]

cases_class_without_cases <- cases_class %>%
  select(-bad_cases)

inTrain <- createDataPartition(y = cases_class_without_cases$bad_deaths, p = 0.8, list = FALSE)
training_deaths_ohe <- cases_class_without_cases[inTrain, ]
testing_deaths_ohe <- cases_class_without_cases[-inTrain, ]

cases_formula_ohe <- as.formula(paste("bad_cases ~ . - county_name"))
deaths_formula_ohe <- as.formula(paste("bad_deaths ~ . - county_name"))

#'
#'
#' Rule-Based Classifier
#'
#'

# Load required libraries
library(caret)
library(RWeka) # For PART implementation

# Ensure the target variable is a factor
training_cases_ohe$bad_cases <- as.factor(training_cases_ohe$bad_cases)

caseFit <- training_cases_ohe |> train(cases_formula_ohe,
                               method = "PART",
                               data = _,
                               tuneLength = 5,
                               trControl = trainControl(method = "cv"))
caseFit 

caseFit$finalModel

#  Use the Data As Is and Hope For The Best
# Ensure the target variable for deaths is a factor
training_deaths_ohe$bad_deaths <- as.factor(training_deaths_ohe$bad_deaths)

# Train the model for deaths classification
deathFit <-  train(deaths_formula_ohe, 
                  method = "PART", 
                  data = training_deaths_ohe,
                  tuneLength = 5, 
                  trControl = trainControl(method = "cv"))

# Check the model results
deathFit 


# Up sampling
# Train the model for deaths classification
deathFit_up_samp <- train(deaths_formula_ohe, 
                  method = "PART", 
                  data = training_deaths_ohe,
                  tuneLength = 5, 
                  trControl = trainControl(method = "cv", sampling  = "up"))

# Check the model results
deathFit_up_samp

# Resampling 

# Build A Larger Tree and use Predicted Probabilities


# Check the model results
deathFit_pred_tree


# Access the final trained model (rules learned by PART)
deathFit$finalModel

#'
#'
#' Decision Tree Classifier
#'
#'
library(xgboost)
# Random Forest cases
randomForestFit_cases <- training_cases_ohe |> train(cases_formula_ohe,
                                      method = "rf",
                                      data = _,
                                      tuneLength = 5,
                                      trControl = trainControl(method = "cv"))
randomForestFit_cases

# Random Forest deaths
randomForestFit_deaths <- training_deaths_ohe |> train(deaths_formula_ohe,
                                                     method = "rf",
                                                     data = _,
                                                     tuneLength = 5,
                                                     trControl = trainControl(method = "cv"))
randomForestFit_deaths

# Gradient Boosted Decision Trees
xgboostFit_cases  <- training_cases_ohe |> train(cases_formula_ohe,
                                        method = "xgbTree",
                                        data = _,
                                        tuneLength = 5,
                                        trControl = trainControl(method = "cv"),
                                        tuneGrid = expand.grid(
                                          nrounds = 20,
                                          max_depth = 3,
                                          colsample_bytree = .6,
                                          eta = 0.1,
                                          gamma=0,
                                          min_child_weight = 1,
                                          subsample = .5
                                        ))
xgboostFit_cases

xgboostFit_deaths  <- training_deaths_ohe |> train(deaths_formula_ohe,
                                                 method = "xgbTree",
                                                 data = _,
                                                 tuneLength = 5,
                                                 trControl = trainControl(method = "cv"),
                                                 tuneGrid = expand.grid(
                                                   nrounds = 20,
                                                   max_depth = 3,
                                                   colsample_bytree = .6,
                                                   eta = 0.1,
                                                   gamma=0,
                                                   min_child_weight = 1,
                                                   subsample = .5
                                                 ))
xgboostFit_deaths


# Plot variable importance for cases
varImp_cases <- varImp(randomForestFit_cases, scale = TRUE)
plot(varImp_cases, top = 10, main = "Variable Importance - Random Forest (Cases)")

# Plot variable importance for deaths
varImp_deaths <- varImp(randomForestFit_deaths, scale = TRUE)
plot(varImp_deaths, top = 10, main = "Variable Importance - Random Forest (Deaths)")

# XGBoost variable importance for cases
xgb.importance.matrix_cases <- xgb.importance(model = xgboostFit_cases$finalModel)
xgb.plot.importance(xgb.importance.matrix_cases, main = "XGBoost Variable Importance (Cases)")

# XGBoost variable importance for deaths
xgb.importance.matrix_deaths <- xgb.importance(model = xgboostFit_deaths$finalModel)
xgb.plot.importance(xgb.importance.matrix_deaths, main = "XGBoost Variable Importance (Deaths)")

# Plot tuning results for Random Forest cases
plot(randomForestFit_cases, main = "Tuning Results - Random Forest (Cases)")

# Plot tuning results for Random Forest deaths
plot(randomForestFit_deaths, main = "Tuning Results - Random Forest (Deaths)")

# Plot tuning results for XGBoost cases
plot(xgboostFit_cases, main = "Tuning Results - XGBoost (Cases)")

# Plot tuning results for XGBoost deaths
plot(xgboostFit_deaths, main = "Tuning Results - XGBoost (Deaths)")


#'
#'
#' SVM
#'
#'


#'
#'
#' Naive Bayes Classifier
#'
#'


#'
#'
#' Artificial Neural Network
#'
#'


