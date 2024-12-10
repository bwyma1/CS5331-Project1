# 'Libraries to be used in the document
library("tidyverse")
library("ggplot2")
library("ggrepel")
library("ggcorrplot")
library("DT")
library(stringr)
library(dplyr)
library(caret)
library(smotefamily)

set.seed(333)

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
         hospital_beds = hospital_beds / total_pop*10000) 

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
          "xgboost")

pkgs_install <- pkgs[!(pkgs %in% installed.packages()[,"Package"])]
if(length(pkgs_install)) install.packages(pkgs_install)

library(caret)

cases_sel <- cases_sel %>%
  select(-county_name)
  
inTrain <- createDataPartition(y = cases_sel$bad_cases, p = 0.8, list = FALSE)
training_cases <- cases_sel[inTrain, ] %>%
  select(-bad_deaths)
testing_cases <- cases_sel[-inTrain, ] %>%
  select(-bad_deaths)

training_deaths <- cases_sel[inTrain, ] %>%
  select(-bad_cases)
testing_deaths <- cases_sel[-inTrain, ] %>%
  select(-bad_cases)

training_deaths_without_county <- training_deaths %>%
  select(-bad_deaths)
training_deaths <- SMOTE(training_deaths_without_county, training_deaths$bad_deaths, K=5, dup_size = (sum(training_deaths$bad_deaths == TRUE)/sum(training_deaths$bad_deaths == FALSE)))
training_deaths <- training_deaths$data %>%
  rename(bad_deaths = class)

cases_formula <- as.formula(paste("bad_cases ~ ."))
deaths_formula <- as.formula(paste("bad_deaths ~ ."))

#'
#'
#' Decision Tree Classifier
#'
#'


#'
#'
#' Rule-Based Classifier
#'
#'


#'
#'
#' SVM
#'
#'

svmFit1 <- training_cases |> train(cases_formula,
                                  method = "svmLinear",
                                  data = _,
                                  tuneLength = 5,
                                  trControl = trainControl(method = "cv"))
svmFit1

svmFit2 <- training_cases |> train(cases_formula,
                                 method = "svmPoly",
                                 data = _,
                                 tuneLength = 5,
                                 trControl = trainControl(method = "cv"))
svmFit2
plot(svmFit2)

svmFit3 <- training_cases |> train(cases_formula,
                                   method = "svmRadial",
                                   data = _,
                                   tuneLength = 5,
                                   trControl = trainControl(method = "cv"))
svmFit3
plot(svmFit3)

svmFit1$finalModel
svmFit2$finalModel
svmFit3$finalModel

svmFit1_deaths <- training_deaths |> train(deaths_formula,
                                   method = "svmLinear",
                                   data = _,
                                   tuneLength = 5,
                                   trControl = trainControl(method = "cv"))
svmFit1_deaths

svmFit2_deaths <- training_deaths |> train(deaths_formula,
                                   method = "svmPoly",
                                   data = _,
                                   tuneLength = 5,
                                   trControl = trainControl(method = "cv"))
svmFit2_deaths

svmFit3_deaths <- training_deaths |> train(deaths_formula,
                                           method = "svmRadial",
                                           data = _,
                                           tuneLength = 5,
                                           trControl = trainControl(method = "cv"))
svmFit3_deaths

svmFit1_deaths$finalModel
svmFit2_deaths$finalModel
svmFit3_deaths$finalModel

# Predicting on test data
predictions <- predict(svmFit1, newdata = testing_cases)

conf_matrix <- confusionMatrix(predictions, testing_cases$bad_cases)  
print(conf_matrix)

predictions <- predict(svmFit2, newdata = testing_cases)

conf_matrix <- confusionMatrix(predictions, testing_cases$bad_cases)  
print(conf_matrix)

predictions <- predict(svmFit2_deaths, newdata = testing_deaths)

conf_matrix <- confusionMatrix(predictions, testing_deaths$bad_deaths)  
print(conf_matrix)

#'
#'
#' Naive Bayes Classifier
#'
#'

NBFit <- train(x = as.data.frame(testing_cases[, -ncol(testing_cases)]), 
               y = pull(testing_cases, "bad_cases"),
               method = "nb",
               tuneGrid = data.frame(fL = c(.2, .5, 1, 5), 
                                     usekernel = TRUE, adjust = 1),
               trControl = trainControl(method = "cv"))
NBFit

NBFit_deaths <- train(x = as.data.frame(testing_deaths[, -ncol(testing_deaths)]), 
               y = pull(testing_deaths, "bad_deaths"),
               method = "nb",
               tuneGrid = data.frame(fL = c(.2, .5, 1, 5), 
                                     usekernel = TRUE, adjust = 1),
               trControl = trainControl(method = "cv"))
NBFit_deaths

NBFit$finalModel$apriori
NBFit_deaths$finalModel$apriori

# Predicting on test data
predictions <- predict(NBFit, newdata = testing_cases)

conf_matrix <- confusionMatrix(predictions, testing_cases$bad_cases)  
print(conf_matrix)

predictions <- predict(NBFit_deaths, newdata = testing_deaths)

conf_matrix <- confusionMatrix(predictions, testing_deaths$bad_deaths) 
print(conf_matrix)


#'
#'
#' Artificial Neural Network
#'
#'

ANNFit <- training_cases |> train(cases_formula,
                                   method = "nnet",
                                   data = _,
                                   tuneLength = 5,
                                   trControl = trainControl(method = "cv"),
                                   trace = FALSE)
ANNFit
plot(ANNFit)

ANNFit_deaths <- training_deaths |> train(deaths_formula,
                                  method = "nnet",
                                  data = _,
                                  tuneLength = 5,
                                  trControl = trainControl(method = "cv"),
                                  trace = FALSE)
ANNFit_deaths
plot(ANNFit_deaths)

ANNFit$finalModel
ANNFit_deaths$finalModel

# Predicting on test data
predictions <- predict(ANNFit, newdata = testing_cases)

conf_matrix <- confusionMatrix(predictions, testing_cases$bad_cases)  
print(conf_matrix)

predictions <- predict(ANNFit_deaths, newdata = testing_deaths)

conf_matrix <- confusionMatrix(predictions, testing_deaths$bad_deaths) 
print(conf_matrix)

