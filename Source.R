#' This document and all rights to this document belong to the Brock Wyma
#' The data provided for use in this document was collected by Google about the spread of the COVID- 19 virus. The data is available on the Google Cloud Platform: https://console.cloud.google.com/marketplace/browse?filter=solution-%20type:dataset&filter=category:covid19
#' The data is provided in three files. 
#' 
#' 

# Libraries to be used in the document
library("tidyverse")
library("ggplot2")
library("ggrepel")
library("ggcorrplot")
library("DT")

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

