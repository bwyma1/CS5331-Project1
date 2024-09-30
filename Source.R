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
cases <- read_csv("COVID-19/COVID-19_cases_plus_census.csv")

