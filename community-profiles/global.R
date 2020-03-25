library(odbc)
library(DBI)
library(data.table)
library(stringr)
library(DT)
library(shiny)
library(shinyjs)
library(shinyBS)
library(ggplot2)
library(scales)
library(plotly)
library(foreign)
library(leaflet)
library(sp)
library(rgdal)
library(raster)
library(dplyr)
library(rgeos)
library(lubridate)
library(RODBC)

wrkdir <- "C:/coding/regional-transportation-plan/community-profiles"

source(file.path(wrkdir, 'functions.R'))

##################################################################################
##################################################################################
### Loading CSV of Model outputs
##################################################################################
##################################################################################
model.outputs <- setDT(read.csv(file.path(wrkdir, 'combined_model_output_summary_data.csv')))
model.outputs$geog_name <- as.character(model.outputs$geog_name)
model.outputs$place_type <- as.character(model.outputs$place_type)

##################################################################################
##################################################################################
### Data for Clean Tables and Charts
##################################################################################
##################################################################################
modes <- c("Walk","Bike","Drive-Alone","Carpool","Transit")
modes_cols <- c("variable_mode","estimate","year")
modes_order <- c("Bike", "Walk", "Transit", "Carpool", "Drive-Alone")
modes_length <- 10
mode_name <- "Mode of Travel"

traveltime <- c("less than 15 minutes","15 to 30 minutes","30 to 45 minutes","45 to 60 minutes","60 to 90 minutes", "more than 90 minutes")
traveltime_cols <- c("variable_description","estimate","year")
traveltime_order <- c("less than 15 minutes","15 to 30 minutes","30 to 45 minutes","45 to 60 minutes","60 to 90 minutes", "more than 90 minutes")
traveltime_length <- 10
traveltime_name <- "Travel Time"

hhsize <- c("1 person","2 people","3 people","4 people","5 or more")
hhsize_cols <- c("variable_description","estimate","year")
hhsize_order <- c("5 or more", "4 people", "3 people", "2 people", "1 person")
hhsize_length <- 10
hhsize_name <- "Household Size"

hhownership <- c("Own","Rent")
hhownership_cols <- c("variable_description","estimate","year")
hhownership_order <- c("Rent", "Own")
hhownership_length <- 5
hhownership_name <- "Type of Ownership"

hhtype <- c("Single-family","Multi-family","Condo","Mobile-home")
hhtype_cols <- c("variable_description","estimate","year")
hhtype_order <- c("Mobile-home", "Condo", "Multi-family", "Single-family")
hhtype_length <- 5
hhtype_name <- "Household Type"

hhincome <- c("less than $25k","$25k to $50k","$50k to $75k","$75k to $100k","$100k to $150k","$150k to $200k","$200k to $250k","more than $250k")
hhincome_cols <- c("variable_description","estimate","year")
hhincome_order <- c("less than $25k","$25k to $50k","$50k to $75k","$75k to $100k","$100k to $150k","$150k to $200k","$200k to $250k","more than $250k")
hhincome_length <- 10
hhincome_name <- "Household Income"

jobs <- c("Education","Food-Service","Government","Industrial","Medical","Office","Retail","Resource","Service","Others")
jobs_cols <- c("variable_description","estimate","year")
jobs_order <- c("Education","Food-Service","Government","Industrial","Medical","Office","Retail","Resource","Service","Others")
jobs_length <- 15
jobs_name <- "Job Type"

age <- c("0-17","18-64","65-84","85+")
age_cols <- c("variable_description","estimate","year")
age_order <- c("85+", "65-84", "18-64", "0-17")
age_length <- 5
age_name <- "Age"

gender <- c("Male","Female")
gender_cols <- c("variable_description","estimate","year")
gender_order <- c("Male","Female")
gender_length <- 5
gender_name <- "Gender"

worker <- c("Full-Time-Worker","Part-Time-Worker","Not-a-Worker")
worker_cols <- c("variable_description","estimate","year")
worker_order <- c("Full-Time-Worker","Part-Time-Worker","Not-a-Worker")
worker_length = 5
worker_name <- "Worker"

student <- c("Full-Time-Student","Part-Time-Student","Not-a-Student")
student_cols <- c("variable_description","estimate","year")
student_order <- c("Full-Time-Student","Part-Time-Student","Not-a-Student")
student_length = 5
student_name <- "Student"

##################################################################################
##################################################################################
### Shapefiles
##################################################################################
##################################################################################
community.shape <- readOGR(dsn='C:/coding/regional-transportation-plan/inputs/shapefiles/wgs1984/no_water',layer='places_no_water_revised',stringsAsFactors = FALSE)
community.shape$ZOOM <- as.integer(community.shape$ZOOM)
community.point <- setDT(community.shape@data)

tract.shape <- readOGR(dsn='C:/coding/regional-transportation-plan/inputs/shapefiles/wgs1984/no_water',layer='tracts',stringsAsFactors = FALSE)

##################################################################################
##################################################################################
### Dropdown Data creation
##################################################################################
##################################################################################
places <- model.outputs[place_type %in% c("pl","co", "re")]

data_years <- as.character(unique(model.outputs$year))
data_places <- sort(unique(places$geog_name))

min_year <- min(data_years)
max_year <- max(data_years)
