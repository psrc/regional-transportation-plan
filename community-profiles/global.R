library(data.table)
library(DT)
library(shiny)
library(shinyjs)
library(shinyBS)
library(ggplot2)
library(scales)
library(plotly)
library(leaflet)
library(sp)
library(rgdal)
library(raster)
library(rgeos)

wrkdir <- "C:/coding/regional-transportation-plan"

source(file.path(wrkdir,'community-profiles' , 'functions.R'))

##################################################################################
##################################################################################
### Loading CSV of Model outputs
##################################################################################
##################################################################################
model.outputs <- setDT(read.csv(file.path(wrkdir, 'output','combined_output_summary_data.csv')))
model.outputs$geog_name <- as.character(model.outputs$geog_name)
model.outputs$place_type <- as.character(model.outputs$place_type)

##################################################################################
##################################################################################
### Data for Clean Tables and Charts
##################################################################################
##################################################################################
modes_var_descr <- c("Walk","Bike","Drive-Alone","Carpool","Transit")
modes_var_name <- "commute-trips"
modes_cols <- c("variable_description","year","data_source")
modes_order <- c("Bike", "Walk", "Transit", "Carpool", "Drive-Alone")
modes_length <- 10
modes_name <- "Mode of Travel"

traveltime_var_descr <- c("less than 15 minutes","15 to 30 minutes","30 to 45 minutes","45 to 60 minutes", "more than 60 minutes")
traveltime_modes <- c("Walk","Drive-Alone","Transit")
traveltime_cols <- c("variable_description","year","data_source")
traveltime_order <- c("less than 15 minutes","15 to 30 minutes","30 to 45 minutes","45 to 60 minutes", "more than 60 minutes")
traveltime_length <- 10
traveltime_name <- "Travel Time"

hhsize_var_descr <- c("1 person","2 people","3 people","4 people","5 or more")
hhsize_var_name <- "household-size"
hhsize_cols <- c("variable_description","year","data_source")
hhsize_order <- c("5 or more", "4 people", "3 people", "2 people", "1 person")
hhsize_length <- 10
hhsize_name <- "Household Size"

hhownership_var_descr <- c("Own","Rent")
hhownership_var_name <- "household-ownership"
hhownership_cols <- c("variable_description","year","data_source")
hhownership_order <- c("Rent", "Own")
hhownership_length <- 5
hhownership_name <- "Type of Ownership"

hhtype_var_descr <- c("Single-Family","Multi-Family","Mobile-Home")
hhtype_var_name <- "household-type"
hhtype_cols <- c("variable_description","year","data_source")
hhtype_order <- c("Mobile-Home", "Multi-Family", "Single-Family")
hhtype_length <- 5
hhtype_name <- "Household Type"

hhincome_var_descr <- c("less than $25k","$25k to $50k","$50k to $75k","$75k to $100k","$100k to $150k","$150k to $200k","more than $200k")
hhincome_var_name <- "household-income"
hhincome_cols <- c("variable_description","year","data_source")
hhincome_order <- c("less than $25k","$25k to $50k","$50k to $75k","$75k to $100k","$100k to $150k","$150k to $200k","more than $200k")
hhincome_length <- 10
hhincome_name <- "Household Income"

jobs_var_descr <- c("Construction","Manufacturing","WTU","Retail","Services","Government","Education")
jobs_var_name <- "jobs"
jobs_cols <- c("variable_description","year","data_source")
jobs_order <- c("Construction","Manufacturing","WTU","Retail","Services","Government","Education")
jobs_length <- 15
jobs_name <- "Sector"

age_var_descr <- c("0-17","18-64","65-84","85+")
age_var_name <- "age"
age_cols <- c("variable_description","year","data_source")
age_order <- c("85+", "65-84", "18-64", "0-17")
age_length <- 5
age_name <- "Age"

gender_var_descr <- c("Male","Female")
gender_var_name <- "gender"
gender_cols <- c("variable_description","year","data_source")
gender_order <- c("Male","Female")
gender_length <- 5
gender_name <- "Gender"

worker_var_descr <- c("Worker","Not-a-Worker")
worker_var_name <- "worker"
worker_cols <- c("variable_description","year","data_source")
worker_order <- c("Worker","Not-a-Worker")
worker_length = 5
worker_name <- "Worker"

student_var_descr <- c("Student","Not-a-Student")
student_var_name <- "student"
student_cols <- c("variable_description","year","data_source")
student_order <- c("Student","Not-a-Student")
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
working_year <- model.outputs[data_source %in% c("UrbanSim","SoundCast")]

data_years <- as.character(unique(working_year$year))
data_places <- sort(unique(places$geog_name))

model_base_year <- min(working_year$year)
model_horizon_year <- max(working_year$year)

##################################################################################
##################################################################################
### PSRC Specific Colors for Data Items
##################################################################################
##################################################################################
# Color Pallete using PSRC Colors
psrc_colors <- c(
  "CoastRhodo" = "#91268F",
  "CedarShake" = "#F05A28",
  "DouglasFirShoot" = "#8CC63E",    
  "FerryWake" = "#00A7A0",
  "DarkGrey" = "#76787A",    
  "LightGrey" = "#BBBDC0",
  "Male" = "#F05A28",
  "Female" = "#8CC63E",
  "0-17" = "#91268F",
  "18-64" = "#F05A28",
  "65-84" = "#8CC63E",
  "85+" = "#00A7A0",
  "Worker" = "#8CC63E",
  "Not-a-Worker" = "#00A7A0",
  "Student" = "#91268F",
  "Not-a-Student" = "#8CC63E",
  "1 person" = "#91268F",
  "2 people" = "#F05A28",
  "3 people" = "#8CC63E",
  "4 people" = "#00A7A0",
  "5 or more" = "#76787A",
  "Own" = "#8CC63E",
  "Rent" = "#91268F",
  "Single-Family" = "#F05A28",
  "Multi-Family" = "#8CC63E",
  "Mobile-Home" = "#00A7A0",
  "less than $25k" = "#BBBDC0",
  "$25k to $50k" = "#91268F",
  "$50k to $75k" = "#F05A28",
  "$75k to $100k" = "#8CC63E",
  "$100k to $150k" = "#00A7A0",
  "$150k to $200k" = "#76787A",
  "more than $200k" = "#F05A28",
  "Construction" = "#BBBDC0",
  "Manufacturing" = "#91268F",
  "WTU" = "#91268F",
  "Retail" = "#F05A28",
  "Services" = "#8CC63E",
  "Government" = "#00A7A0",
  "Education" = "#76787A",
  "Walk" = "#91268F",
  "Bike" = "#F05A28",
  "Drive-Alone" = "#76787A",
  "Carpool" = "#8CC63E",
  "Transit" = "#00A7A0",
  "less than 15 minutes" = "#BBBDC0",
  "15 to 30 minutes" = "#91268F",
  "30 to 45 minutes" = "#F05A28",
  "45 to 60 minutes" = "#8CC63E", 
  "more than 60 minutes" = "#00A7A0"
)