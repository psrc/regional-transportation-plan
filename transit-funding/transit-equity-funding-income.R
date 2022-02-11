# Packages for Data Wrangling
library(tidyverse)
library(here)
library(psrccensus)
library(tidyr)

# Packages for Maps
library(sf)
library(leaflet)

# Packages for Tables
library(scales)
library(kableExtra)
library(data.table)

# Lists and Variables -----------------------------------------------------
c.yr <- 2018

freq.threshold <- 4*12*2

# Lists
agency.lookup <- c("Unknown" = 0,
                   "King County Metro" = 1,
                   "Pierce Transit" = 2,
                   "Community Transit" = 3,
                   "Kitsap Transit" = 4,
                   "WSF" = 5,
                   "Sound Transit" = 6,
                   "Everett Transit" = 7)

agency.names <- enframe(agency.lookup)

psrc.colors <- c("People of Color" = "#91268F", "White" = "#8CC63E", "People of Lower Income" = "#91268F", "People of Higher Income" = "#8CC63E")

county.order <- c("King County", "Kitsap County", "Pierce County", "Snohomish County", "Region")

# Spatial Data

wgs84 <- 4326
spn <- 2285 

# Functions ---------------------------------------------------------------
create_tract_map<-function(tract.tbl, tract.lyr=tracts, tract.var, tract.color, c.title){
  
  tbl <- tract.tbl %>% select(GEOID, all_of(tract.var)) %>% rename(population=tract.var)
  c.layer <- left_join(tract.lyr,tbl, by = c("GEOID10"="GEOID")) %>% st_transform(wgs84)
  
  m.pal <- colorFactor(
    palette = c("white", tract.color),
    levels = c(0, 1))
  
  m <- leaflet() %>%
    addMapPane(name = "polygons", zIndex = 410) %>%
    addMapPane(name = "maplabels", zIndex = 500) %>% # higher zIndex rendered on top
    
    addProviderTiles("CartoDB.VoyagerNoLabels") %>%
    addProviderTiles("CartoDB.VoyagerOnlyLabels",
                     options = leafletOptions(pane = "maplabels"),
                     group = "Labels") %>%
    
    addPolygons(data=c.layer,
                fillOpacity = 0.75,
                fillColor = ~m.pal(population),
                opacity = 0.5,
                weight = 0.5,
                color = "#BCBEC0",
                group="Population",
                options = leafletOptions(pane = "polygons"),
                dashArray = "") %>%
    
    addLegend(pal = m.pal,
              values = c.layer$population,
              group = "Population",
              position = "bottomright",
              title = c.title) %>%
    
    setView(lng=-122.257, lat=47.615, zoom=8.25)
  
  return(m)
}

create_tract_map_stop<-function(tract.tbl, tract.lyr=tracts, tract.var, tract.color, c.title, stops.tbl=stops.layer, stops.color){
  
  tbl <- tract.tbl %>% select(GEOID, all_of(tract.var)) %>% rename(population=tract.var)
  c.layer <- left_join(tract.lyr,tbl, by = c("GEOID10"="GEOID")) %>% st_transform(wgs84)
  
  m.pal <- colorFactor(
    palette = c("white", tract.color),
    levels = c(0, 1))
  
  m <- leaflet() %>%
    addMapPane(name = "polygons", zIndex = 410) %>%
    addMapPane(name = "maplabels", zIndex = 500) %>% # higher zIndex rendered on top
    
    addProviderTiles("CartoDB.VoyagerNoLabels") %>%
    addProviderTiles("CartoDB.VoyagerOnlyLabels",
                     options = leafletOptions(pane = "maplabels"),
                     group = "Labels") %>%
    
    addPolygons(data=c.layer,
                fillOpacity = 0.25,
                fillColor = ~m.pal(population),
                opacity = 0.5,
                weight = 0.5,
                color = "#BCBEC0",
                group="Population",
                options = leafletOptions(pane = "polygons"),
                dashArray = "") %>%
    
    addCircles(data=stops.tbl,
               weight = 2, 
               radius = 24,
               fill = TRUE,
               opacity = 1,
               color = stops.color) %>%
    
    setView(lng=-122.257, lat=47.615, zoom=8.25)
  
  return(m)
}

create_bar_chart <- function(w.data, w.x, w.y, w.fill, w.palette=psrc.colors, w.bartype) {
  
  g <-  ggplot(data = w.data,
               aes(x = get(eval(w.x)),
                   y = get(eval(w.y)),
                   fill = get(eval(w.fill)),
                   group=1)) +
    geom_bar(position=w.bartype, stat="identity") +
    geom_text(aes(label=paste0(round(get(eval(w.y))*100,0),"%")), size = 4, position = position_stack(vjust = 0.5), colour="#FFFFFF", fontface = 'bold') +
    scale_y_continuous(labels = label_percent()) +
    scale_fill_manual(values = w.palette) +
    labs(x = NULL, y = NULL) +
    theme(plot.title = element_text(size = 10, face = 'bold'),
          axis.ticks.x = element_blank(),
          axis.line = element_blank(),
          panel.background = element_blank(),
          panel.grid.major.y = element_line(colour="#BBBDC0",size = 0.25),
          panel.grid.minor.y = element_line(colour="#BBBDC0",size = 0.25),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          legend.position = "bottom",
          legend.title = element_blank())
  
  return(g)
  
}


# Create Transit Trips ----------------------------------------------------
stops.data <- read_csv(here('data','gtfs','stops.txt')) %>% select(stop_id, stop_name, stop_lat, stop_lon)
stoptime.data <- read_csv(here('data','gtfs','stop_times.txt')) %>% select(stop_id, trip_id)
trip.data <- read_csv(here('data','gtfs','trips.txt')) %>% select(route_id, trip_id)
routes.data <- read_csv(here('data','gtfs','routes.csv')) %>% select(agency_id, route_id, route_type)

transit.trips <- left_join(stoptime.data, trip.data, by=c("trip_id"))
transit.trips <- left_join(transit.trips, routes.data, by=c("route_id")) %>% mutate(trip=1)

# Flag Stops by Geography Type --------------------------------------------
counties <- st_read(here('data','shapefiles','counties.shp')) %>% st_transform(spn) %>% filter(psrc==1) %>% select(county_nm)
tracts <- st_read("https://services6.arcgis.com/GWxg6t7KXELn1thE/arcgis/rest/services/tract2010_nowater/FeatureServer/0/query?where=0=0&outFields=*&f=pgeojson")

# Figure out the Tract Data
inc.vars <- c("002","003","004","005","006","007")

tract.income.data <- get_acs_recs('tract', years=c(2019), table.names = c('C17002'))
  
total.population <- tract.income.data %>% 
  separate(variable,into=c("table","variable"), "_") %>%
  filter(variable == "001") %>%
  mutate(county=substr(GEOID, 3, 5)) %>%
  rename(total=estimate) %>%
  select(GEOID,total, county)

low.income.population <- tract.income.data %>%
  separate(variable,into=c("table","variable"), "_") %>%
  filter(variable %in% inc.vars) %>%
  select(GEOID,estimate) %>%
  group_by(GEOID) %>%
  summarize(total = sum(estimate)) %>%
  rename(lowinc=total)

tract.by.income <- left_join(total.population, low.income.population,by=c("GEOID"))  

total.pop.region <- tract.by.income %>% select(total) %>% pull() %>% sum()
total.pop.king <- tract.by.income %>% filter(county=="033") %>% select(total) %>% pull() %>% sum()
total.pop.kitsap <- tract.by.income %>% filter(county=="035") %>% select(total) %>% pull() %>% sum()
total.pop.pierce <- tract.by.income %>% filter(county=="053") %>% select(total) %>% pull() %>% sum()
total.pop.snohomish <- tract.by.income %>% filter(county=="061") %>% select(total) %>% pull() %>% sum()

lowinc.pop.region <- tract.by.income %>% select(lowinc) %>% pull() %>% sum()
lowinc.pop.king <- tract.by.income %>% filter(county=="033") %>% select(lowinc) %>% pull() %>% sum()
lowinc.pop.kitsap <- tract.by.income %>% filter(county=="035") %>% select(lowinc) %>% pull() %>% sum()
lowinc.pop.pierce <- tract.by.income %>% filter(county=="053") %>% select(lowinc) %>% pull() %>% sum()
lowinc.pop.snohomish <- tract.by.income %>% filter(county=="061") %>% select(lowinc) %>% pull() %>% sum()

lowinc.share.region <- lowinc.pop.region / total.pop.region
lowinc.share.king <- lowinc.pop.king / total.pop.king
lowinc.share.kitsap <- lowinc.pop.kitsap / total.pop.kitsap
lowinc.share.pierce <- lowinc.pop.pierce / total.pop.pierce
lowinc.share.snohomish <- lowinc.pop.snohomish / total.pop.snohomish

tract.by.income <- tract.by.income %>%
  mutate(lowinc_percent = lowinc/total) %>%
  mutate(county_average = case_when(
    county == "033" ~ lowinc.share.king,
    county == "035" ~ lowinc.share.kitsap,
    county == "053" ~ lowinc.share.pierce,
    county == "061" ~ lowinc.share.snohomish)) %>%
  mutate(regional_average = lowinc.share.region) %>%
  mutate(above_regional = case_when(
    lowinc_percent >= regional_average ~ 1,
    lowinc_percent < regional_average ~ 0)) %>%
  mutate(above_county = case_when(
    lowinc_percent >= county_average ~ 1,
    lowinc_percent < county_average ~ 0)) %>%
  mutate(across(everything(), ~replace_na(.x, 0)))

# Join to Tract layer for Stop Analysis
temp <- tract.by.income %>% select(GEOID,lowinc_percent,above_regional,above_county)
tracts <- left_join(tracts,temp, by=c("GEOID10"="GEOID")) %>% st_transform(spn)

# Now join stops and trips
stops.layer = st_as_sf(stops.data, coords = c("stop_lon", "stop_lat"), crs = wgs84) %>% st_transform(spn) %>% mutate(temp=stop_id) %>% separate(temp, c("agency", "route"), "_")
stops.county <- st_intersection(stops.layer, counties) %>% st_drop_geometry() %>% select(stop_id,county_nm)

temp <- st_intersection(stops.layer, tracts) %>% st_drop_geometry() %>% select(stop_id,above_regional,above_county)
transit.trips <- left_join(transit.trips, temp, by=c("stop_id"))
transit.trips <- left_join(transit.trips, agency.names, by=c("agency_id"="value")) %>% rename(agency_name=name)
transit.trips <- left_join(transit.trips, stops.county, by=c("stop_id"))

# Define Frequent Transit -------------------------------------------------
frequent.routes <- transit.trips %>% select(trip_id, route_id) %>% distinct(trip_id, route_id) %>% mutate(trip=1) %>% 
  select(-trip_id) %>% group_by(route_id) %>% summarize(trips = sum(trip)) %>% filter(trips>=freq.threshold) %>%
  mutate(frequent=1) %>% select(-trips)

transit.trips <- left_join(transit.trips, frequent.routes, by=c("route_id")) %>% mutate(frequent = replace_na(frequent, 0))

frequent.stops <- transit.trips %>% filter(frequent==1) %>% select(stop_id) %>% distinct() %>% mutate(frequent=1)
stops.layer <- left_join(stops.layer,frequent.stops,by="stop_id") %>% mutate(frequent = replace_na(frequent, 0))

trips.stops <- transit.trips %>% select(stop_id, trip) %>% group_by(stop_id) %>% summarize(trips = sum(trip))
stops.layer <- left_join(stops.layer,trips.stops,by="stop_id") %>% mutate(trips = replace_na(trips, 0)) %>% st_transform(wgs84)

frequent.stops <- stops.layer %>% filter(frequent==1)
local.stops <- stops.layer %>% filter(frequent!=1)

unique.frequent.transit.trips <- transit.trips %>%
  filter(frequent==1) %>%
  select(trip_id, agency_name, trip, above_regional, above_county) %>%
  group_by(trip_id, agency_name) %>%
  summarize(total_stops = sum(trip), stops_over_region = sum(above_regional), stops_over_county = sum(above_county)) %>%
  mutate(percent_over_region = stops_over_region/total_stops, percent_over_county = stops_over_county/total_stops)

# Daily Frequent Transit Trips by Operator
freq.trips.ct <- unique.frequent.transit.trips %>% filter(agency_name=="Community Transit") %>% select(trip_id) %>% pull() %>% length()
freq.trips.et <- unique.frequent.transit.trips %>% filter(agency_name=="Everett Transit") %>% select(trip_id) %>% pull() %>% length()
freq.trips.kcm <- unique.frequent.transit.trips %>% filter(agency_name=="King County Metro") %>% select(trip_id) %>% pull() %>% length()
freq.trips.kt <- unique.frequent.transit.trips %>% filter(agency_name=="Kitsap Transit") %>% select(trip_id) %>% pull() %>% length()
freq.trips.pt <- unique.frequent.transit.trips %>% filter(agency_name=="Pierce Transit") %>% select(trip_id) %>% pull() %>% length()
freq.trips.st <- unique.frequent.transit.trips %>% filter(agency_name=="Sound Transit") %>% select(trip_id) %>% pull() %>% length()

freq.trips.reg.ct <- unique.frequent.transit.trips %>% filter(agency_name=="Community Transit" & percent_over_region>=0.50) %>% select(trip_id) %>% pull() %>% length()
freq.trips.reg.et <- unique.frequent.transit.trips %>% filter(agency_name=="Everett Transit" & percent_over_region>=0.50) %>% select(trip_id) %>% pull() %>% length()
freq.trips.reg.kcm <- unique.frequent.transit.trips %>% filter(agency_name=="King County Metro" & percent_over_region>=0.50) %>% select(trip_id) %>% pull() %>% length()
freq.trips.reg.kt <- unique.frequent.transit.trips %>% filter(agency_name=="Kitsap Transit" & percent_over_region>=0.50) %>% select(trip_id) %>% pull() %>% length()
freq.trips.reg.pt <- unique.frequent.transit.trips %>% filter(agency_name=="Pierce Transit" & percent_over_region>=0.50) %>% select(trip_id) %>% pull() %>% length()
freq.trips.reg.st <- unique.frequent.transit.trips %>% filter(agency_name=="Sound Transit" & percent_over_region>=0.50) %>% select(trip_id) %>% pull() %>% length()

freq.trips.cnty.ct <- unique.frequent.transit.trips %>% filter(agency_name=="Community Transit" & percent_over_county>=0.50) %>% select(trip_id) %>% pull() %>% length()
freq.trips.cnty.et <- unique.frequent.transit.trips %>% filter(agency_name=="Everett Transit" & percent_over_county>=0.50) %>% select(trip_id) %>% pull() %>% length()
freq.trips.cnty.kcm <- unique.frequent.transit.trips %>% filter(agency_name=="King County Metro" & percent_over_county>=0.50) %>% select(trip_id) %>% pull() %>% length()
freq.trips.cnty.kt <- unique.frequent.transit.trips %>% filter(agency_name=="Kitsap Transit" & percent_over_county>=0.50) %>% select(trip_id) %>% pull() %>% length()
freq.trips.cnty.pt <- unique.frequent.transit.trips %>% filter(agency_name=="Pierce Transit" & percent_over_county>=0.50) %>% select(trip_id) %>% pull() %>% length()
freq.trips.cnty.st <- unique.frequent.transit.trips %>% filter(agency_name=="Sound Transit" & percent_over_county>=0.50) %>% select(trip_id) %>% pull() %>% length()

# Local Transit Summary
unique.local.transit.trips <- transit.trips %>%
  filter(frequent!=1) %>%
  select(trip_id, agency_name, trip, above_regional, above_county) %>%
  group_by(trip_id, agency_name) %>%
  summarize(total_stops = sum(trip), stops_over_region = sum(above_regional), stops_over_county = sum(above_county)) %>%
  mutate(percent_over_region = stops_over_region/total_stops, percent_over_county = stops_over_county/total_stops)

# Daily Local Transit Trips by Operator
local.trips.ct <- unique.local.transit.trips %>% filter(agency_name=="Community Transit") %>% select(trip_id) %>% pull() %>% length()
local.trips.et <- unique.local.transit.trips %>% filter(agency_name=="Everett Transit") %>% select(trip_id) %>% pull() %>% length()
local.trips.kcm <- unique.local.transit.trips %>% filter(agency_name=="King County Metro") %>% select(trip_id) %>% pull() %>% length()
local.trips.kt <- unique.local.transit.trips %>% filter(agency_name=="Kitsap Transit") %>% select(trip_id) %>% pull() %>% length()
local.trips.pt <- unique.local.transit.trips %>% filter(agency_name=="Pierce Transit") %>% select(trip_id) %>% pull() %>% length()
local.trips.st <- unique.local.transit.trips %>% filter(agency_name=="Sound Transit") %>% select(trip_id) %>% pull() %>% length()

local.trips.reg.ct <- unique.local.transit.trips %>% filter(agency_name=="Community Transit" & percent_over_region>=0.50) %>% select(trip_id) %>% pull() %>% length()
local.trips.reg.et <- unique.local.transit.trips %>% filter(agency_name=="Everett Transit" & percent_over_region>=0.50) %>% select(trip_id) %>% pull() %>% length()
local.trips.reg.kcm <- unique.local.transit.trips %>% filter(agency_name=="King County Metro" & percent_over_region>=0.50) %>% select(trip_id) %>% pull() %>% length()
local.trips.reg.kt <- unique.local.transit.trips %>% filter(agency_name=="Kitsap Transit" & percent_over_region>=0.50) %>% select(trip_id) %>% pull() %>% length()
local.trips.reg.pt <- unique.local.transit.trips %>% filter(agency_name=="Pierce Transit" & percent_over_region>=0.50) %>% select(trip_id) %>% pull() %>% length()
local.trips.reg.st <- unique.local.transit.trips %>% filter(agency_name=="Sound Transit" & percent_over_region>=0.50) %>% select(trip_id) %>% pull() %>% length()

local.trips.cnty.ct <- unique.local.transit.trips %>% filter(agency_name=="Community Transit" & percent_over_county>=0.50) %>% select(trip_id) %>% pull() %>% length()
local.trips.cnty.et <- unique.local.transit.trips %>% filter(agency_name=="Everett Transit" & percent_over_county>=0.50) %>% select(trip_id) %>% pull() %>% length()
local.trips.cnty.kcm <- unique.local.transit.trips %>% filter(agency_name=="King County Metro" & percent_over_county>=0.50) %>% select(trip_id) %>% pull() %>% length()
local.trips.cnty.kt <- unique.local.transit.trips %>% filter(agency_name=="Kitsap Transit" & percent_over_county>=0.50) %>% select(trip_id) %>% pull() %>% length()
local.trips.cnty.pt <- unique.local.transit.trips %>% filter(agency_name=="Pierce Transit" & percent_over_county>=0.50) %>% select(trip_id) %>% pull() %>% length()
local.trips.cnty.st <- unique.local.transit.trips %>% filter(agency_name=="Sound Transit" & percent_over_county>=0.50) %>% select(trip_id) %>% pull() %>% length()

# Create Stop and Tract Maps ----------------------------------------------
lowinc.tract.map.regional <- create_tract_map(tract.tbl = tract.by.income, tract.var = "above_regional", tract.color = "#91268F", c.title = "Above Regional Average")
lowinc.tract.map.regional.freq.stops <- create_tract_map_stop(tract.tbl = tract.by.income, tract.var = "above_regional", tract.color = "#91268F", c.title = "Frequent Transit", stops.tbl = frequent.stops, stops.color = "#F05A28")
lowinc.tract.map.regional.local.stops <- create_tract_map_stop(tract.tbl = tract.by.income, tract.var = "above_regional", tract.color = "#91268F", c.title = "Local Transit", stops.tbl = local.stops, stops.color = "#8CC63E")

lowinc.tract.map.county <- create_tract_map(tract.tbl = tract.by.income, tract.var = "above_county", tract.color = "#91268F", c.title = "Above County Average")
lowinc.tract.map.county.freq.stops <- create_tract_map_stop(tract.tbl = tract.by.income, tract.var = "above_county", tract.color = "#91268F", c.title = "Frequent Transit", stops.tbl = frequent.stops, stops.color = "#F05A28")
lowinc.tract.map.county.local.stops <- create_tract_map_stop(tract.tbl = tract.by.income, tract.var = "above_county", tract.color = "#91268F", c.title = "Local Transit", stops.tbl = local.stops, stops.color = "#8CC63E")


# County Charts -----------------------------------------------------------
county.pop.data <- get_acs_recs('county', years=c(2019), table.names = c('C17002'), acs.type = 'acs5')

total.population <- county.pop.data %>% 
  separate(variable,into=c("table","variable"), "_") %>%
  filter(variable == "001") %>%
  rename(total=estimate) %>%
  select(name,total)

lowinc.population <- county.pop.data %>%
  separate(variable,into=c("table","variable"), "_") %>%
  filter(variable %in% inc.vars) %>%
  select(name,estimate) %>%
  group_by(name) %>%
  summarize(total = sum(estimate)) %>%
  rename(lowinc=total)

highinc.population <- county.pop.data %>%
  separate(variable,into=c("table","variable"), "_") %>%
  filter(variable == "008") %>%
  select(name,estimate) %>%
  group_by(name) %>%
  rename(highinc=estimate) %>%
  select(name,highinc)

county.by.income <- left_join(total.population,lowinc.population,by=c("name"))
county.by.income <- left_join(county.by.income,highinc.population,by=c("name"))

county.by.income <- county.by.income %>%
  mutate(`People of Lower Income`=lowinc/total, `People of Higher Income`=highinc/total) %>%
  select(-total,-lowinc,-highinc) %>%
  pivot_longer(!name, names_to="population")

county.by.income$name <- factor(county.by.income$name, levels=county.order)
income.colors <- c("People of Lower Income" = "#91268F", "People of Higher Income" = "#8CC63E")
county.by.income.chart <- create_bar_chart(w.data=county.by.income, w.x="name",w.y="value",w.fill="population",w.bartype="stack", w.palette=income.colors)

pop.by.tract.lowinc.county.region <-tract.by.income %>% filter(above_county==1) %>% select(total) %>% pull() %>% sum()
pop.by.tract.lowinc.county.king <-tract.by.income %>% filter(above_county==1 & county=="033") %>% select(total) %>% pull() %>% sum()
pop.by.tract.lowinc.county.kitsap <-tract.by.income %>% filter(above_county==1 & county=="035") %>% select(total) %>% pull() %>% sum()
pop.by.tract.lowinc.county.pierce <-tract.by.income %>% filter(above_county==1 & county=="053") %>% select(total) %>% pull() %>% sum()
pop.by.tract.lowinc.county.snohomish <-tract.by.income %>% filter(above_county==1 & county=="061") %>% select(total) %>% pull() %>% sum()

  
  