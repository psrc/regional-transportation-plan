# Packages used in Analysis -----------------------------------------------
library(dplyr)
library(readr)
library(tidyr)
library(readr)
library(stringr)
library(lubridate)
library(tibble)

library(sf)
library(odbc)
library(DBI)

library(data.table)

# Basic Inputs ------------------------------------------------------------
service.period <- 'Fall' #enter either Fall or Spring
transit.year <- 2018 # enter an year from 2015 to latest service period

# Census Year
if (transit.year <= 2019) {
  census.year <- transit.year
} else {
  census.year = 2019
}

transit.type <- c("Light Rail / Streetcar" = 0,
                  "Subway" = 1,
                  "Commuter Rail" = 2,
                  "Bus" = 3,
                  "Ferry" = 4,
                  "Monorail" = 12)

transit.type <- enframe(transit.type) %>% rename(route_type=value, transit_mode=name)

frequent.span.threshold <- 16
frequent.hourly.trip.threshold <- 8
express.span.threshold <- 8
equity.stop.threshold <- 0.5

output.csv <- 'yes'

# Database Connection and Queries -----------------------------------------
server_name <- "AWS-PROD-SQL\\SOCKEYE"
database_name <- "Elmer"

disabled_query <- paste0("SELECT geoid, equity_geog_vs_reg_total FROM Census.disability_equity_geographies(",census.year,",'Tract')")
elderly_query <- paste0("SELECT geoid, equity_geog_vs_reg_total FROM Census.elderly_equity_geographies(",census.year,",'Tract')")
youth_query <- paste0("SELECT geoid, equity_geog_vs_reg_total FROM Census.youth_equity_geographies(",census.year,",'Tract')")
lep_query <- paste0("SELECT geoid, equity_geog_vs_reg_total FROM Census.limited_english_equity_geographies(",census.year,",'Tract')")
poverty_query <- paste0("SELECT geoid, equity_geog_vs_reg_total FROM Census.poverty_equity_geographies(",census.year,",'Tract')")
peopleofcolor_query <- paste0("SELECT geoid, equity_geog_vs_reg_total FROM Census.racial_equity_geographies(",census.year,",'Tract')")

# Spatial Data ------------------------------------------------------------
wgs84 <- 4326
spn <- 2285 

tracts <- st_read("https://services6.arcgis.com/GWxg6t7KXELn1thE/arcgis/rest/services/tract2010_nowater/FeatureServer/0/query?where=0=0&outFields=*&f=pgeojson") %>% 
  st_transform(spn) %>%
  select(COUNTYFP10,GEOID10) %>%
  rename(county=COUNTYFP10, geoid=GEOID10)

# Load Regional GTFS Files ------------------------------------------------

stops <- as_tibble(fread("output/region_stops.csv"))
routes <- as_tibble(fread("output/region_routes.csv"))
trips <- as_tibble(fread("output/region_trips.csv"))
stop.times <- as_tibble(fread("output/region_stoptimes.csv"))

# Calculate Population Areas for Equity Analysis  --------------------------------------
db_con <- dbConnect(odbc::odbc(),
                    driver = "SQL Server",
                    server = server_name,
                    database = database_name,
                    trusted_connection = "yes"
  )
  
tract.disability <- as_tibble(DBI::dbGetQuery(db_con, disabled_query)) %>% rename(disabled = equity_geog_vs_reg_total)
tract.elderly <- as_tibble(DBI::dbGetQuery(db_con, elderly_query)) %>% rename(elderly=equity_geog_vs_reg_total)
tract.limited.english <- as_tibble(DBI::dbGetQuery(db_con, lep_query)) %>% rename(limited_english=equity_geog_vs_reg_total)
tract.poverty <- as_tibble(DBI::dbGetQuery(db_con, poverty_query)) %>% rename(poverty=equity_geog_vs_reg_total)
tract.people.of.color <- as_tibble(DBI::dbGetQuery(db_con, peopleofcolor_query)) %>%  rename(people_of_color=equity_geog_vs_reg_total)
tract.youth <- as_tibble(DBI::dbGetQuery(db_con, youth_query)) %>% rename(youth=equity_geog_vs_reg_total)

odbc::dbDisconnect(db_con)

tracts <- left_join(tracts, tract.disability, by=c("geoid"))
tracts <- left_join(tracts, tract.elderly, by=c("geoid"))
tracts <- left_join(tracts, tract.limited.english, by=c("geoid"))
tracts <- left_join(tracts, tract.poverty, by=c("geoid"))
tracts <- left_join(tracts, tract.people.of.color, by=c("geoid"))
tracts <- left_join(tracts, tract.youth, by=c("geoid"))

rm(tract.disability, tract.elderly, tract.limited.english, tract.poverty, tract.people.of.color, tract.youth)

# Join stops with tracts
stops.layer = st_as_sf(stops, coords = c("stop_lon", "stop_lat"), crs = wgs84) %>% st_transform(spn) 
temp <- st_intersection(stops.layer, tracts) %>% st_drop_geometry() %>% select(-geoid,-county, -agency, -stop_name)
stops <- left_join(stops, temp, by=c("stop_id")) %>% mutate(across(everything(), ~replace_na(.x, 0)))

rm(stops.layer, temp)

# Join stops to Stop Times
temp <- stops %>% select(-agency,-stop_name,-stop_lat,-stop_lon)
stop.times <- left_join(stop.times, temp, by=c("stop_id"))

# Basic Trip Statistics ---------------------------------------------------
temp <- stop.times %>% select(trip_id, arrival_time, disabled, elderly, limited_english, poverty, people_of_color, youth) %>% mutate(stop=1)

temp <- temp %>%
  group_by(trip_id) %>%
  summarize(total_stops=sum(stop),
            disabled_stops=sum(disabled), elderly_stops=sum(elderly), limited_english_stops=sum(limited_english),
            poverty_stops=sum(poverty), people_of_color_stops=sum(people_of_color), youth_stops=sum(youth)) %>%
  mutate(disabled_stops=round(disabled_stops/total_stops,2),elderly_stops=round(elderly_stops/total_stops,2),
         limited_english_stops=round(limited_english_stops/total_stops,2),poverty_stops=round(poverty_stops/total_stops,2),
         people_of_color_stops=round(people_of_color_stops/total_stops,2),youth_stops=round(youth_stops/total_stops,2)) %>%
  mutate(disabled_stops = case_when(
    disabled_stops>=equity.stop.threshold ~ 1,
    disabled_stops<equity.stop.threshold ~ 0)) %>%
  mutate(elderly_stops = case_when(
    elderly_stops>=equity.stop.threshold ~ 1,
    elderly_stops<equity.stop.threshold ~ 0)) %>%
  mutate(limited_english_stops = case_when(
    limited_english_stops>=equity.stop.threshold ~ 1,
    limited_english_stops<equity.stop.threshold ~ 0)) %>%
  mutate(poverty_stops = case_when(
    poverty_stops>=equity.stop.threshold ~ 1,
    poverty_stops<equity.stop.threshold ~ 0)) %>%
  mutate(people_of_color_stops = case_when(
    people_of_color_stops>=equity.stop.threshold ~ 1,
    people_of_color_stops<equity.stop.threshold ~ 0)) %>%
  mutate(youth_stops = case_when(
    youth_stops>=equity.stop.threshold ~ 1,
    youth_stops<equity.stop.threshold ~ 0))

trips <- left_join(trips, temp, by=c("trip_id"))

# Determine Route Tiems from first and last stop in sttop time by trip
trip.run.time <- stop.times %>%
  select(trip_id,stop_sequence) %>%
  group_by(trip_id) %>%
  summarize(first_stop = min(stop_sequence), last_stop = max(stop_sequence)) 

trip.start.time <- stop.times %>%
  select(trip_id,stop_sequence,arrival_time) %>%
  rename(first_stop=stop_sequence, start_time=arrival_time)

trip.end.time <- stop.times %>%
  select(trip_id,stop_sequence,arrival_time) %>%
  rename(last_stop=stop_sequence, end_time=arrival_time)

trip.run.time <- left_join(trip.run.time, trip.start.time, by=c("trip_id","first_stop"))
trip.run.time <- left_join(trip.run.time, trip.end.time, by=c("trip_id","last_stop"))
trip.run.time <- trip.run.time %>% mutate(running_time=period_to_seconds(seconds(end_time) - seconds(start_time))/3600) %>% drop_na() %>% select(trip_id,running_time)

trips <- left_join(trips, trip.run.time, by=c("trip_id"))

rm(temp, trip.run.time, trip.start.time, trip.end.time)

# Basic Route Statistics --------------------------------------------------
temp <- trips %>%
  select(route_id, total_stops, disabled_stops, elderly_stops, limited_english_stops, poverty_stops, people_of_color_stops, youth_stops) %>%
  mutate(total_trips=1) %>%
  group_by(route_id) %>%
  summarize(stops=round(mean(total_stops),0), trips=sum(total_trips),
            disabled=sum(disabled_stops), elderly=sum(elderly_stops), limited_english=sum(limited_english_stops),
            poverty=sum(poverty_stops), people_of_color=sum(people_of_color_stops), youth=sum(youth_stops)) %>%
  mutate(disabled=round(disabled/trips,2),elderly=round(elderly/trips,2),
         limited_english=round(limited_english/trips,2),poverty=round(poverty/trips,2),
         people_of_color=round(people_of_color/trips,2),youth=round(youth/trips,2)) %>%
  mutate(disabled = case_when(
    disabled>=equity.stop.threshold ~ 1,
    disabled<equity.stop.threshold ~ 0)) %>%
  mutate(elderly = case_when(
    elderly>=equity.stop.threshold ~ 1,
    elderly<equity.stop.threshold ~ 0)) %>%
  mutate(limited_english = case_when(
    limited_english>=equity.stop.threshold ~ 1,
    limited_english<equity.stop.threshold ~ 0)) %>%
  mutate(poverty = case_when(
    poverty>=equity.stop.threshold ~ 1,
    poverty<equity.stop.threshold ~ 0)) %>%
  mutate(people_of_color = case_when(
    people_of_color>=equity.stop.threshold ~ 1,
    people_of_color<equity.stop.threshold ~ 0)) %>%
  mutate(youth = case_when(
    youth>=equity.stop.threshold ~ 1,
    youth<equity.stop.threshold ~ 0))

routes <- left_join(routes, temp, by=c("route_id"))

temp <- trips %>%
  select(route_id, running_time) %>%
  drop_na() %>%
  group_by(route_id) %>%
  summarize(min_trip_time=round(min(running_time),2),max_trip_time=round(max(running_time),2), average_trip_time=round(mean(running_time),2))

routes <- left_join(routes, temp, by=c("route_id")) %>%
  drop_na() %>%
  mutate(daily_hours = average_trip_time*trips)

# Define Service Typology -------------------------------------------------
temp <- stop.times %>% 
  select(-trip_id) %>% 
  filter(stop_sequence==1) %>% 
  mutate(trip_hour=hour(arrival_time)) %>%
  select(route_id, trip_hour) %>%
  mutate(trip=1) %>%
  group_by(route_id, trip_hour) %>%
  summarize(trips=sum(trip)) %>%
  mutate(any_trips=1) %>%
  select(-trip_hour) %>%
  group_by(route_id) %>%
  summarize(total_span = sum(any_trips), daily_trips = sum(trips)) %>%
  mutate(hourly_trips = round(daily_trips/total_span,1)) %>%
  select(-daily_trips) %>%
  mutate(typology = case_when(
    total_span>=frequent.span.threshold & hourly_trips>= frequent.hourly.trip.threshold ~ "Frequent",
    total_span<=express.span.threshold ~ "Express",
    total_span>express.span.threshold & hourly_trips< frequent.hourly.trip.threshold ~ "Local")) %>%
  mutate(typology = replace_na(typology,"Local"))
  
routes <- left_join(routes, temp, by=c("route_id")) %>% 
  mutate(typology = case_when(
    route_type==0 ~ "Light Rail / Streetcar",
    route_type==2 ~ "Commuter Rail",
    route_type==3 ~ typology,
    route_type==4 ~ "Ferry"))

if (output.csv=='yes') {
  fwrite(routes, "output/route_summary.csv")
}

route.summary.tbl <- routes
rm(temp, routes, stop.times,stops,trips,tracts,transit.type, db_con)
