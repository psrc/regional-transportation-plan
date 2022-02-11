# Packages used in Analysis -----------------------------------------------
library(dplyr)
library(readr)
library(tidyr)
library(readr)
library(stringr)
library(lubridate)
library(here)

# Basic Inputs ------------------------------------------------------------

stops.data <- read_csv(here('data','gtfs','stops.txt'), show_col_types = FALSE)




stoptime.data <- read_csv(here('data','gtfs','stop_times.txt'), show_col_types = FALSE) %>%
  filter(stop_sequence==1) %>%
  select(trip_id,arrival_time)


trips.hour <- temp %>% 
  select(route_id, trip_hour) %>%
  mutate(trip=1) %>%
  group_by(route_id, trip_hour) %>%
  summarize(trips=sum(trip))







trip.data <- read_csv(here('data','gtfs','trips.txt'), show_col_types = FALSE)
routes.data <- read_csv(here('data','gtfs','routes.csv'), show_col_types = FALSE)


























st.express <- c("510","511","512","513","522","532","535","540","541","542","545",
                "550","554","555","556","560","566","567","574","577","578","580",
                "586","590","592","594","595","596", "Link light rail")

seattle.routes <- c("South Lake Union Streetcar","First Hill Streetcar")

weekday.calendars <- c("CT_MCOB-DO:121:0:Weekday:1:18SEP:12345", "CT_KPOB-CS:221:0:Weekday:1:18SEP:12345",
                       "ET_1", "KT_mtwtf", "PT_Default-62",
                       "ST_WD_SNDR", "ST_WD_TL", "WSF_20181018")

frequent.span.threshold <- 16
frequent.hourly.trip.threshold <- 8
express.span.threshold <- 8

ct.url <- 'https://transitfeeds.com/p/community-transit/454/20181014/download'
et.url <- 'https://transitfeeds.com/p/everett-transit/455/20181013/download'
kcm.url <- 'https://transitfeeds.com/p/king-county-metro/73/20180925/download'
kt.url <- 'https://transitfeeds.com/p/kitsap-transit/296/20180928/download'
pt.url <- 'https://transitfeeds.com/p/pierce-transit/448/20180926/download'
st.url <- 'https://transitfeeds.com/p/sound-transit/44/20181013/download'
wsf.url <- 'https://transitfeeds.com/p/washington-state-ferries/586/20181019/download'
it.url <- 'https://transitfeeds.com/p/intercity-transit/356/20181010/download'

gtfs.urls <- list(list(ct.url,'CT','Community Transit'), list(et.url,'ET','Everett Transit'), list(kcm.url,'KCM','King County Metro'), 
                  list(kt.url,'KT','Kitsap Transit'), list(pt.url,'PT','Pierce Transit'), list(st.url,'ST','Sound Transit'), 
                  list(wsf.url,'WSF','Washington State Ferries'))


# Process GTFS Data into Regional Data-set ---------------------------------
region.stops <- NULL
region.routes <- NULL
region.trips <- NULL
region.calendar <- NULL
region.stop.times <- NULL

for (gtfs in gtfs.urls) {
  
  print(paste0('Working on ', gtfs[[2]]))
  
  # Download the latest GTFS archive from the Transit Feeds
  download.file(gtfs[[1]], "working.zip", quiet = TRUE, mode = "wb")
  
  # Pull Out Routes, Stops
  current.stops <- readr::read_csv(unz("working.zip","stops.txt"), show_col_types = FALSE) %>%
    mutate(agency=gtfs[[2]]) %>%
    mutate(stop_id = paste0(agency,"_",stop_id)) %>%
    select(stop_id, agency, stop_name, stop_lat, stop_lon)
  
  current.routes <- readr::read_csv(unz("working.zip","routes.txt"), show_col_types = FALSE) %>%
    mutate(agency=gtfs[[2]]) %>%
    mutate(route_id = paste0(agency,"_",route_id)) %>%
    select(route_id, agency, route_short_name, route_long_name, route_type) %>%
    mutate(route_short_name = as.character(route_short_name))
  
  current.calendar <- readr::read_csv(unz("working.zip","calendar.txt"), show_col_types = FALSE) %>%
    mutate(agency=gtfs[[2]]) %>%
    mutate(service_id = paste0(agency,"_",service_id))
  
  current.trips <- readr::read_csv(unz("working.zip","trips.txt"), show_col_types = FALSE) %>%
    mutate(agency=gtfs[[2]]) %>%
    mutate(route_id = paste0(agency,"_",route_id), trip_id = paste0(agency,"_",trip_id), service_id = paste0(agency,"_",service_id)) %>%
    select(route_id, agency, trip_id, service_id, direction_id)
  
  current.stop.times <- readr::read_csv(unz("working.zip","stop_times.txt"), show_col_types = FALSE) %>%
    mutate(agency=gtfs[[2]]) %>%
    mutate(stop_id = paste0(agency,"_",stop_id), trip_id = paste0(agency,"_",trip_id)) %>%
    select(trip_id, agency, arrival_time, departure_time, stop_id, stop_sequence) %>%
    mutate(arrival_time = hms(arrival_time), departure_time = hms(departure_time))
  
  # Combine into a Regional File
  ifelse(is.null(region.stops), region.stops <- current.stops, region.stops <- bind_rows(region.stops, current.stops))
  ifelse(is.null(region.routes), region.routes <- current.routes, region.routes <- bind_rows(region.routes, current.routes))
  ifelse(is.null(region.trips), region.trips <- current.trips, region.trips <- bind_rows(region.trips, current.trips))
  ifelse(is.null(region.calendar), region.calendar <- current.calendar, region.calendar <- bind_rows(region.calendar, current.calendar))
  ifelse(is.null(region.stop.times), region.stop.times <- current.stop.times, region.stop.times <- bind_rows(region.stop.times, current.stop.times))

}

rm(current.stops, current.routes, current.calendar, current.trips, current.stop.times, gtfs.urls, gtfs)
file.remove("working.zip")

# Create Weekday Only Trips and Routes ------------------------------------
weekday.service.ids <- region.calendar %>%
  mutate(weekday = case_when(
    (agency=="KCM" & monday==1 | agency=="KCM" & tuesday==1 | agency=="KCM" & wednesday==1 | agency=="KCM"& thursday==1| agency=="KCM" & friday==1) ~ 1,
    (service_id %in% weekday.calendars) ~ 1)) %>%
  mutate(weekday = replace_na(weekday, 0)) %>%
  select(service_id,weekday)

region.trips <- left_join(region.trips, weekday.service.ids, by=c("service_id")) %>% select(-agency) 
region.stop.times <- left_join(region.stop.times, region.trips, by=c("trip_id"))
region.trips <- region.trips %>% filter(weekday==1)
region.stop.times <- region.stop.times %>% filter(weekday==1)

rm(weekday.service.ids)

# Determine Route Characteristics ------------------------------------------

# Remove any duplicate trip records
temp <- region.stop.times %>% 
  select(-trip_id,-service_id) %>% 
  filter(stop_sequence==1) %>% 
  distinct() %>% 
  mutate(trip_hour=arrival_time@hour)

route.trips.hour <- temp %>% 
  select(route_id, trip_hour) %>%
  mutate(trip=1) %>%
  group_by(route_id, trip_hour) %>%
  summarize(trips=sum(trip))

route.hours.of.service <- route.trips.hour %>% 
  mutate(any_trips=1) %>%
  mutate(frequent_trips = case_when(
    trips>=frequent.hourly.trip.threshold ~ 1,
    trips<frequent.hourly.trip.threshold ~ 0)) %>%
  select(-trip_hour) %>%
  group_by(route_id) %>%
  summarize(total_span = sum(any_trips), frequent_span = sum(frequent_trips), daily_trips = sum(trips)) %>%
  mutate(hourly_trips = round(daily_trips/total_span,1))

# Clean up Agency and Names in Route Names
route.names <- region.routes %>% select(route_id, agency, route_short_name, route_type, route_long_name)  %>%
  mutate(temp = case_when(
    is.na(route_short_name) ~ route_long_name,
    !(is.na(route_short_name)) ~ route_short_name)) %>%
  mutate(route_short_name=temp) %>%
  select(-temp) %>%
  mutate(temp = case_when(
    route_short_name %in% st.express ~ "ST",
    !(route_short_name %in% st.express) ~ agency)) %>%
  mutate(agency=temp) %>%
  select(-temp) %>%
  mutate(temp = case_when(
    is.na(route_long_name) ~ route_short_name,
    !(is.na(route_long_name)) ~ route_long_name)) %>%
  mutate(route_long_name=temp) %>%
  select(-temp) %>%
  mutate(temp = case_when(
    route_short_name %in% seattle.routes ~ "SEA",
    !(route_short_name %in% seattle.routes) ~ agency)) %>%
  mutate(agency=temp) %>%
  select(-temp)
  
route.hours.of.service <- left_join(route.hours.of.service, route.names, by=c("route_id"))

rm(temp, route.trips.hour, route.names)

# Summarize by Agency -----------------------------------------------------

temp <- route.hours.of.service %>%
  select(agency, daily_trips) %>%
  mutate(route=1) %>%
  group_by(agency) %>%
  summarize(total_routes=sum(route), total_trips=sum(daily_trips)) %>%
  bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else "Total")) %>%
  mutate(trips_per_route=total_trips/total_routes)



