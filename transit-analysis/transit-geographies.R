# Packages used in Analysis -----------------------------------------------
library(dplyr)
library(readr)
library(tidyr)
library(readr)
library(stringr)
library(lubridate)
library(tibble)
library(psrccensus)
library(sf)
library(odbc)
library(DBI)

# Basic Inputs ------------------------------------------------------------
service.period <- 'Fall' #enter either Fall or Spring
transit.year <- 2018 # enter an year from 2015 to latest service period

# Census Year
if (transit.year <= 2019) {
  census.year <- transit.year
} else {
  census.year = 2019
}

source("gtfs-urls.R")

st.routes <- c("510","511","512","513","522","532","535","540","541","542","545",
                "550","554","555","556","560","566","567","574","577","578","580",
                "586","590","592","594","595","596", "Link light rail",
               "100232", "100235", "102640", "100511", "100236", "100239",
               "100240","100241","100451", "100479")

seattle.routes <- c("South Lake Union Streetcar","First Hill Streetcar", 
                    "102638", "100340")

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

# Functions ---------------------------------------------------------------


# Process GTFS Data into Regional Data-set ---------------------------------
gtfs.urls <- transit.feeds.url[[service.period]][[as.character(transit.year)]]

if (service.period=="Fall") {
  transit.month = 10
} else {
  transit.month = 3
}

month.dates <- seq(ymd(paste0(transit.year,"-",transit.month,"-01")),ymd(paste0(transit.year,"-",transit.month,"-28")),by="1 day")
first.mon <- month.dates[wday(month.dates,label = TRUE) == "Mon" & day(month.dates) <= 7]
first.tue <- month.dates[wday(month.dates,label = TRUE) == "Tue" & day(month.dates) <= 7]
first.wed <- month.dates[wday(month.dates,label = TRUE) == "Wed" & day(month.dates) <= 7]
first.thu <- month.dates[wday(month.dates,label = TRUE) == "Thu" & day(month.dates) <= 7]
first.fri <- month.dates[wday(month.dates,label = TRUE) == "Fri" & day(month.dates) <= 7]
first.week <- c(first.mon, first.tue, first.wed, first.thu, first.fri)

# WSF calendar is different and 2015 only has fall data so set date for WSF depending on the year
if (transit.year == 2015) {
  wsf.dates <- seq(ymd(paste0(transit.year,"-12-01")),ymd(paste0(transit.year,"-12-28")),by="1 day")
  wsf.first.day <- month.dates[wday(month.dates,label = TRUE) == "Mon" & day(month.dates) >=17]
} else {
  wsf.dates <- seq(ymd(paste0(transit.year,"-",transit.month,"-01")),ymd(paste0(transit.year,"-",transit.month,"-28")),by="1 day")
  wsf.first.day <- month.dates[wday(month.dates,label = TRUE) == "Mon" & day(month.dates) >=17]
}
  

region.stops <- NULL
region.routes <- NULL
region.trips <- NULL
region.calendar <- NULL
region.stop.times <- NULL

gtfs <- gtfs.urls[['wsf']]

for (gtfs in gtfs.urls) {
  
  print(paste0('Working on ', gtfs[[2]]))
  
  # Download the latest GTFS archive from the Transit Feeds
  download.file(gtfs[[1]], "working.zip", quiet = TRUE, mode = "wb")
  
  #########################################################################################################
  #########################################################################################################
  ### Dates
  #########################################################################################################
  #########################################################################################################
  # Community Transit uses detailed calendar dates to identify weekday service
  if(gtfs[[2]]=="CT"|gtfs[[2]]=="KCM") {
    weekday.ids <- readr::read_csv(unz("working.zip","calendar_dates.txt"), show_col_types = FALSE) %>%
      mutate(date=ymd(date)) %>%
      filter(date%in%first.week & exception_type==1) %>% 
      select(service_id) %>% 
      pull() %>%
      unique()
  }
  
  if(gtfs[[2]]=="ET"|gtfs[[2]]=="PT"|gtfs[[2]]=="ST") {
    weekday.ids <- readr::read_csv(unz("working.zip","calendar.txt"), show_col_types = FALSE) %>%
      filter(monday==1 & tuesday==1 & wednesday==1 & thursday==1 & friday==1) %>%
      select(service_id) %>%
      pull()
  }
  
  if(gtfs[[2]]=="KT") {
    weekday.ids <- readr::read_csv(unz("working.zip","calendar.txt"), show_col_types = FALSE) %>%
      filter(monday==1 & tuesday==1 & wednesday==1 & thursday==1 & friday==1) %>%
      filter(!(str_detect(service_id, "hol"))) %>%
      select(service_id) %>%
      pull()
  }
  
  if(gtfs[[2]]=="WSF") {
    weekday.ids <- readr::read_csv(unz("working.zip","calendar.txt"), show_col_types = FALSE) %>%
      mutate(date=ymd(service_id)) %>%
      filter(date==wsf.first.day) %>%
      select(service_id) %>%
      pull()
  }
  
  #########################################################################################################
  #########################################################################################################
  ### Stops
  #########################################################################################################
  #########################################################################################################
  
  current.stops <- readr::read_csv(unz("working.zip","stops.txt"), show_col_types = FALSE) %>%
    mutate(agency=gtfs[[2]]) %>%
    mutate(stop_id = paste0(agency,"_",stop_id)) %>%
    select(stop_id, agency, stop_name, stop_lat, stop_lon) %>%
    mutate(agency=gtfs[[3]])
  
  #########################################################################################################
  #########################################################################################################
  ### Routes
  #########################################################################################################
  #########################################################################################################
  current.routes <- readr::read_csv(unz("working.zip","routes.txt"), show_col_types = FALSE) %>%
    
    mutate(route_short_name=as.character(route_short_name), route_long_name=as.character(route_long_name)) %>%
    
    # Clean Up Short Name if it is NULL
    mutate(temp = case_when(
      is.na(route_short_name) ~ route_long_name,
      !(is.na(route_short_name)) ~ route_short_name)) %>%
    mutate(route_short_name=temp) %>%
    select(-temp) %>%
    
    # Clean Up Long Name if it is NULL
    mutate(temp = case_when(
      is.na(route_long_name) ~ route_short_name,
      !(is.na(route_long_name)) ~ route_long_name)) %>%
    mutate(route_long_name=temp) %>%
    select(-temp) %>%
    
    mutate(agency=gtfs[[2]]) %>%
    
    # Clean Up Agency if it is a ST Route
    mutate(temp = case_when(
      route_short_name %in% st.routes ~ "ST",
      !(route_short_name %in% st.routes) ~ agency)) %>%
    mutate(agency=temp) %>%
    select(-temp) %>%
    
    # Clean Up Agency if it is a Seattle Route
    mutate(temp = case_when(
      route_short_name %in% seattle.routes ~ "SEA",
      !(route_short_name %in% seattle.routes) ~ agency)) %>%
    mutate(agency=temp) %>%
    select(-temp) %>%
    
    # Final Clean Up
    mutate(route_id = paste0(agency,"_",route_id)) %>%
    select(route_id, agency, route_short_name, route_long_name, route_type) %>%
    mutate(route_short_name = as.character(route_short_name)) %>%
    mutate(agency=gtfs[[3]]) %>%
    
    # Clean Up Agency if it is a ST Route
    mutate(temp = case_when(
      route_short_name %in% st.routes ~ "Sound Transit",
      !(route_short_name %in% st.routes) ~ agency)) %>%
    mutate(agency=temp) %>%
    select(-temp) %>%
    
    # Clean Up Agency if it is a Seattle Route
    mutate(temp = case_when(
      route_short_name %in% seattle.routes ~ "City of Seattle",
      !(route_short_name %in% seattle.routes) ~ agency)) %>%
    mutate(agency=temp) %>%
    select(-temp)
  
  current.routes <- left_join(current.routes, transit.type, by=c("route_type"))
  
  #########################################################################################################
  #########################################################################################################
  ### Weekday Trips
  #########################################################################################################
  #########################################################################################################
  
  current.trips <- readr::read_csv(unz("working.zip","trips.txt"), show_col_types = FALSE) %>%
    filter(service_id %in% weekday.ids) %>%
    
    mutate(agency=gtfs[[2]]) %>%
    mutate(trip_id = paste0(agency,"_",trip_id)) %>%
    
    # Clean Up Agency if it is a ST Route
    mutate(temp = case_when(
      route_id %in% st.routes ~ "ST",
      !(route_id %in% st.routes) ~ agency)) %>%
    mutate(agency=temp) %>%
    select(-temp) %>%
    
    # Clean Up Agency if it is a Seattle Route
    mutate(temp = case_when(
      route_id %in% seattle.routes ~ "SEA",
      !(route_id %in% seattle.routes) ~ agency)) %>%
    mutate(agency=temp) %>%
    select(-temp) %>%
    
    # Final Clean Up
    mutate(route_id = paste0(agency,"_",route_id)) %>%
    mutate(shape_id = paste0(agency,"_",shape_id)) %>%
    mutate(agency=gtfs[[3]]) %>%
    
    # Clean Up Agency if it is a ST Route
    mutate(temp = case_when(
      route_id %in% paste0("ST_",st.routes) ~ "Sound Transit",
      !(route_id %in% paste0("ST_",st.routes)) ~ agency)) %>%
    mutate(agency=temp) %>%
    select(-temp) %>%
    
    # Clean Up Agency if it is a Seattle Route
    mutate(temp = case_when(
      route_id %in% paste0("SEA_",seattle.routes) ~ "City of Seattle",
      !(route_id %in% paste0("SEA_",seattle.routes)) ~ agency)) %>%
    mutate(agency=temp) %>%
    select(-temp) %>%
    
    mutate(shape_id = as.character(shape_id)) %>%
    
    select(trip_id, route_id, direction_id, shape_id, agency)

  weekday.trips <- current.trips %>% select(trip_id) %>% pull
  
  #########################################################################################################
  #########################################################################################################
  ### Weekday Stop Times
  #########################################################################################################
  #########################################################################################################
  
  current.stop.times <- readr::read_csv(unz("working.zip","stop_times.txt"), show_col_types = FALSE) %>%
    mutate(agency=gtfs[[2]]) %>%
    mutate(trip_id = paste0(agency,"_",trip_id)) %>%
    mutate(stop_id = paste0(agency,"_",stop_id)) %>%
    mutate(arrival_time = hms(arrival_time), departure_time = hms(departure_time)) %>%
    filter(trip_id %in% weekday.trips) %>%
    select(trip_id, stop_id, arrival_time, stop_sequence)
    
  current.stop.times <- left_join(current.stop.times, current.trips, by=c("trip_id"))
  
  #########################################################################################################
  #########################################################################################################
  ### Weekday Stop Times from Frequencies
  #########################################################################################################
  #########################################################################################################
  if(gtfs[[2]]=="ST") {
    
    current.freq <- readr::read_csv(unz("working.zip","frequencies.txt"), show_col_types = FALSE) %>%
      filter(str_detect(trip_id,"WD")) %>%
      mutate(agency=gtfs[[2]]) %>%
      mutate(trip_id = paste0(agency,"_",trip_id))
    
    unique.trips <- current.freq %>% select(trip_id) %>% pull() %>% unique()
    
    full.trips.by.frequency <- NULL
    
    for(trips in unique.trips) {
      
      trips.by.frequency <- NULL
      
      t <- current.freq %>% filter(trip_id==trips)
      s <- current.stop.times %>% filter(trip_id==trips)
    
      # Details of Trip Sequence
      num.stops <- s %>% select(stop_sequence) %>% pull() %>% length()
      starting.time <- period_to_seconds(hms(s %>% filter(stop_sequence==1) %>% select(arrival_time) %>% pull()))
      ending.time <- period_to_seconds(hms(s %>% filter(stop_sequence==num.stops) %>% select(arrival_time) %>% pull()))
      run.time <- ending.time - starting.time
      stop.to.stop <- run.time/(num.stops-1)
      
      for (row in 1:nrow(t)) {
    
        starting.time <- period_to_seconds(hms(t[row, "start_time"] %>% pull()))
        ending.time <- period_to_seconds(hms(t[row, "end_time"] %>% pull()))
        trip.interval <- t[row, "headway_secs"] %>% pull()
        
        i <- as_tibble(seq(from=starting.time, to=ending.time, by=trip.interval)) %>% mutate(trip_id=trips)
        
        ifelse(is.null(trips.by.frequency), trips.by.frequency <- i, trips.by.frequency <- bind_rows(trips.by.frequency,i))
      }
      
      trips.by.frequency <- trips.by.frequency %>% 
        mutate(arrival_time=seconds_to_period(value)) %>%
        mutate(stop_sequence=1) %>%
        rowid_to_column() %>%
        mutate(trip_id = paste0(trip_id,"_",rowid)) %>%
        select(-rowid, -value)
      
      temp <- trips.by.frequency
      
      for (stop.num in 2:num.stops) {
        start.time <- period_to_seconds(hms(temp %>% select(arrival_time) %>% pull()))
        
        end.time <- as_tibble(start.time + (stop.to.stop*(stop.num-1))) %>% 
          mutate(arrival_time=seconds_to_period(value)) %>%
          mutate(stop_sequence=stop.num) %>% 
          mutate(trip_id=trips) %>%
          rowid_to_column() %>%
          mutate(trip_id = paste0(trip_id,"_",rowid)) %>%
          select(-rowid, -value)
        
        trips.by.frequency <- bind_rows(trips.by.frequency,end.time)
        
      }
      
      # Add in other stop-time details
      s <- s %>% select(-trip_id, -arrival_time)
      r <- s %>% select(route_id) %>% pull() %>% unique()
      
      trips.by.frequency <- trips.by.frequency %>%
        mutate(route_id = r)
      
      trips.by.frequency <- left_join(trips.by.frequency, s, by=c("route_id","stop_sequence"))
      
      ifelse(is.null(full.trips.by.frequency), full.trips.by.frequency <- trips.by.frequency, full.trips.by.frequency <- bind_rows(full.trips.by.frequency,trips.by.frequency))
      
    }
    
    current.stop.times <- bind_rows(current.stop.times, full.trips.by.frequency)  
    rm(end.time, full.trips.by.frequency, i, s, t, temp, trips.by.frequency)  
    
  } # end of stop frequency if statement
    
  #########################################################################################################
  #########################################################################################################
  ### Combine into Regional GTFS
  #########################################################################################################
  #########################################################################################################
  
  ifelse(is.null(region.stops), region.stops <- current.stops, region.stops <- bind_rows(region.stops, current.stops))
  ifelse(is.null(region.routes), region.routes <- current.routes, region.routes <- bind_rows(region.routes, current.routes))
  ifelse(is.null(region.trips), region.trips <- current.trips, region.trips <- bind_rows(region.trips, current.trips))
  ifelse(is.null(region.stop.times), region.stop.times <- current.stop.times, region.stop.times <- bind_rows(region.stop.times, current.stop.times))
  
  file.remove("working.zip")
  rm(current.freq,current.routes,current.stop.times,current.stops,current.trips)

}


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
stops.layer = st_as_sf(region.stops, coords = c("stop_lon", "stop_lat"), crs = wgs84) %>% st_transform(spn) 
temp <- st_intersection(stops.layer, tracts) %>% st_drop_geometry() %>% select(-geoid,-county, -agency, -stop_name)
region.stops <- left_join(region.stops, temp, by=c("stop_id")) %>% mutate(across(everything(), ~replace_na(.x, 0)))

rm(stops.layer, temp)

# Join stops to Stop Times
temp <- region.stops %>% select(-agency,-stop_name,-stop_lat,-stop_lon)
region.stop.times <- left_join(region.stop.times, temp, by=c("stop_id"))

# Basic Trip Statistics ---------------------------------------------------
temp <- region.stop.times %>% select(trip_id, arrival_time, disabled, elderly, limited_english, poverty, people_of_color, youth) %>% mutate(stop=1)

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

region.trips <- left_join(region.trips, temp, by=c("trip_id"))

trip.run.time <- region.stop.times %>%
  select(trip_id,stop_sequence) %>%
  group_by(trip_id) %>%
  summarize(first_stop = min(stop_sequence), last_stop = max(stop_sequence)) 

trip.start.time <- region.stop.times %>%
  select(trip_id,stop_sequence,arrival_time) %>%
  rename(first_stop=stop_sequence, start_time=arrival_time)

trip.end.time <- region.stop.times %>%
  select(trip_id,stop_sequence,arrival_time) %>%
  rename(last_stop=stop_sequence, end_time=arrival_time)

trip.run.time <- left_join(trip.run.time, trip.start.time, by=c("trip_id","first_stop"))
trip.run.time <- left_join(trip.run.time, trip.end.time, by=c("trip_id","last_stop"))
trip.run.time <- trip.run.time %>% mutate(running_time=period_to_seconds(end_time - start_time)/3600) %>% drop_na() %>% select(trip_id,running_time)

region.trips <- left_join(region.trips, trip.run.time, by=c("trip_id"))

rm(temp, trip.run.time, trip.start.time, trip.end.time)

# Basic Route Statistics --------------------------------------------------
temp <- region.trips %>%
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

region.routes <- left_join(region.routes, temp, by=c("route_id"))

temp <- region.trips %>%
  select(route_id, running_time) %>%
  drop_na() %>%
  group_by(route_id) %>%
  summarize(min_trip_time=round(min(running_time),2),max_trip_time=round(max(running_time),2), average_trip_time=round(mean(running_time),2))

region.routes <- left_join(region.routes, temp, by=c("route_id")) %>%
  drop_na() %>%
  mutate(daily_hours = average_trip_time*trips)


# Define Service Typology -------------------------------------------------
temp <- region.stop.times %>% 
  select(-trip_id) %>% 
  filter(stop_sequence==1) %>% 
  mutate(trip_hour=arrival_time@hour) %>%
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
  
region.routes <- left_join(region.routes, temp, by=c("route_id")) %>% 
  mutate(typology = case_when(
    route_type==0 ~ "Light Rail / Streetcar",
    route_type==2 ~ "Commuter Rail",
    route_type==3 ~ typology,
    route_type==4 ~ "Ferry"))

rm(temp)