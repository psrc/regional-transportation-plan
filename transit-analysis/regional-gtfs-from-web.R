# Packages used in Analysis -----------------------------------------------
library(dplyr)
library(readr)
library(tidyr)
library(readr)
library(stringr)
library(lubridate)
library(tibble)
library(data.table)

# Basic Inputs ------------------------------------------------------------
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

# Functions ---------------------------------------------------------------
ConvertToSeconds <- function(X)
{
  X <- strsplit(X, ":")
  sapply(X, function(Y) sum(as.numeric(Y) * c(3600, 60, 1)))
}

# Process GTFS Data into Regional Data-set ---------------------------------
gtfs.urls <- transit.feeds.url[[service.period]][[as.character(transit.year)]]

if (service.period=="Fall") {
  transit.month = 10
} else {
  transit.month = 3
}

month.dates <- seq(ymd(paste0(transit.year,"-",transit.month,"-15")),ymd(paste0(transit.year,"-",transit.month,"-28")),by="1 day")
first.mon <- month.dates[lubridate::wday(month.dates,label = TRUE) == "Mon" & day(month.dates) >= 17]
first.tue <- month.dates[lubridate::wday(month.dates,label = TRUE) == "Tue" & day(month.dates) >= 17]
first.wed <- month.dates[lubridate::wday(month.dates,label = TRUE) == "Wed" & day(month.dates) >= 17]
first.thu <- month.dates[lubridate::wday(month.dates,label = TRUE) == "Thu" & day(month.dates) >= 17]
first.fri <- month.dates[lubridate::wday(month.dates,label = TRUE) == "Fri" & day(month.dates) >= 17]
first.week <- c(first.mon, first.tue, first.wed, first.thu, first.fri)

# WSF calendar is different and 2015 only has fall data so set date for WSF depending on the year
if (transit.year == 2015) {
  wsf.dates <- seq(ymd(paste0(transit.year,"-12-01")),ymd(paste0(transit.year,"-12-28")),by="1 day")
  wsf.first.day <- month.dates[lubridate::wday(month.dates,label = TRUE) == "Mon" & day(month.dates) >=17]
} else {
  wsf.dates <- seq(ymd(paste0(transit.year,"-",transit.month,"-01")),ymd(paste0(transit.year,"-",transit.month,"-28")),by="1 day")
  wsf.first.day <- month.dates[lubridate::wday(month.dates,label = TRUE) == "Mon" & day(month.dates) >=17]
}
  
region.stops <- NULL
region.routes <- NULL
region.trips <- NULL
region.calendar <- NULL
region.stop.times <- NULL
region.shapes <- NULL

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
  if(gtfs[[2]]=="CT") {
    
    if(transit.year >= 2018) {
      weekday.ids <- readr::read_csv(unz("working.zip","calendar_dates.txt"), show_col_types = FALSE) %>%
        mutate(date=ymd(date)) %>%
        filter(date%in%first.week & exception_type==1) %>% 
        select(service_id) %>% 
        pull() %>%
        unique()
      
    } else {
      weekday.ids <- readr::read_csv(unz("working.zip","calendar.txt"), show_col_types = FALSE) %>%
        filter((str_detect(service_id, "SEP-Weekday"))) %>%
        select(service_id) %>%
        pull()
    }
  }
    
  if(gtfs[[2]]=="KCM") {
    
    if(transit.year==2015) {
      weekday.ids <- readr::read_csv(unz("working.zip","calendar.txt"), show_col_types = FALSE) %>%
        filter((str_detect(service_id, "WEEKDAY")))
    } else {
    
        weekday.ids <- readr::read_csv(unz("working.zip","calendar_dates.txt"), show_col_types = FALSE) %>%
          mutate(date=ymd(date)) %>%
          filter(date%in%first.week & exception_type==1) %>% 
          select(service_id) %>% 
          pull() %>%
          unique()
    }
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
    
    select(trip_id, route_id, shape_id, agency)

  weekday.trips <- current.trips %>% select(trip_id) %>% pull
  
  #########################################################################################################
  #########################################################################################################
  ### Weekday Stop Times
  #########################################################################################################
  #########################################################################################################
  current.stop.times <- readr::read_csv(unz("working.zip","stop_times.txt"), 
                                        col_types = list(arrival_time = col_character(),
                                                         departure_time = col_character())) %>%
    mutate(agency=gtfs[[2]]) %>%
    mutate(trip_id = paste0(agency,"_",trip_id)) %>%
    mutate(stop_id = paste0(agency,"_",stop_id)) %>%
    
    # Deal with times coded as past 24hrs for overnight transit
    mutate(arr_secs = ConvertToSeconds(arrival_time)) %>%
    
    mutate(arr_time = case_when(
      arr_secs >= 86400 ~ arr_secs-86400,
      arr_secs < 86400 ~ arr_secs)) %>%
    
    mutate(arr_time = hms::as_hms(arr_time)) %>%
    
    mutate(arr_time = case_when(
      arr_secs >= 86400 ~ paste0(transit.year,"-",transit.month,"-02 ",arr_time),
      arr_secs < 86400 ~ paste0(transit.year,"-",transit.month,"-01 ",arr_time))) %>%
    
    mutate(arr_time = as_datetime(arr_time)) %>%
    
    mutate(arrival_time = arr_time) %>%
    
    filter(trip_id %in% weekday.trips) %>%
    select(trip_id, stop_id, arrival_time, stop_sequence)
    
  current.stop.times <- left_join(current.stop.times, current.trips, by=c("trip_id"))
  
  #########################################################################################################
  #########################################################################################################
  ### Weekday Stop Times from Frequencies
  #########################################################################################################
  #########################################################################################################
  zip.files <- unzip("working.zip", list = TRUE)$Name
  
  if ("frequencies.txt" %in% zip.files) {
  
    current.freq <- readr::read_csv(unz("working.zip","frequencies.txt"), 
                                    col_types = list(start_time = col_character(),
                                                     end_time = col_character()))
    
    freq.rows <- nrow(current.freq)
    
    if(freq.rows >0) {
      
      if(gtfs[[2]]=="ST") {
        current.freq <- current.freq %>% filter(str_detect(trip_id,"WD"))
      }
    
      current.freq <- current.freq %>%
        mutate(agency=gtfs[[2]]) %>%
        mutate(trip_id = paste0(agency,"_",trip_id)) %>%
      
        # Deal Start with times coded as past 24hrs for overnight transit
        mutate(arr_secs = ConvertToSeconds(start_time)) %>%
      
        mutate(arr_time = case_when(
          arr_secs >= 86400 ~ arr_secs-86400,
          arr_secs < 86400 ~ arr_secs)) %>%
      
        mutate(arr_time = hms::as_hms(arr_time)) %>%
      
        mutate(arr_time = case_when(
          arr_secs >= 86400 ~ paste0(transit.year,"-",transit.month,"-02 ",arr_time),
          arr_secs < 86400 ~ paste0(transit.year,"-",transit.month,"-01 ",arr_time))) %>%
      
        mutate(start_time = as_datetime(arr_time)) %>%
      
        # Deal End with times coded as past 24hrs for overnight transit
        mutate(e_secs = ConvertToSeconds(end_time)) %>%
      
        mutate(e_time = case_when(
          e_secs >= 86400 ~ e_secs-86400,
          e_secs < 86400 ~ e_secs)) %>%
      
        mutate(e_time = hms::as_hms(e_time)) %>%
      
        mutate(e_time = case_when(
          e_secs >= 86400 ~ paste0(transit.year,"-",transit.month,"-02 ",e_time),
          e_secs < 86400 ~ paste0(transit.year,"-",transit.month,"-01 ",e_time))) %>%
      
        mutate(end_time = as_datetime(e_time)) %>%
      
        select(-arr_secs, -arr_time, -e_secs, -e_time)
    
      unique.trips <- current.freq %>% select(trip_id) %>% pull() %>% unique()
    
      full.trips.by.frequency <- NULL
    
      for(trips in unique.trips) {
      
        trips.by.frequency <- NULL
      
        t <- current.freq %>% filter(trip_id==trips)
        s <- current.stop.times %>% filter(trip_id==trips)
    
        # Details of Trip Sequence
        num.stops <- s %>% select(stop_sequence) %>% pull() %>% length()
        starting.time <- seconds(s %>% filter(stop_sequence==1) %>% select(arrival_time) %>% pull())
        ending.time <- seconds(s %>% filter(stop_sequence==num.stops) %>% select(arrival_time) %>% pull())
        run.time <- period_to_seconds(ending.time - starting.time)
        stop.to.stop <- run.time/(num.stops-1)
      
        for (row in 1:nrow(t)) {
    
          starting.time <- period_to_seconds(seconds(t[row, "start_time"] %>% pull()))
          ending.time <- period_to_seconds(seconds(t[row, "end_time"] %>% pull()))
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
          start.time <- period_to_seconds(seconds(temp %>% select(arrival_time) %>% pull()))
        
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
        trips.by.frequency <- trips.by.frequency %>% mutate(arrival_time = as_datetime(arrival_time))
      
        ifelse(is.null(full.trips.by.frequency), full.trips.by.frequency <- trips.by.frequency, full.trips.by.frequency <- bind_rows(full.trips.by.frequency,trips.by.frequency))
      
      }
    
      current.stop.times <- bind_rows(current.stop.times, full.trips.by.frequency)
    
      # Remove the original trips listed in stop times
      current.stop.times <- current.stop.times %>%
        filter(!(trip_id %in% unique.trips))
    
      # Generate New Tips to Include in Trips File
      freq.trips <- full.trips.by.frequency %>%
        filter(stop_sequence==1) %>%
        select(trip_id, route_id, shape_id, agency)
    
      current.trips <- bind_rows(current.trips, freq.trips)
    
      # Remove the original trips listed in trips
      current.trips <- current.trips %>%
        filter(!(trip_id %in% unique.trips))
    
      rm(current.freq, end.time, full.trips.by.frequency, i, s, t, temp, trips.by.frequency, freq.trips)  
    
    } # end of check in the file length is greater than 0
    
  } # end of stop frequency if statement
  
  #########################################################################################################
  #########################################################################################################
  ### Shape Points
  #########################################################################################################
  #########################################################################################################
  current.shapes <- readr::read_csv(unz("working.zip","shapes.txt"), show_col_types = FALSE)
  temp <- readr::read_csv(unz("working.zip","trips.txt"), show_col_types = FALSE) %>% select(shape_id,route_id, service_id) %>% distinct()
  current.shapes <- left_join(current.shapes,temp,by=c("shape_id"))
  
  current.shapes <- current.shapes %>%
    filter(service_id %in% weekday.ids) %>%
    mutate(agency=gtfs[[2]]) %>%
    
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
    select(-service_id)

  #########################################################################################################
  #########################################################################################################
  ### Combine into Regional GTFS
  #########################################################################################################
  #########################################################################################################
  
  ifelse(is.null(region.stops), region.stops <- current.stops, region.stops <- bind_rows(region.stops, current.stops))
  ifelse(is.null(region.routes), region.routes <- current.routes, region.routes <- bind_rows(region.routes, current.routes))
  ifelse(is.null(region.trips), region.trips <- current.trips, region.trips <- bind_rows(region.trips, current.trips))
  ifelse(is.null(region.stop.times), region.stop.times <- current.stop.times, region.stop.times <- bind_rows(region.stop.times, current.stop.times))
  ifelse(is.null(region.shapes), region.shapes <- current.shapes, region.shapes <- bind_rows(region.shapes, current.shapes))
  
  file.remove("working.zip")
  rm(current.routes,current.stop.times,current.stops,current.trips, current.shapes, temp)

}

# Add Year and Service Period to Tables
region.stops <- region.stops %>% mutate(year=transit.year, service_change=service.period)
region.routes <- region.routes %>% mutate(year=transit.year, service_change=service.period)
region.trips <- region.trips %>% mutate(year=transit.year, service_change=service.period)
region.stop.times <- region.stop.times %>% mutate(year=transit.year, service_change=service.period, arrival_time=as_datetime(arrival_time))
region.shapes <- region.shapes %>% mutate(year=transit.year, service_change=service.period)

if (output.annual.csv=='yes') {
  fwrite(region.stops, paste0("output/region_stops_",transit.year,".csv"))
  fwrite(region.routes, paste0("output/region_routes_",transit.year,".csv"))
  fwrite(region.trips, paste0("output/region_trips_",transit.year,".csv"))
  fwrite(region.stop.times, paste0("output/region_stoptimes_",transit.year,".csv"))
  fwrite(region.shapes, paste0("output/region_shapes_",transit.year,".csv"))
}
