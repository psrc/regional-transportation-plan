# Packages used in Analysis -----------------------------------------------
library(odbc)
library(DBI)
library(data.table)

# Basic Inputs ------------------------------------------------------------
service.period <- 'Fall'
years <- c(seq(2015,2020,1))
output.annual.csv <- 'no'
output.combined.csv <- 'yes'

all.stops <- NULL
all.routes <- NULL
all.trips <- NULL
all.stop.times <- NULL
all.shapes <- NULL

for (transit.year in years) {
  
  source("regional-gtfs-from-web.R")
  
  # Combine yearly gtfs files into one file by gtfs file type
  ifelse(is.null(all.stops), all.stops <- region.stops, all.stops <- bind_rows(all.stops,region.stops))
  ifelse(is.null(all.routes), all.routes <- region.routes, all.routes <- bind_rows(all.routes,region.routes))
  ifelse(is.null(all.trips), all.trips <- region.trips, all.trips <- bind_rows(all.trips,region.trips))
  ifelse(is.null(all.stop.times), all.stop.times <- region.stop.times, all.stop.times <- bind_rows(all.stop.times,region.stop.times))
  ifelse(is.null(all.shapes), all.shapes <- region.shapes, all.shapes<- bind_rows(all.shapes,region.shapes))
  
}

rm(region.stops, region.trips, region.routes, region.stop.times, region.shapes, current.freq)

if (output.combined.csv=='yes') {
  fwrite(all.stops, paste0("output/region_stops_",min(years),"_to_",max(years),".csv"))
  fwrite(all.routes, paste0("output/region_routes_",min(years),"_to_",max(years),".csv"))
  fwrite(all.trips, paste0("output/region_trips_",min(years),"_to_",max(years),".csv"))
  fwrite(all.stop.times, paste0("output/region_stoptimes_",min(years),"_to_",max(years),".csv"))
  fwrite(all.shapes, paste0("output/region_shapes_",min(years),"_to_",max(years),".csv"))
}