library(tidyverse)
library(data.table)
library(sf)

# Inputs -------------------------------------------------------------------
output.dir <- file.path('c://model-outputs//rtp2022')
wgs84 <- 4326
spn <- 2285 

# Non-Motorized Network ---------------------------------------------------
nonmotor.layer <- st_read(file.path(output.dir,'spatial','ElmerGeo_BikePed_042021.gdb')) %>% 
  select(ped_complete, bike_complete) %>%
  st_transform(spn) %>%
  mutate(len=st_length(x=Shape))

# Parcels -----------------------------------------------------------------
parcels <- as_tibble(fread(file.path(output.dir,'base','parcels_urbansim.txt'))) %>% select(PARCELID,XCOORD_P,YCOORD_P)
parcel.ids <- parcels %>% select(PARCELID) %>% distinct() %>% pull()

qtr.layer <- st_as_sf(parcels, coords = c("XCOORD_P", "YCOORD_P"), crs = spn) %>% 
  st_transform(spn) %>% 
  st_buffer(dist=0.25*5280)

# Quarter Mile Buffer -----------------------------------------------------
p <- qtr.layer
nm <- nonmotor.layer

parcel.buffers <- NULL
for (i in parcel.ids) {
  print(paste0("Working on Parcel ID ", i))
  c <- p %>% filter(PARCELID == i)
  o <- st_intersection(nm, c) %>% mutate(len=st_length(x=Shape))

  # Summarize for Sidewalks
  s <- o %>% 
    st_drop_geometry() %>%
    select(PARCELID, ped_complete, len) %>%
    group_by(PARCELID, ped_complete) %>%
    summarize(Length_Feet = sum(len)) %>%
    mutate(Facility = "Sidewalk") %>%
    rename(Designation=ped_complete)

  # Summarize for Bike Facilities
  b <- o %>% 
    st_drop_geometry() %>%
    select(PARCELID, bike_complete, len) %>%
    group_by(PARCELID, bike_complete) %>%
    summarize(Length_Feet = sum(len)) %>%
    mutate(Facility = "Bike Facility") %>%
    rename(Designation=bike_complete)

  # Get a Total Length
  t <- o %>% 
    st_drop_geometry() %>%
    select(PARCELID, len) %>%
    group_by(PARCELID) %>%
    summarize(Length_Feet = sum(len)) %>%
    mutate(Facility = "All Facilities") %>%
    mutate(Designation="All Facilities")

  # Combine by Parcel and JOin to Full Parcel List
  tbl <- bind_rows(list(s,b,t)) %>% mutate(Buffer="Quarter Mile")
  
  ifelse(is.null(parcel.buffers), parcel.buffers <- tbl, parcel.buffers <- bind_rows(parcel.buffers, tbl))
}
