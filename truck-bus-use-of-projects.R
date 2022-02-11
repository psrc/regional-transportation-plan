library(tidyverse)
library(data.table)
library(sf)

# General Inputs ----------------------------------------------------------
network.outputs.file <- '//data/network_results.csv'
md.shape <- "//data//MD_edges.shp"
wgs84 <- 4326
spn <- 2285 

network.outputs <- as_tibble(fread(file.path(getwd(),"/data/network_results.csv")))
network.links <- st_read(file.path(file.path(getwd(),md.shape)))
segment.outputs <- as_tibble(fread(file.path(getwd(),"/data/transit_segment_results.csv")))

transit.segments <- segment.outputs %>%
  mutate(id=paste0(i_node,"-",j_node)) %>%
  select(id, segment_volume) %>%
  group_by(id) %>%
  summarize(transit_passenger_volume=sum(segment_volume))

network.summary <- network.links %>%
  st_drop_geometry() %>%
  select(id, length, lanes, vdf, projRteID, FGTS) %>%
  filter(!(vdf%in%c(9,10,31,40)))

network.summary <- left_join(network.summary, transit.segments, by=c("id")) %>%
  mutate(across(everything(), ~replace_na(.x, 0)))

n <- network.outputs %>%
  mutate(truck_vmt = (`@mveh`+`@hveh`)*length, total_vmt = (`@tveh`)*length) %>%
  select(ij, truck_vmt, total_vmt) %>%
  mutate(across(everything(), ~replace_na(.x, 0))) %>%
  group_by(ij) %>%
  summarize(truck_vmt=sum(truck_vmt), total_vmt=sum(total_vmt))
  
network.summary <- left_join(network.summary, n, by=c("id"="ij")) %>%
  mutate(across(everything(), ~replace_na(.x, 0))) %>%
  mutate(transit_pmt = transit_passenger_volume * length)

total.vmt <- network.summary %>% select(total_vmt) %>% pull() %>% sum()
truck.vmt <- network.summary %>% select(truck_vmt) %>% pull() %>% sum()
transit.pmt <- network.summary %>% select(transit_pmt) %>% pull() %>% sum()

truck.vmt.projects <- network.summary %>% filter(projRteID>0) %>% select(truck_vmt) %>% pull() %>% sum()
truck.vmt.projects.shr <- truck.vmt.projects / truck.vmt

truck.vmt.fgts <- network.summary %>% filter(FGTS>0) %>% select(truck_vmt) %>% pull() %>% sum()
truck.vmt.fgts.shr <- truck.vmt.fgts / truck.vmt

bus.pmt.projects <- network.summary %>% filter(projRteID>0) %>% select(transit_pmt) %>% pull() %>% sum()
bus.pmt.projects.shr <- bus.pmt.projects / transit.pmt
