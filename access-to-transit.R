# Packages for Data Wrangling
library(tidyverse)
library(here)
library(data.table)

# Packages for Maps
library(sf)
library(leaflet)

# Packages for Database Connections
library(odbc)
library(DBI)

# Inputs ------------------------------------------------------------------
analysis.year <- 2018
census.year <- 2018

wgs84 <- 4326
spn <- 2285 
hct.buffer.dist <- 0.5
bus.buffer.dist <- 0.25

transit.url <- 'X:/DSA/GTFS/spring/2018/'
server_name <- 'AWS-PROD-SQL\\SOCKEYE'
database_name <- 'Elmer'

process.block.nonmotorized <- 'no'
run.hct.stop.overlay <- 'no'
run.bus.stop.overlay <- 'no'
run.block.overlay <- 'no'
run.hct.network.overlay <- 'no'
run.transit.network.overlay <- 'no'

# Lists -------------------------------------------------------------------
disabled_query <- paste0("SELECT geoid, percent_disabled, equity_geog_vs_reg_total FROM Census.disability_equity_geographies(",census.year,",'Tract')")
elderly_query <- paste0("SELECT geoid, percent_elderly, equity_geog_vs_reg_total FROM Census.elderly_equity_geographies(",census.year,",'Tract')")
youth_query <- paste0("SELECT geoid, percent_youth, equity_geog_vs_reg_total FROM Census.youth_equity_geographies(",census.year,",'Tract')")
lep_query <- paste0("SELECT geoid, percent_limited_english, equity_geog_vs_reg_total FROM Census.limited_english_equity_geographies(",census.year,",'Tract')")
poverty_query <- paste0("SELECT geoid, percent_in_poverty, equity_geog_vs_reg_total FROM Census.poverty_equity_geographies(",census.year,",'Tract')")
people_of_color_query <- paste0("SELECT geoid, percent_of_color, equity_geog_vs_reg_total FROM Census.racial_equity_geographies(",census.year,",'Tract')")

brt.constrained <- c('69930699','86386337','059d725b','0e632d9b','1f4e7147','2819732c','2b10424c','2d969405','35d8b755','3ac667df',
                     '3b7a72ab','4551b28a','54daeb98','604f7a45','628fdd6e','64335d82','64b42288','652b10f6','78bfd309','858586eb',
                     '8cd15f5d','8ced6ebb','9383b962','9d2a97f3','9e423688','af734d73','b19647c7','b5fe11d9','b7b2e507','d06fcd19',
                     'd10ce9f2','d5b2b21b','d5cac403','d7a11731','dac342ce','f49ff308',
                     'ct_701','ct_702',
                     'kcm_100512', 'kcm_102548', 'kcm_102576', 'kcm_102581', 'kcm_102615', 'kcm_102619')

brt.unconstrained <- c('8fd32294','927d06ce','a85c881c','b073cfd8','c7599818')

brt.full.plan <- c(brt.constrained,brt.unconstrained)

pof.routes <- c('838df28c','fe6cc15a','4ddaf1d8','d1a584e2','7c6aed77','f09e3012','97bdbd4c','94f23b4b','213ab30d','bb27165f',
                     'kcm_100336', 'kcm_100337',
                     'kt_Ferry', 'kt_Annapolis', 'kt_Kitsap Fast Ferry')

ferry.routes <- c('WSF2100','WSF2101','WSF2102', 'WSF2103', 'WSF2104','WSF2105', 'WSF2106','WSF2107',
                       'PCF_Anderson_Ketron', 'PCF_Steilacoom_Ketron', 'PCF_Steilacoom-Anderson')

srt.routes <- c('04663955', '6d393241', '9fda70a6',
                     'kcm_100340', 'kcm_102638')

monorail.routes <- c('8e776757')

crt.routes <- c('d0c48cae', 'acaa2738',
                     'st_SNDR_EV','st_SNDR_TL','st_AMTK',
                     'st_a-AMTK', 'st_a-SNDR_EV', 'st_a-SNDR_TL')

lrt.routes <- c('961fa992','edb59098','a100f6d5','d41e3912','c5ad7b63',
                     'kcm_100479', 'st_TLINK', 'st_100479', 'st_a-TLINK')

hct.modes <- c("Light Rail", "Streetcar", "Monorail", "Commuter Rail", "BRT", "Passenger Only Ferry")

bus.modes <- c("Bus")

# Functions ---------------------------------------------------------------
create.stops.buffer <- function(f, r, d, t) {
  
  # Load Routes and filter to Route Types for Analysis
  routes <- as_tibble(fread(file.path(paste0(f,'routes.txt')))) %>% 
    select(route_id) %>%
    mutate(typology = "Bus") %>%
    mutate(typology = case_when(
      route_id %in% srt.routes ~ "Streetcar",
      route_id %in% lrt.routes ~ "Light Rail",
      route_id %in% monorail.routes ~ "Monorail",
      route_id %in% crt.routes ~ "Commuter Rail",
      route_id %in% ferry.routes ~ "Ferry",
      route_id %in% pof.routes ~ "Passenger Only Ferry",
      route_id %in% brt.constrained ~ "BRT")) %>%
    mutate(across(typology, ~replace_na(.x, "Bus"))) %>%
    filter(typology %in% r)
  
  # Load Trips and Filter to Route Types for Analysis
  trips <- as_tibble(fread(file.path(paste0(f,'trips.txt')))) %>% select(route_id,trip_id)
  trips <- left_join(trips, routes, by=c("route_id")) %>% drop_na()
  
  # Load Stop Times and Filter to Route Types for Analysis to get list of Stops for further analysis
  stoptimes <- as_tibble(fread(file.path(paste0(f,'stop_times.txt')))) %>% select(trip_id,stop_id)
  stoptimes <- left_join(stoptimes, trips, by=c("trip_id")) 
  stop.list <- stoptimes %>% drop_na() %>% select(stop_id) %>% distinct() %>% pull()
  
  # Load Stops and Filter to Route Types for Analysis
  stops <- as_tibble(fread(file.path(paste0(f,'stops.txt')))) %>% select(stop_id,stop_lat,stop_lon) %>% filter(stop_id %in% stop.list)
  
  # Create Stops Layer for further spatial analysis
  stops.layer <- st_as_sf(stops, coords = c("stop_lon", "stop_lat"), crs = wgs84) %>% 
    st_transform(spn) %>% 
    st_buffer(dist=d*5280) %>%
    st_union() %>%
    st_sf() %>%
    mutate(station_area=t)
  
  return(stops.layer)
  
}

create.stops.buffer.no.dissolve <- function(f, r, d, t) {
  
  # Load Routes and filter to Route Types for Analysis
  routes <- as_tibble(fread(file.path(paste0(f,'routes.txt')))) %>% 
    select(route_id) %>%
    mutate(typology = "Bus") %>%
    mutate(typology = case_when(
      route_id %in% srt.routes ~ "Streetcar",
      route_id %in% lrt.routes ~ "Light Rail",
      route_id %in% monorail.routes ~ "Monorail",
      route_id %in% crt.routes ~ "Commuter Rail",
      route_id %in% ferry.routes ~ "Ferry",
      route_id %in% pof.routes ~ "Passenger Only Ferry",
      route_id %in% brt.constrained ~ "BRT")) %>%
    mutate(across(typology, ~replace_na(.x, "Bus"))) %>%
    filter(typology %in% r)
  
  # Load Trips and Filter to Route Types for Analysis
  trips <- as_tibble(fread(file.path(paste0(f,'trips.txt')))) %>% select(route_id,trip_id)
  trips <- left_join(trips, routes, by=c("route_id")) %>% drop_na()
  
  # Load Stop Times and Filter to Route Types for Analysis to get list of Stops for further analysis
  stoptimes <- as_tibble(fread(file.path(paste0(f,'stop_times.txt')))) %>% select(trip_id,stop_id)
  stoptimes <- left_join(stoptimes, trips, by=c("trip_id")) 
  stop.list <- stoptimes %>% drop_na() %>% select(stop_id) %>% distinct() %>% pull()
  
  # Load Stops and Filter to Route Types for Analysis
  stops <- as_tibble(fread(file.path(paste0(f,'stops.txt')))) %>% select(stop_id,stop_lat,stop_lon) %>% filter(stop_id %in% stop.list)
  
  # Create Stops Layer for further spatial analysis
  stops.layer <- st_as_sf(stops, coords = c("stop_lon", "stop_lat"), crs = wgs84) %>% 
    st_transform(spn) %>% 
    st_buffer(dist=d*5280) %>%
    mutate(station_area=t)
  
  return(stops.layer)
  
}

# Census Block Data -------------------------------------------------------
if(run.block.overlay == 'yes') {

  tract.layer <- st_read("https://services6.arcgis.com/GWxg6t7KXELn1thE/arcgis/rest/services/tract2010_nowater/FeatureServer/0/query?where=0=0&outFields=*&f=pgeojson") %>% 
    st_transform(spn) %>%
    select(GEOID10) %>%
    rename(geoid=GEOID10)

  db_con <- dbConnect(odbc::odbc(),
                    driver = "SQL Server",
                    server = server_name,
                    database = database_name,
                    trusted_connection = "yes")

  tract.disability <- as_tibble(DBI::dbGetQuery(db_con, disabled_query)) %>% rename(disabled = equity_geog_vs_reg_total)
  tract.elderly <- as_tibble(DBI::dbGetQuery(db_con, elderly_query)) %>% rename(elderly=equity_geog_vs_reg_total)
  tract.limited.english <- as_tibble(DBI::dbGetQuery(db_con, lep_query)) %>% rename(limited_english=equity_geog_vs_reg_total)
  tract.poverty <- as_tibble(DBI::dbGetQuery(db_con, poverty_query)) %>% rename(poverty=equity_geog_vs_reg_total)
  tract.people.of.color <- as_tibble(DBI::dbGetQuery(db_con, people_of_color_query)) %>%  rename(people_of_color=equity_geog_vs_reg_total)
  tract.youth <- as_tibble(DBI::dbGetQuery(db_con, youth_query)) %>% rename(youth=equity_geog_vs_reg_total)

  tract.layer <- list(tract.layer, tract.disability, tract.elderly, tract.limited.english, tract.poverty, tract.people.of.color, tract.youth) %>% 
    reduce(left_join, by = "geoid") %>%
    mutate(across(everything(), ~replace_na(.x, 0)))

  rm(tract.disability, tract.elderly, tract.limited.english, tract.poverty, tract.people.of.color, tract.youth)

  geodatabase.server <- "AWS-PROD-SQL\\Sockeye"
  geodatabase.name <- "ElmerGeo"
  
  gdb.nm <- paste0("MSSQL:server=",geodatabase.server,";database=",geodatabase.name,";trusted_connection=yes")

  block.layer <- st_read(gdb.nm, "dbo.block2010", crs = spn) %>% 
    st_transform(spn) %>%
    select(geoid10) %>%
    rename(block_geoid=geoid10)

  tract.data <- tract.layer %>% st_drop_geometry()
  ofm.facts <- as_tibble(dbReadTable(db_con, SQL("ofm.v_current_block_estimates"))) 
  dbDisconnect(db_con)

  ofm.facts <- ofm.facts %>% 
    filter(estimate_year==analysis.year) %>%
    mutate(total_population = household_population + group_quarters_population) %>%
    rename(year=estimate_year) %>%
    mutate(tract_geoid=substr(block_geoid, 1, 11))

  ofm.facts <-left_join(ofm.facts, tract.data, by=c("tract_geoid"="geoid"))
  block.layer <-left_join(block.layer, ofm.facts, by=c("block_geoid")) %>% select(-tract_geoid) %>% mutate(across(everything(), ~replace_na(.x, 0)))
  rm(ofm.facts, tract.data, tract.layer)

  block.layer <- block.layer %>%
    mutate(disabled_population = round(total_population * percent_disabled,0)) %>%
    mutate(elderly_population = round(total_population * percent_elderly,0)) %>%
    mutate(low_income_population = round(total_population * percent_in_poverty,0)) %>%
    mutate(limited_english_population = round(total_population * percent_limited_english,0)) %>%
    mutate(people_of_color_population = round(total_population * percent_of_color,0)) %>%
    mutate(youth_population = round(total_population * percent_youth,0)) %>%
    mutate(total_population = round(total_population,0))

  temp <- block.layer %>% st_drop_geometry()

  block_points <- block.layer %>% select(block_geoid) %>% st_centroid() %>% st_buffer(dist=hct.buffer.dist*5280)
  block.points <- block.layer %>% select(block_geoid) %>% st_centroid()
  fwrite(temp, here('output','blocks_with_pop_equity.csv'))
  rm(temp)
} else {
  block.layer <- fread(here('output','blocks_with_pop_equity.csv')) %>% mutate(block_geoid = as.character(block_geoid))
}

# Sidewalk and Bike Lane Coverage by Census Block -------------------------
if (process.block.nonmotorized=='yes') {
  block.list <- block_points %>% st_drop_geometry() %>% pull()
  block.nonmotorized.summary <- NULL
  i <- 1
  
  bike.lane.codes <- list ("No Facilities" = 0,
                           "Striped Bike Lane" = 1,
                           "Protected Bike Lanes" = 2,
                           "Paved/Striped Shoulder" = 3,
                           "Marked Shared Lane" = 4,
                           "Defined Bike Route No Provisions" = 6,
                           "Shared Use Path" = 8,
                           "Buffered Bike Lane" = 9)
  
  nonmotorized.layer <- st_read(here('data','ElmerGeo_BikePed_042021.gdb')) %>% 
    mutate(bike_wo_shoulder = case_when(
      ij_bike_type %in% c(1,2,4,6,8,9) & ji_bike_type %in% c(1,2,4,6,8,9) ~ "Complete Facilities",
      ij_bike_type %in% c(1,2,4,6,8,9) | ji_bike_type %in% c(1,2,4,6,8,9) ~ "Partial Facilities",
      ij_bike_type %in% c(0,3) & ji_bike_type %in% c(0,3) ~ "No Facilities")) %>%
    select(ped_complete, full_name, bike_complete, bike_wo_shoulder) %>%
    st_transform(spn) %>%
    mutate(len=st_length(x=Shape))

  for (b in block.list) {

    print(paste0("Working on Block # ", i, " of ", length(block.list)," blocks"))
    bl <- block_points %>% filter(block_geoid %in% b)
    bln <- st_intersection(nonmotorized.layer, bl)
    bln <- bln %>% mutate(len=st_length(x=Shape))

    tot <- bln %>% st_drop_geometry() %>% select(len) %>% pull() %>% sum()

    full.sw <- bln %>% st_drop_geometry() %>% filter(ped_complete=="Complete Facilities") %>% select(len) %>% pull() %>% sum()
    partial.sw <- bln %>% st_drop_geometry() %>% filter(ped_complete=="Partial Facilities") %>% select(len) %>% pull() %>% sum()
    no.sw <- bln %>% st_drop_geometry() %>% filter(ped_complete=="No Facilities") %>% select(len) %>% pull() %>% sum()

    full.bk <- bln %>% st_drop_geometry() %>% filter(bike_complete=="Complete Facilities") %>% select(len) %>% pull() %>% sum()
    partial.bk <- bln %>% st_drop_geometry() %>% filter(bike_complete=="Partial Facilities") %>% select(len) %>% pull() %>% sum()
    no.bk <- bln %>% st_drop_geometry() %>% filter(bike_complete=="No Facilities") %>% select(len) %>% pull() %>% sum()

    full.bk.shd <- bln %>% st_drop_geometry() %>% filter(bike_wo_shoulder=="Complete Facilities") %>% select(len) %>% pull() %>% sum()
    partial.bk.shd <- bln %>% st_drop_geometry() %>% filter(bike_wo_shoulder=="Partial Facilities") %>% select(len) %>% pull() %>% sum()
    no.bk.shd <- bln %>% st_drop_geometry() %>% filter(bike_wo_shoulder=="No Facilities") %>% select(len) %>% pull() %>% sum()

    temp <- as_tibble(data.table(`block_geoid`=b, `total_length`=tot, 
                             `full_sidewalk_length`=full.sw, `partial_sidewalk_length`=partial.sw, `no_sidewalk_length`=no.sw,
                             `full_bike_length`=full.bk, `partial_bike_length`=partial.bk, `no_bike_length`=no.bk,
                             `full_bike_shld_length`=full.bk.shd, `partial_bike_shld_length`=partial.bk.shd, `no_bike_shld_length`=no.bk.shd))

    ifelse(is.null(block.nonmotorized.summary), block.nonmotorized.summary <- temp, block.nonmotorized.summary <- bind_rows(block.nonmotorized.summary, temp))
    i <- i + 1
  }

  fwrite(block.nonmotorized.summary, here('output','blocks_with_nonmotorized.csv'))
} else {
  block.nonmotorized.summary <- fread(here('output','blocks_with_nonmotorized.csv')) %>% mutate(block_geoid = as.character(block_geoid))
}

# Blocks with HCT ---------------------------------------------------------
if(run.hct.stop.overlay == 'yes') {
  hct.stops <- create.stops.buffer(f=transit.url, r=hct.modes, d=hct.buffer.dist, t="High Capacity Transit")
  temp <- st_intersection(hct.stops, block.points) %>% st_drop_geometry()
  block.hct.summary <- left_join(block.points, temp, by=c("block_geoid")) %>%
    rename(hct=station_area) %>% 
    st_drop_geometry() %>%
    mutate(across(hct, ~replace_na(.x, "Not HCT")))
  fwrite(block.hct.summary, here('output','blocks_with_hct.csv'))
} else {
  block.hct.summary <- fread(here('output','blocks_with_hct.csv')) %>% mutate(block_geoid = as.character(block_geoid))
}

# Blocks with Non-HCT Transit ---------------------------------------------------------
if(run.bus.stop.overlay == 'yes') {
  bus.stops <- create.stops.buffer(f=transit.url, r=bus.modes, d=bus.buffer.dist, t="Bus")
  temp <- st_intersection(bus.stops, block.points) %>% st_drop_geometry()
  block.bus.summary <- left_join(block.points, temp, by=c("block_geoid")) %>%
    rename(non_hct=station_area) %>% 
    st_drop_geometry() %>%
    mutate(across(non_hct, ~replace_na(.x, "No Non-HCT Transit")))
  fwrite(block.bus.summary, here('output','blocks_with_nonhct.csv'))
} else {
  block.bus.summary <- fread(here('output','blocks_with_nonhct.csv')) %>% mutate(block_geoid = as.character(block_geoid))
}

# Analysis of Population with access to Transit -------------------------------
combined.data <- block.layer %>%
  mutate(county = substr(block_geoid, 3, 5))

combined.data <- list(combined.data, block.bus.summary, block.hct.summary, block.nonmotorized.summary) %>% 
  reduce(left_join, by = "block_geoid") %>%
  mutate(across(everything(), ~replace_na(.x, 0)))

total.population <- combined.data %>% select(total_population) %>% pull() %>% sum()
king.population <- combined.data %>% filter(county=="033") %>% select(total_population) %>% pull() %>% sum()
kitsap.population <- combined.data %>% filter(county=="035") %>% select(total_population) %>% pull() %>% sum()
pierce.population <- combined.data %>% filter(county=="053") %>% select(total_population) %>% pull() %>% sum()
snohomish.population <- combined.data %>% filter(county=="061") %>% select(total_population) %>% pull() %>% sum()
poc.population <- combined.data %>% select(people_of_color_population) %>% pull() %>% sum()
pov.population <- combined.data %>% select(low_income_population) %>% pull() %>% sum()
dis.population <- combined.data %>% select(disabled_population) %>% pull() %>% sum()
eld.population <- combined.data %>% select(elderly_population) %>% pull() %>% sum()
lep.population <- combined.data %>% select(limited_english_population) %>% pull() %>% sum()
you.population <- combined.data %>% select(youth_population) %>% pull() %>% sum()

total.population.shr <- total.population / total.population
king.population.shr <- king.population / total.population
kitsap.population.shr <- kitsap.population / total.population
pierce.population.shr <- pierce.population / total.population
snohomish.population.shr <- snohomish.population / total.population
poc.population.shr <- poc.population / total.population
pov.population.shr <- pov.population / total.population
dis.population.shr <- dis.population / total.population
eld.population.shr <- eld.population / total.population
lep.population.shr <- lep.population / total.population
you.population.shr <- you.population / total.population

# Near HCT
total.population.near.hct <- combined.data %>% filter(hct=="High Capacity Transit") %>% select(total_population) %>% pull() %>% sum()
total.population.near.hct.share <- total.population.near.hct / total.population

king.population.near.hct <- combined.data %>% filter(hct=="High Capacity Transit" & county=="033") %>% select(total_population) %>% pull() %>% sum()
king.population.near.hct.share <- king.population.near.hct / king.population

kitsap.population.near.hct <- combined.data %>% filter(hct=="High Capacity Transit" & county=="035") %>% select(total_population) %>% pull() %>% sum()
kitsap.population.near.hct.share <- kitsap.population.near.hct / kitsap.population

pierce.population.near.hct <- combined.data %>% filter(hct=="High Capacity Transit" & county=="053") %>% select(total_population) %>% pull() %>% sum()
pierce.population.near.hct.share <- pierce.population.near.hct / pierce.population

snohomish.population.near.hct <- combined.data %>% filter(hct=="High Capacity Transit" & county=="061") %>% select(total_population) %>% pull() %>% sum()
snohomish.population.near.hct.share <- snohomish.population.near.hct / snohomish.population

poc.population.near.hct <- combined.data %>% filter(hct=="High Capacity Transit") %>% select(people_of_color_population) %>% pull() %>% sum()
poc.population.near.hct.share <- poc.population.near.hct / poc.population

pov.population.near.hct <- combined.data %>% filter(hct=="High Capacity Transit") %>% select(low_income_population) %>% pull() %>% sum()
pov.population.near.hct.share <- pov.population.near.hct / pov.population

dis.population.near.hct <- combined.data %>% filter(hct=="High Capacity Transit") %>% select(disabled_population) %>% pull() %>% sum()
dis.population.near.hct.share <- dis.population.near.hct / dis.population

eld.population.near.hct <- combined.data %>% filter(hct=="High Capacity Transit") %>% select(elderly_population) %>% pull() %>% sum()
eld.population.near.hct.share <- eld.population.near.hct / eld.population

lep.population.near.hct <- combined.data %>% filter(hct=="High Capacity Transit") %>% select(limited_english_population) %>% pull() %>% sum()
lep.population.near.hct.share <- lep.population.near.hct / lep.population

you.population.near.hct <- combined.data %>% filter(hct=="High Capacity Transit") %>% select(youth_population) %>% pull() %>% sum()
you.population.near.hct.share <- you.population.near.hct / you.population

# Near Transit
total.population.near.bus <- combined.data %>% filter(non_hct=="Bus") %>% select(total_population) %>% pull() %>% sum()
total.population.near.bus.share <- total.population.near.bus / total.population

king.population.near.bus <- combined.data %>% filter(non_hct=="Bus" & county=="033") %>% select(total_population) %>% pull() %>% sum()
king.population.near.bus.share <- king.population.near.bus / king.population

kitsap.population.near.bus <- combined.data %>% filter(non_hct=="Bus" & county=="035") %>% select(total_population) %>% pull() %>% sum()
kitsap.population.near.bus.share <- kitsap.population.near.bus / kitsap.population

pierce.population.near.bus <- combined.data %>% filter(non_hct=="Bus" & county=="053") %>% select(total_population) %>% pull() %>% sum()
pierce.population.near.bus.share <- pierce.population.near.bus / pierce.population

snohomish.population.near.bus <- combined.data %>% filter(non_hct=="Bus" & county=="061") %>% select(total_population) %>% pull() %>% sum()
snohomish.population.near.bus.share <- snohomish.population.near.bus / snohomish.population

poc.population.near.bus <- combined.data %>% filter(non_hct=="Bus") %>% select(people_of_color_population) %>% pull() %>% sum()
poc.population.near.bus.share <- poc.population.near.bus / poc.population

pov.population.near.bus <- combined.data %>% filter(non_hct=="Bus") %>% select(low_income_population) %>% pull() %>% sum()
pov.population.near.bus.share <- pov.population.near.bus / pov.population

dis.population.near.bus <- combined.data %>% filter(non_hct=="Bus") %>% select(disabled_population) %>% pull() %>% sum()
dis.population.near.bus.share <- dis.population.near.bus / dis.population

eld.population.near.bus <- combined.data %>% filter(non_hct=="Bus") %>% select(elderly_population) %>% pull() %>% sum()
eld.population.near.bus.share <- eld.population.near.bus / eld.population

lep.population.near.bus <- combined.data %>% filter(non_hct=="Bus") %>% select(limited_english_population) %>% pull() %>% sum()
lep.population.near.bus.share <- lep.population.near.bus / lep.population

you.population.near.bus <- combined.data %>% filter(non_hct=="Bus") %>% select(youth_population) %>% pull() %>% sum()
you.population.near.bus.share <- you.population.near.bus / you.population

population.near.transit.table <- data.table(
  `Geography` = c("Region", 
                  "King County",
                  "Kitsap County",
                  "Pierce County",
                  "Snohomish County",
                  "People with a Disability", 
                  "People over 65", 
                  "People with Limited English Proficiency", 
                  "People of Color", 
                  "People in Poverty", 
                  "People under 18"),
  `Population` = c(total.population,
                   king.population,
                   kitsap.population,
                   pierce.population,
                   snohomish.population,
                  dis.population,
                  eld.population,
                  lep.population,
                  poc.population,
                  pov.population,
                  you.population),
  `Share of Population` = c(total.population.shr,
                            king.population.shr,
                            kitsap.population.shr,
                            pierce.population.shr,
                            snohomish.population.shr,
                            dis.population.shr,
                            eld.population.shr,
                            lep.population.shr,
                            poc.population.shr,
                            pov.population.shr,
                            you.population.shr),
  `Share of Population within 1/4 mile of any Transit` = c(total.population.near.bus.share,
                                                           king.population.near.bus.share,
                                                           kitsap.population.near.bus.share,
                                                           pierce.population.near.bus.share,
                                                           snohomish.population.near.bus.share,
                                                          dis.population.near.bus.share,
                                                          eld.population.near.bus.share,
                                                          lep.population.near.bus.share,
                                                          poc.population.near.bus.share,
                                                          pov.population.near.bus.share,
                                                          you.population.near.bus.share),
  `Share of Population within 1/2 mile of High Capacity Transit` = c(total.population.near.hct.share,
                                                                     king.population.near.hct.share,
                                                                     kitsap.population.near.hct.share,
                                                                     pierce.population.near.hct.share,
                                                                     snohomish.population.near.hct.share,
                                                                    dis.population.near.hct.share,
                                                                    eld.population.near.hct.share,
                                                                    lep.population.near.hct.share,
                                                                    poc.population.near.hct.share,
                                                                    pov.population.near.hct.share,
                                                                    you.population.near.hct.share)
)

fwrite(population.near.transit.table, here('output','people_near_transit.csv'))

# Analysis of Population with Sidewalk Facilities --------------------

# Population that has Full Sidewalks within 1/2 mile of their home location
total.population.sw.full.25 <- combined.data %>% filter(full_sidewalk_length/total_length>=0.25) %>% select(total_population) %>% pull() %>% sum()
total.population.sw.full.50 <- combined.data %>% filter(full_sidewalk_length/total_length>=0.50) %>% select(total_population) %>% pull() %>% sum()
total.population.sw.full.75 <- combined.data %>% filter(full_sidewalk_length/total_length>=0.75) %>% select(total_population) %>% pull() %>% sum()
total.population.sw.full.100 <- combined.data %>% filter(full_sidewalk_length/total_length>=1) %>% select(total_population) %>% pull() %>% sum()
total.population.sw.full.25.share <- total.population.sw.full.25 / total.population
total.population.sw.full.50.share <- total.population.sw.full.50 / total.population
total.population.sw.full.75.share <- total.population.sw.full.75 / total.population
total.population.sw.full.100.share <- total.population.sw.full.100 / total.population

king.population.sw.full.25 <- combined.data %>% filter(full_sidewalk_length/total_length>=0.25 & county=="033") %>% select(total_population) %>% pull() %>% sum()
king.population.sw.full.50 <- combined.data %>% filter(full_sidewalk_length/total_length>=0.50 & county=="033") %>% select(total_population) %>% pull() %>% sum()
king.population.sw.full.75 <- combined.data %>% filter(full_sidewalk_length/total_length>=0.75 & county=="033") %>% select(total_population) %>% pull() %>% sum()
king.population.sw.full.100 <- combined.data %>% filter(full_sidewalk_length/total_length>=1 & county=="033") %>% select(total_population) %>% pull() %>% sum()
king.population.sw.full.25.share <- king.population.sw.full.25 / king.population
king.population.sw.full.50.share <- king.population.sw.full.50 / king.population
king.population.sw.full.75.share <- king.population.sw.full.75 / king.population
king.population.sw.full.100.share <- king.population.sw.full.100 / king.population

kitsap.population.sw.full.25 <- combined.data %>% filter(full_sidewalk_length/total_length>=0.25 & county=="035") %>% select(total_population) %>% pull() %>% sum()
kitsap.population.sw.full.50 <- combined.data %>% filter(full_sidewalk_length/total_length>=0.50 & county=="035") %>% select(total_population) %>% pull() %>% sum()
kitsap.population.sw.full.75 <- combined.data %>% filter(full_sidewalk_length/total_length>=0.75 & county=="035") %>% select(total_population) %>% pull() %>% sum()
kitsap.population.sw.full.100 <- combined.data %>% filter(full_sidewalk_length/total_length>=1 & county=="035") %>% select(total_population) %>% pull() %>% sum()
kitsap.population.sw.full.25.share <- kitsap.population.sw.full.25 / kitsap.population
kitsap.population.sw.full.50.share <- kitsap.population.sw.full.50 / kitsap.population
kitsap.population.sw.full.75.share <- kitsap.population.sw.full.75 / kitsap.population
kitsap.population.sw.full.100.share <- kitsap.population.sw.full.100 / kitsap.population

pierce.population.sw.full.25 <- combined.data %>% filter(full_sidewalk_length/total_length>=0.25 & county=="053") %>% select(total_population) %>% pull() %>% sum()
pierce.population.sw.full.50 <- combined.data %>% filter(full_sidewalk_length/total_length>=0.50 & county=="053") %>% select(total_population) %>% pull() %>% sum()
pierce.population.sw.full.75 <- combined.data %>% filter(full_sidewalk_length/total_length>=0.75 & county=="053") %>% select(total_population) %>% pull() %>% sum()
pierce.population.sw.full.100 <- combined.data %>% filter(full_sidewalk_length/total_length>=1 & county=="053") %>% select(total_population) %>% pull() %>% sum()
pierce.population.sw.full.25.share <- pierce.population.sw.full.25 / pierce.population
pierce.population.sw.full.50.share <- pierce.population.sw.full.50 / pierce.population
pierce.population.sw.full.75.share <- pierce.population.sw.full.75 / pierce.population
pierce.population.sw.full.100.share <- pierce.population.sw.full.100 / pierce.population

snohomish.population.sw.full.25 <- combined.data %>% filter(full_sidewalk_length/total_length>=0.25 & county=="061") %>% select(total_population) %>% pull() %>% sum()
snohomish.population.sw.full.50 <- combined.data %>% filter(full_sidewalk_length/total_length>=0.50 & county=="061") %>% select(total_population) %>% pull() %>% sum()
snohomish.population.sw.full.75 <- combined.data %>% filter(full_sidewalk_length/total_length>=0.75 & county=="061") %>% select(total_population) %>% pull() %>% sum()
snohomish.population.sw.full.100 <- combined.data %>% filter(full_sidewalk_length/total_length>=1 & county=="061") %>% select(total_population) %>% pull() %>% sum()
snohomish.population.sw.full.25.share <- snohomish.population.sw.full.25 / snohomish.population
snohomish.population.sw.full.50.share <- snohomish.population.sw.full.50 / snohomish.population
snohomish.population.sw.full.75.share <- snohomish.population.sw.full.75 / snohomish.population
snohomish.population.sw.full.100.share <- snohomish.population.sw.full.100 / snohomish.population

poc.population.sw.full.25 <- combined.data %>% filter(full_sidewalk_length/total_length>=0.25) %>% select(people_of_color_population) %>% pull() %>% sum()
poc.population.sw.full.50 <- combined.data %>% filter(full_sidewalk_length/total_length>=0.50) %>% select(people_of_color_population) %>% pull() %>% sum()
poc.population.sw.full.75 <- combined.data %>% filter(full_sidewalk_length/total_length>=0.75) %>% select(people_of_color_population) %>% pull() %>% sum()
poc.population.sw.full.100 <- combined.data %>% filter(full_sidewalk_length/total_length>=1) %>% select(people_of_color_population) %>% pull() %>% sum()
poc.population.sw.full.25.share <- poc.population.sw.full.25 / poc.population
poc.population.sw.full.50.share <- poc.population.sw.full.50 / poc.population
poc.population.sw.full.75.share <- poc.population.sw.full.75 / poc.population
poc.population.sw.full.100.share <- poc.population.sw.full.100 / poc.population

pov.population.sw.full.25 <- combined.data %>% filter(full_sidewalk_length/total_length>=0.25) %>% select(low_income_population) %>% pull() %>% sum()
pov.population.sw.full.50 <- combined.data %>% filter(full_sidewalk_length/total_length>=0.50) %>% select(low_income_population) %>% pull() %>% sum()
pov.population.sw.full.75 <- combined.data %>% filter(full_sidewalk_length/total_length>=0.75) %>% select(low_income_population) %>% pull() %>% sum()
pov.population.sw.full.100 <- combined.data %>% filter(full_sidewalk_length/total_length>=1) %>% select(low_income_population) %>% pull() %>% sum()
pov.population.sw.full.25.share <- pov.population.sw.full.25 / pov.population
pov.population.sw.full.50.share <- pov.population.sw.full.50 / pov.population
pov.population.sw.full.75.share <- pov.population.sw.full.75 / pov.population
pov.population.sw.full.100.share <- pov.population.sw.full.100 / pov.population

dis.population.sw.full.25 <- combined.data %>% filter(full_sidewalk_length/total_length>=0.25) %>% select(disabled_population) %>% pull() %>% sum()
dis.population.sw.full.50 <- combined.data %>% filter(full_sidewalk_length/total_length>=0.50) %>% select(disabled_population) %>% pull() %>% sum()
dis.population.sw.full.75 <- combined.data %>% filter(full_sidewalk_length/total_length>=0.75) %>% select(disabled_population) %>% pull() %>% sum()
dis.population.sw.full.100 <- combined.data %>% filter(full_sidewalk_length/total_length>=1) %>% select(disabled_population) %>% pull() %>% sum()
dis.population.sw.full.25.share <- dis.population.sw.full.25 / dis.population
dis.population.sw.full.50.share <- dis.population.sw.full.50 / dis.population
dis.population.sw.full.75.share <- dis.population.sw.full.75 / dis.population
dis.population.sw.full.100.share <- dis.population.sw.full.100 / dis.population

eld.population.sw.full.25 <- combined.data %>% filter(full_sidewalk_length/total_length>=0.25) %>% select(elderly_population) %>% pull() %>% sum()
eld.population.sw.full.50 <- combined.data %>% filter(full_sidewalk_length/total_length>=0.50) %>% select(elderly_population) %>% pull() %>% sum()
eld.population.sw.full.75 <- combined.data %>% filter(full_sidewalk_length/total_length>=0.75) %>% select(elderly_population) %>% pull() %>% sum()
eld.population.sw.full.100 <- combined.data %>% filter(full_sidewalk_length/total_length>=1) %>% select(elderly_population) %>% pull() %>% sum()
eld.population.sw.full.25.share <- eld.population.sw.full.25 / eld.population
eld.population.sw.full.50.share <- eld.population.sw.full.50 / eld.population
eld.population.sw.full.75.share <- eld.population.sw.full.75 / eld.population
eld.population.sw.full.100.share <- eld.population.sw.full.100 / eld.population

lep.population.sw.full.25 <- combined.data %>% filter(full_sidewalk_length/total_length>=0.25) %>% select(limited_english_population) %>% pull() %>% sum()
lep.population.sw.full.50 <- combined.data %>% filter(full_sidewalk_length/total_length>=0.50) %>% select(limited_english_population) %>% pull() %>% sum()
lep.population.sw.full.75 <- combined.data %>% filter(full_sidewalk_length/total_length>=0.75) %>% select(limited_english_population) %>% pull() %>% sum()
lep.population.sw.full.100 <- combined.data %>% filter(full_sidewalk_length/total_length>=1) %>% select(limited_english_population) %>% pull() %>% sum()
lep.population.sw.full.25.share <- lep.population.sw.full.25 / lep.population
lep.population.sw.full.50.share <- lep.population.sw.full.50 / lep.population
lep.population.sw.full.75.share <- lep.population.sw.full.75 / lep.population
lep.population.sw.full.100.share <- lep.population.sw.full.100 / lep.population

you.population.sw.full.25 <- combined.data %>% filter(full_sidewalk_length/total_length>=0.25) %>% select(youth_population) %>% pull() %>% sum()
you.population.sw.full.50 <- combined.data %>% filter(full_sidewalk_length/total_length>=0.50) %>% select(youth_population) %>% pull() %>% sum()
you.population.sw.full.75 <- combined.data %>% filter(full_sidewalk_length/total_length>=0.75) %>% select(youth_population) %>% pull() %>% sum()
you.population.sw.full.100 <- combined.data %>% filter(full_sidewalk_length/total_length>=1) %>% select(youth_population) %>% pull() %>% sum()
you.population.sw.full.25.share <- you.population.sw.full.25 / you.population
you.population.sw.full.50.share <- you.population.sw.full.50 / you.population
you.population.sw.full.75.share <- you.population.sw.full.75 / you.population
you.population.sw.full.100.share <- you.population.sw.full.100 / you.population


# Population that has Partial Sidewalks within 1/2 mile of their home location
total.population.sw.partial.25 <- combined.data %>% filter(partial_sidewalk_length/total_length>=0.25) %>% select(total_population) %>% pull() %>% sum()
total.population.sw.partial.50 <- combined.data %>% filter(partial_sidewalk_length/total_length>=0.50) %>% select(total_population) %>% pull() %>% sum()
total.population.sw.partial.75 <- combined.data %>% filter(partial_sidewalk_length/total_length>=0.75) %>% select(total_population) %>% pull() %>% sum()
total.population.sw.partial.100 <- combined.data %>% filter(partial_sidewalk_length/total_length>=1) %>% select(total_population) %>% pull() %>% sum()
total.population.sw.partial.25.share <- total.population.sw.partial.25 / total.population
total.population.sw.partial.50.share <- total.population.sw.partial.50 / total.population
total.population.sw.partial.75.share <- total.population.sw.partial.75 / total.population
total.population.sw.partial.100.share <- total.population.sw.partial.100 / total.population

king.population.sw.partial.25 <- combined.data %>% filter(partial_sidewalk_length/total_length>=0.25 & county=="033") %>% select(total_population) %>% pull() %>% sum()
king.population.sw.partial.50 <- combined.data %>% filter(partial_sidewalk_length/total_length>=0.50 & county=="033") %>% select(total_population) %>% pull() %>% sum()
king.population.sw.partial.75 <- combined.data %>% filter(partial_sidewalk_length/total_length>=0.75 & county=="033") %>% select(total_population) %>% pull() %>% sum()
king.population.sw.partial.100 <- combined.data %>% filter(partial_sidewalk_length/total_length>=1 & county=="033") %>% select(total_population) %>% pull() %>% sum()
king.population.sw.partial.25.share <- king.population.sw.partial.25 / king.population
king.population.sw.partial.50.share <- king.population.sw.partial.50 / king.population
king.population.sw.partial.75.share <- king.population.sw.partial.75 / king.population
king.population.sw.partial.100.share <- king.population.sw.partial.100 / king.population

kitsap.population.sw.partial.25 <- combined.data %>% filter(partial_sidewalk_length/total_length>=0.25 & county=="035") %>% select(total_population) %>% pull() %>% sum()
kitsap.population.sw.partial.50 <- combined.data %>% filter(partial_sidewalk_length/total_length>=0.50 & county=="035") %>% select(total_population) %>% pull() %>% sum()
kitsap.population.sw.partial.75 <- combined.data %>% filter(partial_sidewalk_length/total_length>=0.75 & county=="035") %>% select(total_population) %>% pull() %>% sum()
kitsap.population.sw.partial.100 <- combined.data %>% filter(partial_sidewalk_length/total_length>=1 & county=="035") %>% select(total_population) %>% pull() %>% sum()
kitsap.population.sw.partial.25.share <- kitsap.population.sw.partial.25 / kitsap.population
kitsap.population.sw.partial.50.share <- kitsap.population.sw.partial.50 / kitsap.population
kitsap.population.sw.partial.75.share <- kitsap.population.sw.partial.75 / kitsap.population
kitsap.population.sw.partial.100.share <- kitsap.population.sw.partial.100 / kitsap.population

pierce.population.sw.partial.25 <- combined.data %>% filter(partial_sidewalk_length/total_length>=0.25 & county=="053") %>% select(total_population) %>% pull() %>% sum()
pierce.population.sw.partial.50 <- combined.data %>% filter(partial_sidewalk_length/total_length>=0.50 & county=="053") %>% select(total_population) %>% pull() %>% sum()
pierce.population.sw.partial.75 <- combined.data %>% filter(partial_sidewalk_length/total_length>=0.75 & county=="053") %>% select(total_population) %>% pull() %>% sum()
pierce.population.sw.partial.100 <- combined.data %>% filter(partial_sidewalk_length/total_length>=1 & county=="053") %>% select(total_population) %>% pull() %>% sum()
pierce.population.sw.partial.25.share <- pierce.population.sw.partial.25 / pierce.population
pierce.population.sw.partial.50.share <- pierce.population.sw.partial.50 / pierce.population
pierce.population.sw.partial.75.share <- pierce.population.sw.partial.75 / pierce.population
pierce.population.sw.partial.100.share <- pierce.population.sw.partial.100 / pierce.population

snohomish.population.sw.partial.25 <- combined.data %>% filter(partial_sidewalk_length/total_length>=0.25 & county=="061") %>% select(total_population) %>% pull() %>% sum()
snohomish.population.sw.partial.50 <- combined.data %>% filter(partial_sidewalk_length/total_length>=0.50 & county=="061") %>% select(total_population) %>% pull() %>% sum()
snohomish.population.sw.partial.75 <- combined.data %>% filter(partial_sidewalk_length/total_length>=0.75 & county=="061") %>% select(total_population) %>% pull() %>% sum()
snohomish.population.sw.partial.100 <- combined.data %>% filter(partial_sidewalk_length/total_length>=1 & county=="061") %>% select(total_population) %>% pull() %>% sum()
snohomish.population.sw.partial.25.share <- snohomish.population.sw.partial.25 / snohomish.population
snohomish.population.sw.partial.50.share <- snohomish.population.sw.partial.50 / snohomish.population
snohomish.population.sw.partial.75.share <- snohomish.population.sw.partial.75 / snohomish.population
snohomish.population.sw.partial.100.share <- snohomish.population.sw.partial.100 / snohomish.population

poc.population.sw.partial.25 <- combined.data %>% filter(partial_sidewalk_length/total_length>=0.25) %>% select(people_of_color_population) %>% pull() %>% sum()
poc.population.sw.partial.50 <- combined.data %>% filter(partial_sidewalk_length/total_length>=0.50) %>% select(people_of_color_population) %>% pull() %>% sum()
poc.population.sw.partial.75 <- combined.data %>% filter(partial_sidewalk_length/total_length>=0.75) %>% select(people_of_color_population) %>% pull() %>% sum()
poc.population.sw.partial.100 <- combined.data %>% filter(partial_sidewalk_length/total_length>=1) %>% select(people_of_color_population) %>% pull() %>% sum()
poc.population.sw.partial.25.share <- poc.population.sw.partial.25 / poc.population
poc.population.sw.partial.50.share <- poc.population.sw.partial.50 / poc.population
poc.population.sw.partial.75.share <- poc.population.sw.partial.75 / poc.population
poc.population.sw.partial.100.share <- poc.population.sw.partial.100 / poc.population

pov.population.sw.partial.25 <- combined.data %>% filter(partial_sidewalk_length/total_length>=0.25) %>% select(low_income_population) %>% pull() %>% sum()
pov.population.sw.partial.50 <- combined.data %>% filter(partial_sidewalk_length/total_length>=0.50) %>% select(low_income_population) %>% pull() %>% sum()
pov.population.sw.partial.75 <- combined.data %>% filter(partial_sidewalk_length/total_length>=0.75) %>% select(low_income_population) %>% pull() %>% sum()
pov.population.sw.partial.100 <- combined.data %>% filter(partial_sidewalk_length/total_length>=1) %>% select(low_income_population) %>% pull() %>% sum()
pov.population.sw.partial.25.share <- pov.population.sw.partial.25 / pov.population
pov.population.sw.partial.50.share <- pov.population.sw.partial.50 / pov.population
pov.population.sw.partial.75.share <- pov.population.sw.partial.75 / pov.population
pov.population.sw.partial.100.share <- pov.population.sw.partial.100 / pov.population

dis.population.sw.partial.25 <- combined.data %>% filter(partial_sidewalk_length/total_length>=0.25) %>% select(disabled_population) %>% pull() %>% sum()
dis.population.sw.partial.50 <- combined.data %>% filter(partial_sidewalk_length/total_length>=0.50) %>% select(disabled_population) %>% pull() %>% sum()
dis.population.sw.partial.75 <- combined.data %>% filter(partial_sidewalk_length/total_length>=0.75) %>% select(disabled_population) %>% pull() %>% sum()
dis.population.sw.partial.100 <- combined.data %>% filter(partial_sidewalk_length/total_length>=1) %>% select(disabled_population) %>% pull() %>% sum()
dis.population.sw.partial.25.share <- dis.population.sw.partial.25 / dis.population
dis.population.sw.partial.50.share <- dis.population.sw.partial.50 / dis.population
dis.population.sw.partial.75.share <- dis.population.sw.partial.75 / dis.population
dis.population.sw.partial.100.share <- dis.population.sw.partial.100 / dis.population

eld.population.sw.partial.25 <- combined.data %>% filter(partial_sidewalk_length/total_length>=0.25) %>% select(elderly_population) %>% pull() %>% sum()
eld.population.sw.partial.50 <- combined.data %>% filter(partial_sidewalk_length/total_length>=0.50) %>% select(elderly_population) %>% pull() %>% sum()
eld.population.sw.partial.75 <- combined.data %>% filter(partial_sidewalk_length/total_length>=0.75) %>% select(elderly_population) %>% pull() %>% sum()
eld.population.sw.partial.100 <- combined.data %>% filter(partial_sidewalk_length/total_length>=1) %>% select(elderly_population) %>% pull() %>% sum()
eld.population.sw.partial.25.share <- eld.population.sw.partial.25 / eld.population
eld.population.sw.partial.50.share <- eld.population.sw.partial.50 / eld.population
eld.population.sw.partial.75.share <- eld.population.sw.partial.75 / eld.population
eld.population.sw.partial.100.share <- eld.population.sw.partial.100 / eld.population

lep.population.sw.partial.25 <- combined.data %>% filter(partial_sidewalk_length/total_length>=0.25) %>% select(limited_english_population) %>% pull() %>% sum()
lep.population.sw.partial.50 <- combined.data %>% filter(partial_sidewalk_length/total_length>=0.50) %>% select(limited_english_population) %>% pull() %>% sum()
lep.population.sw.partial.75 <- combined.data %>% filter(partial_sidewalk_length/total_length>=0.75) %>% select(limited_english_population) %>% pull() %>% sum()
lep.population.sw.partial.100 <- combined.data %>% filter(partial_sidewalk_length/total_length>=1) %>% select(limited_english_population) %>% pull() %>% sum()
lep.population.sw.partial.25.share <- lep.population.sw.partial.25 / lep.population
lep.population.sw.partial.50.share <- lep.population.sw.partial.50 / lep.population
lep.population.sw.partial.75.share <- lep.population.sw.partial.75 / lep.population
lep.population.sw.partial.100.share <- lep.population.sw.partial.100 / lep.population

you.population.sw.partial.25 <- combined.data %>% filter(partial_sidewalk_length/total_length>=0.25) %>% select(youth_population) %>% pull() %>% sum()
you.population.sw.partial.50 <- combined.data %>% filter(partial_sidewalk_length/total_length>=0.50) %>% select(youth_population) %>% pull() %>% sum()
you.population.sw.partial.75 <- combined.data %>% filter(partial_sidewalk_length/total_length>=0.75) %>% select(youth_population) %>% pull() %>% sum()
you.population.sw.partial.100 <- combined.data %>% filter(partial_sidewalk_length/total_length>=1) %>% select(youth_population) %>% pull() %>% sum()
you.population.sw.partial.25.share <- you.population.sw.partial.25 / you.population
you.population.sw.partial.50.share <- you.population.sw.partial.50 / you.population
you.population.sw.partial.75.share <- you.population.sw.partial.75 / you.population
you.population.sw.partial.100.share <- you.population.sw.partial.100 / you.population

population.near.sidewalks.table <- data.table(
  `Geography` = c("Region", 
                  "King County",
                  "Kitsap County",
                  "Pierce County",
                  "Snohomish County",
                  "People with a Disability", 
                  "People over 65", 
                  "People with Limited English Proficiency", 
                  "People of Color", 
                  "People in Poverty", 
                  "People under 18"),
  `Population` = c(total.population,
                   king.population,
                   kitsap.population,
                   pierce.population,
                   snohomish.population,
                   dis.population,
                   eld.population,
                   lep.population,
                   poc.population,
                   pov.population,
                   you.population),
  `Share of Population` = c(total.population.shr,
                            king.population.shr,
                            kitsap.population.shr,
                            pierce.population.shr,
                            snohomish.population.shr,
                            dis.population.shr,
                            eld.population.shr,
                            lep.population.shr,
                            poc.population.shr,
                            pov.population.shr,
                            you.population.shr),
  `Share of Population with at least 25% Complete Sidewalks` = c(total.population.sw.full.25.share,
                                                                 king.population.sw.full.25.share,
                                                                 kitsap.population.sw.full.25.share,
                                                                 pierce.population.sw.full.25.share,
                                                                 snohomish.population.sw.full.25.share,
                                                                 dis.population.sw.full.25.share,
                                                                 eld.population.sw.full.25.share,
                                                                 lep.population.sw.full.25.share,
                                                                 poc.population.sw.full.25.share,
                                                                 pov.population.sw.full.25.share,
                                                                 you.population.sw.full.25.share),
  `Share of Population with at least 50% Complete Sidewalks` = c(total.population.sw.full.50.share,
                                                                 king.population.sw.full.50.share,
                                                                 kitsap.population.sw.full.50.share,
                                                                 pierce.population.sw.full.50.share,
                                                                 snohomish.population.sw.full.50.share,
                                                                 dis.population.sw.full.50.share,
                                                                 eld.population.sw.full.50.share,
                                                                 lep.population.sw.full.50.share,
                                                                 poc.population.sw.full.50.share,
                                                                 pov.population.sw.full.50.share,
                                                                 you.population.sw.full.50.share),
  `Share of Population with at least 75% Complete Sidewalks` = c(total.population.sw.full.75.share,
                                                                 king.population.sw.full.75.share,
                                                                 kitsap.population.sw.full.75.share,
                                                                 pierce.population.sw.full.75.share,
                                                                 snohomish.population.sw.full.75.share,
                                                                 dis.population.sw.full.75.share,
                                                                 eld.population.sw.full.75.share,
                                                                 lep.population.sw.full.75.share,
                                                                 poc.population.sw.full.75.share,
                                                                 pov.population.sw.full.75.share,
                                                                 you.population.sw.full.75.share),
  `Share of Population with 100% Complete Sidewalks` = c(total.population.sw.full.100.share,
                                                         king.population.sw.full.100.share,
                                                         kitsap.population.sw.full.100.share,
                                                         pierce.population.sw.full.100.share,
                                                         snohomish.population.sw.full.100.share,
                                                         dis.population.sw.full.100.share,
                                                         eld.population.sw.full.100.share,
                                                         lep.population.sw.full.100.share,
                                                         poc.population.sw.full.100.share,
                                                         pov.population.sw.full.100.share,
                                                         you.population.sw.full.100.share),
  `Share of Population with at least 25% Partial Sidewalks` = c(total.population.sw.partial.25.share,
                                                                 king.population.sw.partial.25.share,
                                                                 kitsap.population.sw.partial.25.share,
                                                                 pierce.population.sw.partial.25.share,
                                                                 snohomish.population.sw.partial.25.share,
                                                                dis.population.sw.partial.25.share,
                                                                eld.population.sw.partial.25.share,
                                                                lep.population.sw.partial.25.share,
                                                                poc.population.sw.partial.25.share,
                                                                pov.population.sw.partial.25.share,
                                                                you.population.sw.partial.25.share),
  `Share of Population with at least 50% Partial Sidewalks` = c(total.population.sw.partial.50.share,
                                                                 king.population.sw.partial.50.share,
                                                                 kitsap.population.sw.partial.50.share,
                                                                 pierce.population.sw.partial.50.share,
                                                                 snohomish.population.sw.partial.50.share,
                                                                dis.population.sw.partial.50.share,
                                                                eld.population.sw.partial.50.share,
                                                                lep.population.sw.partial.50.share,
                                                                poc.population.sw.partial.50.share,
                                                                pov.population.sw.partial.50.share,
                                                                you.population.sw.partial.50.share),
  `Share of Population with at least 75% Partial Sidewalks` = c(total.population.sw.partial.75.share,
                                                                 king.population.sw.partial.75.share,
                                                                 kitsap.population.sw.partial.75.share,
                                                                 pierce.population.sw.partial.75.share,
                                                                 snohomish.population.sw.partial.75.share,
                                                                dis.population.sw.partial.75.share,
                                                                eld.population.sw.partial.75.share,
                                                                lep.population.sw.partial.75.share,
                                                                poc.population.sw.partial.75.share,
                                                                pov.population.sw.partial.75.share,
                                                                you.population.sw.partial.75.share),
  `Share of Population with 100% Partial Sidewalks` = c(total.population.sw.partial.100.share,
                                                         king.population.sw.partial.100.share,
                                                         kitsap.population.sw.partial.100.share,
                                                         pierce.population.sw.partial.100.share,
                                                         snohomish.population.sw.partial.100.share,
                                                        dis.population.sw.partial.100.share,
                                                        eld.population.sw.partial.100.share,
                                                        lep.population.sw.partial.100.share,
                                                        poc.population.sw.partial.100.share,
                                                        pov.population.sw.partial.100.share,
                                                        you.population.sw.partial.100.share)

)

fwrite(population.near.sidewalks.table, here('output','people_near_sidewalks.csv'))

# Analysis of Population with Sidewalk Facilities that Liver Near HCt--------------------

# Population that has Full or Partial Sidewalks within 1/2 mile of their home location and live near HCT
total.population.sw.full.25.hct <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=0.25 & hct=="High Capacity Transit") %>% select(total_population) %>% pull() %>% sum()
total.population.sw.full.50.hct <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=0.50 & hct=="High Capacity Transit") %>% select(total_population) %>% pull() %>% sum()
total.population.sw.full.75.hct <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=0.75 & hct=="High Capacity Transit") %>% select(total_population) %>% pull() %>% sum()
total.population.sw.full.100.hct <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=1 & hct=="High Capacity Transit") %>% select(total_population) %>% pull() %>% sum()
total.population.sw.full.25.hct.share <- total.population.sw.full.25.hct / total.population.near.hct
total.population.sw.full.50.hct.share <- total.population.sw.full.50.hct / total.population.near.hct
total.population.sw.full.75.hct.share <- total.population.sw.full.75.hct / total.population.near.hct
total.population.sw.full.100.hct.share <- total.population.sw.full.100.hct / total.population.near.hct

king.population.sw.full.25.hct <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=0.25 & hct=="High Capacity Transit" & county=="033") %>% select(total_population) %>% pull() %>% sum()
king.population.sw.full.50.hct <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=0.50 & hct=="High Capacity Transit" & county=="033") %>% select(total_population) %>% pull() %>% sum()
king.population.sw.full.75.hct <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=0.75 & hct=="High Capacity Transit" & county=="033") %>% select(total_population) %>% pull() %>% sum()
king.population.sw.full.100.hct <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=1 & hct=="High Capacity Transit" & county=="033") %>% select(total_population) %>% pull() %>% sum()
king.population.sw.full.25.hct.share <- king.population.sw.full.25.hct / king.population.near.hct
king.population.sw.full.50.hct.share <- king.population.sw.full.50.hct / king.population.near.hct
king.population.sw.full.75.hct.share <- king.population.sw.full.75.hct / king.population.near.hct
king.population.sw.full.100.hct.share <- king.population.sw.full.100.hct / king.population.near.hct

kitsap.population.sw.full.25.hct <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=0.25 & hct=="High Capacity Transit" & county=="035") %>% select(total_population) %>% pull() %>% sum()
kitsap.population.sw.full.50.hct <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=0.50 & hct=="High Capacity Transit" & county=="035") %>% select(total_population) %>% pull() %>% sum()
kitsap.population.sw.full.75.hct <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=0.75 & hct=="High Capacity Transit" & county=="035") %>% select(total_population) %>% pull() %>% sum()
kitsap.population.sw.full.100.hct <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=1 & hct=="High Capacity Transit" & county=="035") %>% select(total_population) %>% pull() %>% sum()
kitsap.population.sw.full.25.hct.share <- kitsap.population.sw.full.25.hct / kitsap.population.near.hct
kitsap.population.sw.full.50.hct.share <- kitsap.population.sw.full.50.hct / kitsap.population.near.hct
kitsap.population.sw.full.75.hct.share <- kitsap.population.sw.full.75.hct / kitsap.population.near.hct
kitsap.population.sw.full.100.hct.share <- kitsap.population.sw.full.100.hct / kitsap.population.near.hct

pierce.population.sw.full.25.hct <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=0.25 & hct=="High Capacity Transit" & county=="053") %>% select(total_population) %>% pull() %>% sum()
pierce.population.sw.full.50.hct <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=0.50 & hct=="High Capacity Transit" & county=="053") %>% select(total_population) %>% pull() %>% sum()
pierce.population.sw.full.75.hct <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=0.75 & hct=="High Capacity Transit" & county=="053") %>% select(total_population) %>% pull() %>% sum()
pierce.population.sw.full.100.hct <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=1 & hct=="High Capacity Transit" & county=="053") %>% select(total_population) %>% pull() %>% sum()
pierce.population.sw.full.25.hct.share <- pierce.population.sw.full.25.hct / pierce.population.near.hct
pierce.population.sw.full.50.hct.share <- pierce.population.sw.full.50.hct / pierce.population.near.hct
pierce.population.sw.full.75.hct.share <- pierce.population.sw.full.75.hct / pierce.population.near.hct
pierce.population.sw.full.100.hct.share <- pierce.population.sw.full.100.hct / pierce.population.near.hct

snohomish.population.sw.full.25.hct <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=0.25 & hct=="High Capacity Transit" & county=="061") %>% select(total_population) %>% pull() %>% sum()
snohomish.population.sw.full.50.hct <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=0.50 & hct=="High Capacity Transit" & county=="061") %>% select(total_population) %>% pull() %>% sum()
snohomish.population.sw.full.75.hct <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=0.75 & hct=="High Capacity Transit" & county=="061") %>% select(total_population) %>% pull() %>% sum()
snohomish.population.sw.full.100.hct <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=1 & hct=="High Capacity Transit" & county=="061") %>% select(total_population) %>% pull() %>% sum()
snohomish.population.sw.full.25.hct.share <- snohomish.population.sw.full.25.hct / snohomish.population.near.hct
snohomish.population.sw.full.50.hct.share <- snohomish.population.sw.full.50.hct / snohomish.population.near.hct
snohomish.population.sw.full.75.hct.share <- snohomish.population.sw.full.75.hct / snohomish.population.near.hct
snohomish.population.sw.full.100.hct.share <- snohomish.population.sw.full.100.hct / snohomish.population.near.hct

poc.population.sw.full.25.hct <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=0.25 & hct=="High Capacity Transit") %>% select(people_of_color_population) %>% pull() %>% sum()
poc.population.sw.full.50.hct <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=0.50 & hct=="High Capacity Transit") %>% select(people_of_color_population) %>% pull() %>% sum()
poc.population.sw.full.75.hct <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=0.75 & hct=="High Capacity Transit") %>% select(people_of_color_population) %>% pull() %>% sum()
poc.population.sw.full.100.hct <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=1 & hct=="High Capacity Transit") %>% select(people_of_color_population) %>% pull() %>% sum()
poc.population.sw.full.25.hct.share <- poc.population.sw.full.25.hct / poc.population.near.hct
poc.population.sw.full.50.hct.share <- poc.population.sw.full.50.hct / poc.population.near.hct
poc.population.sw.full.75.hct.share <- poc.population.sw.full.75.hct / poc.population.near.hct
poc.population.sw.full.100.hct.share <- poc.population.sw.full.100.hct / poc.population.near.hct

pov.population.sw.full.25.hct <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=0.25 & hct=="High Capacity Transit") %>% select(low_income_population) %>% pull() %>% sum()
pov.population.sw.full.50.hct <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=0.50 & hct=="High Capacity Transit") %>% select(low_income_population) %>% pull() %>% sum()
pov.population.sw.full.75.hct <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=0.75 & hct=="High Capacity Transit") %>% select(low_income_population) %>% pull() %>% sum()
pov.population.sw.full.100.hct <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=1 & hct=="High Capacity Transit") %>% select(low_income_population) %>% pull() %>% sum()
pov.population.sw.full.25.hct.share <- pov.population.sw.full.25.hct / pov.population.near.hct
pov.population.sw.full.50.hct.share <- pov.population.sw.full.50.hct / pov.population.near.hct
pov.population.sw.full.75.hct.share <- pov.population.sw.full.75.hct / pov.population.near.hct
pov.population.sw.full.100.hct.share <- pov.population.sw.full.100.hct / pov.population.near.hct

dis.population.sw.full.25.hct <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=0.25 & hct=="High Capacity Transit") %>% select(disabled_population) %>% pull() %>% sum()
dis.population.sw.full.50.hct <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=0.50 & hct=="High Capacity Transit") %>% select(disabled_population) %>% pull() %>% sum()
dis.population.sw.full.75.hct <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=0.75 & hct=="High Capacity Transit") %>% select(disabled_population) %>% pull() %>% sum()
dis.population.sw.full.100.hct <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=1 & hct=="High Capacity Transit") %>% select(disabled_population) %>% pull() %>% sum()
dis.population.sw.full.25.hct.share <- dis.population.sw.full.25.hct / dis.population.near.hct
dis.population.sw.full.50.hct.share <- dis.population.sw.full.50.hct / dis.population.near.hct
dis.population.sw.full.75.hct.share <- dis.population.sw.full.75.hct / dis.population.near.hct
dis.population.sw.full.100.hct.share <- dis.population.sw.full.100.hct / dis.population.near.hct

eld.population.sw.full.25.hct <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=0.25 & hct=="High Capacity Transit") %>% select(elderly_population) %>% pull() %>% sum()
eld.population.sw.full.50.hct <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=0.50 & hct=="High Capacity Transit") %>% select(elderly_population) %>% pull() %>% sum()
eld.population.sw.full.75.hct <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=0.75 & hct=="High Capacity Transit") %>% select(elderly_population) %>% pull() %>% sum()
eld.population.sw.full.100.hct <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=1 & hct=="High Capacity Transit") %>% select(elderly_population) %>% pull() %>% sum()
eld.population.sw.full.25.hct.share <- eld.population.sw.full.25.hct / eld.population.near.hct
eld.population.sw.full.50.hct.share <- eld.population.sw.full.50.hct / eld.population.near.hct
eld.population.sw.full.75.hct.share <- eld.population.sw.full.75.hct / eld.population.near.hct
eld.population.sw.full.100.hct.share <- eld.population.sw.full.100.hct / eld.population.near.hct

lep.population.sw.full.25.hct <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=0.25 & hct=="High Capacity Transit") %>% select(limited_english_population) %>% pull() %>% sum()
lep.population.sw.full.50.hct <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=0.50 & hct=="High Capacity Transit") %>% select(limited_english_population) %>% pull() %>% sum()
lep.population.sw.full.75.hct <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=0.75 & hct=="High Capacity Transit") %>% select(limited_english_population) %>% pull() %>% sum()
lep.population.sw.full.100.hct <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=1 & hct=="High Capacity Transit") %>% select(limited_english_population) %>% pull() %>% sum()
lep.population.sw.full.25.hct.share <- lep.population.sw.full.25.hct / lep.population.near.hct
lep.population.sw.full.50.hct.share <- lep.population.sw.full.50.hct / lep.population.near.hct
lep.population.sw.full.75.hct.share <- lep.population.sw.full.75.hct / lep.population.near.hct
lep.population.sw.full.100.hct.share <- lep.population.sw.full.100.hct / lep.population.near.hct

you.population.sw.full.25.hct <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=0.25 & hct=="High Capacity Transit") %>% select(youth_population) %>% pull() %>% sum()
you.population.sw.full.50.hct <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=0.50 & hct=="High Capacity Transit") %>% select(youth_population) %>% pull() %>% sum()
you.population.sw.full.75.hct <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=0.75 & hct=="High Capacity Transit") %>% select(youth_population) %>% pull() %>% sum()
you.population.sw.full.100.hct <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=1 & hct=="High Capacity Transit") %>% select(youth_population) %>% pull() %>% sum()
you.population.sw.full.25.hct.share <- you.population.sw.full.25.hct / you.population.near.hct
you.population.sw.full.50.hct.share <- you.population.sw.full.50.hct / you.population.near.hct
you.population.sw.full.75.hct.share <- you.population.sw.full.75.hct / you.population.near.hct
you.population.sw.full.100.hct.share <- you.population.sw.full.100.hct / you.population.near.hct

population.near.hct.sidewalks.table <- data.table(
  `Geography` = c("Region", 
                  "King County",
                  "Kitsap County",
                  "Pierce County",
                  "Snohomish County",
                  "People with a Disability", 
                  "People over 65", 
                  "People with Limited English Proficiency", 
                  "People of Color", 
                  "People in Poverty", 
                  "People under 18"),
  `Population` = c(total.population,
                   king.population,
                   kitsap.population,
                   pierce.population,
                   snohomish.population,
                   dis.population,
                   eld.population,
                   lep.population,
                   poc.population,
                   pov.population,
                   you.population),
  `Share of Population` = c(total.population.shr,
                            king.population.shr,
                            kitsap.population.shr,
                            pierce.population.shr,
                            snohomish.population.shr,
                            dis.population.shr,
                            eld.population.shr,
                            lep.population.shr,
                            poc.population.shr,
                            pov.population.shr,
                            you.population.shr),
  `Share of Population within 1/2 mile of High Capacity Transit` = c(total.population.near.hct.share,
                                                                     king.population.near.hct.share,
                                                                     kitsap.population.near.hct.share,
                                                                     pierce.population.near.hct.share,
                                                                     snohomish.population.near.hct.share,
                                                                     dis.population.near.hct.share,
                                                                     eld.population.near.hct.share,
                                                                     lep.population.near.hct.share,
                                                                     poc.population.near.hct.share,
                                                                     pov.population.near.hct.share,
                                                                     you.population.near.hct.share),
  `Share of Population near HCT with at least 25% Sidewalks` = c(total.population.sw.full.25.hct.share,
                                                                 king.population.sw.full.25.hct.share,
                                                                 kitsap.population.sw.full.25.hct.share,
                                                                 pierce.population.sw.full.25.hct.share,
                                                                 snohomish.population.sw.full.25.hct.share,
                                                                 dis.population.sw.full.25.hct.share,
                                                                 eld.population.sw.full.25.hct.share,
                                                                 lep.population.sw.full.25.hct.share,
                                                                 poc.population.sw.full.25.hct.share,
                                                                 pov.population.sw.full.25.hct.share,
                                                                 you.population.sw.full.25.hct.share),
  `Share of Population near HCT with at least 50% Sidewalks` = c(total.population.sw.full.50.hct.share,
                                                                 king.population.sw.full.50.hct.share,
                                                                 kitsap.population.sw.full.50.hct.share,
                                                                 pierce.population.sw.full.50.hct.share,
                                                                 snohomish.population.sw.full.50.hct.share,
                                                                 dis.population.sw.full.50.hct.share,
                                                                 eld.population.sw.full.50.hct.share,
                                                                 lep.population.sw.full.50.hct.share,
                                                                 poc.population.sw.full.50.hct.share,
                                                                 pov.population.sw.full.50.hct.share,
                                                                 you.population.sw.full.50.hct.share),
  `Share of Population near HCT with at least 75% Sidewalks` = c(total.population.sw.full.75.hct.share,
                                                                 king.population.sw.full.75.hct.share,
                                                                 kitsap.population.sw.full.75.hct.share,
                                                                 pierce.population.sw.full.75.hct.share,
                                                                 snohomish.population.sw.full.75.hct.share,
                                                                 dis.population.sw.full.75.hct.share,
                                                                 eld.population.sw.full.75.hct.share,
                                                                 lep.population.sw.full.75.hct.share,
                                                                 poc.population.sw.full.75.hct.share,
                                                                 pov.population.sw.full.75.hct.share,
                                                                 you.population.sw.full.75.hct.share),
  `Share of Population near HCT with 100% Sidewalks` = c(total.population.sw.full.100.hct.share,
                                                         king.population.sw.full.100.hct.share,
                                                         kitsap.population.sw.full.100.hct.share,
                                                         pierce.population.sw.full.100.hct.share,
                                                         snohomish.population.sw.full.100.hct.share,
                                                         dis.population.sw.full.100.hct.share,
                                                         eld.population.sw.full.100.hct.share,
                                                         lep.population.sw.full.100.hct.share,
                                                         poc.population.sw.full.100.hct.share,
                                                         pov.population.sw.full.100.hct.share,
                                                         you.population.sw.full.100.hct.share)

)

fwrite(population.near.hct.sidewalks.table, here('output','people_near_hct_with_sidewalks.csv'))

# Analysis of Population with Sidewalk Facilities that Live Near Transit--------------------

# Population that has Full or Partial Sidewalks within 1/2 mile of their home location and live near Transit
total.population.sw.full.25.bus <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=0.25 & non_hct=="Bus") %>% select(total_population) %>% pull() %>% sum()
total.population.sw.full.50.bus <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=0.50 & non_hct=="Bus") %>% select(total_population) %>% pull() %>% sum()
total.population.sw.full.75.bus <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=0.75 & non_hct=="Bus") %>% select(total_population) %>% pull() %>% sum()
total.population.sw.full.100.bus <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=1 & non_hct=="Bus") %>% select(total_population) %>% pull() %>% sum()
total.population.sw.full.25.bus.share <- total.population.sw.full.25.bus / total.population.near.bus
total.population.sw.full.50.bus.share <- total.population.sw.full.50.bus / total.population.near.bus
total.population.sw.full.75.bus.share <- total.population.sw.full.75.bus / total.population.near.bus
total.population.sw.full.100.bus.share <- total.population.sw.full.100.bus / total.population.near.bus

king.population.sw.full.25.bus <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=0.25 & non_hct=="Bus" & county=="033") %>% select(total_population) %>% pull() %>% sum()
king.population.sw.full.50.bus <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=0.50 & non_hct=="Bus" & county=="033") %>% select(total_population) %>% pull() %>% sum()
king.population.sw.full.75.bus <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=0.75 & non_hct=="Bus" & county=="033") %>% select(total_population) %>% pull() %>% sum()
king.population.sw.full.100.bus <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=1 & non_hct=="Bus" & county=="033") %>% select(total_population) %>% pull() %>% sum()
king.population.sw.full.25.bus.share <- king.population.sw.full.25.bus / king.population.near.bus
king.population.sw.full.50.bus.share <- king.population.sw.full.50.bus / king.population.near.bus
king.population.sw.full.75.bus.share <- king.population.sw.full.75.bus / king.population.near.bus
king.population.sw.full.100.bus.share <- king.population.sw.full.100.bus / king.population.near.bus

kitsap.population.sw.full.25.bus <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=0.25 & non_hct=="Bus" & county=="035") %>% select(total_population) %>% pull() %>% sum()
kitsap.population.sw.full.50.bus <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=0.50 & non_hct=="Bus" & county=="035") %>% select(total_population) %>% pull() %>% sum()
kitsap.population.sw.full.75.bus <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=0.75 & non_hct=="Bus" & county=="035") %>% select(total_population) %>% pull() %>% sum()
kitsap.population.sw.full.100.bus <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=1 & non_hct=="Bus" & county=="035") %>% select(total_population) %>% pull() %>% sum()
kitsap.population.sw.full.25.bus.share <- kitsap.population.sw.full.25.bus / kitsap.population.near.bus
kitsap.population.sw.full.50.bus.share <- kitsap.population.sw.full.50.bus / kitsap.population.near.bus
kitsap.population.sw.full.75.bus.share <- kitsap.population.sw.full.75.bus / kitsap.population.near.bus
kitsap.population.sw.full.100.bus.share <- kitsap.population.sw.full.100.bus / kitsap.population.near.bus

pierce.population.sw.full.25.bus <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=0.25 & non_hct=="Bus" & county=="053") %>% select(total_population) %>% pull() %>% sum()
pierce.population.sw.full.50.bus <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=0.50 & non_hct=="Bus" & county=="053") %>% select(total_population) %>% pull() %>% sum()
pierce.population.sw.full.75.bus <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=0.75 & non_hct=="Bus" & county=="053") %>% select(total_population) %>% pull() %>% sum()
pierce.population.sw.full.100.bus <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=1 & non_hct=="Bus" & county=="053") %>% select(total_population) %>% pull() %>% sum()
pierce.population.sw.full.25.bus.share <- pierce.population.sw.full.25.bus / pierce.population.near.bus
pierce.population.sw.full.50.bus.share <- pierce.population.sw.full.50.bus / pierce.population.near.bus
pierce.population.sw.full.75.bus.share <- pierce.population.sw.full.75.bus / pierce.population.near.bus
pierce.population.sw.full.100.bus.share <- pierce.population.sw.full.100.bus / pierce.population.near.bus

snohomish.population.sw.full.25.bus <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=0.25 & non_hct=="Bus" & county=="061") %>% select(total_population) %>% pull() %>% sum()
snohomish.population.sw.full.50.bus <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=0.50 & non_hct=="Bus" & county=="061") %>% select(total_population) %>% pull() %>% sum()
snohomish.population.sw.full.75.bus <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=0.75 & non_hct=="Bus" & county=="061") %>% select(total_population) %>% pull() %>% sum()
snohomish.population.sw.full.100.bus <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=1 & non_hct=="Bus" & county=="061") %>% select(total_population) %>% pull() %>% sum()
snohomish.population.sw.full.25.bus.share <- snohomish.population.sw.full.25.bus / snohomish.population.near.bus
snohomish.population.sw.full.50.bus.share <- snohomish.population.sw.full.50.bus / snohomish.population.near.bus
snohomish.population.sw.full.75.bus.share <- snohomish.population.sw.full.75.bus / snohomish.population.near.bus
snohomish.population.sw.full.100.bus.share <- snohomish.population.sw.full.100.bus / snohomish.population.near.bus

poc.population.sw.full.25.bus <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=0.25 & non_hct=="Bus") %>% select(people_of_color_population) %>% pull() %>% sum()
poc.population.sw.full.50.bus <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=0.50 & non_hct=="Bus") %>% select(people_of_color_population) %>% pull() %>% sum()
poc.population.sw.full.75.bus <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=0.75 & non_hct=="Bus") %>% select(people_of_color_population) %>% pull() %>% sum()
poc.population.sw.full.100.bus <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=1 & non_hct=="Bus") %>% select(people_of_color_population) %>% pull() %>% sum()
poc.population.sw.full.25.bus.share <- poc.population.sw.full.25.bus / poc.population.near.bus
poc.population.sw.full.50.bus.share <- poc.population.sw.full.50.bus / poc.population.near.bus
poc.population.sw.full.75.bus.share <- poc.population.sw.full.75.bus / poc.population.near.bus
poc.population.sw.full.100.bus.share <- poc.population.sw.full.100.bus / poc.population.near.bus

pov.population.sw.full.25.bus <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=0.25 & non_hct=="Bus") %>% select(low_income_population) %>% pull() %>% sum()
pov.population.sw.full.50.bus <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=0.50 & non_hct=="Bus") %>% select(low_income_population) %>% pull() %>% sum()
pov.population.sw.full.75.bus <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=0.75 & non_hct=="Bus") %>% select(low_income_population) %>% pull() %>% sum()
pov.population.sw.full.100.bus <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=1 & non_hct=="Bus") %>% select(low_income_population) %>% pull() %>% sum()
pov.population.sw.full.25.bus.share <- pov.population.sw.full.25.bus / pov.population.near.bus
pov.population.sw.full.50.bus.share <- pov.population.sw.full.50.bus / pov.population.near.bus
pov.population.sw.full.75.bus.share <- pov.population.sw.full.75.bus / pov.population.near.bus
pov.population.sw.full.100.bus.share <- pov.population.sw.full.100.bus / pov.population.near.bus

dis.population.sw.full.25.bus <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=0.25 & non_hct=="Bus") %>% select(disabled_population) %>% pull() %>% sum()
dis.population.sw.full.50.bus <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=0.50 & non_hct=="Bus") %>% select(disabled_population) %>% pull() %>% sum()
dis.population.sw.full.75.bus <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=0.75 & non_hct=="Bus") %>% select(disabled_population) %>% pull() %>% sum()
dis.population.sw.full.100.bus <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=1 & non_hct=="Bus") %>% select(disabled_population) %>% pull() %>% sum()
dis.population.sw.full.25.bus.share <- dis.population.sw.full.25.bus / dis.population.near.bus
dis.population.sw.full.50.bus.share <- dis.population.sw.full.50.bus / dis.population.near.bus
dis.population.sw.full.75.bus.share <- dis.population.sw.full.75.bus / dis.population.near.bus
dis.population.sw.full.100.bus.share <- dis.population.sw.full.100.bus / dis.population.near.bus

eld.population.sw.full.25.bus <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=0.25 & non_hct=="Bus") %>% select(elderly_population) %>% pull() %>% sum()
eld.population.sw.full.50.bus <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=0.50 & non_hct=="Bus") %>% select(elderly_population) %>% pull() %>% sum()
eld.population.sw.full.75.bus <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=0.75 & non_hct=="Bus") %>% select(elderly_population) %>% pull() %>% sum()
eld.population.sw.full.100.bus <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=1 & non_hct=="Bus") %>% select(elderly_population) %>% pull() %>% sum()
eld.population.sw.full.25.bus.share <- eld.population.sw.full.25.bus / eld.population.near.bus
eld.population.sw.full.50.bus.share <- eld.population.sw.full.50.bus / eld.population.near.bus
eld.population.sw.full.75.bus.share <- eld.population.sw.full.75.bus / eld.population.near.bus
eld.population.sw.full.100.bus.share <- eld.population.sw.full.100.bus / eld.population.near.bus

lep.population.sw.full.25.bus <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=0.25 & non_hct=="Bus") %>% select(limited_english_population) %>% pull() %>% sum()
lep.population.sw.full.50.bus <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=0.50 & non_hct=="Bus") %>% select(limited_english_population) %>% pull() %>% sum()
lep.population.sw.full.75.bus <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=0.75 & non_hct=="Bus") %>% select(limited_english_population) %>% pull() %>% sum()
lep.population.sw.full.100.bus <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=1 & non_hct=="Bus") %>% select(limited_english_population) %>% pull() %>% sum()
lep.population.sw.full.25.bus.share <- lep.population.sw.full.25.bus / lep.population.near.bus
lep.population.sw.full.50.bus.share <- lep.population.sw.full.50.bus / lep.population.near.bus
lep.population.sw.full.75.bus.share <- lep.population.sw.full.75.bus / lep.population.near.bus
lep.population.sw.full.100.bus.share <- lep.population.sw.full.100.bus / lep.population.near.bus

you.population.sw.full.25.bus <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=0.25 & non_hct=="Bus") %>% select(youth_population) %>% pull() %>% sum()
you.population.sw.full.50.bus <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=0.50 & non_hct=="Bus") %>% select(youth_population) %>% pull() %>% sum()
you.population.sw.full.75.bus <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=0.75 & non_hct=="Bus") %>% select(youth_population) %>% pull() %>% sum()
you.population.sw.full.100.bus <- combined.data %>% filter((full_sidewalk_length+partial_sidewalk_length)/total_length>=1 & non_hct=="Bus") %>% select(youth_population) %>% pull() %>% sum()
you.population.sw.full.25.bus.share <- you.population.sw.full.25.bus / you.population.near.bus
you.population.sw.full.50.bus.share <- you.population.sw.full.50.bus / you.population.near.bus
you.population.sw.full.75.bus.share <- you.population.sw.full.75.bus / you.population.near.bus
you.population.sw.full.100.bus.share <- you.population.sw.full.100.bus / you.population.near.bus

population.near.bus.sidewalks.table <- data.table(
  `Geography` = c("Region", 
                  "King County",
                  "Kitsap County",
                  "Pierce County",
                  "Snohomish County",
                  "People with a Disability", 
                  "People over 65", 
                  "People with Limited English Proficiency", 
                  "People of Color", 
                  "People in Poverty", 
                  "People under 18"),
  `Population` = c(total.population,
                   king.population,
                   kitsap.population,
                   pierce.population,
                   snohomish.population,
                   dis.population,
                   eld.population,
                   lep.population,
                   poc.population,
                   pov.population,
                   you.population),
  `Share of Population` = c(total.population.shr,
                            king.population.shr,
                            kitsap.population.shr,
                            pierce.population.shr,
                            snohomish.population.shr,
                            dis.population.shr,
                            eld.population.shr,
                            lep.population.shr,
                            poc.population.shr,
                            pov.population.shr,
                            you.population.shr),
  `Share of Population within 1/4 mile of Transit` = c(total.population.near.bus.share,
                                                                     king.population.near.bus.share,
                                                                     kitsap.population.near.bus.share,
                                                                     pierce.population.near.bus.share,
                                                                     snohomish.population.near.bus.share,
                                                                     dis.population.near.bus.share,
                                                                     eld.population.near.bus.share,
                                                                     lep.population.near.bus.share,
                                                                     poc.population.near.bus.share,
                                                                     pov.population.near.bus.share,
                                                                     you.population.near.bus.share),
  `Share of Population near Transit with at least 25% Sidewalks` = c(total.population.sw.full.25.bus.share,
                                                                 king.population.sw.full.25.bus.share,
                                                                 kitsap.population.sw.full.25.bus.share,
                                                                 pierce.population.sw.full.25.bus.share,
                                                                 snohomish.population.sw.full.25.bus.share,
                                                                 dis.population.sw.full.25.bus.share,
                                                                 eld.population.sw.full.25.bus.share,
                                                                 lep.population.sw.full.25.bus.share,
                                                                 poc.population.sw.full.25.bus.share,
                                                                 pov.population.sw.full.25.bus.share,
                                                                 you.population.sw.full.25.bus.share),
  `Share of Population near Transit with at least 50% Sidewalks` = c(total.population.sw.full.50.bus.share,
                                                                 king.population.sw.full.50.bus.share,
                                                                 kitsap.population.sw.full.50.bus.share,
                                                                 pierce.population.sw.full.50.bus.share,
                                                                 snohomish.population.sw.full.50.bus.share,
                                                                 dis.population.sw.full.50.bus.share,
                                                                 eld.population.sw.full.50.bus.share,
                                                                 lep.population.sw.full.50.bus.share,
                                                                 poc.population.sw.full.50.bus.share,
                                                                 pov.population.sw.full.50.bus.share,
                                                                 you.population.sw.full.50.bus.share),
  `Share of Population near Transit with at least 75% Sidewalks` = c(total.population.sw.full.75.bus.share,
                                                                 king.population.sw.full.75.bus.share,
                                                                 kitsap.population.sw.full.75.bus.share,
                                                                 pierce.population.sw.full.75.bus.share,
                                                                 snohomish.population.sw.full.75.bus.share,
                                                                 dis.population.sw.full.75.bus.share,
                                                                 eld.population.sw.full.75.bus.share,
                                                                 lep.population.sw.full.75.bus.share,
                                                                 poc.population.sw.full.75.bus.share,
                                                                 pov.population.sw.full.75.bus.share,
                                                                 you.population.sw.full.75.bus.share),
  `Share of Population near Transit with 100% Sidewalks` = c(total.population.sw.full.100.bus.share,
                                                         king.population.sw.full.100.bus.share,
                                                         kitsap.population.sw.full.100.bus.share,
                                                         pierce.population.sw.full.100.bus.share,
                                                         snohomish.population.sw.full.100.bus.share,
                                                         dis.population.sw.full.100.bus.share,
                                                         eld.population.sw.full.100.bus.share,
                                                         lep.population.sw.full.100.bus.share,
                                                         poc.population.sw.full.100.bus.share,
                                                         pov.population.sw.full.100.bus.share,
                                                         you.population.sw.full.100.bus.share)
  
)

fwrite(population.near.bus.sidewalks.table, here('output','people_near_transit_with_sidewalks.csv'))

# NonMotorized Network Near HCT ---------------------------------------------------------
if(run.hct.network.overlay == 'yes') {
  hct.stops <- create.stops.buffer.no.dissolve(f=transit.url, r=hct.modes, d=hct.buffer.dist, t="High Capacity Transit")
  
  nonmotorized.layer <- st_read(here('data','ElmerGeo_BikePed_042021.gdb')) %>% 
    select(ped_complete, bike_complete) %>%
    st_transform(spn) %>%
    mutate(len=st_length(x=Shape))
  
  temp.blocks <- block.layer %>% select(block_geoid, disabled, elderly, limited_english, poverty, people_of_color, youth)
  temp.equ <- st_intersection(hct.stops, temp.blocks) %>% 
    st_drop_geometry() %>%
    select(-block_geoid, -station_area) %>%
    group_by(stop_id) %>%
    summarize(disabled=sum(disabled), elderly=sum(elderly), limited_english=sum(limited_english), poverty=sum(poverty), people_of_color=sum(people_of_color), youth=sum(youth))
  
  hct.stops <- left_join(hct.stops, temp.equ, by=c("stop_id"))
  
  temp.nm <- st_intersection(hct.stops, nonmotorized.layer) %>% 
    st_drop_geometry()
  
  # Sidewalks
  t <- temp.nm %>% select(stop_id,len) %>% group_by(stop_id) %>% summarize(facility_length=sum(len))
  c <- temp.nm %>% filter(ped_complete=="Complete Facilities") %>% select(stop_id,len) %>% group_by(stop_id) %>% summarize(complete_sidewalk=sum(len))
  p <- temp.nm %>% filter(ped_complete=="Partial Facilities") %>% select(stop_id,len) %>% group_by(stop_id) %>% summarize(partial_sidewalk=sum(len))
  n <- temp.nm %>% filter(ped_complete=="No Facilities") %>% select(stop_id,len) %>% group_by(stop_id) %>% summarize(no_sidewalk=sum(len))
  
  hct.stops <- list(hct.stops, t, c, p, n) %>% 
    reduce(left_join, by = "stop_id") %>%
    mutate(across(everything(), ~replace_na(.x, 0)))

  # Bike Facilities
  c <- temp.nm %>% filter(bike_complete=="Complete Facilities") %>% select(stop_id,len) %>% group_by(stop_id) %>% summarize(complete_bike=sum(len))
  p <- temp.nm %>% filter(bike_complete=="Partial Facilities") %>% select(stop_id,len) %>% group_by(stop_id) %>% summarize(partial_bike=sum(len))
  n <- temp.nm %>% filter(bike_complete=="No Facilities") %>% select(stop_id,len) %>% group_by(stop_id) %>% summarize(no_bike=sum(len))
  
  hct.stops <- list(hct.stops, c, p, n) %>% 
    reduce(left_join, by = "stop_id") %>%
    mutate(across(everything(), ~replace_na(.x, 0)))
 
  # County Location for Stops
  county.layer <- st_read(here('data','countybackground.shp')) %>% st_transform(spn) %>% filter(PSRC==1) %>% select(COUNTY_NM)
  hct.points <- hct.stops %>% select(stop_id) %>% st_centroid()
  s <- st_intersection(hct.points, county.layer) %>% st_drop_geometry() %>% rename(county=COUNTY_NM)
  hct.stop.summary <- left_join(hct.stops, s, by=c("stop_id")) %>% st_drop_geometry()
  fwrite(hct.stop.summary, here('output','hct_station_areas_network_data.csv'))

} else {
  hct.stop.summary <- fread(here('output','hct_station_areas_network_data.csv'))
}

# NonMotorized Network Near Transit ---------------------------------------------------------
if(run.transit.network.overlay == 'yes') {
  bus.stops <- create.stops.buffer.no.dissolve(f=transit.url, r=bus.modes, d=bus.buffer.dist, t="Bus")
  
  nonmotorized.layer <- st_read(here('data','ElmerGeo_BikePed_042021.gdb')) %>% 
    select(ped_complete, bike_complete) %>%
    st_transform(spn) %>%
    mutate(len=st_length(x=Shape))
  
  temp.blocks <- block.layer %>% select(block_geoid, disabled, elderly, limited_english, poverty, people_of_color, youth)
  temp.equ <- st_intersection(bus.stops, temp.blocks) %>% 
    st_drop_geometry() %>%
    select(-block_geoid, -station_area) %>%
    group_by(stop_id) %>%
    summarize(disabled=sum(disabled), elderly=sum(elderly), limited_english=sum(limited_english), poverty=sum(poverty), people_of_color=sum(people_of_color), youth=sum(youth))
  
  bus.stops <- left_join(bus.stops, temp.equ, by=c("stop_id"))
  
  temp.nm <- st_intersection(bus.stops, nonmotorized.layer) %>% 
    st_drop_geometry()
  
  # Sidewalks
  t <- temp.nm %>% select(stop_id,len) %>% group_by(stop_id) %>% summarize(facility_length=sum(len))
  c <- temp.nm %>% filter(ped_complete=="Complete Facilities") %>% select(stop_id,len) %>% group_by(stop_id) %>% summarize(complete_sidewalk=sum(len))
  p <- temp.nm %>% filter(ped_complete=="Partial Facilities") %>% select(stop_id,len) %>% group_by(stop_id) %>% summarize(partial_sidewalk=sum(len))
  n <- temp.nm %>% filter(ped_complete=="No Facilities") %>% select(stop_id,len) %>% group_by(stop_id) %>% summarize(no_sidewalk=sum(len))
  
  bus.stops <- list(bus.stops, t, c, p, n) %>% 
    reduce(left_join, by = "stop_id") %>%
    mutate(across(everything(), ~replace_na(.x, 0)))
  
  # Bike Facilities
  c <- temp.nm %>% filter(bike_complete=="Complete Facilities") %>% select(stop_id,len) %>% group_by(stop_id) %>% summarize(complete_bike=sum(len))
  p <- temp.nm %>% filter(bike_complete=="Partial Facilities") %>% select(stop_id,len) %>% group_by(stop_id) %>% summarize(partial_bike=sum(len))
  n <- temp.nm %>% filter(bike_complete=="No Facilities") %>% select(stop_id,len) %>% group_by(stop_id) %>% summarize(no_bike=sum(len))
  
  bus.stops <- list(bus.stops, c, p, n) %>% 
    reduce(left_join, by = "stop_id") %>%
    mutate(across(everything(), ~replace_na(.x, 0)))
  
  # County Location for Stops
  county.layer <- st_read(here('data','countybackground.shp')) %>% st_transform(spn) %>% filter(PSRC==1) %>% select(COUNTY_NM)
  bus.points <- bus.stops %>% select(stop_id) %>% st_centroid()
  s <- st_intersection(bus.points, county.layer) %>% st_drop_geometry() %>% rename(county=COUNTY_NM)
  bus.stop.summary <- left_join(bus.stops, s, by=c("stop_id")) %>% st_drop_geometry()
  fwrite(bus.stop.summary, here('output','nonhct_station_areas_network_data.csv'))
  
} else {
  bus.stop.summary <- fread(here('output','nonhct_station_areas_network_data.csv'))
}

# Network Detail Near HCT -------------------------------------------------
region.hct.stations <- hct.stop.summary %>% select(stop_id) %>% distinct() %>% pull() %>% length()
king.hct.stations <- hct.stop.summary %>% filter(county%in%"King") %>% select(stop_id) %>% distinct() %>% pull() %>% length()
kitsap.hct.stations <- hct.stop.summary %>% filter(county%in%"Kitsap") %>% select(stop_id) %>% distinct() %>% pull() %>% length()
pierce.hct.stations <- hct.stop.summary %>% filter(county%in%"Pierce") %>% select(stop_id) %>% distinct() %>% pull() %>% length()
snohomish.hct.stations <- hct.stop.summary %>% filter(county%in%"Snohomish") %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
eld.hct.stations <- hct.stop.summary %>% filter(elderly>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
dis.hct.stations <- hct.stop.summary %>% filter(disabled>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
lep.hct.stations <- hct.stop.summary %>% filter(limited_english>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
poc.hct.stations <- hct.stop.summary %>% filter(people_of_color>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
pov.hct.stations <- hct.stop.summary %>% filter(poverty>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
you.hct.stations <- hct.stop.summary %>% filter(youth>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 

region.hct.stations.share <- region.hct.stations / region.hct.stations
king.hct.stations.share <- king.hct.stations / region.hct.stations
kitsap.hct.stations.share <- kitsap.hct.stations / region.hct.stations
pierce.hct.stations.share <- pierce.hct.stations / region.hct.stations
snohomish.hct.stations.share <- snohomish.hct.stations / region.hct.stations
eld.hct.stations.share <- eld.hct.stations / region.hct.stations
dis.hct.stations.share <- dis.hct.stations / region.hct.stations
lep.hct.stations.share <- lep.hct.stations / region.hct.stations
poc.hct.stations.share <- poc.hct.stations / region.hct.stations
pov.hct.stations.share <- pov.hct.stations / region.hct.stations
you.hct.stations.share <- you.hct.stations / region.hct.stations

# 25% Sidewalk Coverage
region.hct.stations.sw.25 <- hct.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=0.25) %>% select(stop_id) %>% distinct() %>% pull() %>% length()
king.hct.stations.sw.25 <- hct.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=0.25 & county%in%"King") %>% select(stop_id) %>% distinct() %>% pull() %>% length()
kitsap.hct.stations.sw.25 <- hct.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=0.25 & county%in%"Kitsap") %>% select(stop_id) %>% distinct() %>% pull() %>% length()
pierce.hct.stations.sw.25 <- hct.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=0.25 & county%in%"Pierce") %>% select(stop_id) %>% distinct() %>% pull() %>% length()
snohomish.hct.stations.sw.25 <- hct.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=0.25 & county%in%"Snohomish") %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
eld.hct.stations.sw.25 <- hct.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=0.25 & elderly>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
dis.hct.stations.sw.25 <- hct.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=0.25 & disabled>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
lep.hct.stations.sw.25 <- hct.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=0.25 & limited_english>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
poc.hct.stations.sw.25 <- hct.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=0.25 & people_of_color>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
pov.hct.stations.sw.25 <- hct.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=0.25 & poverty>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
you.hct.stations.sw.25 <- hct.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=0.25 & youth>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 

region.hct.stations.sw.25.share <- region.hct.stations.sw.25 / region.hct.stations
king.hct.stations.sw.25.share <- king.hct.stations.sw.25 / king.hct.stations
kitsap.hct.stations.sw.25.share <- kitsap.hct.stations.sw.25 / kitsap.hct.stations
pierce.hct.stations.sw.25.share <- pierce.hct.stations.sw.25 / pierce.hct.stations
snohomish.hct.stations.sw.25.share <- snohomish.hct.stations.sw.25 / snohomish.hct.stations
eld.hct.stations.sw.25.share <- eld.hct.stations.sw.25 / eld.hct.stations
dis.hct.stations.sw.25.share <- dis.hct.stations.sw.25 / dis.hct.stations
lep.hct.stations.sw.25.share <- lep.hct.stations.sw.25 / lep.hct.stations
poc.hct.stations.sw.25.share <- poc.hct.stations.sw.25 / poc.hct.stations
pov.hct.stations.sw.25.share <- pov.hct.stations.sw.25 / pov.hct.stations
you.hct.stations.sw.25.share <- you.hct.stations.sw.25 / you.hct.stations

# 50% Sidewalk Coverage
region.hct.stations.sw.50 <- hct.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=0.50) %>% select(stop_id) %>% distinct() %>% pull() %>% length()
king.hct.stations.sw.50 <- hct.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=0.50 & county%in%"King") %>% select(stop_id) %>% distinct() %>% pull() %>% length()
kitsap.hct.stations.sw.50 <- hct.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=0.50 & county%in%"Kitsap") %>% select(stop_id) %>% distinct() %>% pull() %>% length()
pierce.hct.stations.sw.50 <- hct.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=0.50 & county%in%"Pierce") %>% select(stop_id) %>% distinct() %>% pull() %>% length()
snohomish.hct.stations.sw.50 <- hct.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=0.50 & county%in%"Snohomish") %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
eld.hct.stations.sw.50 <- hct.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=0.50 & elderly>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
dis.hct.stations.sw.50 <- hct.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=0.50 & disabled>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
lep.hct.stations.sw.50 <- hct.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=0.50 & limited_english>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
poc.hct.stations.sw.50 <- hct.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=0.50 & people_of_color>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
pov.hct.stations.sw.50 <- hct.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=0.50 & poverty>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
you.hct.stations.sw.50 <- hct.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=0.50 & youth>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 

region.hct.stations.sw.50.share <- region.hct.stations.sw.50 / region.hct.stations
king.hct.stations.sw.50.share <- king.hct.stations.sw.50 / king.hct.stations
kitsap.hct.stations.sw.50.share <- kitsap.hct.stations.sw.50 / kitsap.hct.stations
pierce.hct.stations.sw.50.share <- pierce.hct.stations.sw.50 / pierce.hct.stations
snohomish.hct.stations.sw.50.share <- snohomish.hct.stations.sw.50 / snohomish.hct.stations
eld.hct.stations.sw.50.share <- eld.hct.stations.sw.50 / eld.hct.stations
dis.hct.stations.sw.50.share <- dis.hct.stations.sw.50 / dis.hct.stations
lep.hct.stations.sw.50.share <- lep.hct.stations.sw.50 / lep.hct.stations
poc.hct.stations.sw.50.share <- poc.hct.stations.sw.50 / poc.hct.stations
pov.hct.stations.sw.50.share <- pov.hct.stations.sw.50 / pov.hct.stations
you.hct.stations.sw.50.share <- you.hct.stations.sw.50 / you.hct.stations

# 75% Sidewalk Coverage
region.hct.stations.sw.75 <- hct.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=0.75) %>% select(stop_id) %>% distinct() %>% pull() %>% length()
king.hct.stations.sw.75 <- hct.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=0.75 & county%in%"King") %>% select(stop_id) %>% distinct() %>% pull() %>% length()
kitsap.hct.stations.sw.75 <- hct.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=0.75 & county%in%"Kitsap") %>% select(stop_id) %>% distinct() %>% pull() %>% length()
pierce.hct.stations.sw.75 <- hct.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=0.75 & county%in%"Pierce") %>% select(stop_id) %>% distinct() %>% pull() %>% length()
snohomish.hct.stations.sw.75 <- hct.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=0.75 & county%in%"Snohomish") %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
eld.hct.stations.sw.75 <- hct.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=0.75 & elderly>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
dis.hct.stations.sw.75 <- hct.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=0.75 & disabled>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
lep.hct.stations.sw.75 <- hct.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=0.75 & limited_english>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
poc.hct.stations.sw.75 <- hct.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=0.75 & people_of_color>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
pov.hct.stations.sw.75 <- hct.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=0.75 & poverty>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
you.hct.stations.sw.75 <- hct.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=0.75 & youth>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 

region.hct.stations.sw.75.share <- region.hct.stations.sw.75 / region.hct.stations
king.hct.stations.sw.75.share <- king.hct.stations.sw.75 / king.hct.stations
kitsap.hct.stations.sw.75.share <- kitsap.hct.stations.sw.75 / kitsap.hct.stations
pierce.hct.stations.sw.75.share <- pierce.hct.stations.sw.75 / pierce.hct.stations
snohomish.hct.stations.sw.75.share <- snohomish.hct.stations.sw.75 / snohomish.hct.stations
eld.hct.stations.sw.75.share <- eld.hct.stations.sw.75 / eld.hct.stations
dis.hct.stations.sw.75.share <- dis.hct.stations.sw.75 / dis.hct.stations
lep.hct.stations.sw.75.share <- lep.hct.stations.sw.75 / lep.hct.stations
poc.hct.stations.sw.75.share <- poc.hct.stations.sw.75 / poc.hct.stations
pov.hct.stations.sw.75.share <- pov.hct.stations.sw.75 / pov.hct.stations
you.hct.stations.sw.75.share <- you.hct.stations.sw.75 / you.hct.stations

# 100% Sidewalk Coverage
region.hct.stations.sw.100 <- hct.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=1) %>% select(stop_id) %>% distinct() %>% pull() %>% length()
king.hct.stations.sw.100 <- hct.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=1 & county%in%"King") %>% select(stop_id) %>% distinct() %>% pull() %>% length()
kitsap.hct.stations.sw.100 <- hct.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=1 & county%in%"Kitsap") %>% select(stop_id) %>% distinct() %>% pull() %>% length()
pierce.hct.stations.sw.100 <- hct.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=1 & county%in%"Pierce") %>% select(stop_id) %>% distinct() %>% pull() %>% length()
snohomish.hct.stations.sw.100 <- hct.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=1 & county%in%"Snohomish") %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
eld.hct.stations.sw.100 <- hct.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=1 & elderly>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
dis.hct.stations.sw.100 <- hct.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=1 & disabled>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
lep.hct.stations.sw.100 <- hct.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=1 & limited_english>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
poc.hct.stations.sw.100 <- hct.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=1 & people_of_color>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
pov.hct.stations.sw.100 <- hct.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=1 & poverty>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
you.hct.stations.sw.100 <- hct.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=1 & youth>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 

region.hct.stations.sw.100.share <- region.hct.stations.sw.100 / region.hct.stations
king.hct.stations.sw.100.share <- king.hct.stations.sw.100 / king.hct.stations
kitsap.hct.stations.sw.100.share <- kitsap.hct.stations.sw.100 / kitsap.hct.stations
pierce.hct.stations.sw.100.share <- pierce.hct.stations.sw.100 / pierce.hct.stations
snohomish.hct.stations.sw.100.share <- snohomish.hct.stations.sw.100 / snohomish.hct.stations
eld.hct.stations.sw.100.share <- eld.hct.stations.sw.100 / eld.hct.stations
dis.hct.stations.sw.100.share <- dis.hct.stations.sw.100 / dis.hct.stations
lep.hct.stations.sw.100.share <- lep.hct.stations.sw.100 / lep.hct.stations
poc.hct.stations.sw.100.share <- poc.hct.stations.sw.100 / poc.hct.stations
pov.hct.stations.sw.100.share <- pov.hct.stations.sw.100 / pov.hct.stations
you.hct.stations.sw.100.share <- you.hct.stations.sw.100 / you.hct.stations

# 25% Bike Coverage
region.hct.stations.bk.25 <- hct.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=0.25) %>% select(stop_id) %>% distinct() %>% pull() %>% length()
king.hct.stations.bk.25 <- hct.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=0.25 & county%in%"King") %>% select(stop_id) %>% distinct() %>% pull() %>% length()
kitsap.hct.stations.bk.25 <- hct.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=0.25 & county%in%"Kitsap") %>% select(stop_id) %>% distinct() %>% pull() %>% length()
pierce.hct.stations.bk.25 <- hct.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=0.25 & county%in%"Pierce") %>% select(stop_id) %>% distinct() %>% pull() %>% length()
snohomish.hct.stations.bk.25 <- hct.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=0.25 & county%in%"Snohomish") %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
eld.hct.stations.bk.25 <- hct.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=0.25 & elderly>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
dis.hct.stations.bk.25 <- hct.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=0.25 & disabled>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
lep.hct.stations.bk.25 <- hct.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=0.25 & limited_english>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
poc.hct.stations.bk.25 <- hct.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=0.25 & people_of_color>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
pov.hct.stations.bk.25 <- hct.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=0.25 & poverty>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
you.hct.stations.bk.25 <- hct.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=0.25 & youth>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 

region.hct.stations.bk.25.share <- region.hct.stations.bk.25 / region.hct.stations
king.hct.stations.bk.25.share <- king.hct.stations.bk.25 / king.hct.stations
kitsap.hct.stations.bk.25.share <- kitsap.hct.stations.bk.25 / kitsap.hct.stations
pierce.hct.stations.bk.25.share <- pierce.hct.stations.bk.25 / pierce.hct.stations
snohomish.hct.stations.bk.25.share <- snohomish.hct.stations.bk.25 / snohomish.hct.stations
eld.hct.stations.bk.25.share <- eld.hct.stations.bk.25 / eld.hct.stations
dis.hct.stations.bk.25.share <- dis.hct.stations.bk.25 / dis.hct.stations
lep.hct.stations.bk.25.share <- lep.hct.stations.bk.25 / lep.hct.stations
poc.hct.stations.bk.25.share <- poc.hct.stations.bk.25 / poc.hct.stations
pov.hct.stations.bk.25.share <- pov.hct.stations.bk.25 / pov.hct.stations
you.hct.stations.bk.25.share <- you.hct.stations.bk.25 / you.hct.stations

# 50% Bike Coverage
region.hct.stations.bk.50 <- hct.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=0.50) %>% select(stop_id) %>% distinct() %>% pull() %>% length()
king.hct.stations.bk.50 <- hct.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=0.50 & county%in%"King") %>% select(stop_id) %>% distinct() %>% pull() %>% length()
kitsap.hct.stations.bk.50 <- hct.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=0.50 & county%in%"Kitsap") %>% select(stop_id) %>% distinct() %>% pull() %>% length()
pierce.hct.stations.bk.50 <- hct.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=0.50 & county%in%"Pierce") %>% select(stop_id) %>% distinct() %>% pull() %>% length()
snohomish.hct.stations.bk.50 <- hct.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=0.50 & county%in%"Snohomish") %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
eld.hct.stations.bk.50 <- hct.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=0.50 & elderly>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
dis.hct.stations.bk.50 <- hct.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=0.50 & disabled>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
lep.hct.stations.bk.50 <- hct.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=0.50 & limited_english>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
poc.hct.stations.bk.50 <- hct.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=0.50 & people_of_color>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
pov.hct.stations.bk.50 <- hct.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=0.50 & poverty>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
you.hct.stations.bk.50 <- hct.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=0.50 & youth>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 

region.hct.stations.bk.50.share <- region.hct.stations.bk.50 / region.hct.stations
king.hct.stations.bk.50.share <- king.hct.stations.bk.50 / king.hct.stations
kitsap.hct.stations.bk.50.share <- kitsap.hct.stations.bk.50 / kitsap.hct.stations
pierce.hct.stations.bk.50.share <- pierce.hct.stations.bk.50 / pierce.hct.stations
snohomish.hct.stations.bk.50.share <- snohomish.hct.stations.bk.50 / snohomish.hct.stations
eld.hct.stations.bk.50.share <- eld.hct.stations.bk.50 / eld.hct.stations
dis.hct.stations.bk.50.share <- dis.hct.stations.bk.50 / dis.hct.stations
lep.hct.stations.bk.50.share <- lep.hct.stations.bk.50 / lep.hct.stations
poc.hct.stations.bk.50.share <- poc.hct.stations.bk.50 / poc.hct.stations
pov.hct.stations.bk.50.share <- pov.hct.stations.bk.50 / pov.hct.stations
you.hct.stations.bk.50.share <- you.hct.stations.bk.50 / you.hct.stations

# 75% Bike Coverage
region.hct.stations.bk.75 <- hct.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=0.75) %>% select(stop_id) %>% distinct() %>% pull() %>% length()
king.hct.stations.bk.75 <- hct.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=0.75 & county%in%"King") %>% select(stop_id) %>% distinct() %>% pull() %>% length()
kitsap.hct.stations.bk.75 <- hct.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=0.75 & county%in%"Kitsap") %>% select(stop_id) %>% distinct() %>% pull() %>% length()
pierce.hct.stations.bk.75 <- hct.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=0.75 & county%in%"Pierce") %>% select(stop_id) %>% distinct() %>% pull() %>% length()
snohomish.hct.stations.bk.75 <- hct.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=0.75 & county%in%"Snohomish") %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
eld.hct.stations.bk.75 <- hct.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=0.75 & elderly>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
dis.hct.stations.bk.75 <- hct.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=0.75 & disabled>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
lep.hct.stations.bk.75 <- hct.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=0.75 & limited_english>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
poc.hct.stations.bk.75 <- hct.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=0.75 & people_of_color>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
pov.hct.stations.bk.75 <- hct.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=0.75 & poverty>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
you.hct.stations.bk.75 <- hct.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=0.75 & youth>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 

region.hct.stations.bk.75.share <- region.hct.stations.bk.75 / region.hct.stations
king.hct.stations.bk.75.share <- king.hct.stations.bk.75 / king.hct.stations
kitsap.hct.stations.bk.75.share <- kitsap.hct.stations.bk.75 / kitsap.hct.stations
pierce.hct.stations.bk.75.share <- pierce.hct.stations.bk.75 / pierce.hct.stations
snohomish.hct.stations.bk.75.share <- snohomish.hct.stations.bk.75 / snohomish.hct.stations
eld.hct.stations.bk.75.share <- eld.hct.stations.bk.75 / eld.hct.stations
dis.hct.stations.bk.75.share <- dis.hct.stations.bk.75 / dis.hct.stations
lep.hct.stations.bk.75.share <- lep.hct.stations.bk.75 / lep.hct.stations
poc.hct.stations.bk.75.share <- poc.hct.stations.bk.75 / poc.hct.stations
pov.hct.stations.bk.75.share <- pov.hct.stations.bk.75 / pov.hct.stations
you.hct.stations.bk.75.share <- you.hct.stations.bk.75 / you.hct.stations

# 100% Bike Coverage
region.hct.stations.bk.100 <- hct.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=1) %>% select(stop_id) %>% distinct() %>% pull() %>% length()
king.hct.stations.bk.100 <- hct.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=1 & county%in%"King") %>% select(stop_id) %>% distinct() %>% pull() %>% length()
kitsap.hct.stations.bk.100 <- hct.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=1 & county%in%"Kitsap") %>% select(stop_id) %>% distinct() %>% pull() %>% length()
pierce.hct.stations.bk.100 <- hct.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=1 & county%in%"Pierce") %>% select(stop_id) %>% distinct() %>% pull() %>% length()
snohomish.hct.stations.bk.100 <- hct.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=1 & county%in%"Snohomish") %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
eld.hct.stations.bk.100 <- hct.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=1 & elderly>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
dis.hct.stations.bk.100 <- hct.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=1 & disabled>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
lep.hct.stations.bk.100 <- hct.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=1 & limited_english>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
poc.hct.stations.bk.100 <- hct.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=1 & people_of_color>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
pov.hct.stations.bk.100 <- hct.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=1 & poverty>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
you.hct.stations.bk.100 <- hct.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=1 & youth>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 

region.hct.stations.bk.100.share <- region.hct.stations.bk.100 / region.hct.stations
king.hct.stations.bk.100.share <- king.hct.stations.bk.100 / king.hct.stations
kitsap.hct.stations.bk.100.share <- kitsap.hct.stations.bk.100 / kitsap.hct.stations
pierce.hct.stations.bk.100.share <- pierce.hct.stations.bk.100 / pierce.hct.stations
snohomish.hct.stations.bk.100.share <- snohomish.hct.stations.bk.100 / snohomish.hct.stations
eld.hct.stations.bk.100.share <- eld.hct.stations.bk.100 / eld.hct.stations
dis.hct.stations.bk.100.share <- dis.hct.stations.bk.100 / dis.hct.stations
lep.hct.stations.bk.100.share <- lep.hct.stations.bk.100 / lep.hct.stations
poc.hct.stations.bk.100.share <- poc.hct.stations.bk.100 / poc.hct.stations
pov.hct.stations.bk.100.share <- pov.hct.stations.bk.100 / pov.hct.stations
you.hct.stations.bk.100.share <- you.hct.stations.bk.100 / you.hct.stations

hct.stations.table <- data.table(
  `Geography` = c("Region", 
                  "King County",
                  "Kitsap County",
                  "Pierce County",
                  "Snohomish County",
                  "People with a Disability", 
                  "People over 65", 
                  "People with Limited English Proficiency", 
                  "People of Color", 
                  "People in Poverty", 
                  "People under 18"),
  `High Capacity Transit Stations` = c(region.hct.stations,
                   king.hct.stations,
                   kitsap.hct.stations,
                   pierce.hct.stations,
                   snohomish.hct.stations,
                   dis.hct.stations,
                   eld.hct.stations,
                   lep.hct.stations,
                   poc.hct.stations,
                   pov.hct.stations,
                   you.hct.stations),
  `Share of High Capacity Transit Stations` = c(region.hct.stations.share,
                                                king.hct.stations.share,
                                                kitsap.hct.stations.share,
                                                pierce.hct.stations.share,
                                                snohomish.hct.stations.share,
                                                dis.hct.stations.share,
                                                eld.hct.stations.share,
                                                lep.hct.stations.share,
                                                poc.hct.stations.share,
                                                pov.hct.stations.share,
                                                you.hct.stations.share),
`Share of High Capacity Transit Stations with at least 25% Sidewalk Coverage` = c(region.hct.stations.sw.25.share,
                                              king.hct.stations.sw.25.share,
                                              kitsap.hct.stations.sw.25.share,
                                              pierce.hct.stations.sw.25.share,
                                              snohomish.hct.stations.sw.25.share,
                                              dis.hct.stations.sw.25.share,
                                              eld.hct.stations.sw.25.share,
                                              lep.hct.stations.sw.25.share,
                                              poc.hct.stations.sw.25.share,
                                              pov.hct.stations.sw.25.share,
                                              you.hct.stations.sw.25.share),
`Share of High Capacity Transit Stations with at least 50% Sidewalk Coverage` = c(region.hct.stations.sw.50.share,
                                                                                  king.hct.stations.sw.50.share,
                                                                                  kitsap.hct.stations.sw.50.share,
                                                                                  pierce.hct.stations.sw.50.share,
                                                                                  snohomish.hct.stations.sw.50.share,
                                                                                  dis.hct.stations.sw.50.share,
                                                                                  eld.hct.stations.sw.50.share,
                                                                                  lep.hct.stations.sw.50.share,
                                                                                  poc.hct.stations.sw.50.share,
                                                                                  pov.hct.stations.sw.50.share,
                                                                                  you.hct.stations.sw.50.share),
`Share of High Capacity Transit Stations with at least 75% Sidewalk Coverage` = c(region.hct.stations.sw.75.share,
                                                                                  king.hct.stations.sw.75.share,
                                                                                  kitsap.hct.stations.sw.75.share,
                                                                                  pierce.hct.stations.sw.75.share,
                                                                                  snohomish.hct.stations.sw.75.share,
                                                                                  dis.hct.stations.sw.75.share,
                                                                                  eld.hct.stations.sw.75.share,
                                                                                  lep.hct.stations.sw.75.share,
                                                                                  poc.hct.stations.sw.75.share,
                                                                                  pov.hct.stations.sw.75.share,
                                                                                  you.hct.stations.sw.75.share),
`Share of High Capacity Transit Stations with Complete Sidewalk Coverage` = c(region.hct.stations.sw.100.share,
                                                                                  king.hct.stations.sw.100.share,
                                                                                  kitsap.hct.stations.sw.100.share,
                                                                                  pierce.hct.stations.sw.100.share,
                                                                                  snohomish.hct.stations.sw.100.share,
                                                                                  dis.hct.stations.sw.100.share,
                                                                                  eld.hct.stations.sw.100.share,
                                                                                  lep.hct.stations.sw.100.share,
                                                                                  poc.hct.stations.sw.100.share,
                                                                                  pov.hct.stations.sw.100.share,
                                                                                  you.hct.stations.sw.100.share),
`Share of High Capacity Transit Stations with at least 25% Bike Coverage` = c(region.hct.stations.bk.25.share,
                                                                                  king.hct.stations.bk.25.share,
                                                                                  kitsap.hct.stations.bk.25.share,
                                                                                  pierce.hct.stations.bk.25.share,
                                                                                  snohomish.hct.stations.bk.25.share,
                                                                                  dis.hct.stations.bk.25.share,
                                                                                  eld.hct.stations.bk.25.share,
                                                                                  lep.hct.stations.bk.25.share,
                                                                                  poc.hct.stations.bk.25.share,
                                                                                  pov.hct.stations.bk.25.share,
                                                                                  you.hct.stations.bk.25.share),
`Share of High Capacity Transit Stations with at least 50% Bike Coverage` = c(region.hct.stations.bk.50.share,
                                                                                  king.hct.stations.bk.50.share,
                                                                                  kitsap.hct.stations.bk.50.share,
                                                                                  pierce.hct.stations.bk.50.share,
                                                                                  snohomish.hct.stations.bk.50.share,
                                                                                  dis.hct.stations.bk.50.share,
                                                                                  eld.hct.stations.bk.50.share,
                                                                                  lep.hct.stations.bk.50.share,
                                                                                  poc.hct.stations.bk.50.share,
                                                                                  pov.hct.stations.bk.50.share,
                                                                                  you.hct.stations.bk.50.share),
`Share of High Capacity Transit Stations with at least 75% Bike Coverage` = c(region.hct.stations.bk.75.share,
                                                                                  king.hct.stations.bk.75.share,
                                                                                  kitsap.hct.stations.bk.75.share,
                                                                                  pierce.hct.stations.bk.75.share,
                                                                                  snohomish.hct.stations.bk.75.share,
                                                                                  dis.hct.stations.bk.75.share,
                                                                                  eld.hct.stations.bk.75.share,
                                                                                  lep.hct.stations.bk.75.share,
                                                                                  poc.hct.stations.bk.75.share,
                                                                                  pov.hct.stations.bk.75.share,
                                                                                  you.hct.stations.bk.75.share),
`Share of High Capacity Transit Stations with Complete Bike Coverage` = c(region.hct.stations.bk.100.share,
                                                                              king.hct.stations.bk.100.share,
                                                                              kitsap.hct.stations.bk.100.share,
                                                                              pierce.hct.stations.bk.100.share,
                                                                              snohomish.hct.stations.bk.100.share,
                                                                              dis.hct.stations.bk.100.share,
                                                                              eld.hct.stations.bk.100.share,
                                                                              lep.hct.stations.bk.100.share,
                                                                              poc.hct.stations.bk.100.share,
                                                                              pov.hct.stations.bk.100.share,
                                                                              you.hct.stations.bk.100.share))


fwrite(hct.stations.table, here('output','hct_station_areas_summary.csv'))

# Network Detail Near Transit -------------------------------------------------
region.bus.stations <- bus.stop.summary %>% select(stop_id) %>% distinct() %>% pull() %>% length()
king.bus.stations <- bus.stop.summary %>% filter(county%in%"King") %>% select(stop_id) %>% distinct() %>% pull() %>% length()
kitsap.bus.stations <- bus.stop.summary %>% filter(county%in%"Kitsap") %>% select(stop_id) %>% distinct() %>% pull() %>% length()
pierce.bus.stations <- bus.stop.summary %>% filter(county%in%"Pierce") %>% select(stop_id) %>% distinct() %>% pull() %>% length()
snohomish.bus.stations <- bus.stop.summary %>% filter(county%in%"Snohomish") %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
eld.bus.stations <- bus.stop.summary %>% filter(elderly>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
dis.bus.stations <- bus.stop.summary %>% filter(disabled>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
lep.bus.stations <- bus.stop.summary %>% filter(limited_english>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
poc.bus.stations <- bus.stop.summary %>% filter(people_of_color>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
pov.bus.stations <- bus.stop.summary %>% filter(poverty>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
you.bus.stations <- bus.stop.summary %>% filter(youth>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 

region.bus.stations.share <- region.bus.stations / region.bus.stations
king.bus.stations.share <- king.bus.stations / region.bus.stations
kitsap.bus.stations.share <- kitsap.bus.stations / region.bus.stations
pierce.bus.stations.share <- pierce.bus.stations / region.bus.stations
snohomish.bus.stations.share <- snohomish.bus.stations / region.bus.stations
eld.bus.stations.share <- eld.bus.stations / region.bus.stations
dis.bus.stations.share <- dis.bus.stations / region.bus.stations
lep.bus.stations.share <- lep.bus.stations / region.bus.stations
poc.bus.stations.share <- poc.bus.stations / region.bus.stations
pov.bus.stations.share <- pov.bus.stations / region.bus.stations
you.bus.stations.share <- you.bus.stations / region.bus.stations

# 25% Sidewalk Coverage
region.bus.stations.sw.25 <- bus.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=0.25) %>% select(stop_id) %>% distinct() %>% pull() %>% length()
king.bus.stations.sw.25 <- bus.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=0.25 & county%in%"King") %>% select(stop_id) %>% distinct() %>% pull() %>% length()
kitsap.bus.stations.sw.25 <- bus.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=0.25 & county%in%"Kitsap") %>% select(stop_id) %>% distinct() %>% pull() %>% length()
pierce.bus.stations.sw.25 <- bus.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=0.25 & county%in%"Pierce") %>% select(stop_id) %>% distinct() %>% pull() %>% length()
snohomish.bus.stations.sw.25 <- bus.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=0.25 & county%in%"Snohomish") %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
eld.bus.stations.sw.25 <- bus.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=0.25 & elderly>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
dis.bus.stations.sw.25 <- bus.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=0.25 & disabled>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
lep.bus.stations.sw.25 <- bus.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=0.25 & limited_english>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
poc.bus.stations.sw.25 <- bus.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=0.25 & people_of_color>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
pov.bus.stations.sw.25 <- bus.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=0.25 & poverty>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
you.bus.stations.sw.25 <- bus.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=0.25 & youth>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 

region.bus.stations.sw.25.share <- region.bus.stations.sw.25 / region.bus.stations
king.bus.stations.sw.25.share <- king.bus.stations.sw.25 / king.bus.stations
kitsap.bus.stations.sw.25.share <- kitsap.bus.stations.sw.25 / kitsap.bus.stations
pierce.bus.stations.sw.25.share <- pierce.bus.stations.sw.25 / pierce.bus.stations
snohomish.bus.stations.sw.25.share <- snohomish.bus.stations.sw.25 / snohomish.bus.stations
eld.bus.stations.sw.25.share <- eld.bus.stations.sw.25 / eld.bus.stations
dis.bus.stations.sw.25.share <- dis.bus.stations.sw.25 / dis.bus.stations
lep.bus.stations.sw.25.share <- lep.bus.stations.sw.25 / lep.bus.stations
poc.bus.stations.sw.25.share <- poc.bus.stations.sw.25 / poc.bus.stations
pov.bus.stations.sw.25.share <- pov.bus.stations.sw.25 / pov.bus.stations
you.bus.stations.sw.25.share <- you.bus.stations.sw.25 / you.bus.stations

# 50% Sidewalk Coverage
region.bus.stations.sw.50 <- bus.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=0.50) %>% select(stop_id) %>% distinct() %>% pull() %>% length()
king.bus.stations.sw.50 <- bus.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=0.50 & county%in%"King") %>% select(stop_id) %>% distinct() %>% pull() %>% length()
kitsap.bus.stations.sw.50 <- bus.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=0.50 & county%in%"Kitsap") %>% select(stop_id) %>% distinct() %>% pull() %>% length()
pierce.bus.stations.sw.50 <- bus.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=0.50 & county%in%"Pierce") %>% select(stop_id) %>% distinct() %>% pull() %>% length()
snohomish.bus.stations.sw.50 <- bus.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=0.50 & county%in%"Snohomish") %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
eld.bus.stations.sw.50 <- bus.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=0.50 & elderly>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
dis.bus.stations.sw.50 <- bus.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=0.50 & disabled>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
lep.bus.stations.sw.50 <- bus.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=0.50 & limited_english>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
poc.bus.stations.sw.50 <- bus.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=0.50 & people_of_color>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
pov.bus.stations.sw.50 <- bus.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=0.50 & poverty>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
you.bus.stations.sw.50 <- bus.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=0.50 & youth>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 

region.bus.stations.sw.50.share <- region.bus.stations.sw.50 / region.bus.stations
king.bus.stations.sw.50.share <- king.bus.stations.sw.50 / king.bus.stations
kitsap.bus.stations.sw.50.share <- kitsap.bus.stations.sw.50 / kitsap.bus.stations
pierce.bus.stations.sw.50.share <- pierce.bus.stations.sw.50 / pierce.bus.stations
snohomish.bus.stations.sw.50.share <- snohomish.bus.stations.sw.50 / snohomish.bus.stations
eld.bus.stations.sw.50.share <- eld.bus.stations.sw.50 / eld.bus.stations
dis.bus.stations.sw.50.share <- dis.bus.stations.sw.50 / dis.bus.stations
lep.bus.stations.sw.50.share <- lep.bus.stations.sw.50 / lep.bus.stations
poc.bus.stations.sw.50.share <- poc.bus.stations.sw.50 / poc.bus.stations
pov.bus.stations.sw.50.share <- pov.bus.stations.sw.50 / pov.bus.stations
you.bus.stations.sw.50.share <- you.bus.stations.sw.50 / you.bus.stations

# 75% Sidewalk Coverage
region.bus.stations.sw.75 <- bus.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=0.75) %>% select(stop_id) %>% distinct() %>% pull() %>% length()
king.bus.stations.sw.75 <- bus.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=0.75 & county%in%"King") %>% select(stop_id) %>% distinct() %>% pull() %>% length()
kitsap.bus.stations.sw.75 <- bus.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=0.75 & county%in%"Kitsap") %>% select(stop_id) %>% distinct() %>% pull() %>% length()
pierce.bus.stations.sw.75 <- bus.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=0.75 & county%in%"Pierce") %>% select(stop_id) %>% distinct() %>% pull() %>% length()
snohomish.bus.stations.sw.75 <- bus.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=0.75 & county%in%"Snohomish") %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
eld.bus.stations.sw.75 <- bus.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=0.75 & elderly>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
dis.bus.stations.sw.75 <- bus.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=0.75 & disabled>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
lep.bus.stations.sw.75 <- bus.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=0.75 & limited_english>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
poc.bus.stations.sw.75 <- bus.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=0.75 & people_of_color>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
pov.bus.stations.sw.75 <- bus.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=0.75 & poverty>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
you.bus.stations.sw.75 <- bus.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=0.75 & youth>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 

region.bus.stations.sw.75.share <- region.bus.stations.sw.75 / region.bus.stations
king.bus.stations.sw.75.share <- king.bus.stations.sw.75 / king.bus.stations
kitsap.bus.stations.sw.75.share <- kitsap.bus.stations.sw.75 / kitsap.bus.stations
pierce.bus.stations.sw.75.share <- pierce.bus.stations.sw.75 / pierce.bus.stations
snohomish.bus.stations.sw.75.share <- snohomish.bus.stations.sw.75 / snohomish.bus.stations
eld.bus.stations.sw.75.share <- eld.bus.stations.sw.75 / eld.bus.stations
dis.bus.stations.sw.75.share <- dis.bus.stations.sw.75 / dis.bus.stations
lep.bus.stations.sw.75.share <- lep.bus.stations.sw.75 / lep.bus.stations
poc.bus.stations.sw.75.share <- poc.bus.stations.sw.75 / poc.bus.stations
pov.bus.stations.sw.75.share <- pov.bus.stations.sw.75 / pov.bus.stations
you.bus.stations.sw.75.share <- you.bus.stations.sw.75 / you.bus.stations

# 100% Sidewalk Coverage
region.bus.stations.sw.100 <- bus.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=1) %>% select(stop_id) %>% distinct() %>% pull() %>% length()
king.bus.stations.sw.100 <- bus.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=1 & county%in%"King") %>% select(stop_id) %>% distinct() %>% pull() %>% length()
kitsap.bus.stations.sw.100 <- bus.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=1 & county%in%"Kitsap") %>% select(stop_id) %>% distinct() %>% pull() %>% length()
pierce.bus.stations.sw.100 <- bus.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=1 & county%in%"Pierce") %>% select(stop_id) %>% distinct() %>% pull() %>% length()
snohomish.bus.stations.sw.100 <- bus.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=1 & county%in%"Snohomish") %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
eld.bus.stations.sw.100 <- bus.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=1 & elderly>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
dis.bus.stations.sw.100 <- bus.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=1 & disabled>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
lep.bus.stations.sw.100 <- bus.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=1 & limited_english>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
poc.bus.stations.sw.100 <- bus.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=1 & people_of_color>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
pov.bus.stations.sw.100 <- bus.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=1 & poverty>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
you.bus.stations.sw.100 <- bus.stop.summary %>% filter((complete_sidewalk+partial_sidewalk)/facility_length>=1 & youth>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 

region.bus.stations.sw.100.share <- region.bus.stations.sw.100 / region.bus.stations
king.bus.stations.sw.100.share <- king.bus.stations.sw.100 / king.bus.stations
kitsap.bus.stations.sw.100.share <- kitsap.bus.stations.sw.100 / kitsap.bus.stations
pierce.bus.stations.sw.100.share <- pierce.bus.stations.sw.100 / pierce.bus.stations
snohomish.bus.stations.sw.100.share <- snohomish.bus.stations.sw.100 / snohomish.bus.stations
eld.bus.stations.sw.100.share <- eld.bus.stations.sw.100 / eld.bus.stations
dis.bus.stations.sw.100.share <- dis.bus.stations.sw.100 / dis.bus.stations
lep.bus.stations.sw.100.share <- lep.bus.stations.sw.100 / lep.bus.stations
poc.bus.stations.sw.100.share <- poc.bus.stations.sw.100 / poc.bus.stations
pov.bus.stations.sw.100.share <- pov.bus.stations.sw.100 / pov.bus.stations
you.bus.stations.sw.100.share <- you.bus.stations.sw.100 / you.bus.stations

# 25% Bike Coverage
region.bus.stations.bk.25 <- bus.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=0.25) %>% select(stop_id) %>% distinct() %>% pull() %>% length()
king.bus.stations.bk.25 <- bus.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=0.25 & county%in%"King") %>% select(stop_id) %>% distinct() %>% pull() %>% length()
kitsap.bus.stations.bk.25 <- bus.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=0.25 & county%in%"Kitsap") %>% select(stop_id) %>% distinct() %>% pull() %>% length()
pierce.bus.stations.bk.25 <- bus.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=0.25 & county%in%"Pierce") %>% select(stop_id) %>% distinct() %>% pull() %>% length()
snohomish.bus.stations.bk.25 <- bus.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=0.25 & county%in%"Snohomish") %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
eld.bus.stations.bk.25 <- bus.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=0.25 & elderly>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
dis.bus.stations.bk.25 <- bus.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=0.25 & disabled>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
lep.bus.stations.bk.25 <- bus.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=0.25 & limited_english>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
poc.bus.stations.bk.25 <- bus.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=0.25 & people_of_color>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
pov.bus.stations.bk.25 <- bus.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=0.25 & poverty>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
you.bus.stations.bk.25 <- bus.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=0.25 & youth>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 

region.bus.stations.bk.25.share <- region.bus.stations.bk.25 / region.bus.stations
king.bus.stations.bk.25.share <- king.bus.stations.bk.25 / king.bus.stations
kitsap.bus.stations.bk.25.share <- kitsap.bus.stations.bk.25 / kitsap.bus.stations
pierce.bus.stations.bk.25.share <- pierce.bus.stations.bk.25 / pierce.bus.stations
snohomish.bus.stations.bk.25.share <- snohomish.bus.stations.bk.25 / snohomish.bus.stations
eld.bus.stations.bk.25.share <- eld.bus.stations.bk.25 / eld.bus.stations
dis.bus.stations.bk.25.share <- dis.bus.stations.bk.25 / dis.bus.stations
lep.bus.stations.bk.25.share <- lep.bus.stations.bk.25 / lep.bus.stations
poc.bus.stations.bk.25.share <- poc.bus.stations.bk.25 / poc.bus.stations
pov.bus.stations.bk.25.share <- pov.bus.stations.bk.25 / pov.bus.stations
you.bus.stations.bk.25.share <- you.bus.stations.bk.25 / you.bus.stations

# 50% Bike Coverage
region.bus.stations.bk.50 <- bus.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=0.50) %>% select(stop_id) %>% distinct() %>% pull() %>% length()
king.bus.stations.bk.50 <- bus.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=0.50 & county%in%"King") %>% select(stop_id) %>% distinct() %>% pull() %>% length()
kitsap.bus.stations.bk.50 <- bus.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=0.50 & county%in%"Kitsap") %>% select(stop_id) %>% distinct() %>% pull() %>% length()
pierce.bus.stations.bk.50 <- bus.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=0.50 & county%in%"Pierce") %>% select(stop_id) %>% distinct() %>% pull() %>% length()
snohomish.bus.stations.bk.50 <- bus.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=0.50 & county%in%"Snohomish") %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
eld.bus.stations.bk.50 <- bus.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=0.50 & elderly>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
dis.bus.stations.bk.50 <- bus.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=0.50 & disabled>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
lep.bus.stations.bk.50 <- bus.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=0.50 & limited_english>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
poc.bus.stations.bk.50 <- bus.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=0.50 & people_of_color>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
pov.bus.stations.bk.50 <- bus.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=0.50 & poverty>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
you.bus.stations.bk.50 <- bus.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=0.50 & youth>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 

region.bus.stations.bk.50.share <- region.bus.stations.bk.50 / region.bus.stations
king.bus.stations.bk.50.share <- king.bus.stations.bk.50 / king.bus.stations
kitsap.bus.stations.bk.50.share <- kitsap.bus.stations.bk.50 / kitsap.bus.stations
pierce.bus.stations.bk.50.share <- pierce.bus.stations.bk.50 / pierce.bus.stations
snohomish.bus.stations.bk.50.share <- snohomish.bus.stations.bk.50 / snohomish.bus.stations
eld.bus.stations.bk.50.share <- eld.bus.stations.bk.50 / eld.bus.stations
dis.bus.stations.bk.50.share <- dis.bus.stations.bk.50 / dis.bus.stations
lep.bus.stations.bk.50.share <- lep.bus.stations.bk.50 / lep.bus.stations
poc.bus.stations.bk.50.share <- poc.bus.stations.bk.50 / poc.bus.stations
pov.bus.stations.bk.50.share <- pov.bus.stations.bk.50 / pov.bus.stations
you.bus.stations.bk.50.share <- you.bus.stations.bk.50 / you.bus.stations

# 75% Bike Coverage
region.bus.stations.bk.75 <- bus.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=0.75) %>% select(stop_id) %>% distinct() %>% pull() %>% length()
king.bus.stations.bk.75 <- bus.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=0.75 & county%in%"King") %>% select(stop_id) %>% distinct() %>% pull() %>% length()
kitsap.bus.stations.bk.75 <- bus.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=0.75 & county%in%"Kitsap") %>% select(stop_id) %>% distinct() %>% pull() %>% length()
pierce.bus.stations.bk.75 <- bus.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=0.75 & county%in%"Pierce") %>% select(stop_id) %>% distinct() %>% pull() %>% length()
snohomish.bus.stations.bk.75 <- bus.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=0.75 & county%in%"Snohomish") %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
eld.bus.stations.bk.75 <- bus.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=0.75 & elderly>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
dis.bus.stations.bk.75 <- bus.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=0.75 & disabled>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
lep.bus.stations.bk.75 <- bus.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=0.75 & limited_english>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
poc.bus.stations.bk.75 <- bus.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=0.75 & people_of_color>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
pov.bus.stations.bk.75 <- bus.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=0.75 & poverty>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
you.bus.stations.bk.75 <- bus.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=0.75 & youth>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 

region.bus.stations.bk.75.share <- region.bus.stations.bk.75 / region.bus.stations
king.bus.stations.bk.75.share <- king.bus.stations.bk.75 / king.bus.stations
kitsap.bus.stations.bk.75.share <- kitsap.bus.stations.bk.75 / kitsap.bus.stations
pierce.bus.stations.bk.75.share <- pierce.bus.stations.bk.75 / pierce.bus.stations
snohomish.bus.stations.bk.75.share <- snohomish.bus.stations.bk.75 / snohomish.bus.stations
eld.bus.stations.bk.75.share <- eld.bus.stations.bk.75 / eld.bus.stations
dis.bus.stations.bk.75.share <- dis.bus.stations.bk.75 / dis.bus.stations
lep.bus.stations.bk.75.share <- lep.bus.stations.bk.75 / lep.bus.stations
poc.bus.stations.bk.75.share <- poc.bus.stations.bk.75 / poc.bus.stations
pov.bus.stations.bk.75.share <- pov.bus.stations.bk.75 / pov.bus.stations
you.bus.stations.bk.75.share <- you.bus.stations.bk.75 / you.bus.stations

# 100% Bike Coverage
region.bus.stations.bk.100 <- bus.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=1) %>% select(stop_id) %>% distinct() %>% pull() %>% length()
king.bus.stations.bk.100 <- bus.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=1 & county%in%"King") %>% select(stop_id) %>% distinct() %>% pull() %>% length()
kitsap.bus.stations.bk.100 <- bus.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=1 & county%in%"Kitsap") %>% select(stop_id) %>% distinct() %>% pull() %>% length()
pierce.bus.stations.bk.100 <- bus.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=1 & county%in%"Pierce") %>% select(stop_id) %>% distinct() %>% pull() %>% length()
snohomish.bus.stations.bk.100 <- bus.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=1 & county%in%"Snohomish") %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
eld.bus.stations.bk.100 <- bus.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=1 & elderly>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
dis.bus.stations.bk.100 <- bus.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=1 & disabled>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
lep.bus.stations.bk.100 <- bus.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=1 & limited_english>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
poc.bus.stations.bk.100 <- bus.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=1 & people_of_color>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
pov.bus.stations.bk.100 <- bus.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=1 & poverty>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 
you.bus.stations.bk.100 <- bus.stop.summary %>% filter((complete_bike+partial_bike)/facility_length>=1 & youth>0) %>% select(stop_id) %>% distinct() %>% pull() %>% length()                 

region.bus.stations.bk.100.share <- region.bus.stations.bk.100 / region.bus.stations
king.bus.stations.bk.100.share <- king.bus.stations.bk.100 / king.bus.stations
kitsap.bus.stations.bk.100.share <- kitsap.bus.stations.bk.100 / kitsap.bus.stations
pierce.bus.stations.bk.100.share <- pierce.bus.stations.bk.100 / pierce.bus.stations
snohomish.bus.stations.bk.100.share <- snohomish.bus.stations.bk.100 / snohomish.bus.stations
eld.bus.stations.bk.100.share <- eld.bus.stations.bk.100 / eld.bus.stations
dis.bus.stations.bk.100.share <- dis.bus.stations.bk.100 / dis.bus.stations
lep.bus.stations.bk.100.share <- lep.bus.stations.bk.100 / lep.bus.stations
poc.bus.stations.bk.100.share <- poc.bus.stations.bk.100 / poc.bus.stations
pov.bus.stations.bk.100.share <- pov.bus.stations.bk.100 / pov.bus.stations
you.bus.stations.bk.100.share <- you.bus.stations.bk.100 / you.bus.stations

bus.stations.table <- data.table(
  `Geography` = c("Region", 
                  "King County",
                  "Kitsap County",
                  "Pierce County",
                  "Snohomish County",
                  "People with a Disability", 
                  "People over 65", 
                  "People with Limited English Proficiency", 
                  "People of Color", 
                  "People in Poverty", 
                  "People under 18"),
  `Transit Stations` = c(region.bus.stations,
                                       king.bus.stations,
                                       kitsap.bus.stations,
                                       pierce.bus.stations,
                                       snohomish.bus.stations,
                                       dis.bus.stations,
                                       eld.bus.stations,
                                       lep.bus.stations,
                                       poc.bus.stations,
                                       pov.bus.stations,
                                       you.bus.stations),
  `Transit Stations` = c(region.bus.stations.share,
                                                king.bus.stations.share,
                                                kitsap.bus.stations.share,
                                                pierce.bus.stations.share,
                                                snohomish.bus.stations.share,
                                                dis.bus.stations.share,
                                                eld.bus.stations.share,
                                                lep.bus.stations.share,
                                                poc.bus.stations.share,
                                                pov.bus.stations.share,
                                                you.bus.stations.share),
  `Share of Transit Stations with at least 25% Sidewalk Coverage` = c(region.bus.stations.sw.25.share,
                                                                                    king.bus.stations.sw.25.share,
                                                                                    kitsap.bus.stations.sw.25.share,
                                                                                    pierce.bus.stations.sw.25.share,
                                                                                    snohomish.bus.stations.sw.25.share,
                                                                                    dis.bus.stations.sw.25.share,
                                                                                    eld.bus.stations.sw.25.share,
                                                                                    lep.bus.stations.sw.25.share,
                                                                                    poc.bus.stations.sw.25.share,
                                                                                    pov.bus.stations.sw.25.share,
                                                                                    you.bus.stations.sw.25.share),
  `Share of Transit Stations with at least 50% Sidewalk Coverage` = c(region.bus.stations.sw.50.share,
                                                                                    king.bus.stations.sw.50.share,
                                                                                    kitsap.bus.stations.sw.50.share,
                                                                                    pierce.bus.stations.sw.50.share,
                                                                                    snohomish.bus.stations.sw.50.share,
                                                                                    dis.bus.stations.sw.50.share,
                                                                                    eld.bus.stations.sw.50.share,
                                                                                    lep.bus.stations.sw.50.share,
                                                                                    poc.bus.stations.sw.50.share,
                                                                                    pov.bus.stations.sw.50.share,
                                                                                    you.bus.stations.sw.50.share),
  `Share of Transit Stations with at least 75% Sidewalk Coverage` = c(region.bus.stations.sw.75.share,
                                                                                    king.bus.stations.sw.75.share,
                                                                                    kitsap.bus.stations.sw.75.share,
                                                                                    pierce.bus.stations.sw.75.share,
                                                                                    snohomish.bus.stations.sw.75.share,
                                                                                    dis.bus.stations.sw.75.share,
                                                                                    eld.bus.stations.sw.75.share,
                                                                                    lep.bus.stations.sw.75.share,
                                                                                    poc.bus.stations.sw.75.share,
                                                                                    pov.bus.stations.sw.75.share,
                                                                                    you.bus.stations.sw.75.share),
  `Share of Transit Stations with Complete Sidewalk Coverage` = c(region.bus.stations.sw.100.share,
                                                                                king.bus.stations.sw.100.share,
                                                                                kitsap.bus.stations.sw.100.share,
                                                                                pierce.bus.stations.sw.100.share,
                                                                                snohomish.bus.stations.sw.100.share,
                                                                                dis.bus.stations.sw.100.share,
                                                                                eld.bus.stations.sw.100.share,
                                                                                lep.bus.stations.sw.100.share,
                                                                                poc.bus.stations.sw.100.share,
                                                                                pov.bus.stations.sw.100.share,
                                                                                you.bus.stations.sw.100.share),
  `Share of Transit Stations with at least 25% Bike Coverage` = c(region.bus.stations.bk.25.share,
                                                                                king.bus.stations.bk.25.share,
                                                                                kitsap.bus.stations.bk.25.share,
                                                                                pierce.bus.stations.bk.25.share,
                                                                                snohomish.bus.stations.bk.25.share,
                                                                                dis.bus.stations.bk.25.share,
                                                                                eld.bus.stations.bk.25.share,
                                                                                lep.bus.stations.bk.25.share,
                                                                                poc.bus.stations.bk.25.share,
                                                                                pov.bus.stations.bk.25.share,
                                                                                you.bus.stations.bk.25.share),
  `Share of Transit Stations with at least 50% Bike Coverage` = c(region.bus.stations.bk.50.share,
                                                                                king.bus.stations.bk.50.share,
                                                                                kitsap.bus.stations.bk.50.share,
                                                                                pierce.bus.stations.bk.50.share,
                                                                                snohomish.bus.stations.bk.50.share,
                                                                                dis.bus.stations.bk.50.share,
                                                                                eld.bus.stations.bk.50.share,
                                                                                lep.bus.stations.bk.50.share,
                                                                                poc.bus.stations.bk.50.share,
                                                                                pov.bus.stations.bk.50.share,
                                                                                you.bus.stations.bk.50.share),
  `Share of Transit Stations with at least 75% Bike Coverage` = c(region.bus.stations.bk.75.share,
                                                                                king.bus.stations.bk.75.share,
                                                                                kitsap.bus.stations.bk.75.share,
                                                                                pierce.bus.stations.bk.75.share,
                                                                                snohomish.bus.stations.bk.75.share,
                                                                                dis.bus.stations.bk.75.share,
                                                                                eld.bus.stations.bk.75.share,
                                                                                lep.bus.stations.bk.75.share,
                                                                                poc.bus.stations.bk.75.share,
                                                                                pov.bus.stations.bk.75.share,
                                                                                you.bus.stations.bk.75.share),
  `Share of Transit Stations with Complete Bike Coverage` = c(region.bus.stations.bk.100.share,
                                                                            king.bus.stations.bk.100.share,
                                                                            kitsap.bus.stations.bk.100.share,
                                                                            pierce.bus.stations.bk.100.share,
                                                                            snohomish.bus.stations.bk.100.share,
                                                                            dis.bus.stations.bk.100.share,
                                                                            eld.bus.stations.bk.100.share,
                                                                            lep.bus.stations.bk.100.share,
                                                                            poc.bus.stations.bk.100.share,
                                                                            pov.bus.stations.bk.100.share,
                                                                            you.bus.stations.bk.100.share))


fwrite(bus.stations.table, here('output','bus_station_areas_summary.csv'))


