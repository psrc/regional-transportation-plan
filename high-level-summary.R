library(tidyverse)
library(data.table)
library(ggplot2)
library(scales)
library(plotly)
library(rlang)
library(knitr)

# Packages for Maps
library(sf)
library(ggpubr)
library(leaflet)
library(mapview)
library(png)

# Database Connections
library(odbc)
library(DBI)

# General Inputs ----------------------------------------------------------
generate.equity.maps <- "no"

census.year <- 2018

pop.2018 <- 4122419
pop.2050 <- 5823165

pop.delta <- (pop.2050-pop.2018)/pop.2018

jobs.2018 <- 2297537
jobs.2050 <- 3391293

jobs.delta <- (jobs.2050-jobs.2018)/jobs.2018

model.runs <- list(list('2018','Base Year', '//modelstation2//c$//Workspace//sc_2018_rtp//soundcast'),
                   list('2050','Plan', '//modelstation2//c$//Workspace//sc_2050_rtp//soundcast'))

# Transit Summary Files
boardings.by.agency.file <- '//outputs//transit//daily_boardings_by_agency.csv'
boardings.by.stop.file <- '//outputs//transit//boardings_by_stop.csv'
boardings.by.tod.agency.file <- '//outputs//transit//daily_boardings_by_tod_agency.csv'
transit.access.file <- '//outputs//transit//transit_access.csv'
transit.lines.file <- '//outputs//transit//transit_line_results.csv'

# Network Summary Files
county.network.file <- '//outputs//network/county_network.csv'
facility.delay.file <- '//outputs//network/delay_facility.csv'
facility.vmt.file <- '//outputs//network/vmt_facility.csv'
facility.vht.file <- '//outputs//network/vmt_facility.csv'
user.class.vmt.file <- '//outputs//network/vmt_user_class.csv'
user.class.vhd.file <- '//outputs//network/delay_user_class.csv'
network.outputs.file <- '//outputs//network/network_results.csv'
truck.trips.file <- '//outputs/trucks/trucks_summary.csv'

# Person Aggregations
person.vmt.file <- '//outputs//agg//dash/person_vmt.csv'
person.vht.file <- '//outputs//agg//dash/person_vht.csv'
person.type.file <- '//outputs//agg//dash/pptyp_county.csv'
person.trips.file <- '//outputs//agg//dash/person_trips.csv'
person.geo.file <- "//outputs//agg//dash//person_geog.csv"
hh.geo.file <- "//outputs//agg//dash//hh_geog.csv"
person.fftime.file <- '//outputs//agg//dash/trip_sov_ff_time.csv'
person.congtime.file <- '//outputs//agg//dash/trip_time_total.csv'

# Emissions Files
emissions.file <- "//outputs//emissions//emissions_summary.csv"

# Shapefiles
am.peak.shape <- "//inputs//scenario//networks//shapefiles//AM//AM_edges.shp"
pm.peak.shape <- "//inputs//scenario//networks//shapefiles//PM//PM_edges.shp"

# Summary Calculations
pop.file <- '1_population.csv'
vmt.file <- '5_vmt.csv'
vhd.file <- '7_delay.csv'

annualization.factor.transit <- 320
annualization.factor.highway <- 300

wgs84 <- 4326
spn <- 2285 

psrc.colors <- c(
  "King" = "#AD5CAB",
  "Kitsap" = "#F4835E",
  "Pierce" = "#A9D46E",
  "Snohomish" = "#40BDB8",
  "Community Transit" = "#8CC63E",
  "Everett Transit" = "#E2F1CF",
  "King County Metro" = "#F05A28",
  "Kitsap Transit" = "#00A7A0",
  "Pierce Transit" = "#FBD6C9",
  "Sound Transit" = "#91268F",
  "Washington Ferries" = "#BFE9E7",
  "Region" = "#630460",
  "Base Year" = "#76787A",
  "Interim Year" = "#00A7A0",
  "Constrained Plan" = "#91268F",
  "No Plan" = "#F05A28",
  "Full Plan" = "#8CC63E"
)

mode.table.cols <- c("County","Drove Alone", "Shared Ride", "Transit", "Walk & Bike")

demographic.categories <- c("hh_p" =  "Total Households",
                "stugrd_p" = "Elementary Students",
                "stuhgh_p" = "High School Students",
                "stuuni_p" = "University Students",
                "empedu_p" = "Education",
                "empfoo_p" = "Food Service",
                "empgov_p" = "Government",
                "empind_p" = "Industrial",
                "empmed_p" = "Medical",
                "empofc_p" = "Office",
                "empret_p" = "Retail",
                "empsvc_p" = "Services",
                "empoth_p" = "Other",
                "emptot_p" = "Total Jobs")

demographic.categories <- enframe(demographic.categories)

simplified.modes <- c("Bike" = "Walk & Bike",
                      "Walk" = "Walk & Bike",
                      "SOV" = "Auto",
                      "TNC" = "Auto",
                      "HOV2" = "Auto",
                      "HOV3+" = "Auto",
                      "School Bus" = "Transit",
                      "Transit" = "Transit")

simplified.modes <- enframe(simplified.modes)

simplified.purposes <- c("None/Home" = "Non-Commute",
                      "Work" = "Commute",
                      "School" = "Non-Commute",
                      "Escort" = "Non-Commute",
                      "Personal Business" = "Non-Commute",
                      "Shop" = "Non-Commute",
                      "Meal" = "Non-Commute",
                      "Social" = "Non-Commute",
                      "Change Mode Inserted Purpose" = "Non-Commute")

simplified.purposes <- enframe(simplified.purposes)

simplified.geography <- c("Disability" = "Disability",
                         "Elderly" = "Over 65",
                         "English" = "Limited English",
                         "Poverty" = "Low Income",
                         "Racial" = "People of Color",
                         "Youth" = "Under 18")

simplified.geography <- enframe(simplified.geography)

simplified.tod <- c("5to6" = "Overnight",
                    "6to7" = "AM Peak",
                    "7to8" = "AM Peak",
                    "8to9" = "AM Peak",
                    "9to10" = "Midday",
                    "10to14" = "Midday",
                    "14to15" = "Midday",
                    "15to16" = "PM Peak",
                    "16to17" = "PM Peak",
                    "17to18" = "PM Peak",
                    "18to20" = "Evening",
                    "20to5" = "Overnight")

simplified.tod <- enframe(simplified.tod)

simplified.pollutants <- c("2" = "CO",
                         "3" = "NOx",
                         "87" = "VOCs",
                         "98" = "CO2 Equivalent",
                         "PM10" = "PM10",
                         "PM25" = "PM25")

simplified.pollutants <- enframe(simplified.pollutants)

pollutants <- c("2","3","87","98","PM10","PM25")

agency.order <- c("Community Transit", "Everett Transit", "King County Metro", "Kitsap Transit", "Pierce Transit", "Sound Transit", "Washington Ferries", "Region")
model.run.order <-c('Base Year', 'Interim Year', 'Plan', 'No Plan', 'Full Plan')
county.order <-c('King', 'Kitsap', 'Pierce', 'Snohomish', 'Region')
population.order <- c('Elementary Students','High School Students','University Students','Total Households')
job.order <- c('Education','Food Service','Government','Industrial','Medical','Office','Retail','Services','Other','Total Jobs')
demographic.order <- c(population.order,job.order)

# Functions ---------------------------------------------------------------

create_facet_bar_chart <- function(w.tbl, w.x, w.y, w.group, w.colors=psrc.colors, w.scales="free", w.facet=3, w.dec=0, w.factor, w.lab="", w.title, w.run, w.label=label_comma()) {
  
  c <- ggplot(data=w.tbl, 
                       aes(y=get(eval(w.y)), 
                           x=get(eval(w.x)), 
                           fill = get(eval(w.run)),
                           ))+
                  geom_bar(position="dodge", stat="identity") +
                  geom_text(aes(label=paste0(format(round(get(eval(w.y))*w.factor,w.dec), nsmall=0, big.mark=","),w.lab)), position=position_dodge(width=0.9), vjust=1.5, size = 3, colour = "white") +
                  xlab(NULL)+
                  ylab(NULL)+
                  scale_y_continuous(labels = w.label) +
                  theme_light() +
                  scale_fill_manual(values=w.colors) +
                  theme(
                    axis.text.y= element_text(size = 8),
                    axis.text.x= element_text(size = 8),
                    axis.title.y = element_blank(),
                    axis.title.x = element_blank(),
                    panel.grid.major.y = element_line(colour="#BBBDC0",size = 0.25),
                    panel.grid.major.x = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_blank(),
                    axis.line = element_blank(),
                    axis.ticks.x = element_blank(),
                    axis.ticks.y = element_blank(),
                    legend.position="bottom",
                    legend.title = element_blank())+
                  facet_wrap(vars(get(eval(w.group))), scales=w.scales, ncol=w.facet)
  return(c)
}

create_facet_bar_chart_no_label <- function(w.tbl, w.x, w.y, w.group, w.colors=psrc.colors, w.scales="free", w.facet=3, w.dec=0, w.factor, w.lab="", w.title, w.run, w.label=label_comma()) {
  
  c <- ggplot(data=w.tbl, 
              aes(y=get(eval(w.y)), 
                  x=get(eval(w.x)), 
                  fill = get(eval(w.run)),
              ))+
    geom_bar(position="dodge", stat="identity") +
    xlab(NULL)+
    ylab(NULL)+
    theme_light() +
    scale_fill_manual(values=w.colors) +
    theme(
      axis.text.y= element_blank(),
      axis.text.x= element_text(size = 8),
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      panel.grid.major.y = element_line(colour="#BBBDC0",size = 0.25),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.line = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position="bottom",
      legend.title = element_blank())+
    facet_wrap(vars(get(eval(w.group))), scales=w.scales, ncol=w.facet)
  return(c)
}

create.mode.table <- function(t, g.col, m.col, v.col, w.title, w.names, w.run) {
  
  t <- t %>%
    select(.data[[g.col]], .data[[m.col]], .data[[v.col]], model_run) %>%
    filter(model_run == w.run) %>%
    pivot_wider(id_cols = c(all_of(g.col)), names_from = c(all_of(m.col), "model_run"),values_from = c(all_of(v.col))) %>%
    setNames(mode.table.cols)
  
  tbl <- knitr::kable(t, caption = paste0("Average Trip Distance by Mode: ",w.run))
    
  return(tbl)
  
}

create.stacked.bars <- function(w.tbl, w.x, w.y, w.group, w.factor=1, w.dec=0, w.lab="", w.colors=psrc.colors, w.lab.pos=1.5, w.title) {
  
  c <- ggplot(data=w.tbl, 
              aes(y=get(eval(w.y)), 
                  x=get(eval(w.x)), 
                  fill = get(eval(w.group)),
              ))+
    geom_col(
      color = "black",
      alpha = 1.0,
      position = "stack") +
    geom_text(aes(y = label_y, label = paste0(round(get(eval(w.y))*w.factor,w.dec),w.lab)), vjust = w.lab.pos, colour = "black", size = 3) +
    scale_color_manual(values=w.colors) +
    xlab(NULL)+
    ylab(NULL)+
    theme_light() +
    theme(
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.line = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position="bottom",
      legend.title = element_blank()) +
    labs(title = w.title,
         caption = "Data source: PSRC SoundCast Model")
  
  return(c)
  
}

create.map <- function(equity.lyr, equity.clr, c.title, c.label) {
  
  m <- leaflet() %>%
    addMapPane(name = "polygons", zIndex = 410) %>%
    addMapPane(name = "maplabels", zIndex = 500) %>% # higher zIndex rendered on top
    
    addProviderTiles("CartoDB.VoyagerNoLabels") %>%
    addProviderTiles("CartoDB.VoyagerOnlyLabels",
                     options = leafletOptions(pane = "maplabels"),
                     group = "Labels") %>%

    addPolygons(data=equity.lyr,
                fillOpacity = 0.75,
                fillColor = equity.clr,
                opacity = 0.5,
                weight = 0.5,
                color = "#BCBEC0",
                options = leafletOptions(pane = "polygons"),
                dashArray = "") %>%
    
    
    setView(lng=-122.257, lat=47.615, zoom=8) %>%
    
    addLegend(values = equity.tracts$variable,
              colors = c(equity.clr),
              labels = c(c.label),
              position = "bottomright",
              title = c.title)
}

create.static.map <- function(equity.lyr, equity.clr, c.title, c.label, img.pth) {
  
  m <- leaflet() %>%
    addMapPane(name = "polygons", zIndex = 410) %>%
    
    addProviderTiles("CartoDB.VoyagerNoLabels") %>%

    addPolygons(data=equity.lyr,
                fillOpacity = 0.75,
                fillColor = equity.clr,
                opacity = 0.5,
                weight = 0.5,
                color = "#BCBEC0",
                options = leafletOptions(pane = "polygons"),
                dashArray = "") %>%
    
    setView(lng=-122.257, lat=47.615, zoom=9)
    
  mapshot(m, file = img.pth)
  
}

create.dissovled.equity.geography.layers <- function(spatial.layer, spatial.att, equity.att) {
  
  dissolved.layer <- spatial.layer %>% 
    st_transform(spn) %>%
    select(.data[[spatial.att]], .data[[equity.att]]) %>%
    filter(.data[[equity.att]]==1) %>%
    st_union() %>%
    st_transform(wgs84) %>%
    st_sf() %>%
    mutate(above_threshold=1)
  
  return(dissolved.layer)
  
}

# Equity Geography Maps ---------------------------------------------------

if(generate.equity.maps=="yes") {

  tracts <- st_read("https://services6.arcgis.com/GWxg6t7KXELn1thE/arcgis/rest/services/tract2010_nowater/FeatureServer/0/query?where=0=0&outFields=*&f=pgeojson") %>% 
    st_transform(spn) %>%
    select(GEOID10) %>%
    rename(geoid=GEOID10) %>%
    st_transform(wgs84)

  server_name <- "AWS-PROD-SQL\\SOCKEYE"
  database_name <- "Elmer"

  db_con <- dbConnect(odbc::odbc(),
                    driver = "SQL Server",
                    server = server_name,
                    database = database_name,
                    trusted_connection = "yes"
  )

  # Queries to pass to Elmer functions
  disabled_query <- paste0("SELECT geoid, equity_geog_vs_reg_total FROM Census.disability_equity_geographies(",census.year,",'Tract')")
  elderly_query <- paste0("SELECT geoid, equity_geog_vs_reg_total FROM Census.elderly_equity_geographies(",census.year,",'Tract')")
  youth_query <- paste0("SELECT geoid, equity_geog_vs_reg_total FROM Census.youth_equity_geographies(",census.year,",'Tract')")
  lep_query <- paste0("SELECT geoid, equity_geog_vs_reg_total FROM Census.limited_english_equity_geographies(",census.year,",'Tract')")
  poverty_query <- paste0("SELECT geoid, equity_geog_vs_reg_total FROM Census.poverty_equity_geographies(",census.year,",'Tract')")
  people_of_color_query <- paste0("SELECT geoid, equity_geog_vs_reg_total FROM Census.racial_equity_geographies(",census.year,",'Tract')")

  # Tract Data by Geography
  tract.disability <- as_tibble(DBI::dbGetQuery(db_con, disabled_query)) %>% rename(disabled = equity_geog_vs_reg_total)
  tract.elderly <- as_tibble(DBI::dbGetQuery(db_con, elderly_query)) %>% rename(elderly=equity_geog_vs_reg_total)
  tract.limited.english <- as_tibble(DBI::dbGetQuery(db_con, lep_query)) %>% rename(limited_english=equity_geog_vs_reg_total)
  tract.poverty <- as_tibble(DBI::dbGetQuery(db_con, poverty_query)) %>% rename(poverty=equity_geog_vs_reg_total)
  tract.people.of.color <- as_tibble(DBI::dbGetQuery(db_con, people_of_color_query)) %>%  rename(people_of_color=equity_geog_vs_reg_total)
  tract.youth <- as_tibble(DBI::dbGetQuery(db_con, youth_query)) %>% rename(youth=equity_geog_vs_reg_total)

  dbDisconnect(db_con)

  tracts <- list(tracts, tract.disability, tract.elderly, tract.limited.english,
               tract.poverty, tract.people.of.color, tract.youth) %>%
    reduce(left_join, by = "geoid")  %>%
    rename(tract_geoid=geoid)

  # Create Shapefiles
  disability.tracts <- create.dissovled.equity.geography.layers(spatial.layer = tracts, spatial.att = "tract_geoid", equity.att = "disabled")
  elderly.tracts <- create.dissovled.equity.geography.layers(spatial.layer = tracts, spatial.att = "tract_geoid", equity.att = "elderly")
  youth.tracts <- create.dissovled.equity.geography.layers(spatial.layer = tracts, spatial.att = "tract_geoid", equity.att = "youth")
  lep.tracts <- create.dissovled.equity.geography.layers(spatial.layer = tracts, spatial.att = "tract_geoid", equity.att = "limited_english")
  poverty.tracts <- create.dissovled.equity.geography.layers(spatial.layer = tracts, spatial.att = "tract_geoid", equity.att = "poverty")
  people.of.color.tracts <- create.dissovled.equity.geography.layers(spatial.layer = tracts, spatial.att = "tract_geoid", equity.att = "people_of_color")

  # Create Maps
  disability.map <- create.static.map(equity.lyr = disability.tracts, equity.clr = "#91268F", c.title = "People with Disabilities", c.label="Over Regional Average", img.pth="images/disability-geographies.png")
  elderly.map <- create.static.map(equity.lyr = elderly.tracts, equity.clr = "#F05A28", c.title = "People over 65", c.label="Over Regional Average", img.pth="images/elderly-geographies.png")
  youth.map <- create.static.map(equity.lyr = youth.tracts, equity.clr = "#8CC63E", c.title = "People under 18", c.label="Over Regional Average", img.pth="images/youth-geographies.png")
  lep.map <- create.static.map(equity.lyr = lep.tracts, equity.clr = "#00A7A0", c.title = "People with Limited English Proficiency", c.label="Over Regional Average", img.pth="images/lep-geographies.png")
  poverty.map <- create.static.map(equity.lyr = poverty.tracts, equity.clr = "#8CC63E", c.title = "People of Lower Incomes", c.label="Over Regional Average", img.pth="images/poverty-geographies.png") 
  people.of.color.map <- create.static.map(equity.lyr = people.of.color.tracts, equity.clr = "#91268F", c.title = "People of Color", c.label="Over Regional Average", img.pth="images/poc-geographies.png")

} # end of creating equity maps

img1 <- readPNG("images/disability-geographies.png")
img2 <- readPNG("images/elderly-geographies.png")
img3 <- readPNG("images/youth-geographies.png")
img4 <- readPNG("images/lep-geographies.png")
img5 <- readPNG("images/poverty-geographies.png")
img6 <- readPNG("images/poc-geographies.png")

im_1 <- ggplot() + background_image(img1) +
  theme(plot.margin = margin(t=1, l=1, r=1, b=1, unit = "cm"))

im_2 <- ggplot() + background_image(img2) + 
  theme(plot.margin = margin(t=1, l=1, r=1, b=1, unit = "cm"))

im_3 <- ggplot() + background_image(img3) + 
  theme(plot.margin = margin(t=1, l=1, r=1, b=1, unit = "cm"))

im_4 <- ggplot() + background_image(img4) + 
  theme(plot.margin = margin(t=1, l=1, r=1, b=1, unit = "cm"))

im_5 <- ggplot() + background_image(img5) + 
  theme(plot.margin = margin(t=1, l=1, r=1, b=1, unit = "cm"))

im_6 <- ggplot() + background_image(img6) + 
  theme(plot.margin = margin(t=1, l=1, r=1, b=1, unit = "cm"))

equity_maps_figure  <- ggarrange(im_1, im_2, im_3, im_4, im_5, im_6, ncol = 3, nrow = 2, 
                                 labels=c("Disability", "Over 65", "Under 18",
                                          "Limited English Proficiency", "Lower Incomes", "People of Color"))

# Person Miles Traveled by Geography -------------------------------------------------------
geogs <- c("racial","poverty", "county", "rg_proposed")

hh.vmt.data <-NULL
for (runs in model.runs) {
  
  t.all <- as_tibble(fread(file.path(runs[[3]],person.vmt.file)))
  p.all <- as_tibble(fread(file.path(runs[[3]],person.geo.file)))
  h.all <- as_tibble(fread(file.path(runs[[3]],hh.geo.file)))
  
  for (geography in geogs) {
    
    if (geography %in% c( "county", "rgc", "rg_proposed", "city")) {
      c.geo <- paste0("hh_",geography)
    } else {
      c.geo <- paste0("hh_",geography,"_50")
    }
    
    t <- t.all %>%
      rename(Place=.data[[c.geo]]) %>%
      filter(mode!="") %>%
      select(mode,Place,travdist_wt)
    
    t <- left_join(t, simplified.modes, by=c("mode"="name")) %>%
      mutate(mode=value) %>%
      select(-value) %>%
      mutate(Geography=str_to_title(geography)) %>%
      rename(Mode=mode, PMT=travdist_wt)
    
    if (!(geography %in% c( "county", "rgc", "rg_proposed", "city"))) {
      t <- t %>%
        mutate(Place=case_when(
          Place==0 ~ paste0("Less than regional average"),
          Place==1 ~ paste0("More than regional average")
        )) 
    }
    
    t <- t %>%
      group_by(Mode,Place,Geography) %>%
      summarize(PMT=sum(PMT)) %>%
      mutate(Year=as.numeric(runs[[1]]), `Model Run`=runs[[2]]) %>%
      mutate(Place=as.character(Place))
    
    # Join Population by Geography
    p <- p.all %>%
      rename(Place=.data[[c.geo]]) %>%
      select(Place,psexpfac)
    
    if (!(geography %in% c( "county", "rgc", "rg_proposed", "city"))) {
      p <- p %>%
        mutate(Place=case_when(
          Place==0 ~ paste0("Less than regional average"),
          Place==1 ~ paste0("More than regional average")
        )) 
    }

    p <- p %>%
      group_by(Place) %>%
      summarize(Population=sum(psexpfac))
    
    t <- left_join(t,p,by=c("Place"))
    
    # Join Households by Geography
    h <- h.all %>%
      rename(Place=.data[[c.geo]]) %>%
      select(Place,hhexpfac)
    
    if (!(geography %in% c( "county", "rgc", "rg_proposed", "city"))) {
      h <- h %>%
        mutate(Place=case_when(
          Place==0 ~ paste0("Less than regional average"),
          Place==1 ~ paste0("More than regional average")
        )) 
    }
    
    h <- h %>%
      group_by(Place) %>%
      summarize(Households=sum(hhexpfac))
    
    t <- left_join(t,h,by=c("Place"))
    
    a <- t %>%
      mutate(Auto=case_when(
        (Mode %in% c("Drove Alone","Shared Ride")) ~ "Auto",
        !(Mode %in% c("Drove Alone","Shared Ride")) ~ "Non-Auto")) %>%
      select(Place,Auto,PMT,Population,Households) %>%
      group_by(Auto,Place) %>%
      summarize(PMT=sum(PMT),Population=sum(Population),Households=sum(Households)) %>%
      mutate(Year=as.numeric(runs[[1]]), `Model Run`=runs[[2]]) %>%
      mutate(Geography=str_to_title(geography)) %>%
      rename(Mode=Auto)
      
    t <- bind_rows(t, a)
    
    if(geography=="rgc") {
      
      r <- t %>%
        filter(!(Place=="Not in RGC")) %>%
        group_by(Mode, Geography, Year, `Model Run`) %>%
        summarize(PMT=sum(PMT), Population=sum(Population), Households=sum(Households)) %>%
        mutate(Place="In RGC")
      
      t <- bind_rows(t, r)
      
    }
    
    t <- t %>%
      mutate(`PMT per Person` = round(PMT/Population,1), `PMT per HH` = round(PMT/Households,1))

    ifelse(is.null(hh.vmt.data), hh.vmt.data <- t, hh.vmt.data <- bind_rows(hh.vmt.data,t))
    
  }
  
}

rm(t,p,h,a, r, t.all, p.all, h.all)

# Person Hours Traveled by Geography -------------------------------------------------------
geogs <- c("racial","poverty", "county", "rg_proposed")

hh.vht.data <-NULL
for (runs in model.runs) {
  
  t.all <- as_tibble(fread(file.path(runs[[3]],person.vht.file)))
  p.all <- as_tibble(fread(file.path(runs[[3]],person.geo.file)))
  h.all <- as_tibble(fread(file.path(runs[[3]],hh.geo.file)))
  
  for (geography in geogs) {
    
    if (geography %in% c( "county", "rgc", "rg_proposed", "city")) {
      c.geo <- paste0("hh_",geography)
    } else {
      c.geo <- paste0("hh_",geography,"_50")
    }
    
    t <- t.all %>%
      rename(Place=.data[[c.geo]]) %>%
      filter(mode!="") %>%
      select(mode,Place,travtime_wt)
    
    t <- left_join(t, simplified.modes, by=c("mode"="name")) %>%
      mutate(mode=value) %>%
      select(-value) %>%
      mutate(Geography=str_to_title(geography)) %>%
      rename(Mode=mode, PHT=travtime_wt)
    
    if (!(geography %in% c( "county", "rgc", "rg_proposed", "city"))) {
      t <- t %>%
        mutate(Place=case_when(
          Place==0 ~ paste0("Less than regional average"),
          Place==1 ~ paste0("More than regional average")
        )) 
    }
    
    t <- t %>%
      group_by(Mode,Place,Geography) %>%
      summarize(PHT=sum(PHT)) %>%
      mutate(Year=as.numeric(runs[[1]]), `Model Run`=runs[[2]]) %>%
      mutate(Place=as.character(Place))
    
    # Join Population by Geography
    p <- p.all %>%
      rename(Place=.data[[c.geo]]) %>%
      select(Place,psexpfac)
    
    if (!(geography %in% c( "county", "rgc", "rg_proposed", "city"))) {
      p <- p %>%
        mutate(Place=case_when(
          Place==0 ~ paste0("Less than regional average"),
          Place==1 ~ paste0("More than regional average")
        )) 
    }
    
    p <- p %>%
      group_by(Place) %>%
      summarize(Population=sum(psexpfac))
    
    t <- left_join(t,p,by=c("Place"))
    
    # Join Households by Geography
    h <- h.all %>%
      rename(Place=.data[[c.geo]]) %>%
      select(Place,hhexpfac)
    
    if (!(geography %in% c( "county", "rgc", "rg_proposed", "city"))) {
      h <- h %>%
        mutate(Place=case_when(
          Place==0 ~ paste0("Less than regional average"),
          Place==1 ~ paste0("More than regional average")
        )) 
    }
    
    h <- h %>%
      group_by(Place) %>%
      summarize(Households=sum(hhexpfac))
    
    t <- left_join(t,h,by=c("Place"))
    
    a <- t %>%
      mutate(Auto=case_when(
        (Mode %in% c("Drove Alone","Shared Ride")) ~ "Auto",
        !(Mode %in% c("Drove Alone","Shared Ride")) ~ "Non-Auto")) %>%
      select(Place,Auto,PHT,Population,Households) %>%
      group_by(Auto,Place) %>%
      summarize(PHT=sum(PHT),Population=sum(Population),Households=sum(Households)) %>%
      mutate(Year=as.numeric(runs[[1]]), `Model Run`=runs[[2]]) %>%
      mutate(Geography=str_to_title(geography)) %>%
      rename(Mode=Auto)
    
    t <- bind_rows(t, a)
    
    if(geography=="rgc") {
      
      r <- t %>%
        filter(!(Place=="Not in RGC")) %>%
        group_by(Mode, Geography, Year, `Model Run`) %>%
        summarize(PHT=sum(PHT), Population=sum(Population), Households=sum(Households)) %>%
        mutate(Place="In RGC")
      
      t <- bind_rows(t, r)
      
    }
    
    t <- t %>%
      mutate(`PHT per Person` = round(PHT/Population,1), `PHT per HH` = round(PHT/Households,1))
    
    ifelse(is.null(hh.vht.data), hh.vht.data <- t, hh.vht.data <- bind_rows(hh.vht.data,t))
    
  }
  
}

rm(t,p,h,a, r, t.all, p.all, h.all)

# Person Delay by Geography -------------------------------------------------------
geogs <- c("racial","poverty", "county", "rg_proposed")

hh.delay.data <-NULL
for (runs in model.runs) {
  
  ff.all <- as_tibble(fread(file.path(runs[[3]],person.fftime.file)))
  t.all <- as_tibble(fread(file.path(runs[[3]],person.congtime.file)))
  p.all <- as_tibble(fread(file.path(runs[[3]],person.geo.file)))
  h.all <- as_tibble(fread(file.path(runs[[3]],hh.geo.file)))
  
  for (geography in geogs) {
    
    if (geography %in% c( "county", "rgc", "rg_proposed", "city")) {
      c.geo <- paste0("hh_",geography)
    } else {
      c.geo <- paste0("hh_",geography,"_50")
    }
    
    t <- t.all %>%
      rename(Place=.data[[c.geo]]) %>%
      filter(mode!="") %>%
      select(mode,Place,travtime_wt)
    
    t <- left_join(t, simplified.modes, by=c("mode"="name")) %>%
      mutate(mode=value) %>%
      select(-value) %>%
      mutate(Geography=str_to_title(geography)) %>%
      rename(Mode=mode, `Congested Time`=travtime_wt)
    
    if (!(geography %in% c( "county", "rgc", "rg_proposed", "city"))) {
      t <- t %>%
        mutate(Place=case_when(
          Place==0 ~ paste0("Less than regional average"),
          Place==1 ~ paste0("More than regional average")
        )) 
    }
    
    t <- t %>%
      group_by(Place,Geography) %>%
      summarize(`Congested Time`=sum(`Congested Time`)) 
    
    ff <- ff.all %>%
      rename(Place=.data[[c.geo]]) %>%
      filter(mode!="") %>%
      select(mode,Place,sov_ff_time_wt)
    
    ff <- left_join(ff, simplified.modes, by=c("mode"="name")) %>%
      mutate(mode=value) %>%
      select(-value) %>%
      mutate(Geography=str_to_title(geography)) %>%
      rename(Mode=mode, `FreeFlow Time`=sov_ff_time_wt)
    
    if (!(geography %in% c( "county", "rgc", "rg_proposed", "city"))) {
      ff <- ff %>%
        mutate(Place=case_when(
          Place==0 ~ paste0("Less than regional average"),
          Place==1 ~ paste0("More than regional average")
        )) 
    }
    
    ff <- ff %>%
      group_by(Place,Geography) %>%
      summarize(`FreeFlow Time`=sum(`FreeFlow Time`)) 
    
    t <-left_join(t,ff,by=c("Place","Geography")) %>%
      mutate(`Daily Delay` = `Congested Time` - `FreeFlow Time`) %>%
      mutate(Year=as.numeric(runs[[1]]), `Model Run`=runs[[2]]) %>%
      mutate(Place=as.character(Place)) %>%
      mutate(Mode="Auto")
    
    # Join Population by Geography
    p <- p.all %>%
      rename(Place=.data[[c.geo]]) %>%
      select(Place,psexpfac)
    
    if (!(geography %in% c( "county", "rgc", "rg_proposed", "city"))) {
      p <- p %>%
        mutate(Place=case_when(
          Place==0 ~ paste0("Less than regional average"),
          Place==1 ~ paste0("More than regional average")
        )) 
    }
    
    p <- p %>%
      group_by(Place) %>%
      summarize(Population=sum(psexpfac))
    
    t <- left_join(t,p,by=c("Place"))
    
    # Join Households by Geography
    h <- h.all %>%
      rename(Place=.data[[c.geo]]) %>%
      select(Place,hhexpfac)
    
    if (!(geography %in% c( "county", "rgc", "rg_proposed", "city"))) {
      h <- h %>%
        mutate(Place=case_when(
          Place==0 ~ paste0("Less than regional average"),
          Place==1 ~ paste0("More than regional average")
        )) 
    }
    
    h <- h %>%
      group_by(Place) %>%
      summarize(Households=sum(hhexpfac))
    
    t <- left_join(t,h,by=c("Place"))
    
    if(geography=="rgc") {
      
      r <- t %>%
        filter(!(Place=="Not in RGC")) %>%
        group_by(Mode,Geography, Year, `Model Run`) %>%
        summarize(`Daily Delay`=sum(`Daily Delay`), `Congested Time`=sum(`Congested Time`), `FreeFlow Time`=sum(`FreeFlow Time`),
                  Population=sum(Population), Households=sum(Households)) %>%
        mutate(Place="In RGC")
      
      t <- bind_rows(t, r)
      
    }
    
    t <- t %>%
      mutate(`Delay per Person` = round(`Daily Delay`/Population,1), `Delay per HH` = round(`Daily Delay`/Households,1))
    
    ifelse(is.null(hh.delay.data), hh.delay.data <- t, hh.delay.data <- bind_rows(hh.delay.data,t))
    
  }
  
}

rm(t, p, h, ff, ff.all, t.all, p.all, h.all, r)

# Mode Share by Equity Geography ------------------------------------------
geogs <- c("disability","racial","poverty", "elderly", "english","youth", "county")

hh.ms.data <-NULL
for (runs in model.runs) {
  
  for (geography in geogs) {
    
    if (geography == "county") {
      c.geo <- paste0("hh_",geography)
      c.file <- paste0('//outputs//agg//dash//mode_share_',geography,'.csv')
      
    } else {
      c.geo <- paste0("hh_",geography,"_reg")
      c.file <- paste0('//outputs//agg//dash//mode_share_hh_',geography,'_reg.csv')
    }
    
    
    t <- as_tibble(fread(file.path(runs[[3]],c.file))) %>%
      rename(Place=.data[[c.geo]]) %>%
      filter(mode!="")
    
    t <- left_join(t, simplified.modes, by=c("mode"="name")) %>%
      mutate(mode=value) %>%
      select(-value) %>%
      mutate(Geography=str_to_title(geography)) %>%
      rename(Mode=mode, Trips=trexpfac)
    
    if (geography != "county") {
    t <- t %>%
      mutate(Place=case_when(
        Place==0 ~ paste0("Less than regional average"),
        Place==1 ~ paste0("More than regional average")
      )) 
    }
    
    t <- left_join(t, simplified.purposes, by=c("dpurp"="name")) %>%
      mutate(Purpose=value) %>%
      select(-value, -dpurp)
    
    t <- t %>%
      group_by(Mode,Place,Geography,Purpose) %>%
      summarize(Trips=sum(Trips))
    
    c <- t %>%
      select(Purpose, Place, Trips) %>%
      group_by(Purpose, Place) %>%
      summarize(Total_Trips = sum(Trips))
    
    t <- left_join(t,c,by=c("Purpose","Place")) %>%
      mutate(Share=Trips/Total_Trips) %>%
      mutate(Year=as.numeric(runs[[1]]), `Model Run`=runs[[2]]) %>%
      mutate(Place=as.character(Place))
    
    ifelse(is.null(hh.ms.data), hh.ms.data <- t, hh.ms.data <- bind_rows(hh.ms.data,t))
    rm(t,c)
    
  }
  
}

# Transit Boardings -------------------------------------------------------
transit.boardings.by.agency <- NULL
for (runs in model.runs) {
  
  # Transit Boardings
  t <- as_tibble(fread(file.path(runs[[3]],boardings.by.agency.file))) %>%
    mutate(daily_boardings = round(boardings,-1), annual_boardings = round(boardings*annualization.factor.transit,-3)) %>%
    mutate(year=as.numeric(runs[[1]]), model_run=runs[[2]]) %>%
    select(-boardings)
  
  ifelse(is.null(transit.boardings.by.agency), transit.boardings.by.agency <- t, transit.boardings.by.agency <- bind_rows(transit.boardings.by.agency,t))
  rm(t)
}

# Create a Regional Summary
temp <- transit.boardings.by.agency %>%
  select(year, model_run, daily_boardings, annual_boardings)%>%
  group_by(year, model_run) %>%
  summarize(daily_boardings=sum(daily_boardings), annual_boardings=sum(annual_boardings)) %>%
  mutate(agency_name="Region")

transit.boardings.by.agency <- bind_rows(transit.boardings.by.agency, temp)
transit.boardings.by.agency$agency_name <- factor(transit.boardings.by.agency$agency_name, levels=agency.order)
transit.boardings.by.agency$model_run <- factor(transit.boardings.by.agency$model_run, levels=model.run.order)
rm(temp)

# Transit Access ----------------------------------------------------------
transit.access <- NULL
for (runs in model.runs) {
  
  # Transit Access
  t <- as_tibble(fread(file.path(runs[[3]],transit.access.file))) %>%
    rename(category=V1) %>%
    mutate(share=round(quarter_mile_transit/total,3), year=as.numeric(runs[[1]]), model_run=runs[[2]])
  
  t <- left_join(t, demographic.categories, by=c("category"="name")) %>%
    mutate(category = case_when(
      value %in% population.order ~ "Population",
      value %in% job.order ~ "Employment"
    ))

  ifelse(is.null(transit.access), transit.access <- t, transit.access <- bind_rows(transit.access,t))
  rm(t)
}

transit.access$value <- factor(transit.access$value, levels=demographic.order)
transit.access$model_run <- factor(transit.access$model_run, levels=model.run.order)

# Network Summary ---------------------------------------------------------
network.summary <- NULL
for (runs in model.runs) {
  
  # VMT, VHT and Delay by County
  t <- as_tibble(fread(file.path(runs[[3]],county.network.file))) %>%
    mutate(daily_vmt = round(VMT,-1), annual_vmt = round(VMT*annualization.factor.highway,-3)) %>%
    mutate(daily_vht = round(VHT,-1), annual_vht = round(VHT*annualization.factor.highway,-3)) %>%
    mutate(daily_vhd = round(delay,-1), annual_vhd = round(delay*annualization.factor.highway,-3)) %>%
    mutate(year=as.numeric(runs[[1]]), model_run=runs[[2]]) %>%
    select(-VMT,-VHT,-delay)
  
  ifelse(is.null(network.summary), network.summary <- t, network.summary <- bind_rows(network.summary,t))
  rm(t)
}

# Create a Regional Summary
temp <- network.summary %>%
  select(year, model_run, daily_vmt, annual_vmt, daily_vht, annual_vht, daily_vhd, annual_vhd)%>%
  group_by(year, model_run) %>%
  summarize(daily_vmt=sum(daily_vmt), annual_vmt=sum(annual_vmt), daily_vht=sum(daily_vht), annual_vht=sum(annual_vht), daily_vhd=sum(daily_vhd), annual_vhd=sum(annual_vhd)) %>%
  mutate(county_name="Region")

network.summary <- bind_rows(network.summary, temp)
network.summary$county_name <- factor(network.summary$county_name, levels=county.order)
network.summary$model_run <- factor(network.summary$model_run, levels=model.run.order)
rm(temp)

# Truck VMT Summary ---------------------------------------------------------
truck.vmt.summary <- NULL

for (runs in model.runs) {
  
  t <- as_tibble(fread(file.path(runs[[3]],user.class.vmt.file))) %>%
    select(tod, `@mveh`, `@hveh`)
  
  tr <- as_tibble(fread(file.path(runs[[3]],truck.trips.file))) %>%
    rename(Type=V1, Trips=prod) %>%
    select(Type, Trips) %>%
    filter(Type != "dt")
  
  mt <- tr %>% filter(Type=="mt") %>% select(Trips) %>% pull()
  ht <- tr %>% filter(Type=="ht") %>% select(Trips) %>% pull()
  
  t <- left_join(t,simplified.tod,by=c("tod"="name")) %>%
    mutate(tod=value) %>%
    select(-value) %>%
    group_by(tod) %>%
    summarize(`Daily Medium Truck VMT`= round(sum(`@mveh`),-1), `Daily Heavy Truck VMT` = round(sum(`@hveh`),-1)) %>%
    mutate(`Annual Medium Truck VMT` = round(`Daily Medium Truck VMT`*annualization.factor.highway,-3)) %>%
    mutate(`Annual Heavy Truck VMT` = round(`Daily Heavy Truck VMT`*annualization.factor.highway,-3)) %>%
    mutate(`Daily Truck VMT` = `Daily Medium Truck VMT` + `Daily Heavy Truck VMT`) %>%
    mutate(`Annual Truck VMT` = `Annual Medium Truck VMT` + `Annual Heavy Truck VMT`)
  
  r <- t %>%
    mutate(tod='Daily') %>%
    group_by(tod) %>%
    summarize(`Daily Medium Truck VMT`=sum(`Daily Medium Truck VMT`), `Daily Heavy Truck VMT`=sum(`Daily Heavy Truck VMT`), `Daily Truck VMT`=sum(`Daily Truck VMT`),
              `Annual Medium Truck VMT`=sum(`Annual Medium Truck VMT`), `Annual Heavy Truck VMT`=sum(`Annual Heavy Truck VMT`), `Annual Truck VMT`=sum(`Annual Truck VMT`))
      
  t <- bind_rows(t,r) %>%
    mutate(year=as.numeric(runs[[1]]), model_run=runs[[2]]) %>%
    mutate(`Medium Truck Trips`=mt, `Heavy Truck Trips`=ht)
  
  ifelse(is.null(truck.vmt.summary), truck.vmt.summary <- t, truck.vmt.summary <- bind_rows(truck.vmt.summary,t))
  rm(t,r)
}

# Bus VMT Summary ---------------------------------------------------------
bus.vmt.summary <- NULL

for (runs in model.runs) {
  
  t <- as_tibble(fread(file.path(runs[[3]],user.class.vmt.file))) %>%
    select(tod, `@bveh`)
  
  t <- left_join(t,simplified.tod,by=c("tod"="name")) %>%
    mutate(tod=value) %>%
    select(-value) %>%
    group_by(tod) %>%
    summarize(`Daily Bus VMT`= round(sum(`@bveh`),-1)) %>%
    mutate(`Annual Bus VMT` = round(`Daily Bus VMT`*annualization.factor.transit,-3))
  
  r <- t %>%
    mutate(tod='Daily') %>%
    group_by(tod) %>%
    summarize(`Daily Bus VMT`=sum(`Daily Bus VMT`), `Annual Bus VMT`=sum(`Annual Bus VMT`))
  
  t <- bind_rows(t,r) %>%
    mutate(year=as.numeric(runs[[1]]), model_run=runs[[2]])
  
  ifelse(is.null(bus.vmt.summary), bus.vmt.summary <- t, bus.vmt.summary <- bind_rows(bus.vmt.summary,t))
  rm(t,r)
}

# Truck Delay Summary ---------------------------------------------------------
truck.vhd.summary <- NULL

for (runs in model.runs) {
  
  t <- as_tibble(fread(file.path(runs[[3]],user.class.vhd.file))) %>%
    select(tod, `@mveh`, `@hveh`)
  
  tr <- as_tibble(fread(file.path(runs[[3]],truck.trips.file))) %>%
    rename(Type=V1, Trips=prod) %>%
    select(Type, Trips) %>%
    filter(Type != "dt")
  
  mt <- tr %>% filter(Type=="mt") %>% select(Trips) %>% pull()
  ht <- tr %>% filter(Type=="ht") %>% select(Trips) %>% pull()
  
  t <- left_join(t,simplified.tod,by=c("tod"="name")) %>%
    mutate(tod=value) %>%
    select(-value) %>%
    group_by(tod) %>%
    summarize(`Daily Medium Truck Delay`= round(sum(`@mveh`),-1), `Daily Heavy Truck Delay` = round(sum(`@hveh`),-1)) %>%
    mutate(`Annual Medium Truck Delay` = round(`Daily Medium Truck Delay`*annualization.factor.highway,-3)) %>%
    mutate(`Annual Heavy Truck Delay` = round(`Daily Heavy Truck Delay`*annualization.factor.highway,-3)) %>%
    mutate(`Daily Truck Delay` = `Daily Medium Truck Delay` + `Daily Heavy Truck Delay`) %>%
    mutate(`Annual Truck Delay` = `Annual Medium Truck Delay` + `Annual Heavy Truck Delay`)
  
  r <- t %>%
    mutate(tod='Daily') %>%
    group_by(tod) %>%
    summarize(`Daily Medium Truck Delay`=sum(`Daily Medium Truck Delay`), `Daily Heavy Truck Delay`=sum(`Daily Heavy Truck Delay`), `Daily Truck Delay`=sum(`Daily Truck Delay`),
              `Annual Medium Truck Delay`=sum(`Annual Medium Truck Delay`), `Annual Heavy Truck Delay`=sum(`Annual Heavy Truck Delay`), `Annual Truck Delay`=sum(`Annual Truck Delay`))
  
  t <- bind_rows(t,r) %>%
    mutate(year=as.numeric(runs[[1]]), model_run=runs[[2]]) %>%
    mutate(`Medium Truck Trips`=mt, `Heavy Truck Trips`=ht)
  
  ifelse(is.null(truck.vhd.summary), truck.vhd.summary <- t, truck.vhd.summary <- bind_rows(truck.vhd.summary,t))
  rm(t,r)
}

# Bus Delay Summary ---------------------------------------------------------
bus.vhd.summary <- NULL

for (runs in model.runs) {
  
  t <- as_tibble(fread(file.path(runs[[3]],user.class.vhd.file))) %>%
    select(tod, `@bveh`)
  
  t <- left_join(t,simplified.tod,by=c("tod"="name")) %>%
    mutate(tod=value) %>%
    select(-value) %>%
    group_by(tod) %>%
    summarize(`Daily Bus Delay`= round(sum(`@bveh`),-1)) %>%
    mutate(`Annual Bus Delay` = round(`Daily Bus Delay`*annualization.factor.transit,-3))
  
  r <- t %>%
    mutate(tod='Daily') %>%
    group_by(tod) %>%
    summarize(`Daily Bus Delay`=sum(`Daily Bus Delay`), `Annual Bus Delay`=sum(`Annual Bus Delay`))
  
  t <- bind_rows(t,r) %>%
    mutate(year=as.numeric(runs[[1]]), model_run=runs[[2]])
  
  ifelse(is.null(bus.vhd.summary), bus.vhd.summary <- t, bus.vhd.summary <- bind_rows(bus.vhd.summary,t))
  rm(t,r)
}

# Person Metrics ----------------------------------------------------------
person.miles.county <- NULL
for (runs in model.runs) {
  
  # Person Miles by Mode and County
  t <- as_tibble(fread(file.path(runs[[3]],person.vmt.file))) %>%
    select(mode,hh_county,travdist_wt) %>%
    filter(mode!="") %>%
    group_by(mode,hh_county) %>%
    summarize(total_miles=sum(travdist_wt))
  
  t <- left_join(t, simplified.modes, by=c("mode"="name")) %>%
    mutate(mode=value) %>%
    select(-value) %>%
    group_by(mode,hh_county) %>%
    summarize(total_miles=sum(total_miles)) %>%
    rename(county=hh_county)
  
  p <- as_tibble(fread(file.path(runs[[3]],person.type.file))) %>%
    select(-pptyp) %>%
    group_by(person_county) %>%
    summarize(population=sum(psexpfac)) %>%
    rename(county=person_county)
  
  pt <- as_tibble(fread(file.path(runs[[3]],person.trips.file))) %>%
    select(mode,hh_county,trexpfac) %>%
    filter(mode!="") %>%
    group_by(mode,hh_county) %>%
    summarize(total_trips=sum(trexpfac))
  
  pt <- left_join(pt, simplified.modes, by=c("mode"="name")) %>%
    mutate(mode=value) %>%
    select(-value) %>%
    group_by(mode,hh_county) %>%
    summarize(total_trips=sum(total_trips)) %>%
    rename(county=hh_county)
  
  t <- left_join(t,p,by=c("county"))
  t <- left_join(t,pt,by=c("county","mode"))
  
  t <- t %>%
    mutate(miles_per_person = round(total_miles/population,1)) %>%
    mutate(miles_per_trip = round(total_miles/total_trips,1)) %>%
    mutate(year=as.numeric(runs[[1]]), model_run=runs[[2]])

  ifelse(is.null(person.miles.county), person.miles.county <- t, person.miles.county <- bind_rows(person.miles.county,t))
  rm(t, p, pt)
}  

# Link Level Outputs ----------------------------------------------------------
link.outputs <- NULL

for (runs in model.runs) {
  
  if (runs[[2]] == "Base Year") {
    network.outputs <- as_tibble(fread(file.path(getwd(),"/data/base/network_results.csv")))
  } else {
    network.outputs <- as_tibble(fread(file.path(getwd(),"/data/plan/network_results.csv")))
  }
  
  #network.outputs <- as_tibble(fread(file.path(runs[[3]],network.outputs.file)))
  
  peak.outputs <- network.outputs %>%
    filter(tod=="7to8" | tod=="16to17") %>%
    filter(volume_delay_func %in% c(1,3,5,7)) %>%
    select(ij,`length`,num_lanes,`@countyid`,data2,auto_time,`@mveh`,`@hveh`,`@tveh`,tod) %>%
    mutate(VMT=`@tveh`*`length`, SPD=(`length`/auto_time)*60) %>%
    mutate(Ratio=SPD/data2) %>%
    mutate(Congestion_Level = case_when(
      Ratio <=0.25 ~ "Severe",
      (Ratio >0.25 & Ratio <=0.50) ~ "Heavy",
      (Ratio >0.50 & Ratio <=0.75) ~ "Moderate",
      Ratio>0.75 ~ "Minimal")) %>%
    mutate(Lane_Miles=num_lanes*length) %>%
    mutate(Period = case_when(
      tod == '7to8' ~ "AM Peak",
      tod == '16to17' ~ "PM Peak")) %>%
    mutate(year=as.numeric(runs[[1]]), model_run=runs[[2]])
  
  ifelse(is.null(link.outputs), link.outputs <- peak.outputs, link.outputs <- bind_rows(link.outputs,peak.outputs))
  
  rm(network.outputs, peak.outputs)
  
}

# Emissions Summary ---------------------------------------------------------

emissions.summary <- NULL
for (runs in model.runs) {
  
  t <- as_tibble(fread(file.path(runs[[3]],emissions.file))) %>%
    mutate(pollutantID=gsub("\\.0","",pollutantID)) %>%
    filter(pollutantID %in% pollutants)
  
  t <- left_join(t,simplified.pollutants,by=c("pollutantID"="name")) %>%
    mutate(pollutant_name=value)
  
  
  t <- t %>%
    select(pollutant_name,total_daily_tons) %>%
    group_by(pollutant_name) %>%
    summarize(total_daily_tons=sum(total_daily_tons)) %>%
    mutate(year=as.numeric(runs[[1]]), model_run=runs[[2]])
  
  ifelse(is.null(emissions.summary), emissions.summary <- t, emissions.summary <- bind_rows(emissions.summary, t))
  
  rm(t)

  
}

# Vehicle Miles Traveled --------------------------------------------------

# by Region
g <- c("Base Year", "Plan")
c <- c("Base Year" = "#76787A", "Plan" = "#F05A28")
t <- c("County", "Poverty","Racial")

tbl <- hh.vmt.data %>%
  filter(Geography %in% t, Mode=="Auto") %>%
  rename(daily_per_person=`PMT per Person`, daily_per_hh=`PMT per HH`) %>%
  select(`Model Run`, PMT, Population, Households, Place, daily_per_person, daily_per_hh, Mode, Geography)

r <- hh.vmt.data %>%
  filter(Geography %in% c("County"), Mode=="Auto") %>%
  group_by(`Model Run`) %>%
  summarize(PMT=sum(PMT),Population=sum(Population),Households=sum(Households)) %>%
  mutate(Place="Region", daily_per_person=PMT/Population, daily_per_hh=PMT/Households, Mode="Auto", Geography="County")

tbl <- bind_rows(tbl,r) %>%
  mutate(annual_per_person = daily_per_person*annualization.factor.highway, annual_per_hh = daily_per_hh*annualization.factor.highway) %>%
  filter(Place %in% c("Region","More than regional average")) %>%
  mutate(Place=case_when(
    Geography=="Racial" ~ "People of Color",
    Geography=="Poverty" ~ "People of Lower Income",
    Geography=="County" ~ "Region"
  ))


vmt.region.chart <- create_facet_bar_chart(w.tbl=tbl, w.x='Model Run', w.y='daily_per_hh', w.factor=1, w.scales="free_x", w.run="Model Run",
                                           w.dec=1, w.group='Place', w.lab="",w.title="", w.facet=3, w.colors = c)

by.vmt.hh <- tbl %>% filter(`Model Run`=="Base Year" & Place =="Region") %>% select(annual_per_hh) %>% pull()
cp.vmt.hh <- tbl %>% filter(`Model Run`=="Plan" & Place =="Region") %>% select(annual_per_hh) %>% pull()
cp.vmt.ratio <- (cp.vmt.hh-by.vmt.hh)/by.vmt.hh

# By People in County, Centers or Regional Geographies
g <- c("County", "Regional Geography")
c <- c("Base Year" = "#76787A", "Plan" = "#F05A28")
t <- c("King","Kitsap","Pierce","Snohomish",
       "Metro","Core","HCT","CitiesTowns","UU","Rural")

tbl <- hh.vmt.data %>%
  mutate(Geography=gsub("Rgc","Regional Growth Center",Geography)) %>%
  mutate(Geography=gsub("Rg_proposed","Regional Geography",Geography)) %>%
  filter(`Model Run`=="Base Year" | `Model Run`=="Plan") %>%
  filter(`Geography` %in% g) %>%
  filter(`Place`%in% t, Mode=="Auto") %>%
  mutate(Place=factor(Place,levels=t)) %>%
  mutate(Geography=factor(Geography, levels=g)) %>%
  mutate(`Model Run`=factor(`Model Run`, levels=model.run.order))

vmt.geo.chart <- create_facet_bar_chart(w.tbl=tbl, w.x='Place', w.y='PMT per HH', w.factor=1, w.scales="free_x", w.run="Model Run",
                                           w.dec=1, w.group='Geography', w.lab="",w.title="Annual Miles Driven per Household", w.facet=2, w.colors = c)

# By People in Equity Geographies
g <- c("County", "Regional Growth Center", "Regional Geography")
c <- c("Base Year" = "#76787A", "Plan" = "#F05A28")
t <- c("Less than regional average","More than regional average")

tbl <- hh.vmt.data %>%
  filter(`Model Run`=="Base Year" | `Model Run`=="Plan") %>%
  filter(`Place`%in% t, Mode=="Auto") 

tbl <- left_join(tbl,simplified.geography,by=c("Geography"="name")) %>%
  mutate(Place=str_wrap(Place,width=10)) %>%
  mutate(`Model Run`=factor(`Model Run`, levels=model.run.order))

vmt.equity.chart <- create_facet_bar_chart(w.tbl=tbl, w.x='Place', w.y='PMT per HH', w.factor=1, w.scales="free_x", w.run="Model Run",
                                           w.dec=1, w.group='value', w.lab="",w.title="Annual Miles Driven per Household", w.facet=3, w.colors = c)


# Person HOurs Walking and Biking --------------------------------------------------

# by Region
g <- c("Base Year", "Plan")
c <- c("Base Year" = "#76787A", "Plan" = "#8CC63E")
t <- c("County", "Poverty","Racial")

tbl <- hh.vht.data %>%
  filter(Geography %in% t, Mode=="Walk & Bike") %>%
  rename(daily_per_person=`PHT per Person`, daily_per_hh=`PHT per HH`) %>%
  select(`Model Run`, PHT, Population, Households, Place, daily_per_person, daily_per_hh, Mode, Geography)

r <- hh.vht.data %>%
  filter(Geography %in% c("County"), Mode=="Walk & Bike") %>%
  group_by(`Model Run`) %>%
  summarize(PHT=sum(PHT),Population=sum(Population),Households=sum(Households)) %>%
  mutate(Place="Region", daily_per_person=PHT/Population, daily_per_hh=PHT/Households, Mode="Walk & Bike", Geography="County")

tbl <- bind_rows(tbl,r) %>%
  mutate(annual_per_person = daily_per_person*annualization.factor.highway, annual_per_hh = daily_per_hh*annualization.factor.highway) %>%
  filter(Place %in% c("Region","More than regional average")) %>%
  mutate(Place=case_when(
    Geography=="Racial" ~ "People of Color",
    Geography=="Poverty" ~ "People of Lower Income",
    Geography=="County" ~ "Region"
  ))


walk.bike.region.chart <- create_facet_bar_chart(w.tbl=tbl, w.x='Model Run', w.y='daily_per_hh', w.factor=1, w.scales="free_x", w.run="Model Run",
                                           w.dec=0, w.group='Place', w.lab="",w.title="", w.facet=3, w.colors = c)

# Hours of Delay ----------------------------------------------------------

# by Region
g <- c("Base Year", "Plan")
c <- c("Base Year" = "#76787A", "Plan" = "#8CC63E")
t <- c("County", "Poverty","Racial")

tbl <- hh.delay.data %>%
  filter(Geography %in% t, Mode=="Auto") %>%
  rename(daily_per_person=`Delay per Person`, daily_per_hh=`Delay per HH`) %>%
  select(`Model Run`, `Daily Delay`, Population, Households, Place, daily_per_person, daily_per_hh, Mode, Geography)

r <- hh.delay.data %>%
  filter(Geography %in% c("County"), Mode=="Auto") %>%
  group_by(`Model Run`) %>%
  summarize(`Daily Delay`=sum(`Daily Delay`),Population=sum(Population),Households=sum(Households)) %>%
  mutate(Place="Region", daily_per_person=`Daily Delay`/Population, daily_per_hh=`Daily Delay`/Households, Mode="Auto", Geography="County")

tbl <- bind_rows(tbl,r) %>%
  mutate(annual_per_person = (daily_per_person*annualization.factor.highway)/60, annual_per_hh = (daily_per_hh*annualization.factor.highway)/60) %>%
  filter(Place %in% c("Region","More than regional average")) %>%
  mutate(Place=case_when(
    Geography=="Racial" ~ "People of Color",
    Geography=="Poverty" ~ "People of Lower Income",
    Geography=="County" ~ "Region"
  ))


vhd.region.chart <- create_facet_bar_chart(w.tbl=tbl, w.x='Model Run', w.y='annual_per_hh', w.factor=1, w.scales="free_x", w.run="Model Run",
                                           w.dec=0, w.group='Place', w.lab="",w.title="", w.facet=3, w.colors = c)

by.delay.hh <- tbl %>% filter(`Model Run`=="Base Year" & Place =="Region") %>% select(annual_per_hh) %>% pull()
cp.delay.hh <- tbl %>% filter(`Model Run`=="Plan" & Place =="Region") %>% select(annual_per_hh) %>% pull()
cp.delay.ratio <- (cp.delay.hh-by.delay.hh)/by.delay.hh

# By People in County, Centers or Regional Geographies
g <- c("County", "Regional Geography")
c <- c("Base Year" = "#76787A", "Plan" = "#8CC63E")
t <- c("King","Kitsap","Pierce","Snohomish",
       "Metro","Core","HCT","CitiesTowns","UU","Rural")

tbl <- hh.delay.data %>%
  mutate(Geography=gsub("Rgc","Regional Growth Center",Geography)) %>%
  mutate(Geography=gsub("Rg_proposed","Regional Geography",Geography)) %>%
  filter(`Model Run`=="Base Year" | `Model Run`=="Plan") %>%
  filter(`Geography` %in% g) %>%
  filter(`Place`%in% t, Mode=="Auto") %>%
  mutate(Place=factor(Place,levels=t)) %>%
  mutate(Geography=factor(Geography, levels=g)) %>%
  mutate(`Model Run`=factor(`Model Run`, levels=model.run.order)) %>%
  mutate(`Annual per HH`=(`Delay per HH`*annualization.factor.highway)/60)

vhd.geo.chart <- create_facet_bar_chart(w.tbl=tbl, w.x='Place', w.y='Annual per HH', w.factor=1, w.scales="free_x", w.run="Model Run",
                                        w.dec=0, w.group='Geography', w.lab="",w.title="", w.facet=2, w.colors = c)

# By People in Equity Geographies
g <- c("County", "Regional Growth Center", "Regional Geography")
c <- c("Base Year" = "#76787A", "Plan" = "#8CC63E")
t <- c("Less than regional average","More than regional average")

tbl <- hh.delay.data %>%
  filter(`Model Run`=="Base Year" | `Model Run`=="Plan") %>%
  filter(`Place`%in% t, Mode=="Auto") 

tbl <- left_join(tbl,simplified.geography,by=c("Geography"="name")) %>%
  mutate(Place=str_wrap(Place,width=10)) %>%
  mutate(`Model Run`=factor(`Model Run`, levels=model.run.order)) %>%
  mutate(`Annual per HH`=(`Delay per HH`*annualization.factor.highway)/60)

vhd.equity.chart <- create_facet_bar_chart(w.tbl=tbl, w.x='Place', w.y='Annual per HH', w.factor=1, w.scales="free_x", w.run="Model Run",
                                           w.dec=0, w.group='value', w.lab="",w.title="", w.facet=3, w.colors = c)

by.vhd <- network.summary %>% filter(model_run=="Base Year" & county_name=="Region") %>% select(daily_vhd) %>% pull()
by.truck.vhd <- truck.vhd.summary %>% filter(model_run=="Base Year" & tod=="Daily") %>% select(`Daily Truck Delay`) %>% pull()
by.truck.vhd.percent <- by.truck.vhd / by.vhd
by.hh <- transit.access %>% filter(model_run=="Base Year" & value=="Total Households") %>% select(total) %>% pull()
by.vhd.hh <- by.vhd/by.hh * annualization.factor.highway

cp.vhd <- network.summary %>% filter(model_run=="Plan" & county_name=="Region") %>% select(daily_vhd) %>% pull()
cp.truck.vhd <- truck.vhd.summary %>% filter(model_run=="Plan" & tod=="Daily") %>% select(`Daily Truck Delay`) %>% pull()
cp.truck.vhd.percent <- cp.truck.vhd / cp.vhd
cp.hh <- transit.access %>% filter(model_run=="Plan" & value=="Total Households") %>% select(total) %>% pull()
cp.vhd.hh <- cp.vhd/cp.hh * annualization.factor.highway
cp.by.vhd.ratio <- (cp.vhd.hh / by.vhd.hh) - 1

# Mode Share ----------------------------------------------------------

# by Region Commute Mode
c <- c("Base Year" = "#76787A", "Plan" = "#91268F")
t <- c("Poverty","Racial")

r <- hh.ms.data %>% 
  filter(Geography=="County" & Purpose=="Commute") %>%
  mutate(Mode=str_wrap(Mode,width=10)) %>%
  group_by(Mode,`Model Run`) %>%
  summarize(Trips=sum(Trips), Total_Trips=sum(Total_Trips)) %>%
  mutate(Place="Region", Share=Trips/Total_Trips) %>% 
  mutate(Geography="County")

tbl <- hh.ms.data %>%
  filter(Geography%in%t & Purpose=="Commute" & Place=="More than regional average") %>%
  mutate(Place=case_when(
    Geography=="Racial" ~ "People of Color",
    Geography=="Poverty" ~ "People of Lower Income")) %>%
  select(-Purpose,-Year)

tbl <- bind_rows(tbl,r)

ms.commute.chart <- create_facet_bar_chart(w.tbl=tbl, w.x='Mode', w.y='Share', w.factor=100, w.scales="free_x", w.run="Model Run",
                                          w.dec=0, w.group='Place', w.lab="%",w.title="Mode Share", w.facet=3, w.colors = c, w.label = label_percent())

# by Region Non-Commute Mode
c <- c("Base Year" = "#76787A", "Plan" = "#91268F")
t <- c("Poverty","Racial")

r <- hh.ms.data %>% 
  filter(Geography=="County" & Purpose=="Non-Commute") %>%
  mutate(Mode=str_wrap(Mode,width=10)) %>%
  group_by(Mode,`Model Run`) %>%
  summarize(Trips=sum(Trips), Total_Trips=sum(Total_Trips)) %>%
  mutate(Place="Region", Share=Trips/Total_Trips) %>% 
  mutate(Geography="County")

tbl <- hh.ms.data %>%
  filter(Geography%in%t & Purpose=="Non-Commute" & Place=="More than regional average") %>%
  mutate(Place=case_when(
    Geography=="Racial" ~ "People of Color",
    Geography=="Poverty" ~ "People of Lower Income")) %>%
  select(-Purpose,-Year)

tbl <- bind_rows(tbl,r)

ms.noncommute.chart <- create_facet_bar_chart(w.tbl=tbl, w.x='Mode', w.y='Share', w.factor=100, w.scales="free_x", w.run="Model Run",
                                              w.dec=0, w.group='Place', w.lab="%",w.title="Mode Share", w.facet=3, w.colors = c, w.label = label_percent())

# by Region any Purpose Mode
c <- c("Base Year" = "#76787A", "Plan" = "#91268F")
t <- c("Poverty","Racial")

r <- hh.ms.data %>% 
  filter(Geography=="County") %>%
  mutate(Mode=str_wrap(Mode,width=10)) %>%
  group_by(Mode,`Model Run`) %>%
  summarize(Trips=sum(Trips), Total_Trips=sum(Total_Trips)) %>%
  mutate(Place="Region", Share=Trips/Total_Trips) %>% 
  mutate(Geography="County")

tbl <- hh.ms.data %>%
  filter(Geography%in%t & Place=="More than regional average") %>%
  mutate(Place=case_when(
    Geography=="Racial" ~ "People of Color",
    Geography=="Poverty" ~ "People of Lower Income")) %>%
  select(-Purpose,-Year) %>%
  group_by(Mode, Place, Geography, `Model Run`) %>%
  summarize(Trips=sum(Trips), Total_Trips=sum(Total_Trips)) %>%
  mutate(Share=Trips/Total_Trips)
  

tbl <- bind_rows(tbl,r)

ms.anypurpose.chart <- create_facet_bar_chart(w.tbl=tbl, w.x='Mode', w.y='Share', w.factor=100, w.scales="free_x", w.run="Model Run",
                                              w.dec=0, w.group='Place', w.lab="%",w.title="Mode Share", w.facet=3, w.colors = c, w.label = label_percent())



# Congestion Maps ---------------------------------------------------------
plan.run <- list('2050','Plan', '//modelstation2//c$//Workspace//sc_2050_rtp//soundcast')
pm.network <- st_read(file.path(plan.run[[3]],pm.peak.shape))

# AM Peak Period Stats
by.am.lane.miles.total <- link.outputs %>% filter(model_run == "Base Year" & Period=="AM Peak") %>% select(Lane_Miles) %>% pull() %>% sum()
by.am.lane.miles.moderate <- link.outputs %>% filter(model_run == "Base Year" & Period=="AM Peak" & Congestion_Level=="Moderate") %>% select(Lane_Miles) %>% pull() %>% sum()
by.am.lane.miles.heavy <- link.outputs %>% filter(model_run == "Base Year" & Period=="AM Peak" & Congestion_Level=="Heavy") %>% select(Lane_Miles) %>% pull() %>% sum()
by.am.lane.miles.severe <- link.outputs %>% filter(model_run == "Base Year" & Period=="AM Peak" & Congestion_Level=="Severe") %>% select(Lane_Miles) %>% pull() %>% sum()

by.am.moderate.shr <- by.am.lane.miles.moderate / by.am.lane.miles.total
by.am.heavy.shr <- by.am.lane.miles.heavy / by.am.lane.miles.total
by.am.severe.shr <- by.am.lane.miles.severe / by.am.lane.miles.total

cp.am.lane.miles.total <- link.outputs %>% filter(model_run == "Plan" & Period=="AM Peak") %>% select(Lane_Miles) %>% pull() %>% sum()
cp.am.lane.miles.moderate <- link.outputs %>% filter(model_run == "Plan" & Period=="AM Peak" & Congestion_Level=="Moderate") %>% select(Lane_Miles) %>% pull() %>% sum()
cp.am.lane.miles.heavy <- link.outputs %>% filter(model_run == "Plan" & Period=="AM Peak" & Congestion_Level=="Heavy") %>% select(Lane_Miles) %>% pull() %>% sum()
cp.am.lane.miles.severe <- link.outputs %>% filter(model_run == "Plan" & Period=="AM Peak" & Congestion_Level=="Severe") %>% select(Lane_Miles) %>% pull() %>% sum()

cp.am.moderate.shr <- cp.am.lane.miles.moderate / cp.am.lane.miles.total
cp.am.heavy.shr <- cp.am.lane.miles.heavy / cp.am.lane.miles.total
cp.am.severe.shr <- cp.am.lane.miles.severe / cp.am.lane.miles.total

# PM Peak Period Stats
by.pm.lane.miles.total <- link.outputs %>% filter(model_run == "Base Year" & Period=="PM Peak") %>% select(Lane_Miles) %>% pull() %>% sum()
by.pm.lane.miles.moderate <- link.outputs %>% filter(model_run == "Base Year" & Period=="PM Peak" & Congestion_Level=="Moderate") %>% select(Lane_Miles) %>% pull() %>% sum()
by.pm.lane.miles.heavy <- link.outputs %>% filter(model_run == "Base Year" & Period=="PM Peak" & Congestion_Level=="Heavy") %>% select(Lane_Miles) %>% pull() %>% sum()
by.pm.lane.miles.severe <- link.outputs %>% filter(model_run == "Base Year" & Period=="PM Peak" & Congestion_Level=="Severe") %>% select(Lane_Miles) %>% pull() %>% sum()

by.pm.moderate.shr <- by.pm.lane.miles.moderate / by.pm.lane.miles.total
by.pm.heavy.shr <- by.pm.lane.miles.heavy / by.pm.lane.miles.total
by.pm.severe.shr <- by.pm.lane.miles.severe / by.pm.lane.miles.total

cp.pm.lane.miles.total <- link.outputs %>% filter(model_run == "Plan" & Period=="PM Peak") %>% select(Lane_Miles) %>% pull() %>% sum()
cp.pm.lane.miles.moderate <- link.outputs %>% filter(model_run == "Plan" & Period=="PM Peak" & Congestion_Level=="Moderate") %>% select(Lane_Miles) %>% pull() %>% sum()
cp.pm.lane.miles.heavy <- link.outputs %>% filter(model_run == "Plan" & Period=="PM Peak" & Congestion_Level=="Heavy") %>% select(Lane_Miles) %>% pull() %>% sum()
cp.pm.lane.miles.severe <- link.outputs %>% filter(model_run == "Plan" & Period=="PM Peak" & Congestion_Level=="Severe") %>% select(Lane_Miles) %>% pull() %>% sum()

cp.pm.moderate.shr <- cp.pm.lane.miles.moderate / cp.pm.lane.miles.total
cp.pm.heavy.shr <- cp.pm.lane.miles.heavy / cp.pm.lane.miles.total
cp.pm.severe.shr <- cp.pm.lane.miles.severe / cp.pm.lane.miles.total

delta.congested <- (cp.pm.moderate.shr + cp.pm.heavy.shr + cp.pm.severe.shr) - (by.pm.moderate.shr + by.pm.heavy.shr + by.pm.severe.shr)

# Maps
pm.links <- link.outputs %>% filter(model_run == "Plan" & Period=="PM Peak") %>% select(ij,Congestion_Level)

pm.network <- left_join(pm.network, pm.links, by=c("id"="ij")) %>%
  drop_na()

pm.severe <- pm.network %>% filter(Congestion_Level=="Severe") %>% st_transform(wgs84)
pm.heavy <- pm.network %>% filter(Congestion_Level=="Heavy") %>% st_transform(wgs84)
pm.moderate <- pm.network %>% filter(Congestion_Level=="Moderate") %>% st_transform(wgs84)

pm.map <- leaflet() %>%
  addMapPane(name = "links", zIndex = 410) %>%
  
  addProviderTiles(providers$CartoDB.Positron) %>%
  
  addPolylines(data = pm.severe,
               color = "black",
               weight = 3,
               fillColor = "black",
               group = "links") %>%
  
  addPolylines(data = pm.heavy,
               color = "red",
               weight = 3,
               fillColor = "red",
               group = "links") %>%
  
  setView(lng=-122.257, lat=47.615, zoom=10)


# Transit -----------------------------------------------------------------
c <- c("Base Year" = "#76787A", "Plan" = "#8CC63E")
boardings.agency.chart <- create_facet_bar_chart(w.tbl=transit.boardings.by.agency, w.x='model_run', w.y='annual_boardings',
                                                 w.factor=1/1000000, w.dec=1, w.group='agency_name', w.lab="M", w.run="model_run", w.facet=3, w.colors = c)

boardings.region <- create_facet_bar_chart(w.tbl=transit.boardings.by.agency%>%filter(agency_name=="Region"), w.x='model_run', w.y='annual_boardings',
                                                 w.factor=1/1000000, w.dec=1, w.group='agency_name', w.lab="M", w.run="model_run", w.facet=3, w.colors = c)


c <- c("Base Year" = "#76787A", "Plan" = "#00A7A0")

tbl <- transit.access %>% 
  filter(value=="Total Households" | value=="Total Jobs")

transit.access.chart <- create_facet_bar_chart(w.tbl=tbl, w.x='model_run', w.y='share',
                                                 w.factor=100, w.dec=0, w.group='value', w.lab="%", w.facet=2, w.run="model_run", w.colors = c)

# Emissions -----------------------------------------------------------------
c <- c("Base Year" = "#76787A", "Plan" = "#8CC63E")

tbl <- emissions.summary %>% mutate(model_run=factor(model_run, levels=model.run.order)) %>%
  filter(pollutant_name != "PM10")

emissions.chart <- create_facet_bar_chart_no_label(w.tbl=tbl, w.x='model_run', w.y='total_daily_tons',
                                          w.factor=1, w.dec=1, w.group='pollutant_name', w.lab="", w.run="model_run", w.facet=3, w.colors = c)
                                                 
# Truck Miles Traveled and Delay Charts--------------------------------------------------

# by Region
c <- c("Base Year" = "#76787A", "Plan" = "#00A7A0")
o <- c("Medium Trucks", "Heavy Trucks")

tbl <- truck.vmt.summary %>%
  filter(tod=="Daily") %>%
  mutate(`Medium Trucks` = `Daily Medium Truck VMT` / `Medium Truck Trips`) %>%
  mutate(`Heavy Trucks` = `Daily Heavy Truck VMT` / `Heavy Truck Trips`) %>%
  select(model_run,`Medium Trucks`, `Heavy Trucks`) %>%
  pivot_longer(cols=c("Medium Trucks", "Heavy Trucks")) %>%
  mutate(name=factor(name,levels = o))

truck.distance.chart <- create_facet_bar_chart(w.tbl=tbl, w.x='model_run', w.y='value', w.factor=1, w.scales="free_x", w.run="model_run",
                                           w.dec=0, w.group='name', w.lab="",w.title="", w.facet=3, w.colors = c)

by.med.truck.vmt <- truck.vmt.summary %>% filter(model_run=="Base Year" & tod=="Daily") %>% select(`Daily Medium Truck VMT`) %>% pull()
by.hvy.truck.vmt <- truck.vmt.summary %>% filter(model_run=="Base Year" & tod=="Daily") %>% select(`Daily Heavy Truck VMT`) %>% pull()

by.med.trips <- truck.vmt.summary %>% filter(model_run=="Base Year" & tod=="Daily") %>% select(`Medium Truck Trips`) %>% pull()
by.hvy.trips <- truck.vmt.summary %>% filter(model_run=="Base Year" & tod=="Daily") %>% select(`Heavy Truck Trips`) %>% pull()
by.truck.trips <- by.med.trips + by.hvy.trips

cp.med.trips <- truck.vmt.summary %>% filter(model_run=="Plan" & tod=="Daily") %>% select(`Medium Truck Trips`) %>% pull()
cp.hvy.trips <- truck.vmt.summary %>% filter(model_run=="Plan" & tod=="Daily") %>% select(`Heavy Truck Trips`) %>% pull()
cp.truck.trips <- cp.med.trips + cp.hvy.trips

truck.trips.ratio <- (cp.truck.trips / by.truck.trips)

# Delay
c <- c("Base Year" = "#76787A", "Plan" = "#F05A28")
o <- c("Medium Trucks", "Heavy Trucks")

tbl <- truck.vhd.summary %>%
  filter(tod=="Daily") %>%
  mutate(`Medium Trucks` = `Annual Medium Truck Delay` / `Medium Truck Trips`) %>%
  mutate(`Heavy Trucks` = `Annual Heavy Truck Delay` / `Heavy Truck Trips`) %>%
  select(model_run,`Medium Trucks`, `Heavy Trucks`) %>%
  pivot_longer(cols=c("Medium Trucks", "Heavy Trucks")) %>%
  mutate(name=factor(name,levels = o))

truck.delay.chart <- create_facet_bar_chart(w.tbl=tbl, w.x='model_run', w.y='value', w.factor=1, w.scales="free_x", w.run="model_run",
                                               w.dec=0, w.group='name', w.lab="",w.title="", w.facet=3, w.colors = c)

by.med.truck.delay <- tbl %>% filter(model_run=="Base Year" & name=="Medium Trucks") %>% select(`value`) %>% pull()
by.hvy.truck.delay <- tbl %>% filter(model_run=="Base Year" & name=="Heavy Trucks") %>% select(`value`) %>% pull()

cp.med.truck.delay <- tbl %>% filter(model_run=="Plan" & name=="Medium Trucks") %>% select(`value`) %>% pull()
cp.hvy.truck.delay <- tbl %>% filter(model_run=="Plan" & name=="Heavy Trucks") %>% select(`value`) %>% pull()

# Sumary Stats
# Base Year
by.vmt <- network.summary %>% filter(model_run=="Base Year" & county_name=="Region") %>% select(daily_vmt) %>% pull()
# All Trucks
by.truck.vmt <- truck.vmt.summary %>% filter(model_run=="Base Year" & tod=="Daily") %>% select(`Daily Truck VMT`) %>% pull()
by.truck.vmt.percent <- by.truck.vmt / by.vmt
# Medium Trucks

by.med.truck.vmt.percent <- by.med.truck.vmt / by.vmt
# Heavy Trucks

by.hvy.truck.vmt.percent <- by.hvy.truck.vmt / by.vmt
# Buses
by.bus.vmt <- bus.vmt.summary %>% filter(model_run=="Base Year" & tod=="Daily") %>% select(`Daily Bus VMT`) %>% pull()
by.bus.vmt.percent <- by.bus.vmt / by.vmt

# Plan
cp.vmt <- network.summary %>% filter(model_run=="Plan" & county_name=="Region") %>% select(daily_vmt) %>% pull()
# All Trucks
cp.truck.vmt <- truck.vmt.summary %>% filter(model_run=="Plan" & tod=="Daily") %>% select(`Daily Truck VMT`) %>% pull()
cp.truck.vmt.percent <- cp.truck.vmt / cp.vmt
# Medium Trucks
cp.med.truck.vmt <- truck.vmt.summary %>% filter(model_run=="Plan" & tod=="Daily") %>% select(`Daily Medium Truck VMT`) %>% pull()
cp.med.truck.vmt.percent <- cp.med.truck.vmt / cp.vmt
# Heavy Trucks
cp.hvy.truck.vmt <- truck.vmt.summary %>% filter(model_run=="Plan" & tod=="Daily") %>% select(`Daily Heavy Truck VMT`) %>% pull()
cp.hvy.truck.vmt.percent <- cp.hvy.truck.vmt / cp.vmt
# Buses
cp.bus.vmt <- bus.vmt.summary %>% filter(model_run=="Plan" & tod=="Daily") %>% select(`Daily Bus VMT`) %>% pull()
cp.bus.vmt.percent <- cp.bus.vmt / cp.vmt
