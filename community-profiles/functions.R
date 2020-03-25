##################################################################################
##################################################################################
### Functions
##################################################################################
##################################################################################

return_estimate <- function(w_tbl, w_plc, w_cat, w_hhtype, w_mode, w_name, w_desc, w_yr, w_dec) {
  w_result <- as.numeric(w_tbl[geog_name %in% w_plc & variable_category %in% w_cat & variable_description %in% w_desc & variable_name %in% w_name & variable_mode %in% w_mode & variable_hhtype %in% w_hhtype & year %in% w_yr, sum(estimate)])
  return(w_result)
}

create_summary_table <- function(w_tbl, w_plc, w_cat, w_hhtype, w_mode, w_name, w_desc, w_cols, w_ord, w_len, f_name) {
  
  # Subset the table and rename columns
  tbl <- w_tbl[geog_name %in% w_plc & variable_category %in% w_cat & variable_description %in% w_desc & variable_name %in% w_name & variable_mode %in% w_mode & variable_hhtype %in% w_hhtype]
  tbl <- tbl[,..w_cols]
  n_nms <- c(f_name,"Estimate","Year")
  setnames(tbl,n_nms)
  
  # Set the order of the table using factors
  tbl[[f_name]] <- factor(tbl[[f_name]], levels = w_ord)
  tbl <- tbl[order(get(f_name)),]
  
  # Convert to long format for table display
  wide_tbl <- dcast(tbl, get(f_name) ~ Year, value.var="Estimate")
  setnames(wide_tbl,c(f_name,data_years))
  
  # Add a total row
  dfTotals <- data.frame(f_name="Total",t(colSums(wide_tbl[,-1])))
  colnames(dfTotals) <- names(wide_tbl)
  wide_tbl <- rbind(wide_tbl, dfTotals)
  
  if (w_cat %in% c("share")) {
    
    c_tbl <- datatable(wide_tbl,rownames = FALSE, options = list(pageLength = w_len, columnDefs = list(list(className = 'dt-center', targets =1:4))))  %>% formatPercentage(data_years, 0)
    
  } else {
    
    c_tbl <- datatable(wide_tbl,rownames = FALSE, options = list(pageLength = w_len, columnDefs = list(list(className = 'dt-center', targets =1:4))))  %>% formatCurrency(data_years, "", digits = 0)
    
  }  
  return(c_tbl)
  
}

create_stacked_bar <- function(w_tbl, w_plc, w_cat, w_hhtype, w_mode, w_name, w_desc, w_cols, w_ord, w_len, f_name, w_title) {
  
  # Subset the table and rename columns
  tbl <- w_tbl[geog_name %in% w_plc & variable_category %in% w_cat & variable_description %in% w_desc & variable_name %in% w_name & variable_mode %in% w_mode & variable_hhtype %in% w_hhtype]
  tbl <- tbl[,..w_cols]
  n_nms <- c(f_name,"Estimate","Year")
  setnames(tbl,n_nms)
  
  # Set the order of the table using factors
  tbl[[f_name]] <- factor(tbl[[f_name]], levels = w_ord)
  tbl <- tbl[order(get(f_name)),]
  
  if (w_cat %in% c("share")) {
    
    w_chart <- ggplotly(ggplot(data=tbl,
                               aes(x=`Year`,
                                   y=`Estimate`,
                                   fill = get(f_name),
                                   text = paste0("<b>",get(f_name)," share of total ",w_title,": ","</b>",prettyNum(round(`Estimate`*100, 1), big.mark = ","),"%")
                               )) +
                          geom_bar(position="stack", stat="identity", width = 5) +
                          scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
                          scale_x_continuous(breaks = tbl$Year, labels = tbl$Year) +
                          xlab("Year") +
                          ylab(paste0("Percent of Total ",w_title)) +
                          theme(legend.title = element_blank(),
                                axis.text=element_text(size=10),
                                axis.text.x.bottom=element_text(size=10),
                                axis.title=element_text(size=10,face="bold"),
                                panel.grid.major = element_blank(),
                                panel.grid.minor = element_blank(),
                                panel.border = element_blank(),
                                axis.line = element_blank())
                        ,tooltip = c("text"))
    
  } else {
    
    w_chart <- ggplotly(ggplot(data=tbl,
                               aes(x=`Year`,
                                   y=`Estimate`,
                                   fill = get(f_name),
                                   text = paste0("<b>","Total ", get(f_name)," ",w_title,": ","</b>",prettyNum(round(`Estimate`, -1), big.mark = ","))
                               )) +
                          geom_bar(position="stack", stat="identity", width = 5) +
                          scale_y_continuous(labels = scales::comma)+
                          scale_x_continuous(breaks = tbl$Year, labels = tbl$Year) +
                          xlab("Year") +
                          ylab(paste0("Total ",w_title)) +
                          theme(legend.title = element_blank(),
                                axis.text=element_text(size=10),
                                axis.text.x.bottom=element_text(size=10),
                                axis.title = element_text(size=10,face="bold"),
                                panel.grid.major = element_blank(),
                                panel.grid.minor = element_blank(),
                                panel.border = element_blank(),
                                axis.line = element_blank())
                        ,tooltip = c("text"))    
  }
  
  return(w_chart)
  
}

find_place_data <- function(wrk_nm, wrk_typ) {
  wrk_coord <- as.numeric(community.point[NAME == wrk_nm,get(wrk_typ)])
  return(wrk_coord)
}

create_tract_map_pick_variable <- function(w_tbl, w_plc, w_cat, w_hhtype, w_mode, w_name, w_desc, w_yr, w_color, w_title, w_pre, w_suff) {
  
  # Trim full Tract table to Variable and Year of interest
  tbl <- w_tbl[variable_category %in% w_cat & variable_description %in% w_desc & variable_name %in% w_name & variable_mode %in% w_mode & variable_hhtype %in% w_hhtype & place_type %in% "tr" & year %in% w_yr]
  cols <- c("geog_name","estimate")
  tbl <- tbl[,..cols]
  setnames(tbl,c("geoid","value"))
  
  # Trim Tracts for current place
  city <- community.shape[which(community.shape$NAME %in% w_plc),]
  interim <- intersect(tract.shape, city)
  tract_ids <- unique(interim$tract_id)
  
  tracts.trimmed <- tract.shape[which(tract.shape$tract_id %in% tract_ids),]
  current_value  <- sp::merge(tracts.trimmed, tbl, by.x = "tract_id", by.y = "geoid")
  
  # Determine Bins
  rng <- range(current_value$value)
  max_bin <- max(abs(rng))
  round_to <- 10^floor(log10(max_bin))
  max_bin <- ceiling(max_bin/round_to)*round_to
  breaks <- (max_bin*c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.8, 1))
  bins <- c(0, breaks)
  
  pal <- colorBin(w_color, domain = current_value$value, bins = bins)
  
  if (w_cat %in% c("share")) {
    
    labels <- paste0("<b>",paste0(w_title,": "), "</b>", w_pre, prettyNum(round(current_value$value*100, 0), big.mark = ","),"%") %>% lapply(htmltools::HTML)
    
  } else {
    
    labels <- paste0("<b>",paste0(w_title,": "), "</b>", w_pre, prettyNum(round(current_value$value, 0), big.mark = ","),"") %>% lapply(htmltools::HTML)
    
  }
  
  # Create Map
  working_map <- leaflet(data = current_value, options = leafletOptions(zoomControl=FALSE)) %>% 
    addProviderTiles(providers$CartoDB.Positron) %>%
    addLayersControl(baseGroups = c("Base Map"),
                     overlayGroups = c("Census Tracts","City Boundary"),
                     options = layersControlOptions(collapsed = FALSE)) %>%
    addPolygons(data = city,
                fillColor = "76787A",
                weight = 4,
                opacity = 1.0,
                color = "#91268F",
                dashArray = "4",
                fillOpacity = 0.0,
                group = "City Boundary")%>% 
    addPolygons(fillColor = pal(current_value$value),
                weight = 1.0,
                opacity = 1,
                color = "white",
                dashArray = "3",
                fillOpacity = 0.7,
                highlight = highlightOptions(
                  weight =5,
                  color = "76787A",
                  dashArray ="",
                  fillOpacity = 0.7,
                  bringToFront = TRUE),
                label = labels,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto"),
                group = "Census Tracts")%>%
    setView(lng=find_place_data(w_plc,"INTPTLON"), lat=find_place_data(w_plc,"INTPTLAT"), zoom=find_place_data(w_plc,"ZOOM"))
  
  return(working_map)
  
}