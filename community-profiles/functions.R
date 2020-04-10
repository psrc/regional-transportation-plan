##################################################################################
##################################################################################
### Functions
##################################################################################
##################################################################################

return_estimate <- function(w_tbl, w_plc, w_name, w_desc, w_yr, w_source) {
  w_result <- as.numeric(w_tbl[geog_name %in% w_plc & variable_description %in% w_desc & variable_name %in% w_name & year %in% w_yr & data_source %in% w_source, sum(estimate)])
  return(w_result)
}

return_share <- function(w_tbl, w_plc, w_name, w_desc, w_yr, w_source) {
  w_result <- as.numeric(w_tbl[geog_name %in% w_plc & variable_description %in% w_desc & variable_name %in% w_name & year %in% w_yr & data_source %in% w_source, sum(share)])
  return(w_result)
}

create_summary_table <- function(w_tbl, w_plc, w_name, w_desc, w_cols, w_ord, w_len, f_name, estimate_type, base_year) {
  
  # Subset the table and rename columns
  w_yrs <- c(base_year,2025,2040,2050)
  tbl <- w_tbl[geog_name %in% w_plc & variable_description %in% w_desc & variable_name %in% w_name & year %in% w_yrs]
  w_cols <- c(w_cols,estimate_type)
  tbl <- tbl[,..w_cols]
  n_nms <- c(f_name,"Year","Source",estimate_type)
  setnames(tbl,n_nms)
  
  # Set the order of the table using factors
  tbl[[f_name]] <- factor(tbl[[f_name]], levels = w_ord)
  tbl <- tbl[order(get(f_name)),]
  
  # Convert to long format for table display
  wide_tbl <- dcast(tbl, get(f_name) ~ Source + Year, value.var= estimate_type)
  setnames(wide_tbl,c(f_name,'Observed',w_yrs))
  
  data_cols <- c('Observed',w_yrs)
  
  # Add a total row
  dfTotals <- data.frame(f_name="Total",t(colSums(wide_tbl[,-1])))
  colnames(dfTotals) <- names(wide_tbl)
  wide_tbl <- rbind(wide_tbl, dfTotals)
  
  if (estimate_type %in% c("share")) {
    
    c_tbl <- datatable(wide_tbl,rownames = FALSE, options = list(pageLength = w_len, columnDefs = list(list(className = 'dt-center', targets =1:5))))  %>% formatPercentage(data_cols, 0)
    
  } else {
    
    c_tbl <- datatable(wide_tbl,rownames = FALSE, options = list(pageLength = w_len, columnDefs = list(list(className = 'dt-center', targets =1:5))))  %>% formatCurrency(data_cols, "", digits = 0)
    
  }  
  return(c_tbl)
  
}

create_bar_chart <- function(w_tbl, w_plc, w_name, w_desc, w_cols, w_ord, f_name, estimate_type, base_year, w_title, bar_type) {
  
  # Subset the table and rename columns
  w_yrs <- c(base_year,2025,2040,2050)
  tbl <- w_tbl[geog_name %in% w_plc & variable_description %in% w_desc & variable_name %in% w_name & year %in% w_yrs]
  w_cols <- c(w_cols,estimate_type)
  tbl <- tbl[,..w_cols]
  n_nms <- c(f_name,"Year","Source",estimate_type)
  setnames(tbl,n_nms)
  
  # Set the order of the table using factors
  tbl[[f_name]] <- factor(tbl[[f_name]], levels = w_ord)
  tbl <- tbl[order(get(f_name)),]
  
  # Rename the Base Year Observed Column
  tbl$Year <- as.character(tbl$Year)
  tbl[tbl$Source %in% c("acs/acs5","lehd/lodes"), "Year"] <- "Observed"
  tbl$Year <- factor(tbl$Year, levels = c("Observed",as.character(base_year),"2025","2040","2050"))
  tbl <- tbl[order(Year),]
  
  plot_labels <- c("Observed","2014","2025","2040","2050")
  
  if (estimate_type %in% c("share")) {
    
    w_chart <- ggplotly(ggplot(data=tbl, aes(fill=get(f_name), y=`share`, x=`Year`,text = paste0("<b>",get(f_name)," share of total ",w_title,": ","</b>",prettyNum(round(`share`*100, 1), big.mark = ","),"%"))) + 
                          geom_bar(position=bar_type, stat="identity") +
                          scale_fill_manual(values= psrc_colors) +
                          scale_y_continuous(labels = scales::percent, limits = c(0, 1))+
                          scale_x_discrete(labels = plot_labels) +
                          ylab(paste0("Percent of Total ",w_title)) +
                          theme(legend.title = element_blank(),
                                axis.text=element_text(size=10),
                                axis.text.x.bottom=element_text(size=10),
                                axis.title.y =element_text(size=10,face="bold"),
                                axis.title.x = element_blank(),
                                panel.grid.major = element_blank(),
                                panel.grid.minor = element_blank(),
                                panel.border = element_blank(),
                                axis.line = element_blank())
                        ,tooltip = c("text"))                        
    
  } else {
    
    w_chart <- ggplotly(ggplot(data=tbl, aes(fill=get(f_name), y=`estimate`, x=`Year`,text = paste0("<b>","Total ", get(f_name)," ",w_title,": ","</b>",prettyNum(round(`estimate`, -1), big.mark = ",")))) + 
                          geom_bar(position=bar_type, stat="identity") +
                          scale_fill_manual(values= psrc_colors) +
                          scale_y_continuous(labels = scales::comma)+
                          scale_x_discrete(labels = plot_labels) +
                          ylab(paste0("Total ",w_title))+
                          theme(legend.title = element_blank(),
                                axis.text=element_text(size=10),
                                axis.text.x.bottom=element_text(size=10),
                                axis.title.y =element_text(size=10,face="bold"),
                                axis.title.x = element_blank(),
                                panel.grid.major = element_blank(),
                                panel.grid.minor = element_blank(),
                                panel.border = element_blank(),
                                axis.line = element_blank())
                        ,tooltip = c("text"))
    
  }
  
  return(w_chart)
  
}

create_pie_chart <- function(w_tbl, w_plc, w_name, w_desc, w_cols, w_ord, f_name, estimate_type, base_year, w_title) {
  
  # Subset the table and rename columns
  w_yrs <- c(base_year,2025,2040,2050)
  tbl <- w_tbl[geog_name %in% w_plc & variable_description %in% w_desc & variable_name %in% w_name & year %in% w_yrs]
  w_cols <- c(w_cols,"estimate","share")
  tbl <- tbl[,..w_cols]
  n_nms <- c(f_name,"Year","Source","estimate","share")
  setnames(tbl,n_nms)
  
  # Set the order of the table using factors
  tbl[[f_name]] <- factor(tbl[[f_name]], levels = w_ord)
  tbl <- tbl[order(get(f_name)),]
  
  # Rename the Base Year Observed Column
  tbl$Year <- as.character(tbl$Year)
  tbl[tbl$Source %in% c("acs/acs5","lehd/lodes"), "Year"] <- "Observed"
  tbl$Year <- factor(tbl$Year, levels = c("Observed",as.character(base_year),"2025","2040","2050"))
  tbl <- tbl[order(Year),]
  
  if (estimate_type %in% c("share")) {
    wrk_pie <- ggplot(tbl, aes(x = "", y = `share`, fill = get(f_name))) +
      geom_bar(width = 1, stat = "identity", color = "white") +
      coord_polar("y", start = 0) +
      geom_text(aes(label = paste0(round(`share`*100), "%")), color = "white", position = position_stack(vjust = 0.5))+
      scale_fill_manual(values = psrc_colors) +
      theme_void()+
      theme(legend.position = "bottom", legend.direction = "horizontal", legend.title = element_blank())
  } else {
      wrk_pie <- ggplot(tbl, aes(x = "", y = `share`, fill = get(f_name))) +
        geom_bar(width = 1, stat = "identity", color = "white") +
        coord_polar("y", start = 0) +
        geom_text(aes(label = `estimate`), color = "white", position = position_stack(vjust = 0.5))+
        scale_fill_manual(values = psrc_colors) +
        theme_void()+
        theme(legend.position = "bottom", legend.direction = "horizontal", legend.title = element_blank())    
  }
  
  wrk_pie <- wrk_pie + facet_wrap(vars(`Year`), ncol = 3)
  
  return(wrk_pie)
  
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