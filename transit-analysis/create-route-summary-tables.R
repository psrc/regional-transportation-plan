# Basic Inputs ------------------------------------------------------------
service.period <- 'Fall'
years <- c(seq(2015,2020,1))
output.annual.csv <- 'no'
output.combined.csv <- 'yes'

all.route.summaries <- NULL

for (transit.year in years) {
  
  print(paste0("Summarizing ",transit.year))
  
  source("route-summary-from-gtfs.R")
  
  # Combine yearly route summaries into one file by gtfs file type
  ifelse(is.null(all.route.summaries), all.route.summaries <- routes, all.route.summaries <- bind_rows(all.route.summaries,routes))
  
}

rm(routes)

if (output.combined.csv=='yes') {
  fwrite(all.route.summaries, paste0("output/route_summaries_",min(years),"_to_",max(years),".csv"))
}