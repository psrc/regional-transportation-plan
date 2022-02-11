library(tidyverse)
library(tidycensus)

Sys.getenv("CENSUS_API_KEY")
ua.pop <- get_acs(geography="urban area",year=2019,survey = "acs1",variable = "B01001_001")

write.csv(ua.pop,"urban_area.pop.csv")
