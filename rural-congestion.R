library(tidyverse)
library(data.table)
library(here)


# Network Summary Files ---------------------------------------------------
base.network.outputs.file <- here(getwd(),'/data/base/network_results.csv')
plan.network.outputs.file <- here(getwd(),'/data/plan/network_results.csv')

base <- as_tibble(fread(base.network.outputs.file)) %>%
  select(`ij`, `length`, `num_lanes`,`volume_delay_func`, `@countyid`,`@facilitytype`, `@tveh`, `auto_time`, `data2`, `data3`, `tod`) %>%
  filter(`volume_delay_func` %in% c(1,3,5,7)) %>%
  mutate(`speed` = (`length` / `auto_time`)*60) %>%
  mutate(`speed_ratio` = `speed` / `data2`) %>%
  mutate(`ffvht` = (`@tveh` / `data2`)*`length`) %>%
  mutate(`vht` = (`@tveh` / `speed`)*`length`) %>%
  mutate(`vhd` = round(`vht`-`ffvht`,0)) %>%
  mutate(`lane-miles` = `length` * `num_lanes`)

plan <- as_tibble(fread(plan.network.outputs.file)) %>%
  select(`ij`, `length`,  `num_lanes`, `volume_delay_func`, `@countyid`,`@facilitytype`, `@tveh`, `auto_time`, `data2`, `data3`, `tod`) %>%
  filter(`volume_delay_func` %in% c(1,3,5,7)) %>%
  mutate(`speed` = (`length` / `auto_time`)*60) %>%
  mutate(`speed_ratio` = `speed` / `data2`) %>%
  mutate(`ffvht` = (`@tveh` / `data2`)*`length`) %>%
  mutate(`vht` = (`@tveh` / `speed`)*`length`) %>%
  mutate(`vhd` = round(`vht`-`ffvht`,0)) %>%
  mutate(`lane-miles` = `length` * `num_lanes`)


# Vehicle Hours of Delay --------------------------------------------------

# Regionwide
afac.vhd.2018.region <- base %>% select(vhd) %>% pull() %>% sum()
frwy.vhd.2018.region <- base %>% filter(`data3` %in% c(2,3)) %>% select(vhd) %>% pull() %>% sum()
uart.vhd.2018.region <- base %>% filter(`data3` %in% c(1,4)) %>% select(vhd) %>% pull() %>% sum()
rart.vhd.2018.region <- base %>% filter(`data3` %in% c(6)) %>% select(vhd) %>% pull() %>% sum()

afac.vhd.2050.region <- plan %>% select(vhd) %>% pull() %>% sum()
frwy.vhd.2050.region <- plan %>% filter(`data3` %in% c(2,3)) %>% select(vhd) %>% pull() %>% sum()
uart.vhd.2050.region <- plan %>% filter(`data3` %in% c(1,4)) %>% select(vhd) %>% pull() %>% sum()
rart.vhd.2050.region <- plan %>% filter(`data3` %in% c(6)) %>% select(vhd) %>% pull() %>% sum()

# King County
afac.vhd.2018.king <- base %>% filter(`@countyid` %in% c(33)) %>% select(vhd) %>% pull() %>% sum()
frwy.vhd.2018.king <- base %>% filter(`@countyid` %in% c(33)) %>% filter(`data3` %in% c(2,3)) %>% select(vhd) %>% pull() %>% sum()
uart.vhd.2018.king <- base %>% filter(`@countyid` %in% c(33)) %>% filter(`data3` %in% c(1,4)) %>% select(vhd) %>% pull() %>% sum()
rart.vhd.2018.king <- base %>% filter(`@countyid` %in% c(33)) %>% filter(`data3` %in% c(6)) %>% select(vhd) %>% pull() %>% sum()

afac.vhd.2050.king <- plan %>% filter(`@countyid` %in% c(33)) %>% select(vhd) %>% pull() %>% sum()
frwy.vhd.2050.king <- plan %>% filter(`@countyid` %in% c(33)) %>% filter(`data3` %in% c(2,3)) %>% select(vhd) %>% pull() %>% sum()
uart.vhd.2050.king <- plan %>% filter(`@countyid` %in% c(33)) %>% filter(`data3` %in% c(1,4)) %>% select(vhd) %>% pull() %>% sum()
rart.vhd.2050.king <- plan %>% filter(`@countyid` %in% c(33)) %>% filter(`data3` %in% c(6)) %>% select(vhd) %>% pull() %>% sum()

# Rural vs Urban
rural.art.share.2018.region <- rart.vhd.2018.region / afac.vhd.2018.region
rural.art.share.2018.king <- rart.vhd.2018.king / afac.vhd.2018.king
rural.art.share.2050.region <- rart.vhd.2050.region / afac.vhd.2050.region
rural.art.share.2050.king <- rart.vhd.2050.king / afac.vhd.2050.king

urban.art.share.2018.region <- uart.vhd.2018.region / afac.vhd.2018.region
urban.art.share.2018.king <- uart.vhd.2018.king / afac.vhd.2018.king
urban.art.share.2050.region <- uart.vhd.2050.region / afac.vhd.2050.region
urban.art.share.2050.king <- uart.vhd.2050.king / afac.vhd.2050.king

freeway.share.2018.region <- frwy.vhd.2018.region / afac.vhd.2018.region
freeway.share.2018.king <- frwy.vhd.2018.king / afac.vhd.2018.king
freeway.share.2050.region <- frwy.vhd.2050.region / afac.vhd.2050.region
freeway.share.2050.king <- frwy.vhd.2050.king / afac.vhd.2050.king

# Congested Lane Miles ---------------------------------------------

# Regionwide
afac.lnml.2018.region <- base %>% filter(`tod` %in% c('17to18')) %>% select(`lane-miles`) %>% pull() %>% sum()
afac.cong.2018.region <- base %>% filter(`tod` %in% c('17to18')) %>% filter(`speed_ratio` <= 0.70) %>% select(`lane-miles`) %>% pull() %>% sum()
afac.cong.shar.2018.region <- afac.cong.2018.region / afac.lnml.2018.region

frwy.lnml.2018.region <- base %>% filter(`data3` %in% c(2,3)) %>% filter(`tod` %in% c('17to18')) %>% select(`lane-miles`) %>% pull() %>% sum()
frwy.cong.2018.region <- base %>% filter(`data3` %in% c(2,3)) %>% filter(`tod` %in% c('17to18')) %>% filter(`speed_ratio` <= 0.70) %>% select(`lane-miles`) %>% pull() %>% sum()
frwy.cong.shar.2018.region <- frwy.cong.2018.region / frwy.lnml.2018.region

uart.lnml.2018.region <- base %>% filter(`data3` %in% c(1,4)) %>% filter(`tod` %in% c('17to18')) %>% select(`lane-miles`) %>% pull() %>% sum()
uart.cong.2018.region <- base %>% filter(`data3` %in% c(1,4)) %>% filter(`tod` %in% c('17to18')) %>% filter(`speed_ratio` <= 0.70) %>% select(`lane-miles`) %>% pull() %>% sum()
uart.cong.shar.2018.region <- uart.cong.2018.region / uart.lnml.2018.region

rart.lnml.2018.region <- base %>% filter(`data3` %in% c(6)) %>% filter(`tod` %in% c('17to18')) %>% select(`lane-miles`) %>% pull() %>% sum()
rart.cong.2018.region <- base %>% filter(`data3` %in% c(6)) %>% filter(`tod` %in% c('17to18')) %>% filter(`speed_ratio` <= 0.70) %>% select(`lane-miles`) %>% pull() %>% sum()
rart.cong.shar.2018.region <- rart.cong.2018.region / rart.lnml.2018.region

afac.lnml.2050.region <- plan %>% filter(`tod` %in% c('17to18')) %>% select(`lane-miles`) %>% pull() %>% sum()
afac.cong.2050.region <- plan %>% filter(`tod` %in% c('17to18')) %>% filter(`speed_ratio` <= 0.70) %>% select(`lane-miles`) %>% pull() %>% sum()
afac.cong.shar.2050.region <- afac.cong.2050.region / afac.lnml.2050.region

frwy.lnml.2050.region <- plan %>% filter(`data3` %in% c(2,3)) %>% filter(`tod` %in% c('17to18')) %>% select(`lane-miles`) %>% pull() %>% sum()
frwy.cong.2050.region <- plan %>% filter(`data3` %in% c(2,3)) %>% filter(`tod` %in% c('17to18')) %>% filter(`speed_ratio` <= 0.70) %>% select(`lane-miles`) %>% pull() %>% sum()
frwy.cong.shar.2050.region <- frwy.cong.2050.region / frwy.lnml.2050.region

uart.lnml.2050.region <- plan %>% filter(`data3` %in% c(1,4)) %>% filter(`tod` %in% c('17to18')) %>% select(`lane-miles`) %>% pull() %>% sum()
uart.cong.2050.region <- plan %>% filter(`data3` %in% c(1,4)) %>% filter(`tod` %in% c('17to18')) %>% filter(`speed_ratio` <= 0.70) %>% select(`lane-miles`) %>% pull() %>% sum()
uart.cong.shar.2050.region <- uart.cong.2050.region / uart.lnml.2050.region

rart.lnml.2050.region <- plan %>% filter(`data3` %in% c(6)) %>% filter(`tod` %in% c('17to18')) %>% select(`lane-miles`) %>% pull() %>% sum()
rart.cong.2050.region <- plan %>% filter(`data3` %in% c(6)) %>% filter(`tod` %in% c('17to18')) %>% filter(`speed_ratio` <= 0.70) %>% select(`lane-miles`) %>% pull() %>% sum()
rart.cong.shar.2050.region <- rart.cong.2050.region / rart.lnml.2050.region

# King County
afac.lnml.2018.king <- base %>% filter(`@countyid` %in% c(33)) %>% filter(`tod` %in% c('17to18')) %>% select(`lane-miles`) %>% pull() %>% sum()
afac.cong.2018.king <- base %>% filter(`@countyid` %in% c(33)) %>% filter(`tod` %in% c('17to18')) %>% filter(`speed_ratio` <= 0.70) %>% select(`lane-miles`) %>% pull() %>% sum()
afac.cong.shar.2018.king <- afac.cong.2018.king / afac.lnml.2018.king

frwy.lnml.2018.king <- base %>% filter(`@countyid` %in% c(33)) %>% filter(`data3` %in% c(2,3)) %>% filter(`tod` %in% c('17to18')) %>% select(`lane-miles`) %>% pull() %>% sum()
frwy.cong.2018.king <- base %>% filter(`@countyid` %in% c(33)) %>% filter(`data3` %in% c(2,3)) %>% filter(`tod` %in% c('17to18')) %>% filter(`speed_ratio` <= 0.70) %>% select(`lane-miles`) %>% pull() %>% sum()
frwy.cong.shar.2018.king <- frwy.cong.2018.king / frwy.lnml.2018.king

uart.lnml.2018.king <- base %>% filter(`@countyid` %in% c(33)) %>% filter(`data3` %in% c(1,4)) %>% filter(`tod` %in% c('17to18')) %>% select(`lane-miles`) %>% pull() %>% sum()
uart.cong.2018.king <- base %>% filter(`@countyid` %in% c(33)) %>% filter(`data3` %in% c(1,4)) %>% filter(`tod` %in% c('17to18')) %>% filter(`speed_ratio` <= 0.70) %>% select(`lane-miles`) %>% pull() %>% sum()
uart.cong.shar.2018.king <- uart.cong.2018.king / uart.lnml.2018.king

rart.lnml.2018.king <- base %>% filter(`@countyid` %in% c(33)) %>% filter(`data3` %in% c(6)) %>% filter(`tod` %in% c('17to18')) %>% select(`lane-miles`) %>% pull() %>% sum()
rart.cong.2018.king <- base %>% filter(`@countyid` %in% c(33)) %>% filter(`data3` %in% c(6)) %>% filter(`tod` %in% c('17to18')) %>% filter(`speed_ratio` <= 0.70) %>% select(`lane-miles`) %>% pull() %>% sum()
rart.cong.shar.2018.king <- rart.cong.2018.king / rart.lnml.2018.king

afac.lnml.2050.king <- plan %>% filter(`@countyid` %in% c(33)) %>% filter(`tod` %in% c('17to18')) %>% select(`lane-miles`) %>% pull() %>% sum()
afac.cong.2050.king <- plan %>% filter(`@countyid` %in% c(33)) %>% filter(`tod` %in% c('17to18')) %>% filter(`speed_ratio` <= 0.70) %>% select(`lane-miles`) %>% pull() %>% sum()
afac.cong.shar.2050.king <- afac.cong.2050.king / afac.lnml.2050.king

frwy.lnml.2050.king <- plan %>% filter(`@countyid` %in% c(33)) %>% filter(`data3` %in% c(2,3)) %>% filter(`tod` %in% c('17to18')) %>% select(`lane-miles`) %>% pull() %>% sum()
frwy.cong.2050.king <- plan %>% filter(`@countyid` %in% c(33)) %>% filter(`data3` %in% c(2,3)) %>% filter(`tod` %in% c('17to18')) %>% filter(`speed_ratio` <= 0.70) %>% select(`lane-miles`) %>% pull() %>% sum()
frwy.cong.shar.2050.king <- frwy.cong.2050.king / frwy.lnml.2050.king

uart.lnml.2050.king <- plan %>% filter(`@countyid` %in% c(33)) %>% filter(`data3` %in% c(1,4)) %>% filter(`tod` %in% c('17to18')) %>% select(`lane-miles`) %>% pull() %>% sum()
uart.cong.2050.king <- plan %>% filter(`@countyid` %in% c(33)) %>% filter(`data3` %in% c(1,4)) %>% filter(`tod` %in% c('17to18')) %>% filter(`speed_ratio` <= 0.70) %>% select(`lane-miles`) %>% pull() %>% sum()
uart.cong.shar.2050.king <- uart.cong.2050.king / uart.lnml.2050.king

rart.lnml.2050.king <- plan %>% filter(`@countyid` %in% c(33)) %>% filter(`data3` %in% c(6)) %>% filter(`tod` %in% c('17to18')) %>% select(`lane-miles`) %>% pull() %>% sum()
rart.cong.2050.king <- plan %>% filter(`@countyid` %in% c(33)) %>% filter(`data3` %in% c(6)) %>% filter(`tod` %in% c('17to18')) %>% filter(`speed_ratio` <= 0.70) %>% select(`lane-miles`) %>% pull() %>% sum()
rart.cong.shar.2050.king <- rart.cong.2050.king / rart.lnml.2050.king




