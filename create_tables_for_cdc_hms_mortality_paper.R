
################################################
# Jason Vargo
# script to process wildfire smoke data for CDC HMS mortality paper
# April 2019
################################################


rm(list = ls())

library(tidyverse)
library(data.table)

setwd("R:/WFSmokeProcessing/HMSdata/")

pop <-
  fread("https://www2.census.gov/geo/docs/reference/cenpop2010/blkgrp/CenPop2010_Mean_BG.txt")[, wt := POPULATION / sum(POPULATION), by = .(STATEFP, COUNTYFP)] %>%
  .[, st_co := paste(STATEFP, COUNTYFP, sep = "_")] %>%
  .[STATEFP < 60]

smoke <- fread("smokeFile.csv")[,date := as.Date(as.character(date),"%Y%m%d")] 
setkey(smoke, date, STATEFP, COUNTYFP, TRACTCE, BLKGRPCE) 

smoke[, year := year(date)]

smoke_year <-function(year_var){
  foo <- smoke[year == year_var, .(date, STATEFP, COUNTYFP, TRACTCE, BLKGRPCE, light, medium, heavy)]  %>%
    replace_na(list(
      light = 0,
      medium = 0,
      heavy = 0
    )) %>% merge(pop, by= c("STATEFP", "COUNTYFP", "TRACTCE", "BLKGRPCE"), all.x = T) %>%
    .[, .(smoke_score = ifelse(heavy == 1, 3,
                               ifelse(medium == 1, 2,
                                      ifelse(light == 1 , 1, 0)))), by = .(date, STATEFP, COUNTYFP, TRACTCE, BLKGRPCE, wt)] %>%
    .[, .(weighted_smoke = sum(smoke_score * wt), max_smoke = max(smoke_score), st_co = paste(STATEFP, COUNTYFP, sep="_")), by = .(date, STATEFP, COUNTYFP)] %>%
    .[,.(date, weighted_smoke, max_smoke, st_co)]
  
  date_list <- format(seq(as.Date(paste0(year_var,"-01-01")),as.Date(paste0(year_var,"-12-31")), by = "day"),"%Y-%m-%d")
  
  date_geo_combo <- CJ(date = as.Date(date_list), st_co = unique(pop$st_co))
  
  return(merge(date_geo_combo,
               foo,
               by = c("date", "st_co"),
               all.x = T) %>%
           replace_na(list(weighted_smoke = 0, max_smoke = 0)) 
  )
}

rbindlist(lapply(2010:2018, smoke_year)) %>% fwrite("weighted_smoke_all_years.csv")