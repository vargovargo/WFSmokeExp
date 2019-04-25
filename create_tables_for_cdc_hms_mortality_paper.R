

################################################
# Jason Vargo
# script to process wildfire smoke data for CDC HMS mortality paper
# April 2019
################################################


rm(list = ls())
library(tidyverse)
library(data.table)

setwd("R:/WFSmokeProcessing/HMSdata/")

############# Population Data ################
# Read in the Census Block Group population Center data directly from the Census
# and crete new column that is the weight (population of the block group / population of the county)
pop <-
  fread(
    "https://www2.census.gov/geo/docs/reference/cenpop2010/blkgrp/CenPop2010_Mean_BG.txt"
  )[, wt := POPULATION / sum(POPULATION), by = .(STATEFP, COUNTYFP)] %>%
  .[, st_co := paste(STATEFP, COUNTYFP, sep = "_")] %>% # create a state_county combo identifier
  .[STATEFP < 60] # exclude US territories

############# Smoke Data ################
# Read in the smoke data and set key columns for easy subsetting
# clean up data field to be treated as a date
smoke <-
  fread("smokeFile.csv")[, date := as.Date(as.character(date), "%Y%m%d")]
setkey(smoke, date, STATEFP, COUNTYFP, TRACTCE, BLKGRPCE)

# create a field for the yeat to break up the file and process
smoke[, year := year(date)]

############# weighting function ################
# For a year, create a weighted smoke file
smoke_year <- function(year_var) {
  foo <-
    smoke[year == year_var, .(date,
                              STATEFP,
                              COUNTYFP,
                              TRACTCE,
                              BLKGRPCE,
                              light,
                              medium,
                              heavy)]  %>% # subset to a single year
    replace_na(list(
      # replace na's in smoke file
      light = 0,
      medium = 0,
      heavy = 0
    )) %>% merge(
      pop,
      by = c("STATEFP", "COUNTYFP", "TRACTCE", "BLKGRPCE"),
      all.x = T
    ) %>% # combine the annual smoke table with the population table with the weights
    .[, .(smoke_score = ifelse(heavy == 1, 3, # use a scaled score to describe the max smoke exposure on a day, by BG
                               ifelse(medium == 1, 2,
                                      ifelse(light == 1 , 1, 0)))), by = .(date, STATEFP, COUNTYFP, TRACTCE, BLKGRPCE, wt)] %>%
    .[, .(
      weighted_smoke = sum(smoke_score * wt),
      max_smoke = max(smoke_score),
      st_co = paste(STATEFP, COUNTYFP, sep = "_")
    ), by = .(date, STATEFP, COUNTYFP)] %>% # create the weighted smoke score for the county and bring in max smoke as well
    .[, .(date, weighted_smoke, max_smoke, st_co)] # limit to fields we want
  
  ############# add missing days ################
  # the original smoke file does not contain days or BGs where there is no smoke
  # create a list of all the dates in a year
  date_list <-
    format(seq(as.Date(paste0(
      year_var, "-01-01"
    )), as.Date(paste0(
      year_var, "-12-31"
    )), by = "day"), "%Y-%m-%d")
  # create a matrix of every day and BG combo
  date_geo_combo <-
    CJ(date = as.Date(date_list), st_co = unique(pop$st_co))
  
  # the function will return a merged table of the day-BG combo matrix
  # and the weighted smoke data
  return(merge(
    date_geo_combo,
    foo,
    by = c("date", "st_co"),
    all.x = T
  ) %>%
    replace_na(list(
      weighted_smoke = 0, max_smoke = 0 # and fill in missing values with zeros
    )))
}

# run for all years and combine the resulting tables into a single csv file
rbindlist(lapply(2010:2018, smoke_year)) %>% fwrite("weighted_smoke_all_years.csv") 