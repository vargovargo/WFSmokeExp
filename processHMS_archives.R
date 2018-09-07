rm(list = ls())

library(tidyverse)
library(sf)
library(parallel)
library(R.utils)
library(lwgeom)
library(RCurl)

HMSday <-"20100911"

intersectHMSarchive <- function(HMSday){
  
  year <- substr(HMSday, 1,4)
  month <- substr(HMSday, 5,6)
  day <- substr(HMSday, 7,8)
  
  gzFileURL <- paste0("http://satepsanone.nesdis.noaa.gov/pub/volcano/FIRE/HMS_ARCHIVE/",year,"/KML/smoke",HMSday,".kml.gz")
  if(url.exists(gzFileURL)){
  
  smokeZipName <- paste0("smoke",HMSday,".kml.gz")
  smokeFileName <- paste0("smoke",HMSday,".kml")
  
  directory <- tempdir()
  
  setwd(directory)
  
  temp <- tempfile(pattern = "", fileext = ".kml.gz")
  
  download.file(url = gzFileURL, destfile = temp)
  
  gunzip(temp)
  
  kmlFile <- list.files(tempdir(), pattern = ".kml")
  
  layers <- st_layers(kmlFile)$name
  
  for(l in 1: length(layers)){
    
    Smk <- st_read(dsn = kmlFile, layer = layers[l]) %>%
      mutate(year = substr(HMSday, 1,4),
             month = substr(HMSday, 5,6),
             day = substr(HMSday, 7,8),
             date = paste0(year, month, day),
             smoke = layers[l],
             Desc = Description
             )
    
     SmkLayerDayInt <- st_intersection(st_make_valid(Smk),spatialFile) %>%
      select(date, smoke, Desc, geoIDer)
    
    st_geometry(SmkLayerDayInt) <- NULL
    
    if(exists("singleDay")){
      singleDay <- rbind(singleDay, SmkLayerDayInt)
    }
    else {
      singleDay <- SmkLayerDayInt
    }
    
  }
  
  # singleDay  %>%
  #   saveRDS(paste0("~/data/CA_tracts/CA_WF_tracts_data_",HMSday,".RDS"))

  singleDay  %>%
    saveRDS(paste0("~/data/US_counties/US_WF_counties_data_",HMSday,".RDS"))
  
  unlink(paste0(tempdir(),'/*'))
  }
  else{
    return()
  }

}


intersectHMS <- function(HMSday){
  
  KMLurl <- paste0("http://satepsanone.nesdis.noaa.gov/pub/FIRE/HMS/KML/ARCHIVE/smoke",HMSday,".kml")
  
  if(url.exists(KMLurl)){
  
  layers <- st_layers()$name
  
  for(l in 1: length(layers)){
    
    Smk <- st_read(dsn = paste0("http://satepsanone.nesdis.noaa.gov/pub/FIRE/HMS/KML/ARCHIVE/smoke",HMSday,".kml"), layer = layers[l]) %>%
      mutate(year = substr(HMSday, 1,4),
             month = substr(HMSday, 5,6),
             day = substr(HMSday, 7,8),
             date = paste0(year, month, day),
             smoke = layers[l],
             Desc = Description)
    
    SmkLayerDayInt <- st_intersection(st_make_valid(Smk),spatialFile) %>%
      select(date, smoke, Desc, geoIDer)
    
    st_geometry(SmkLayerDayInt) <- NULL
    
    if(exists("singleDay")){
      singleDay <- rbind(singleDay, SmkLayerDayInt)
    }
    else {
      singleDay <- SmkLayerDayInt
    }
    
  }
  
  # singleDay  %>%
  #   saveRDS(paste0("~/data/CA_tracts/CA_WF_tracts_data_",HMSday,".RDS"))
  
  singleDay  %>%
    saveRDS(paste0("~/data/US_counties/US_WF_counties_data_",HMSday,".RDS"))
  
  }
  else{
    return()
  }
  
}


############### end first function #########

############### run multiple intersects #########

dateList <- format(seq(as.Date("2010/09/10"),as.Date("2010/09/12"), by = "day"),"%Y%m%d")

# spatialFile <-  st_read(dsn = "~/GitHub/WFSmokeExp/SmokeExposures/tractsSM.GeoJSON", stringsAsFactors = F) %>%
#   st_transform(crs = 4326) %>%
#   mutate(COUNTYFI_1 = as.character(paste0(STATE, COUNTY))) %>%
#   rename(geoIDer = ct10)

spatialFile <-  st_read(dsn = "~/GitHub/WFSmokeExp_US/cb_2017_us_county_20m.kml", stringsAsFactors = F) %>% st_transform(crs = 4326) %>%
  mutate(County = sapply(str_split(Name, pattern = "[<>]+"), "[[",4),
         CountyFIPS = sapply(str_split(Description, pattern = "[<>]+"), "[[", 62))  %>%
  rename(geoIDer = CountyFIPS)

mclapply(dateList, FUN = intersectHMSarchive)


########### function to combine ############


oneyearFiles <- list.files("~/data/US_counties/", full.names = T)

file <- oneyearFiles[253]
foo <- readRDS("~/data/US_counties/US_WF_counties_data_20100911.RDS")

head(foo)
unique(foo$smoke)

head({foo %>%
    mutate(test = str_extract_all(smoke, pattern = ""))
})
  
regexp <- "[[:digit:]]+"
  
  mutate(smoke = factor(ifelse(smoke %in% c("Smoke (Light)", "Layer #0"), "light", 
                               ifelse(smoke %in% c("Smoke (Heavy)", "Layer #0"),"heavy", "medium")), levels=c("light","medium","heavy")))

sapply(str_split(foo$smoke, pattern = "<br />"), "[", 5)
str_extract(foo$smoke, pattern %in% c("5","16","27"))






