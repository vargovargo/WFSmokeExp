rm(list = ls())

library(tidyverse)
library(sf)
library(parallel)
library(R.utils)
library(lwgeom)
library(RCurl)


unlink(paste0(tempdir(),'/*'))
# HMSday <-"20180303"

HMSgunzip <- function(gzFiles){
  
  directory <- tempdir()
    setwd(directory)
  
  DBFpath <- unlist(gzFiles[1])
  SHPpath <- unlist(gzFiles[2])
  SHXpath <- unlist(gzFiles[3])
  
  DBFgzName <- basename(DBFpath)
  SHPgzName <- basename(SHPpath)
  SHXgzName <- basename(SHXpath)
  
  tempDBF <- tempfile(pattern = "", fileext = ".dbf.gz")
  tempSHP <- tempfile(pattern = "", fileext = ".shp.gz")
  tempSHX <- tempfile(pattern = "", fileext = ".shx.gz")
  
  download.file(url = DBFpath, destfile = tempDBF)
  download.file(url = SHPpath, destfile = tempSHP)
  download.file(url = SHXpath, destfile = tempSHX)
  
  gunzip(tempDBF, destname = "gisfile.dbf" )
  gunzip(tempSHP, destname = "gisfile.shp")
  gunzip(tempSHX, destname = "gisfile.shx")
  
}



intersectHMSarchive <- function(HMSday){
  
  year <- substr(HMSday, 1,4)
  month <- substr(HMSday, 5,6)
  day <- substr(HMSday, 7,8)
  
  if(url.exists(paste0("http://satepsanone.nesdis.noaa.gov/pub/volcano/FIRE/HMS_ARCHIVE/",year,"/GIS/SMOKE/hms_smoke",HMSday,".shp.gz"))){
    
    directory <- tempdir()
    
    setwd(directory)
  
    gzFiles <- list(paste0("http://satepsanone.nesdis.noaa.gov/pub/volcano/FIRE/HMS_ARCHIVE/",year,"/GIS/SMOKE/hms_smoke",HMSday,".dbf.gz"),
                    paste0("http://satepsanone.nesdis.noaa.gov/pub/volcano/FIRE/HMS_ARCHIVE/",year,"/GIS/SMOKE/hms_smoke",HMSday,".shp.gz"),
                    paste0("http://satepsanone.nesdis.noaa.gov/pub/volcano/FIRE/HMS_ARCHIVE/",year,"/GIS/SMOKE/hms_smoke",HMSday,".shx.gz")
    )
    
    GISfiles <- HMSgunzip(gzFiles)
    
    #path <- "~/tempZip/hms_smoke20180303.shp"
    
    Smk <- st_read(dsn = "gisfile.shp") %>%  st_set_crs(4326) %>%
      mutate(year = substr(HMSday, 1,4),
             month = substr(HMSday, 5,6),
             day = substr(HMSday, 7,8),
             date = paste0(year, month, day),
             smoke = factor(ifelse(Density %in% c(5, "5.000"), "light", 
                                          ifelse(Density %in% c(27, "27.000") ,"heavy", 
                                                 ifelse(Density %in% c(16, "16.000"), "medium", "missing"))), levels=c("light","medium","heavy","missing"))
             )
    
    
    SmkTractDayInt <- st_intersection(st_make_valid(Smk), popCentroidsBG) %>%
      select(date, smoke, STATEFP, COUNTYFP, TRACTCE, BLKGRPCE, POPULATION)
    
    st_geometry(SmkTractDayInt) <- NULL
    
    SmkTractDayInt  %>%
      mutate(yn = 1) %>%
      group_by(date, smoke, STATEFP, COUNTYFP, TRACTCE, BLKGRPCE, POPULATION) %>%
      summarise(yn = mean(yn)) %>%
      spread(key = smoke,value = yn) %>%
      fwrite(paste0("~/data/US_BG/CSV/US_BG",HMSday,".csv"))
    
    # clear temp folder
    
  unlink(paste0(tempdir(),'/*'))
  
  
  }
  else{
    return()
  }

}

############### function to update with days not in the archive ############

unlink(paste0(tempdir(),'/*'))

# list.files(tempdir())
# 
  HMSday <- dateListrecent[1]



intersectHMSrecent <- function(HMSday){
  
  year <- substr(HMSday, 1,4)
  month <- substr(HMSday, 5,6)
  day <- substr(HMSday, 7,8)
  
  # if(url.exists(paste0("http://satepsanone.nesdis.noaa.gov/pub/FIRE/HMS/GIS/ARCHIVE/hms_smoke",HMSday,".dbf"))){
    
    directory <- tempdir()
    
    setwd(directory)

    shpFiles <- list(paste0("http://satepsanone.nesdis.noaa.gov/pub/FIRE/HMS/GIS/ARCHIVE/hms_smoke",HMSday,".dbf"),
                    paste0("http://satepsanone.nesdis.noaa.gov/pub/FIRE/HMS/GIS/ARCHIVE/hms_smoke",HMSday,".shp"),
                    paste0("http://satepsanone.nesdis.noaa.gov/pub/FIRE/HMS/GIS/ARCHIVE/hms_smoke",HMSday,".shx")
    )

    shpNames <- list("HMSGIS.dbf","HMSGIS.shp","HMSGIS.shx")
    
    GISfiles <- mapply(download.file, shpFiles, shpNames, MoreArgs = list(method = "auto", mode = "wb"))
    
    Smk <- st_read(dsn = "HMSGIS.shp") %>%  st_set_crs(4326) %>%
      mutate(year = substr(HMSday, 1,4),
             month = substr(HMSday, 5,6),
             day = substr(HMSday, 7,8),
             date = paste0(year, month, day),
             smoke = factor(ifelse(Density %in% c(5, "5.000"), "light", 
                                   ifelse(Density %in% c(27, "27.000") ,"heavy", 
                                          ifelse(Density %in% c(16, "16.000"), "medium", "missing"))), levels=c("light","medium","heavy","missing"))
      )
    
    
    SmkTractDayInt <- st_intersection(st_make_valid(Smk), popCentroidsBG) %>%
      select(date, smoke, STATEFP, COUNTYFP, TRACTCE, BLKGRPCE, POPULATION)
    
    st_geometry(SmkTractDayInt) <- NULL
    
    SmkTractDayInt  %>%
      mutate(yn = 1) %>%
      group_by(date, smoke, STATEFP, COUNTYFP, TRACTCE, BLKGRPCE, POPULATION) %>%
      summarise(yn = mean(yn)) %>%
      spread(key = smoke,value = yn) %>%
      fwrite(paste0("~/data/US_BG/CSV/US_BG",HMSday,".csv"))
    
    # clear temp folder
    
    unlink(paste0(tempdir(),'/*'))
    
    
  # }
  # else{
  #   return()
  # }
  
}


intersectHMSdirect <- function(HMSday){
  
  year <- substr(HMSday, 1,4)
  month <- substr(HMSday, 5,6)
  day <- substr(HMSday, 7,8)
  
    
    Smk <- st_read(dsn = paste0("~/temp/smokepolygons.",HMSday,".shp")) %>%  st_set_crs(4326) %>%
      mutate(year = substr(HMSday, 1,4),
             month = substr(HMSday, 5,6),
             day = substr(HMSday, 7,8),
             date = paste0(year, month, day),
             smoke = factor(ifelse(Density %in% c(5, "5.000"), "light", 
                                   ifelse(Density %in% c(27, "27.000") ,"heavy", 
                                          ifelse(Density %in% c(16, "16.000"), "medium", "missing"))), levels=c("light","medium","heavy","missing"))
      )
    
    
    SmkTractDayInt <- st_intersection(st_make_valid(Smk), popCentroidsBG) %>%
      select(date, smoke, STATEFP, COUNTYFP, TRACTCE, BLKGRPCE, POPULATION)

    st_geometry(SmkTractDayInt) <- NULL

    SmkTractDayInt  %>%
      mutate(yn = 1) %>%
      group_by(date, smoke, STATEFP, COUNTYFP, TRACTCE, BLKGRPCE, POPULATION) %>%
      summarise(yn = mean(yn)) %>%
      spread(key = smoke,value = yn) %>%
      fwrite(paste0("~/data/US_BG/CSV/US_BG",HMSday,".csv"))
    
    
    # clear temp folder
    
    unlink(paste0(tempdir(),'/*'))
    
    
  
}


############### run multiple intersects  for archived data #########
###############   download here https://www.census.gov/geo/maps-data/data/tiger-kml.html ##########

library(data.table)

stateKey <- fread("~/GitHub/WFSmokeExp/stateKey.csv") %>%
  mutate(charST = ifelse(stFIPS<10, paste0("0",stFIPS), as.character(stFIPS)))
# 
# BG_list <- lapply(stateKey$charST, function(x) paste0("https://www2.census.gov/geo/docs/reference/cenpop2010/blkgrp/CenPop2010_Mean_BG",x,".txt"))
# 
# tableList <- lapply(BG_list, fread)

tableList <- fread("https://www2.census.gov/geo/docs/reference/cenpop2010/blkgrp/CenPop2010_Mean_BG.txt")

popCentroidsBG <- st_as_sf(tableList, coords = c("LONGITUDE", "LATITUDE"), crs = 4326) 
 

# 
# CAtractFile <-  st_read(dsn = "~/GitHub/WFSmokeExp/SmokeExposures/tractsSM.GeoJSON", stringsAsFactors = F) %>%
#   st_transform(crs = 4326) %>%
#   mutate(COUNTYFI_1 = as.character(paste0(STATE, COUNTY))) %>%
#   rename(geoIDer = ct10)
# 
# WAtractFile <-  st_read(dsn = "~/data/cb_2017_53_tract_500k.kml", stringsAsFactors = F) %>% st_transform(crs = 4326) %>%
#   mutate(tractID = sapply(str_split(Description, pattern = "[<>]+"), "[[", 62))  %>%
#   rename(geoIDer = tractID)
# 
# ORtractFile <-  st_read(dsn = "~/data/cb_2017_41_tract_500k.kml", stringsAsFactors = F) %>% st_transform(crs = 4326) %>%
#   mutate(tractID = sapply(str_split(Description, pattern = "[<>]+"), "[[", 62))  %>%
#   rename(geoIDer = tractID)
# 
# UScountyFile <-  st_read(dsn = "~/GitHub/WFSmokeExp_US/cb_2017_us_county_20m.kml", stringsAsFactors = F) %>% st_transform(crs = 4326) %>%
#   mutate(County = sapply(str_split(Name, pattern = "[<>]+"), "[[",4),
#          CountyFIPS = sapply(str_split(Description, pattern = "[<>]+"), "[[", 62))  %>%
#   rename(geoIDer = CountyFIPS)


# dateListarchive <- format(seq(as.Date("2010/05/30"),as.Date("2010/05/30"), by = "day"),"%Y%m%d")
# mclapply(dateListarchive, FUN = intersectHMSarchive)

# check this website to see the most recent date available http://satepsanone.nesdis.noaa.gov/pub/FIRE/HMS/GIS/ARCHIVE/

dateListrecent <- format(seq(as.Date("2018/09/30"),as.Date("2018/10/19"), by = "day"),"%Y%m%d")
mclapply(dateListrecent, FUN = intersectHMSrecent)

