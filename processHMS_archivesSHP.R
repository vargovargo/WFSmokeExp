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
    
     SmkTractDayInt <- st_intersection(st_make_valid(Smk),CAtractFile) %>%
      select(date, smoke, geoIDer)
    
    st_geometry(SmkTractDayInt) <- NULL

    SmkTractDayInt  %>%
      mutate(yn = 1) %>%
      group_by(date, smoke, geoIDer) %>%
      summarise(yn = mean(yn)) %>%
      spread(key = smoke,value = yn) %>%
      saveRDS(paste0("~/data/CA_tracts/CA_WF_tracts_data_",HMSday,".RDS"))
      
      
    SmkCountyDayInt <- st_intersection(st_make_valid(Smk),UScountyFile) %>%
      select(date, smoke, geoIDer)
    
    st_geometry(SmkCountyDayInt) <- NULL
    
    SmkCountyDayInt  %>%
      mutate(yn = 1) %>%
      group_by(date, smoke, geoIDer) %>%
      summarise(yn = mean(yn)) %>%
      spread(key = smoke,value = yn) %>%
      saveRDS(paste0("~/data/US_counties/US_WF_counties_data_",HMSday,".RDS"))
    
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
# HMSday <- dateListrecent[1]

intersectHMSrecent <- function(HMSday){
  
  year <- substr(HMSday, 1,4)
  month <- substr(HMSday, 5,6)
  day <- substr(HMSday, 7,8)
  
  if(url.exists(paste0("http://satepsanone.nesdis.noaa.gov/pub/FIRE/HMS/GIS/ARCHIVE/hms_smoke",HMSday,".shp"))){
    
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
    
    SmkTractDayInt <- st_intersection(st_make_valid(Smk),CAtractFile) %>%
      select(date, smoke, geoIDer)
    
    st_geometry(SmkTractDayInt) <- NULL
    
    SmkTractDayInt  %>%
      mutate(yn = 1) %>%
      group_by(date, smoke, geoIDer) %>%
      summarise(yn = mean(yn)) %>%
      spread(key = smoke,value = yn) %>%
      saveRDS(paste0("~/data/CA_tracts/CA_WF_tracts_data_",HMSday,".RDS"))
    
    
    SmkCountyDayInt <- st_intersection(st_make_valid(Smk),UScountyFile) %>%
      select(date, smoke, geoIDer)
    
    st_geometry(SmkCountyDayInt) <- NULL
    
    SmkCountyDayInt  %>%
      mutate(yn = 1) %>%
      group_by(date, smoke, geoIDer) %>%
      summarise(yn = mean(yn)) %>%
      spread(key = smoke,value = yn) %>%
      saveRDS(paste0("~/data/US_counties/US_WF_counties_data_",HMSday,".RDS"))
    
    # clear temp folder
    
    unlink(paste0(tempdir(),'/*'))
    
    
  }
  else{
    return()
  }
  
}


############### run multiple intersects  for archived data #########

CAtractFile <-  st_read(dsn = "~/GitHub/WFSmokeExp/SmokeExposures/tractsSM.GeoJSON", stringsAsFactors = F) %>%
  st_transform(crs = 4326) %>%
  mutate(COUNTYFI_1 = as.character(paste0(STATE, COUNTY))) %>%
  rename(geoIDer = ct10)

UScountyFile <-  st_read(dsn = "~/GitHub/WFSmokeExp_US/cb_2017_us_county_20m.kml", stringsAsFactors = F) %>% st_transform(crs = 4326) %>%
  mutate(County = sapply(str_split(Name, pattern = "[<>]+"), "[[",4),
         CountyFIPS = sapply(str_split(Description, pattern = "[<>]+"), "[[", 62))  %>%
  rename(geoIDer = CountyFIPS)

# UStractFile <- st_read(dsn = "~/GitHub/WFSmokeExp_US/cb_2017_us_county_20m.kml", stringsAsFactors = F) %>% st_transform(crs = 4326) %>%
#   mutate(County = sapply(str_split(Name, pattern = "[<>]+"), "[[",4),
#          CountyFIPS = sapply(str_split(Description, pattern = "[<>]+"), "[[", 62))  %>%
#   rename(geoIDer = CountyFIPS)

# dateListarchive <- format(seq(as.Date("2015/03/17"),as.Date("2016/02/12"), by = "day"),"%Y%m%d")
# mclapply(dateListarchive, FUN = intersectHMSarchive)

# dateListrecent <- format(seq(as.Date("2018/06/29"),as.Date("2018/09/05"), by = "day"),"%Y%m%d")
# mclapply(dateListrecent, FUN = intersectHMSrecent)


########### function to combine ############


# when combining several 

library(data.table)
library(ggplot2)
library(scales)

dateList <- format(seq(as.Date("2010/06/01"),as.Date("2018/09/05"), by = "day"),"%Y%m%d")

area <- "County" # enter 'County' or 'CA' 

if(area == "County"){
  directory <- "~/data/US_counties/"
  spatialFile <- UScountyFile
} else if(area == "CA"){
  directory <- "~/data/CA_tracts/"
  spatialFile <- CAtractFile
} else{
  return()
}

baseGrid <- data.table(expand.grid(date = dateList, geoIDer = unique(spatialFile$geoIDer)), key = c("date","geoIDer"))
smokeFile <- rbindlist(mclapply(list.files(directory, full.names = T, pattern = ".RDS"), readRDS), fill = T) %>% setkey(date, geoIDer)

foo <- merge(baseGrid, smokeFile, all.x=TRUE) %>%
  replace_na(list(light = 0, medium=0, heavy=0)) 

foo[, "maxSmoke" := factor(ifelse(heavy == 1, "heavy",
                               ifelse(medium == 1, "medium", 
                                      ifelse(light ==1, "light", "none"))), levels=c("none","light","medium","heavy"))]


foo[, c("lightSmokeAreas","mediumSmokeAreas","heavySmokeAreas") := lapply(.SD, sum), .SDcols = c("light","medium","heavy"), by = "date"]

plotData <- foo[, lapply(.SD, sum), .SDcols = c("light","medium","heavy"), by = "date"] %>%
  gather(light, medium, heavy,key = smoke,value = value) 


plotData%>%
  ggplot() +  geom_line(aes(x = as.Date(date, "%Y%m%d"), , y= value, group =smoke, color = smoke)) +
  scale_color_manual(values = c("red","orange","yellow")) +
  scale_x_date(labels = date_format("%m-%Y"), , breaks='1 year') +
  theme_minimal()
         


foo




