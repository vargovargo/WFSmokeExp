rm(list = ls())

library(tidyverse)
library(sf)
library(parallel)
library(R.utils)
library(lwgeom)
library(RCurl)

# HMSday <-"20100911"

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
    
    Smk <- st_read(dsn = "gisfile.shp") %>%  st_set_crs(4326) %>%
      mutate(year = substr(HMSday, 1,4),
             month = substr(HMSday, 5,6),
             day = substr(HMSday, 7,8),
             date = paste0(year, month, day),
             smoke = factor(ifelse(Density %in% c(5, "5.000"), "light", 
                                          ifelse(Density %in% c(27, "27.000") ,"heavy", "medium")), levels=c("light","medium","heavy"))
             )
    
     SmkLayerDayInt <- st_intersection(st_make_valid(Smk),spatialFile) %>%
      select(date, smoke, geoIDer)
    
    st_geometry(SmkLayerDayInt) <- NULL
    
 
  
  
  # SmkLayerDayInt  %>%
  #   saveRDS(paste0("~/data/CA_tracts/CA_WF_tracts_data_",HMSday,".RDS"))

    SmkLayerDayInt  %>%
      mutate(yn = 1) %>%
      group_by(date, smoke, geoIDer) %>%
      summarise(yn = mean(yn)) %>%
      spread(key = smoke,value = yn) %>%
      replace_na(list(light = 0, medium=0, heavy=0)) %>%
      mutate(maxSmoke = factor(ifelse(heavy == 1, "heavy",
                                      ifelse(medium == 1, "medium", "light")), levels=c("light","medium","heavy")))
    saveRDS(paste0("~/data/US_counties/US_WF_counties_data_",HMSday,".RDS"))
  
  unlink(paste0(tempdir(),'/*'))
  }
  else{
    return()
  }

}


# intersectHMS <- function(HMSday){
#   
#   KMLurl <- paste0("http://satepsanone.nesdis.noaa.gov/pub/FIRE/HMS/KML/ARCHIVE/smoke",HMSday,".kml")
#   
#   if(url.exists(KMLurl)){
#   
#   layers <- st_layers()$name
#   
#   for(l in 1: length(layers)){
#     
#     Smk <- st_read(dsn = paste0("http://satepsanone.nesdis.noaa.gov/pub/FIRE/HMS/KML/ARCHIVE/smoke",HMSday,".kml"), layer = layers[l]) %>%
#       mutate(year = substr(HMSday, 1,4),
#              month = substr(HMSday, 5,6),
#              day = substr(HMSday, 7,8),
#              date = paste0(year, month, day),
#              smoke = layers[l],
#              Desc = Description)
#     
#     SmkLayerDayInt <- st_intersection(st_make_valid(Smk),spatialFile) %>%
#       select(date, smoke, Desc, geoIDer)
#     
#     st_geometry(SmkLayerDayInt) <- NULL
#     
#     if(exists("singleDay")){
#       singleDay <- rbind(singleDay, SmkLayerDayInt)
#     }
#     else {
#       singleDay <- SmkLayerDayInt
#     }
#     
#   }
#   
#   # singleDay  %>%
#   #   saveRDS(paste0("~/data/CA_tracts/CA_WF_tracts_data_",HMSday,".RDS"))
#   
#   singleDay  %>%
#     saveRDS(paste0("~/data/US_counties/US_WF_counties_data_",HMSday,".RDS"))
#   
#   }
#   else{
#     return()
#   }
#   
# }


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


foo %>% 
  mutate(yn = 1) %>%
  group_by(date, smoke, geoIDer) %>%
  summarise(yn = mean(yn)) %>%
  spread(key = smoke,value = yn) %>%
  replace_na(list(light = 0, medium=0, heavy=0)) %>%
  mutate(maxSmoke = factor(ifelse(heavy == 1, "heavy",
                                  ifelse(medium == 1, "medium", "light")), levels=c("light","medium","heavy")))







