rm(list = ls())

library(tidyverse)
library(sf)
library(parallel)

HMSday = dateList[1]

intersectHMSarchive <- function(HMSday){
  
  year <- substr(HMSday, 1,4)
  month <- substr(HMSday, 5,6)
  day <- substr(HMSday, 7,8)
  
  tarFileURL <- paste0("http://satepsanone.nesdis.noaa.gov/pub/volcano/FIRE/HMS_ARCHIVE/",year,"/KML/smoke",HMSday,".kml.gz")
  smokeFileName <- paste0("smoke",HMSday,".kml")
  
  rootgz <- basename(tarFileURL)
  
  temp <- tempfile()
  download.file(url = tarFileURL, destfile = temp)
  
   gzcon(url(tarFileURL),) 
 
  untar(tarfile = tarFileName)
  
st_layers(foo)
    
  layers <- st_layers(paste0("http://satepsanone.nesdis.noaa.gov/pub/FIRE/HMS/KML/ARCHIVE/smoke",HMSday,".kml"))$name
  
  for(l in 1: length(layers)){
    
    Smk <- st_read(dsn = paste0("http://satepsanone.nesdis.noaa.gov/pub/FIRE/HMS/KML/ARCHIVE/smoke",HMSday,".kml"), layer = layers[l]) %>%
      mutate(year = substr(HMSday, 1,4),
             month = substr(HMSday, 5,6),
             day = substr(HMSday, 7,8),
             date = paste0(year, month, day),
             smoke = layers[l])
    
    SmkLayerDayInt <- st_intersection(Smk,spatialFile) %>%
      select(date, smoke, geoIDer)
    
    st_geometry(SmkLayerDayInt) <- NULL
    
    if(exists("singleDay")){
      singleDay <- rbind(singleDay, SmkLayerDayInt)
    }
    else {
      singleDay <- SmkLayerDayInt
    }
    
  }
  
  singleDay  %>%
    saveRDS(paste0("~/data/CA_tracts/CA_WF_tracts_data_",HMSday,".RDS"))
  
  
  
}

############### end first function #########

dateList <- format(seq(as.Date("2010/01/01"),as.Date("2010/12/31"), by = "day"),"%Y%m%d")

spatialFile <-  st_read(dsn = "~/GitHub/WFSmokeExp/SmokeExposures/tractsSM.GeoJSON", stringsAsFactors = F) %>% 
  st_transform(crs = 4326) %>%   
  mutate(COUNTYFI_1 = as.character(paste0(STATE, COUNTY))) %>%
  rename(geoIDer = ct10)

mclapply(dateList, FUN = intersectHMS)



dateList <- format(seq(as.Date("2010/01/01"),as.Date("2010/12/31"), by = "day"),"%Y%m%d")

# smokeFileZip <- "http://satepsanone.nesdis.noaa.gov/pub/FIRE/HMS/GIS/ARCHIVE/hms_smoke20180808.zip"
CAtracts <-  st_read(dsn = "~/GitHub/WFSmokeExp/SmokeExposures/tractsSM.GeoJSON", stringsAsFactors = F) %>% st_transform(crs = 4326) %>%
  mutate(COUNTYFI_1 = as.character(paste0(STATE, COUNTY)))

HMSday <- dateList[1]

smokeDay <- function(HMSday){
  
      year <- substr(HMSday, 1,4)
      month <- substr(HMSday, 5,6)
      day <- substr(HMSday, 7,8)
        
      tarFileName <- paste0("http://satepsanone.nesdis.noaa.gov/pub/volcano/FIRE/HMS_ARCHIVE/",year,"/KML/smoke",HMSday,".kml.gz")
      smokeFileName <- paste0("smoke",HMSday,".kml")
      
      
      layers <- st_layers(untar(tarfile = tarFileName,files = smokeFileName  ))$name
      
      for(l in 1: length(layers)){
      
        Smk <- st_read(dsn = smokeFileName, layer = layers[l]) %>%
          mutate(year = year,
                 month = month,
                 day = day,
                 date = paste0(year, month, day),
                 smoke = layers[l])
        
        SmkLayerDayInt <- st_intersection(Smk, CAtracts) 
        st_geometry(SmkLayerDayInt) <- NULL
        # Smk <-Smk %>% select(date, year, month, day, smoke, ct10) %>%
          
      
        if(exists("singleDay")){
          singleDay <- rbind(singleDay, SmkLayerDayInt)
        }
        else{
          singleDay <- SmkLayerDayInt
        }
      
      }
      
    return(singleDay)
      
}
      
for (day in dateList){
  
 oneDay <- smokeDay(HMSday = day) 
 
 if(exists("AllDays")){
   AllDays <- rbind(AllDays, oneDay)
 }
 else{
   AllDays <- oneDay
 } 
 
}

saveRDS(AllDays,file = "~/GitHub/WFSmokeExp/Aug13ToAug142018.rds")
