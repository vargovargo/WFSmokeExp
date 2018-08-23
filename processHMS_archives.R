rm(list = ls())

library(tidyverse)
library(sf)

dateList <- format(seq(as.Date("2010/01/01"),as.Date("2010/12/31"), by = "day"),"%Y%m%d")

# smokeFileZip <- "http://satepsanone.nesdis.noaa.gov/pub/FIRE/HMS/GIS/ARCHIVE/hms_smoke20180808.zip"
CAtracts <-  st_read(dsn = "~/GitHub/WFSmokeExp/SmokeExposures/tractsSM.GeoJSON", stringsAsFactors = F) %>% st_transform(crs = 4326) %>%
  mutate(COUNTYFI_1 = as.character(paste0(STATE, COUNTY)))

# HMSday <- dateList[1]

smokeDay <- function(HMSday){
  
      year <- substr(HMSday, 1,4)
      month <- substr(HMSday, 5,6)
      day <- substr(HMSday, 7,8)
        
      smokeFileName <- paste0("http://satepsanone.nesdis.noaa.gov/pub/volcano/FIRE/HMS_ARCHIVE/",year,"/KML/smoke",HMSday,".kml.gz")
      
      readLines(gzfile(smokeFileName))
      
      layers <- st_layers(gzfile(smokeFileName, 'rb'))$name
      
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
