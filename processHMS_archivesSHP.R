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

# dateListarchive <- format(seq(as.Date("2015/06/22"),as.Date("2016/02/12"), by = "day"),"%Y%m%d")
# mclapply(dateListarchive, FUN = intersectHMSarchive)


# check this website to see the most recent date available http://satepsanone.nesdis.noaa.gov/pub/FIRE/HMS/GIS/ARCHIVE/

dateListrecent <- format(seq(as.Date("2018/09/06"),as.Date("2018/09/07"), by = "day"),"%Y%m%d")
mclapply(dateListrecent, FUN = intersectHMSrecent)

########### function to combine ############

# when combining several 

library(data.table)
library(ggplot2)
library(scales)
library(lubridate)

dateList <- format(seq(as.Date("2010/06/01"),as.Date("2018/09/07"), by = "day"),"%Y%m%d")

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

master <- merge(baseGrid, smokeFile, all.x=TRUE) %>%
  replace_na(list(light = 0, medium=0, heavy=0)) 

master[, "maxSmoke" := factor(ifelse(heavy == 1, "heavy",
                               ifelse(medium == 1, "medium", 
                                      ifelse(light ==1, "light", "none"))), levels=c("none","light","medium","heavy"))]


master[, c("lightSmokeAreas","mediumSmokeAreas","heavySmokeAreas") := lapply(.SD, sum), .SDcols = c("light","medium","heavy"), by = "date"]

stateKey <- fread("~/GitHub/WFSmokeExp/stateKey.csv") 

# number of counties experiencing different smoke levels 

plotData <- master[, lapply(.SD, sum), .SDcols = c("light","medium","heavy"), by = "date"] %>%
  gather(heavy, medium, light, key = smoke,value = value) 


plotData %>%
  ggplot() +  geom_line(aes(x = as.Date(date, "%Y%m%d"), y= value, group =smoke, color = smoke)) +
  scale_color_manual(values = c("red","orange","yellow")) +
  scale_x_date(labels = date_format("%Y-%m-%d"), breaks='1 year') +
  theme_minimal() + 
  xlab("Date") + 
  ylab("number of Counties in US experiencing smoke")
       


plotData3 <-  master[, c("Year","stFIPS") := list(year(as.Date(date, "%Y%m%d")),
                                                 as.integer(str_sub(geoIDer, 0,2)))] %>%
  .[, lapply(.SD, sum), .SDcols = c("light","medium","heavy"), by = c("stFIPS","date")] %>%
  gather(heavy, medium, light, key = smoke,value = value) 

plotData3 %>% left_join(stateKey) %>%
  filter(ST %in% c("CA","WA","OR","MT")) %>%
  mutate(smoke= factor(smoke, levels = c("light","medium","heavy"))) %>%
  ggplot() +  geom_area(aes(x = as.Date(date, "%Y%m%d"), y= value, group =smoke, fill = smoke), position = "dodge") +
  scale_fill_manual(values = c("yellow","orange","red")) +
  scale_x_date(labels = date_format("%Y-%m-%d"), breaks='1 year') +
  theme_minimal() + 
  xlab("Date") + 
  ylab("number of Counties experiencing smoke") +
  facet_grid(ST ~ ., scales = "free_y")


# number of days of heavy smoke by county, by year

plotData2 <-   master[, c("Year","stFIPS") := list(year(as.Date(date, "%Y%m%d")),
                                                  as.integer(str_sub(geoIDer, 0,2)))] %>% 
  .[, lapply(.SD, sum), .SDcols = c("heavy"), by = c("stFIPS", "Year")] 
  


plotData2 %>% left_join(stateKey) %>%
  mutate(Year = factor(Year, levels = c(2018,2017,2016,2015,2014,2013,2012,2011,2010))) %>%
  ggplot() + geom_bar(aes(x=stateName, y= heavy, fill=factor(Year)),stat="identity") +coord_flip()
  

plotData2 %>% left_join(stateKey) %>%
  select(Year, heavy, ST) %>% 
  spread(key = Year, value = heavy)  %>%
  mutate(total = .[[2]] + .[[3]] + .[[4]] + .[[5]] + .[[6]] + .[[7]] + .[[8]] + .[[9]] + .[[10]]) %>% 
  ggplot() + geom_bar(aes(x=reorder(ST, total), y=total, fill=total), stat="identity") + 
  scale_fill_gradient(low = "blue", high = "red")


plotData2 %>% left_join(stateKey) %>%
  select(Year, heavy, ST) %>% 
  spread(key = Year, value = heavy)  %>%
  mutate(years10to12 = .[[2]] + .[[3]] + .[[4]],
         years13to15 = .[[5]] + .[[6]] + .[[7]],
         years16to18 = .[[8]] + .[[9]] + .[[10]],
         pctChange = (years16to18 - years10to12)/years10to12) %>% na.omit() %>%
  ggplot() + geom_bar(aes(x=reorder(ST, pctChange), y=pctChange, fill=pctChange), stat="identity") + 
  scale_fill_gradient(low = "blue", high = "red")
 
mapData <-   master[, "Year" := year(as.Date(date, "%Y%m%d"))] %>% 
  .[, lapply(.SD, sum), .SDcols = c("heavy"), by = c("geoIDer", "Year")] %>% 
  spread(key = Year, value = heavy) %>%
  mutate(stFIPS =  as.integer(str_sub(geoIDer, 0,2)))  %>%  
  filter(stFIPS < 57 & stFIPS != 15 )
  
  


 fart <- UScountyFile %>% left_join(mapData) %>%  
   filter(stFIPS < 57 & stFIPS != 15 ) %>% st_crop(c(xmin=-145, xmax=90, ymin=20, ymax=50))

 breaks <- c(0,10,20,30,40, 50,60)
 
 plot(fart["2010"], breaks = breaks)
 plot(fart["2011"], breaks = breaks)
 plot(fart["2012"], breaks = breaks)
 plot(fart["2013"], breaks = breaks)
 plot(fart["2014"], breaks = breaks)
 plot(fart["2015"], breaks = breaks)
 plot(fart["2016"], breaks = breaks)
 plot(fart["2017"], breaks = breaks)
 plot(fart["2018"], breaks = breaks)
 
