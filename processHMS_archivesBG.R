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
 # HMSday <- dateListrecent[1]



intersectHMSrecent <- function(HMSday){
  
  year <- substr(HMSday, 1,4)
  month <- substr(HMSday, 5,6)
  day <- substr(HMSday, 7,8)
  
  if(url.exists(paste0("http://satepsanone.nesdis.noaa.gov/pub/FIRE/HMS/GIS/ARCHIVE/hms_smoke",HMSday,".dbf"))){
    
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
    
    
  }
  else{
    return()
  }
  
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

# stateKey <- fread("~/GitHub/WFSmokeExp/stateKey.csv") %>% 
#   mutate(charST = ifelse(stFIPS<10, paste0("0",stFIPS), as.character(stFIPS)))
# 
# BG_list <- lapply(stateKey$charST, function(x) paste0("https://www2.census.gov/geo/docs/reference/cenpop2010/blkgrp/CenPop2010_Mean_BG",x,".txt"))
# 
# tableList <- lapply(BG_list, fread)

tableList <- fread("https://www2.census.gov/geo/docs/reference/cenpop2010/blkgrp/CenPop2010_Mean_BG.txt")

popCentroidsBG <- st_as_sf(tableList, coords = c("LONGITUDE", "LATITUDE"), crs = 4326) 
 


############# combine with zips for EHIB 

# CAzips <- st_read("~/GitHub/WFSmokeExp_US/cb_2016_us_zcta510_500k.kml") %>% st_transform(crs = 4326) %>%
#     mutate(zip = sapply(str_split(Description, pattern = "[<>]+"), "[[", 18)) %>% select(zip, geometry)

 # CAzipsBG <-st_intersection({popCentroidsBG %>% filter(STATEFP == 6)}, CAzips)


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

dateListrecent <- format(seq(as.Date("2018/09/23"),as.Date("2018/09/30"), by = "day"),"%Y%m%d")
mclapply(dateListrecent, FUN = intersectHMSrecent)

########### function to combine ############

# when combining several 

library(data.table)
library(scales)
library(lubridate)

directory <- "~/data/US_BG/RDS"
allBGs <- tableList[, geoIDer := paste0(STATEFP,"_",COUNTYFP,"_",TRACTCE,"_",BLKGRPCE)]


convert <- function(RDSfile){
  
  fwrite(readRDS(RDSfile), paste0("~/data/US_BG/CSV/", sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(RDSfile)),".csv"))
  
}

mclapply(list.files(directory,  full.names = T), convert)


smokeFile <- rbindlist(mclapply(list.files("~/data/US_BG/CSV/",full.names = T), fread), fill = T)

setkey(smokeFile, date, STATEFP, COUNTYFP, TRACTCE, BLKGRPCE) 

smokeFile <-copy(smokeFile)


crunchDayCA <- function(Year){
  
  smokeFile <-
    rbindlist(mclapply(list.files(
      directory,
      full.names = T,
      pattern = paste0("BG", Year)
    ), readRDS), fill = T) %>% setkey(date, STATEFP, COUNTYFP, TRACTCE, BLKGRPCE) %>% replace_na(list(
      light = 0,
      medium = 0,
      heavy = 0
    )) %>%
    .[, maxSmoke := factor(ifelse(heavy == 1, "heavy",
                                    ifelse(medium == 1, "medium", 
                                           ifelse(light ==1, "light", "none"))), levels=c("none","light","medium","heavy"))] %>%
    .[STATEFP == 6]
    
  
}

CA <- rbindlist(lapply(X = c(2011:2018), FUN = crunchDayCA)) %>% merge(CAzipsBG)


  CA[date > "20141231"]%>%
    .[order(date)]
  
  

# Year= 2010

crunchYearState<- function(Year){
  
  smokeFile <-
    rbindlist(mclapply(list.files(
      directory,
      full.names = T,
      pattern = paste0("BG", Year)
    ), readRDS), fill = T) %>% setkey(date, STATEFP, COUNTYFP, TRACTCE, BLKGRPCE) %>% replace_na(list(
      light = 0,
      medium = 0,
      heavy = 0
    )) 
  
  
    smokeFile[,c("lightPD", "mediumPD","heavyPD") := list(light * POPULATION,
                                                  medium * POPULATION,
                                                  heavy * POPULATION)]     
    
    return({
      smokeFile[, list(sum(lightPD),
                       sum(mediumPD),
                       sum(heavyPD)), by = "STATEFP"] %>% mutate(Year = Year)
    })

}

crunchYearCounty<- function(Year){
  
  smokeFile <-
    rbindlist(mclapply(list.files(
      directory,
      full.names = T,
      pattern = paste0("BG", Year)
    ), readRDS), fill = T) %>% setkey(date, STATEFP, COUNTYFP, TRACTCE, BLKGRPCE) %>% replace_na(list(
      light = 0,
      medium = 0,
      heavy = 0
    )) 
  
  
  smokeFile[,c("lightPD", "mediumPD","heavyPD") := list(light * POPULATION,
                                                        medium * POPULATION,
                                                        heavy * POPULATION)]     
  
  return({
    smokeFile[, list(sum(),
                     sum(lightPD),
                     sum(mediumPD),
                     sum(heavyPD)), by = c("STATEFP", "COUNTYFP")] %>% mutate(Year = Year)
  })
  
}

crunchYearBG<- function(Year){
  
  smokeFile <-
    rbindlist(mclapply(list.files(
      directory,
      full.names = T,
      pattern = paste0("BG", Year)
    ), readRDS), fill = T) %>% setkey(date, STATEFP, COUNTYFP, TRACTCE, BLKGRPCE) %>% replace_na(list(
      light = 0,
      medium = 0,
      heavy = 0
    )) 
  
  
  smokeFile[,c("lightPD", "mediumPD","heavyPD") := list(light * POPULATION,
                                                        medium * POPULATION,
                                                        heavy * POPULATION)]     
  
  return({
    smokeFile[, list(mean(POPULATION),
                     sum(lightPD),
                     sum(mediumPD),
                     sum(heavyPD)), by = c("STATEFP", "COUNTYFP", "TRACTCE", "BLKGRPCE")] %>% mutate(Year = Year)
  })
  
}

# look at heavy smoke by state and year

annualPersonDaysState <- rbindlist(lapply(X = c(2011:2018), FUN = crunchYearState))
names(annualPersonDaysState) <- c("stFIPS","LightPDs","MediumPDs","HeavyPDs","Year")
annualPersonDaysState %>% merge(stateKey) %>% write.csv("~/tempZip/stateAnnualPDs.csv", row.names = F)


annualPersonDays %>% merge(stateKey) %>% 
  gather(LightPDs,MediumPDs,HeavyPDs,key = smoke, value = PersonDays) %>%
  filter(smoke == "HeavyPDs") %>%
  ggplot() + 
  geom_bar(aes(x=Year, y=PersonDays, fill= smoke), stat="identity",position="dodge") + facet_wrap(~ stateName)

westCoast <- c("CA","OR","WA") 
midWest <- c("MI","WI","MN","IL","OH") 

annualPersonDays %>% merge(stateKey) %>% 
  gather(LightPDs,MediumPDs,HeavyPDs,key = smoke, value = PersonDays) %>%
  filter(ST %in% westCoast) %>%
  mutate(smoke = factor(smoke, levels=c("LightPDs","MediumPDs","HeavyPDs")))%>%
  ggplot() + 
  geom_bar(aes(x=Year, y=PersonDays, fill= smoke), stat="identity",position="dodge") + facet_grid( smoke ~stateName , scales = "free_y")




# look at heavy smoke by County and year

annualPersonDaysCounty <- rbindlist(lapply(X = c(2011:2018), FUN = crunchYearCounty)) 
annualPersonDaysCounty <- annualPersonDaysCounty %>% 
  mutate(geoIDer = paste0(
    {
      ifelse(STATEFP < 10, paste0("0",STATEFP),STATEFP)
    },
    {
      ifelse(COUNTYFP < 10, paste0("00",COUNTYFP), 
             ifelse(COUNTYFP < 100, paste0("0",COUNTYFP),COUNTYFP))
      
    }
  ))
names(annualPersonDaysCounty) <- c("stFIPS", "ctFIPS","LightPDs","MediumPDs","HeavyPDs","Year", "geoIDer")

UScountyFile <- st_read(dsn = "~/GitHub/WFSmokeExp_US/cb_2017_us_county_20m.kml", stringsAsFactors = F) %>% st_transform(crs = 4326) %>%
  mutate(County = sapply(str_split(Name, pattern = "[<>]+"), "[[",4),
         CountyFIPS = sapply(str_split(Description, pattern = "[<>]+"), "[[", 62))  %>%
  rename(geoIDer = CountyFIPS) %>%
  merge(annualPersonDaysCounty) %>%  
  filter(stFIPS == 6) %>% 
  st_crop(c(xmin=-145, xmax=90, ymin=20, ymax=50)) # lower 48 bounds


UScountyFile %>%
  gather(LightPDs,MediumPDs,HeavyPDs,key = smoke, value = PersonDays) %>%
  mutate(smoke = factor(smoke, levels=c("LightPDs","MediumPDs","HeavyPDs")))%>%
  filter(smoke == "HeavyPDs") %>%
ggplot() + geom_sf(aes(fill = PersonDays)) +
  scale_fill_distiller(palette = "Spectral") + 
  facet_grid(smoke~Year)



# look at heavy smoke by County and year

annualPersonDaysBG <- rbindlist(lapply(X = c(2011:2018), FUN = crunchYearBG))
names(annualPersonDaysBG) <- c("STATEFP", "COUNTYFP", "TRACTCE", "BLKGRPCE","POPULATION", "LightPDs","MediumPDs","HeavyPDs","Year")

foo <- annualPersonDaysBG[, geoIDer := paste0(STATEFP,"_",COUNTYFP,"_",TRACTCE,"_",BLKGRPCE)] # %>%
  .[STATEFP ==6]


#######  function to create the cummulative percentages for each year
cumPCT <- function(year){
  
  return({foo[Year==year] %>%
      .[order(-HeavyPDs)] %>%
      .[,CUMpop:= cumsum(POPULATION)/sum(POPULATION)] %>%
      .[,CUMexp:= cumsum(HeavyPDs)/sum(HeavyPDs)] })

}

test <- rbindlist(lapply(X = c(2011:2018), FUN = cumPCT))

  
test %>%  
  ggplot() + 
  geom_line(aes(x=CUMpop, y=CUMpop), color="black", linetype="dashed") +
  geom_line(aes(x=CUMpop, y=CUMexp, color=factor(Year), group= Year), size=2) + 
  scale_x_continuous(name="Fraction of US Population", limits=c(0, .75)) + 
  ylab("Fraction of the Annual Heavy WF Smoke Exposure")

  test[,sum(HeavyPDs)/1000000, by="Year"] %>% 
    ggplot() + geom_bar(aes(x=Year, y=V1), stat= "identity")







dateList <- format(seq(as.Date(paste0(Year,"/01/01")),as.Date(paste0(Year,"/12/31")), by = "day"),"%Y%m%d")



baseGrid <- data.table(expand.grid(date = dateList, geoIDer = unique(allBGs$geoIDer)), key = c("date","STATEFP","COUNTYFP","TRACTCE","BLKGRPCE"))

return(baseGrid)



BG <- rbindlist(tableList) %>% .[, geoIDer := paste0(STATEFP,"_",COUNTYFP,"_",TRACTCE,"_",BLKGRPCE)]


baseGrid <- expand.grid(date = dateList, geoIDer = unique(BG$geoIDer)), key = c("date","geoIDer"))
smokeFile <- rbindlist(mclapply(list.files(directory, full.names = T, pattern = ".RDS"), readRDS), fill = T) %>% setkey(date, geoIDer)

master <- merge(baseGrid, smokeFile, all.x=TRUE) %>%
  replace_na(list(light = 0, medium=0, heavy=0)) 

master[, "maxSmoke" := factor(ifelse(heavy == 1, "heavy",
                               ifelse(medium == 1, "medium", 
                                      ifelse(light ==1, "light", "none"))), levels=c("none","light","medium","heavy"))]


# master[geoIDer > 41000 & geoIDer < 42000] %>% write.csv("~/tempZip/OregonSmoke.csv", row.names = F)

## prepare 16-17 data for sumi crosstab tract x date 
## for Cali it is about 35 Mb

master[, c("Year","Month", "ST") := list(year(as.Date(date, "%Y%m%d")), month(as.Date(date, "%Y%m%d")),area)] %>% 
  # .[Year == 2016 | Year == 2018] %>% 
  # ungroup() %>%
  # select(date, geoIDer, maxSmoke) %>%
  # spread(key = date, value = maxSmoke) %>%
  fwrite(paste0("~/tempZip/",area,"TractsSmoke.csv"), row.names = F)


CA <- fread("~/tempZip/CATractsSmoke.csv") %>% .[,date := as.Date(as.character(date),"%Y%m%d") ]
WA <- fread("~/tempZip/WATractsSmoke.csv") %>% .[,date := as.Date(as.character(date),"%Y%m%d") ]
OR <- fread("~/tempZip/ORTractsSmoke.csv") %>% .[,date := as.Date(as.character(date),"%Y%m%d") ]

westStates <- rbindlist(list(CA, WA, OR))


SmokeDaysByTract <- westStates[,.N, by = c("geoIDer","maxSmoke", "Year")] %>% spread(key = maxSmoke, value = N) %>% replace_na(list(none=0,light = 0, medium=0, heavy=0)) %>%.[,c("State","County") := list(str_sub(geoIDer, 1, -10), str_sub(geoIDer,-9, -7))] %>%
  fwrite(paste0("~/tempZip/AnnualSmokeDaysbyTract.csv"), row.names = F)



SmokeTractsByDay <- westStates[,julian :=yday(date)] %>%
  .[,.N, by = c("Year", "julian","ST","maxSmoke")] %>% 
  spread(key = maxSmoke, value = N) %>% 
  replace_na(list(none=0,light = 0, medium=0, heavy=0))

SmokeTractsByDay[ ST =="CA"] %>% ggplot() + geom_tile(aes(x=Year, y = julian, fill=heavy, alpha =heavy)) +
  scale_fill_gradient(low = "gray20", high = "red") + facet_grid(.~ ST)  




SmokeTractsByDay %>%
  ggplot() +  geom_line(aes(x = date, y= heavy)) +
  scale_x_date(labels = date_format("%b-%y"), breaks='6 months') +
  theme_minimal() + 
  xlab("Date") + 
  ylab("Number of Counties experiencing smoke")




# number of counties experiencing different smoke levels 

plotData <- master[, lapply(.SD, sum), .SDcols = c("light","medium","heavy"), by = "date"] %>%
  gather(heavy, medium, light, key = smoke,value = value) %>%
  mutate(smoke = factor(smoke, levels = c("light", "medium", 'heavy')))


plotData %>%
  ggplot() +  geom_line(aes(x = as.Date(date, "%Y%m%d"), y= value, group =smoke, color = smoke)) +
  scale_color_manual(values = c("yellow","orange","red")) +
  scale_x_date(labels = date_format("%b-%y"), breaks='6 months') +
  theme_minimal() + 
  xlab("Date") + 
  ylab("Number of Counties experiencing smoke") + facet_grid(smoke~.)
       


plotData3 <-  master[, c("Year","stFIPS") := list(year(as.Date(date, "%Y%m%d")),
                                                 as.integer(str_sub(geoIDer, 0,2)))] %>%
  .[, lapply(.SD, sum), .SDcols = c("light","medium","heavy"), by = c("stFIPS","date")] %>%
  gather(heavy, medium, light, key = smoke,value = value) 

plotData3 %>% left_join(stateKey) %>%
  filter(ST %in% c("CA","WA","OR")) %>%
  mutate(smoke= factor(smoke, levels = c("light","medium","heavy"))) %>%
  ggplot() +  geom_area(aes(x = as.Date(date, "%Y%m%d"), y= value, group =smoke, fill = smoke), position = "dodge") +
  scale_fill_manual(values = c("yellow","orange","red")) +
  scale_x_date(labels = date_format("%b-%y"), breaks='1 year') +
  theme_minimal() + 
  xlab("Date") + 
  ylab("number of Counties experiencing smoke") +
  facet_grid(ST ~ smoke, scales = "free_y")


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
   filter(stFIPS < 57 & stFIPS != 15 ) %>% st_crop(c(xmin=-125, xmax=-116, ymin=41.5, ymax=46.5)) # crop to lower 48

   
   
   st_crop(c(xmin=-145, xmax=90, ymin=20, ymax=50)) # crop to OR
   st_crop(c(xmin=-145, xmax=90, ymin=20, ymax=50)) # crop to CA

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
 
