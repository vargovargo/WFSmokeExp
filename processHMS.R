rm(list = ls())

library(tidyverse)
library(sf)
library(parallel)


# testing the function
# HMSday <- "20180301"
# spatialFile <- UScounty
# l = 1

intersectHMS <- function(HMSday){
  
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

dateList <- format(seq(as.Date("2018/06/01"),as.Date("2018/09/01"), by = "day"),"%Y%m%d")

spatialFile <-  st_read(dsn = "~/GitHub/WFSmokeExp/SmokeExposures/tractsSM.GeoJSON", stringsAsFactors = F) %>% 
  st_transform(crs = 4326) %>%   
  mutate(COUNTYFI_1 = as.character(paste0(STATE, COUNTY))) %>%
  rename(geoIDer = ct10)

mclapply(dateList, FUN = intersectHMS)






saveRDS(AllDays,file = "~/GitHub/WFSmokeExp/Aug16ToAug202018.rds")

temp1 <- bind_rows(readRDS("~/GitHub/WFSmokeExp/Jan29ToAug152018_2.rds"), readRDS("~/GitHub/WFSmokeExp/Aug16ToAug202018.rds"))

saveRDS(temp1,file = "~/GitHub/WFSmokeExp/Jan29ToAug202018.rds")

AllData <- readRDS(file = "~/GitHub/WFSmokeExp/Jan29ToAug202018.rds") %>%
  mutate(date = as.Date(paste0(year, month, day), format= "%Y%m%d"))



length(seq(as.Date("2018/08/13"),as.Date("2018/08/14"), by = "day"))
dateList2 <- c(as.Date("2018-08-12"), as.Date("2018-08-13"))


################ create last 2 weeks table #################

dateList <- format(seq(as.Date("2018/06/01"),as.Date("2018/09/01"), by = "day"),"%Y%m%d")


start <- as.Date(dateList[length(dateList)-14],"%Y%m%d")
end <- as.Date(dateList[length(dateList)],"%Y%m%d")




 AllData %>% 
    filter(
      as.Date(date) > as.character(start) &
        as.Date(date) <= as.character(end)
    ) %>%
    mutate(smoke = factor(ifelse(smoke == "Smoke (Light)", "light", 
                               ifelse(smoke == "Smoke (Heavy)","heavy", "medium")), levels=c("light","medium","heavy"))) %>%   
    group_by(smoke, ct10) %>%
    summarize(NumberOfDays = length(unique(date))) %>%
    ungroup() %>% 
    spread(key = smoke,value = NumberOfDays) %>% saveRDS("~/GitHub/WFSmokeExp/SmokeExposures/last2weeks.RDS")

  
  
################ create last 30 days table #################
  
  start <- as.Date(dateList[length(dateList)-30],"%Y%m%d")
  end <- as.Date(dateList[length(dateList)],"%Y%m%d")
  
  AllData %>% 
    filter(
      as.Date(date) > as.character(start) &
        as.Date(date) <= as.character(end)
    ) %>%
    mutate(smoke = factor(ifelse(smoke == "Smoke (Light)", "light", 
                                 ifelse(smoke == "Smoke (Heavy)","heavy", "medium")), levels=c("light","medium","heavy"))) %>%   
    group_by(smoke, ct10) %>%
    summarize(NumberOfDays = length(unique(date))) %>%
    ungroup() %>% 
    spread(key = smoke,value = NumberOfDays) %>% saveRDS("~/GitHub/WFSmokeExp/SmokeExposures/last30days.RDS")
  
  
  
  ################ create last 2 months table #################
  
  start <- as.Date(dateList[length(dateList)-60],"%Y%m%d")
  end <- as.Date(dateList[length(dateList)],"%Y%m%d")
  
  AllData %>% 
    filter(
      as.Date(date) > as.character(start) &
        as.Date(date) <= as.character(end)
    ) %>%
    mutate(smoke = factor(ifelse(smoke == "Smoke (Light)", "light", 
                                 ifelse(smoke == "Smoke (Heavy)","heavy", "medium")), levels=c("light","medium","heavy"))) %>%   
    group_by(smoke, ct10) %>%
    summarize(NumberOfDays = length(unique(date))) %>%
    ungroup() %>% 
    spread(key = smoke,value = NumberOfDays) %>% saveRDS("~/GitHub/WFSmokeExp/SmokeExposures/last2months.RDS")
  


######## filter tract by date range ####################

SmokeDaysByTract <- 
  AllData %>% 
  filter(
    as.Date(date) >= as.character(dateList2[1]) &
      as.Date(date) <= as.character(dateList2[2])
  ) %>%
  mutate(smoke = factor(ifelse(smoke == "Smoke (Light)", "light", 
                               ifelse(smoke == "Smoke (Heavy)","heavy", "medium")), levels=c("light","medium","heavy"))) %>%   
  group_by(smoke, ct10) %>%
  summarize(NumberOfDays = length(unique(date))) %>%
  ungroup() %>% 
  spread(key = smoke,value = NumberOfDays) 



CAtracts <-  st_read(dsn = "~/GitHub/WFSmokeExp/SmokeExposures/tractsSM.GeoJSON", stringsAsFactors = F) %>% st_transform(crs = 4326) %>%
  mutate(COUNTYFI_1 = as.character(paste0(STATE, COUNTY)))

dateList




###### create the vulnerable peoples table ###########

children <- read.csv("~/CHVI_copy/data/tables/tracts/CHVI_children_tract.csv", header = T, stringsAsFactors = F) %>% 
  select(geotypv, numratr, denmntr, race) %>% 
  rename(ct10 = geotypv, 
         children = numratr, 
         total = denmntr)

elderly <- read.csv("~/CHVI_copy/data/tables/tracts/CHVI_elderly_tract.csv", header = T, stringsAsFactors = F) %>% 
  select(geotypv, numratr, denmntr, race) %>% 
  rename(ct10 = geotypv, 
         elderly = numratr, 
         total = denmntr)

vuln <- inner_join(children, elderly) %>% 
  mutate(other = total - children - elderly) %>%
  gather(children, elderly, other, total, key = cohort, value = population)


vuln %>% saveRDS("~/GitHub/WFSmokeExp/SmokeExposures/vuln.rds")


##### Create SmokeByDay file for timeline plot #######


SmokeByDay <- 
  AllData %>% 
  # filter(
  #   as.Date(date) >= as.character(dateList[1]) &
  #     as.Date(date) <= as.character(dateList[2])
  # ) %>%
  mutate(smoke = factor(ifelse(smoke == "Smoke (Light)", "light", 
                               ifelse(smoke == "Smoke (Heavy)","heavy", "medium")), levels=c("light","medium","heavy"))) %>%
  select(smoke, ct10, date) %>% inner_join({
    readRDS("~/GitHub/WFSmokeExp/SmokeExposures/vuln.rds") %>% 
      mutate(ct10 = paste0("0",as.character(as.numeric(ct10)))) %>%
      filter(race == "Total") %>%
      spread(key = cohort, value = population) %>%
      select(-race, )
  }) %>%
  group_by(ct10, date, smoke) %>%
  summarize(Persons = mean(total, na.rm=T)) %>%
  group_by(smoke, date) %>%
  summarize(Persons = sum(Persons, na.rm=T)) 

SmokeByDay %>% saveRDS(SmokeByDay, "~/GitHub/WFSmokeExp/SmokeExposures/SmokeByDay.RDS")








#################### mapping and testing outside shiny #######################
library(leaflet)


pal_l <- colorBin(palette =  "Reds",
                  bins = 7,
                  domain = na.exclude(SmokeDaysByTract$light))

pal_m <- colorBin(palette =  "Reds",
                  bins = 7,
                  domain = na.exclude(SmokeDaysByTract$medium))

pal_h <- colorBin(palette =  "Reds",
                  bins = 7,
                  domain = na.exclude(SmokeDaysByTract$heavy))


mapTemp <- CAtracts %>% inner_join(SmokeDaysByTract)


mapTemp %>%
  leaflet()  %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    color = "#444444",
    weight = 1,
    smoothFactor = 0.1,
    fillOpacity = 0.6,
    fillColor = ~ pal_l(light),
    highlightOptions = highlightOptions(
      color = "white",
      weight = 2,
      bringToFront = TRUE
    ),
    popup = paste0(
      "This tract has experienced ",
      mapTemp$light,
      " days (",
      round(100 * mapTemp$light / 199, 1),
      "%) of Light Smoke Exposure since Jan 29, 2018."
    ),
    group = "Light Smoke"
  ) %>%
  addPolygons(
    color = "#444444",
    weight = 1,
    smoothFactor = 0.1,
    fillOpacity = 0.6,
    fillColor = ~ pal_m(medium),
    highlightOptions = highlightOptions(
      color = "white",
      weight = 2,
      bringToFront = TRUE
    ),
    popup = paste0(
      "This tract has experienced ",
      mapTemp$medium,
      " days (",
      round(100 * mapTemp$medium / 199, 1),
      "%) of Medium Smoke Exposure since Jan 29, 2018."
    ),
    group = "Medium Smoke"
  ) %>%
  addPolygons(
    color = "#444444",
    weight = 1,
    smoothFactor = 0.1,
    fillOpacity = 0.6,
    fillColor = ~ pal_h(heavy),
    highlightOptions = highlightOptions(
      color = "white",
      weight = 2,
      bringToFront = TRUE
    ),
    popup = paste0(
      "This tract has experienced ",
      mapTemp$heavy,
      " days (",
      round(100 * mapTemp$heavy / 199, 1),
      "%) of Heavy Smoke Exposure since Jan 29, 2018."
    ),
    group = "Heavy Smoke"
  ) %>%
  addLayersControl(
    baseGroups = c("Heavy Smoke", "Medium Smoke", "Light Smoke"),
    options = layersControlOptions(collapsed = TRUE)
  )




################# test person-day plot outside shiny ############

library(plotly)

 plot <- vuln %>%
  mutate(ct10 = paste0("0",as.character(as.numeric(ct10)))) %>%
  inner_join(SmokeDaysByTract) %>% 
  mutate(lightPD = population * light,
         mediumPD = population * medium,
         heavyPD = population * heavy) %>%
  filter(cohort != "total", race != "Total") %>%
  group_by(cohort, race) %>%
  summarize(Heavy = sum(heavyPD, na.rm = T),
            Medium = sum(mediumPD, na.rm = T),
            Light = sum(lightPD, na.rm = T)) %>% ungroup() %>%
   gather(Heavy, Medium, Light, key = SmokeDensity, value = PersonDays) %>%
   mutate(SmokeDensity = factor(SmokeDensity, levels = c("Light","Medium","Heavy")))
   
  plot %>% ggplot(aes(x=race, y= PersonDays, fill = cohort)) + geom_bar(stat="identity") + facet_grid(.~SmokeDensity~., scales="free_y")






############ TEst TImeline plot outside shiny #################

SmokeByDay %>% ggplot() + 
  geom_area(aes(x = date, y= Persons/1000000, fill = smoke), position = "dodge") + 
  ylab("Millions of People") + xlab("Date") + 
  geom_hline(yintercept = 37.25, color = "red", linetype="dashed") + 
  geom_label(aes(x = as.Date("2018-05-01"),y = 37.25, label="Total CA Population")) + 
  geom_vline(xintercept = as.Date("2018-07-27"), color = "gray20", linetype="twodash") + 
  geom_label(aes(x = as.Date("2018-07-27"),y = 35, label="Mendocino Complex Start", hjust="right")) +
  geom_vline(xintercept = as.Date("2018-07-23"), color = "gray20", linetype="dotdash") + 
  geom_label(aes(x = as.Date("2018-07-23"),y = 32, label="Carr Fire Start", hjust="right")) +
  geom_vline(xintercept = as.Date("2018-07-13"), color = "gray20", linetype="longdash") + 
  geom_label(aes(x = as.Date("2018-07-13"),y = 29, label="Ferguson Fire Start", hjust="right"))















