rm(list = ls())

library(tidyverse)
library(sf)

dateList <- format(seq(as.Date("2018/08/13"),as.Date("2018/08/14"), by = "day"),"%Y%m%d")

# smokeFileZip <- "http://satepsanone.nesdis.noaa.gov/pub/FIRE/HMS/GIS/ARCHIVE/hms_smoke20180808.zip"
CAtracts <-  st_read(dsn = "~/GitHub/WFSmokeExp/SmokeExposures/tractsSM.GeoJSON", stringsAsFactors = F) %>% st_transform(crs = 4326) %>%
  mutate(COUNTYFI_1 = as.character(paste0(STATE, COUNTY)))

smokeDay <- function(HMSday){
  
      year <- substr(HMSday, 1,4)
      month <- substr(HMSday, 5,6)
      day <- substr(HMSday, 7,8)
        
      smokeFileName <- paste0("http://satepsanone.nesdis.noaa.gov/pub/FIRE/HMS/KML/ARCHIVE/smoke",HMSday,".kml")
      
      layers <- st_layers(smokeFileName)$name
      
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

AllData <- readRDS(file = "~/GitHub/WFSmokeExp/Jan29ToAug152018_2.rds") %>%
  mutate(date = as.Date(paste0(year, month, day), format= "%Y%m%d"))


length(seq(as.Date("2018/08/13"),as.Date("2018/08/14"), by = "day"))
dateList <- c(as.Date("2018-08-12"), as.Date("2018-08-13"))


SmokeDaysByTract <- 
  AllData %>% 
    filter(
      as.Date(date) >= as.character(dateList[1]) &
        as.Date(date) <= as.character(dateList[2])
    ) %>%
    group_by(smoke, ct10) %>%
    summarize(NumberOfDays = length(unique(date))) %>%
    ungroup() %>% 
    spread(key = smoke,value = NumberOfDays) %>%
    rename(light = "Smoke (Light)",
           medium = "Smoke (Medium)",
           heavy = "Smoke (Heavy)")





head(AllData)


HMLwide <- AllData %>% 
  group_by(smoke, ct10) %>%
  summarize(NumberOfDays = length(date)) %>%
  ungroup() %>% 
  spread(key = smoke,value = NumberOfDays) %>%
  rename(light = "Smoke (Light)",
         medium = "Smoke (Medium)",
         heavy = "Smoke (Heavy)")

saveRDS(HMLwide,file = "~/GitHub/WFSmokeExp/Jan29ToAug152018_wide.rds")

library(leaflet)

CAtracts <-  st_read(dsn = "~/GitHub/WFSmokeExp/SmokeExposures/tractsSM.GeoJSON", stringsAsFactors = F) %>% st_transform(crs = 4326) %>%
  mutate(COUNTYFI_1 = as.character(paste0(STATE, COUNTY)))

dateList



#################### mapping #######################

pal_l <- colorBin(palette =  "Reds",
                  bins = 7,
                  domain = na.exclude(HMLwide$light))

pal_m <- colorBin(palette =  "Reds",
                  bins = 7,
                  domain = na.exclude(HMLwide$medium))

pal_h <- colorBin(palette =  "Reds",
                  bins = 7,
                  domain = na.exclude(HMLwide$heavy))


mapTemp <- CAtracts %>% inner_join(HMLwide)


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
  gather(children, elderly, total, key = cohort, value = population) %>% 
  saveRDS("~/GitHub/WFSmokeExp/SmokeExposures/vuln.rds")


library(plotly)

 plot <- vuln %>%
  inner_join({mutate(HMLwide, ct10 = as.numeric(ct10))}) %>% 
  mutate(lightPD = population * light,
         mediumPD = population * medium,
         heavyPD = population * heavy) %>%
  filter(cohort != "total", race != "Total") %>%
  group_by(cohort, race) %>%
  summarize(Heavy = sum(heavyPD), 
             Medium = sum(mediumPD),
             Light = sum(lightPD)) %>% ungroup() %>%
   gather(Heavy, Medium, Light, key = SmokeDensity, value = PersonDays) %>%
   mutate(SmokeDensity = factor(SmokeDensity, levels = c("Light","Medium","Heavy")))
   
  plot %>% ggplot(aes(x=race, y= PersonDays, fill = cohort)) + geom_bar(stat="identity") + facet_grid(.~SmokeDensity~., scales="free_y")











