rm(list = ls())

library(tidyverse)
library(sf)

dateList <- format(seq(as.Date("2018/01/29"),as.Date("2018/08/15"), by = "day"),"%Y%m%d")

# smokeFileZip <- "http://satepsanone.nesdis.noaa.gov/pub/FIRE/HMS/GIS/ARCHIVE/hms_smoke20180808.zip"
CAtracts <-  st_read(dsn = "~/GitHub/WFSmokeExp/tracts.GeoJSON", stringsAsFactors = F) %>% st_transform(crs = 4326) %>%
  mutate(COUNTYFI_1 = as.character(paste0(STATE, COUNTY)))

smokeDay <- function(HMSday){
  
      year <- substr(HMSday, 1,4)
      month <- substr(HMSday, 5,6)
      day <- substr(HMSday, 7,8)
        
      smokeFileName <- paste0("http://satepsanone.nesdis.noaa.gov/pub/FIRE/HMS/KML/ARCHIVE/smoke",HMSday,".kml")
      
      layers <- st_layers(smokeFileName)$name
      
      for(l in 1: length(layers)){
      
        Smk <- st_read(dsn = smokeFileName, layer = layers[l]) %>%
          mutate(date = sapply(str_split(Description, pattern = " "),"[",5),
                 time = sapply(str_split(Description, pattern = " "),"[",6), 
                 year = year,
                 month = month,
                 day = day, 
                 smoke = layers[l])
        
        SmkLayerDayInt <- st_intersection(Smk, CAtracts) 
        st_geometry(SmkLayerDayInt) <- NULL
      
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

saveRDS(AllDays,file = "~/GitHub/WFSmokeExp/Jan29ToAug152018_2.rds")

AllDays <- readRDS(file = "~/GitHub/WFSmokeExp/Jan29ToAug152018.rds")



head(AllDays)

foo <- AllDays %>% 
  group_by(smoke, ct10) %>%
  summarize(NumberOfDays = length(date)) %>%
  ungroup()

foo2 <- foo %>% spread(key = smoke,value = NumberOfDays) %>%
  rename(light = "Smoke (Light)",
         medium = "Smoke (Medium)",
         heavy = "Smoke (Heavy)")

saveRDS(foo2,file = "~/GitHub/WFSmokeExp/SmokeExposures/Jan29ToAug152018_wide.rds")

library(leaflet)

CAtracts <-  st_read(dsn = "~/GitHub/WFSmokeExp/tracts.GeoJSON", stringsAsFactors = F) %>% st_transform(crs = 4326) %>%
  mutate(COUNTYFI_1 = as.character(paste0(STATE, COUNTY)))




#################### mapping #######################

pal_l <- colorBin(palette =  "Reds",
                  bins = 7,
                  domain = na.exclude(foo2$light))

pal_m <- colorBin(palette =  "Reds",
                  bins = 7,
                  domain = na.exclude(foo2$medium))

pal_h <- colorBin(palette =  "Reds",
                  bins = 7,
                  domain = na.exclude(foo2$heavy))


mapTemp <- CAtracts %>% inner_join(foo2)


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




vuln <- inner_join(children, elderly) %>% gather(children, elderly, total, key = cohort, value = population) %>% saveRDS("~/GitHub/WFSmokeExp/SmokeExposures/vuln.rds")


library(plotly)

 plot <- vuln %>%
  inner_join({mutate(foo2, ct10 = as.numeric(ct10))}) %>% 
  mutate(lightPD = population * light,
         mediumPD = population * medium,
         heavyPD = population * heavy) %>%
  filter(cohort != "total", race != "Total") %>%
  group_by(cohort, race) %>%
  summarize(Heavy = sum(heavyPD), 
             Medium = sum(mediumPD),
             Light = sum(lightPD)) %>% ungroup() %>%
   gather(Heavy, Medium, Light, key = SmokeDensity, value = PersonDays)
   
  plot %>% ggplot(aes(x=race, y= PersonDays, fill = cohort)) + geom_bar(stat="identity") + facet_grid(.~SmokeDensity~., scales="free_y")










