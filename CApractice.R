########### function to combine ############

# when combining several 
rm(list = ls())

library(tidyverse)
library(sf)
library(data.table)
library(scales)
library(lubridate)
library("jsonlite", lib.loc="~/R/win-library/3.4")

stateKey <- fread("~/GitHub/WFSmokeExp/stateKey.csv") %>%
  mutate(charST = ifelse(stFIPS<10, paste0("0",stFIPS), as.character(stFIPS)))

tableList <- fread("https://www2.census.gov/geo/docs/reference/cenpop2010/blkgrp/CenPop2010_Mean_BG.txt")

# popCentroidsBG <- st_as_sf(tableList, coords = c("LONGITUDE", "LATITUDE"), crs = 4326) 

allBGs <- tableList[, geoIDer := paste0(STATEFP,"_",COUNTYFP,"_",TRACTCE,"_",BLKGRPCE)]
allBGs <- copy(allBGs)
setkey(allBGs, STATEFP, COUNTYFP, geoIDer)

# mclapply(list.files(directory,  full.names = T), convert)

# smokeFile <- rbindlist(mclapply(list.files("~/data/US_BG/CSV/",full.names = T), fread), fill = T) 
# smokeFile %>% fwrite("~/data/smokeFile.csv" )

smokeFile <- fread("~/data/smokeFile.csv")
setkey(smokeFile, STATEFP, COUNTYFP, TRACTCE, BLKGRPCE) 
smokeFile <-copy(smokeFile)

# make Ca smokeFile
smokeFile[.(6L)]  %>% fwrite("~/data/CAsmokeFile.csv")

# fullGrid <- expand.grid(date = format(seq(as.Date(start, format ="%Y%m%d" ),as.Date(end, format ="%Y%m%d"), by = "day"), format ="%Y%m%d"), state = unique(tableList$STATEFP))

######## create CA file for app ######

input <- data.frame(state = 6L, county = 1L)

allBGs[.(input$state, input$county) ]

smokeFile[STATEFP == input$state & COUNTYFP == input$county & TRACTCE =="409300" ] %>% fwrite("~/tempZip/testBG.csv")

start <- as.character(min(unique(smokeFile$date))) # determine first day
end <- as.character(max(unique(smokeFile$date))) # determine last day
# create grid of all possible day and BG combinations for the selected county
grid <- CJ(date = as.integer(format(seq(as.Date(start, format ="%Y%m%d" ),as.Date(end, format ="%Y%m%d"), by = "day"), format ="%Y%m%d")), 
           geoIDer = 
             allBGs[.(input$state, input$county), geoIDer])
setkey(grid, date, geoIDer)


selectedSmoke <- smokeFile[.(input$state, input$county)]%>%.[, geoIDer := paste0(STATEFP,"_",COUNTYFP,"_",TRACTCE,"_",BLKGRPCE)]
key(selectedSmoke)

# list of all days, and bloack groups in the County

merge(grid, selectedSmoke[, .(STATEFP = mean(STATEFP),
                              COUNTYFP  = mean(COUNTYFP),
                              TRACTCE  = mean(TRACTCE),
                              BLKGRPCE  = mean(BLKGRPCE)),  by=.(geoIDer)], by="geoIDer", all.x = T) %>%
  merge(selectedSmoke[,.(date, geoIDer, light, medium, heavy, POPULATION)], by=c("date", "geoIDer") , all.x = T) %>%
  replace_na(list(light = 0, # replace the na's for days with no smoke plumes to zero
                  medium = 0, 
                  heavy = 0, 
                  POPULATION = 0)) %>%
  .[, .(
    lightPD = sum(POPULATION * light, na.rm = T),
    mediumPD = sum(POPULATION * medium, na.rm = T),
    heavyPD = sum(POPULATION * heavy, na.rm = T), 
    lightDAYS = 
  ), by = .(date, COUNTYFP )] %>%
  melt.data.table(id.vars = "date", measure.vars = c("lightPD","mediumPD","heavyPD")) %>% 
  ggplot() + 
  geom_area(aes(x = date, y= value/1000000, fill = variable), position = "dodge") + 
  scale_fill_manual(values = c("yellow","orange","red"))






