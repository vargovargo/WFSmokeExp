
################################################
# Jason Vargo
# script to process wildfire smoke data for MMWR
# April 2019
################################################


rm(list = ls())

library(tidyverse)
library(sf)
library(data.table)

 
stateKey <- fread("~/GitHub/WFSmokeExp/stateKey.csv") %>%
  mutate(charST = ifelse(stFIPS<10, paste0("0",stFIPS), as.character(stFIPS))) %>%
  rename(STATEFP = stFIPS)

tableList <- fread("https://www2.census.gov/geo/docs/reference/cenpop2010/blkgrp/CenPop2010_Mean_BG.txt")
 
pop <- tableList[,.(totalPOP = sum(POPULATION)), by=.(STATEFP)] %>% merge(stateKey)


tableListTract <- fread("https://www2.census.gov/geo/docs/reference/cenpop2010/tract/CenPop2010_Mean_TR.txt")

smokeFile <- fread("~/data/smokeFile.csv")[,date := as.Date(as.character(date),"%Y%m%d")] 
setkey(smokeFile, date, STATEFP, COUNTYFP, TRACTCE, BLKGRPCE) 
smokeFile <-copy(smokeFile)


# number of people exposed in a year by state
ExposedPOP.stateYear  <- smokeFile[,year := year(date)] %>% 
  replace_na(list(
    light = 0,
    medium = 0,
    heavy = 0
  )) %>%
  .[,.(lightdays = max(light, na.rm = T),
       mediumdays = max(medium, na.rm = T),
       heavydays = max(heavy, na.rm = T), 
       POP = mean(POPULATION, na.rm = T)), 
    by=.(STATEFP, COUNTYFP, TRACTCE, BLKGRPCE, year)] %>%
  .[, .(lightPOP = sum(POP*lightdays),
        mediumPOP = sum(POP*mediumdays),
        heavyPOP = sum(POP*heavydays)), by =.(STATEFP, year)] %>% 
  merge(pop, by = "STATEFP",all.x=T) # %>%
# fwrite("~/tempZip/ExposedPOP_stateYear.csv")


ExposedPOP.stateYear[, `:=` (light = lightPOP / totalPOP, 
                              medium = mediumPOP / totalPOP, 
                              heavy = heavyPOP / totalPOP)] %>%
  melt.data.table(measure.vars = c("light", "medium", "heavy"), id.vars = c("stateName", "year"), variable.name = "Exposure", value.name = "Percent")  %>%
# fwrite("~/tempZip/ExposedPOP_stateYear_percents.csv")
# 
# 
# %>% 
  ggplot() + geom_line(aes(x= year, y= Percent, color = factor(Exposure))) + facet_wrap(~ stateName) 



#
############ create persondays_by_svi_raw table ##################
#

######## tract file for joining with SVI ###########

tractAnnual<- 
  smokeFile[,year := year(date)] %>% 
  .[, .(
    light = sum(light * POPULATION, na.rm = T),
    medium = sum(medium * POPULATION, na.rm = T),
    heavy = sum(heavy * POPULATION, na.rm = T)
  ), by = .(year, STATEFP, COUNTYFP, TRACTCE)] %>% merge(tableListTract)


svi <- fread("/Users/jvargo/Downloads/SVI2010_US.csv")[,.(GEO_ID, STATEFP = STATE_FIPS, COUNTYFP = CNTY_FIPS, TRACTCE = TRACT , STCOFIPS, FIPS,STATE_ABBR,STATE_NAME, COUNTY,TOTPOP , R_PL_THEME1, R_PL_THEME2, R_PL_THEME3, R_PL_THEME4, R_PL_THEMES)]

merged <- merge(tractAnnual, svi) # %>% fwrite("./tempZip/FullSVItracts.csv")

quartify <- function(values){
  
  foo <- ifelse(values == -999, "missing", 
                ifelse(values > 0 & values <= .25, "1 least vulnerable",
                       ifelse(values > .25 & values <= .5, "2 percentile 25-50",
                              ifelse(values > 0.5 & values <= .75,"3 percentile 50-75",
                                     ifelse(values > .75 & values <= 1, "4 most vulnerable","missing")))))
  
  return(foo)
                
}


westStates <- c("California","Oregon","Washington")

SES <- merged[R_PL_THEMES != -999, .(quartile = quartify(R_PL_THEME1), light, medium, heavy, STATE_NAME, year)] %>%
  .[, .(
    light = sum(light, na.rm = T),
    medium = sum(medium, na.rm = T),
    heavy = sum(heavy, na.rm = T)
  ), by = . (quartile, year, STATE_NAME)] %>%
  melt.data.table(id.vars = c("quartile", "year", "STATE_NAME"), variable.name = "smoke_level")  %>% mutate(svi_theme = "SES", 
                                                                          region = ifelse(STATE_NAME %in% westStates,  "west", "not west")) %>% data.table()

HC <- merged[R_PL_THEMES != -999, .(quartile = quartify(R_PL_THEME2), light, medium, heavy, STATE_NAME, year)] %>%
  .[, .(
    light = sum(light, na.rm = T),
    medium = sum(medium, na.rm = T),
    heavy = sum(heavy, na.rm = T)
  ), by = . (quartile, year, STATE_NAME)] %>%
  melt.data.table(id.vars = c("quartile", "year", "STATE_NAME"), variable.name = "smoke_level") %>% mutate(svi_theme = "HousingComposition", 
                                                                         region = ifelse(STATE_NAME %in% westStates,  "west", "not west"))%>% data.table()

MSL <- merged[R_PL_THEMES != -999, .(quartile = quartify(R_PL_THEME3), light, medium, heavy, STATE_NAME, year)] %>%
  .[, .(
    light = sum(light, na.rm = T),
    medium = sum(medium, na.rm = T),
    heavy = sum(heavy, na.rm = T)
  ), by = . (quartile, year, STATE_NAME)] %>%
  melt.data.table(id.vars = c("quartile", "year", "STATE_NAME"), variable.name = "smoke_level") %>% mutate(svi_theme = "Minority", 
                                                                         region = ifelse(STATE_NAME %in% westStates, "west", "not west"))%>% data.table()

HT <- merged[R_PL_THEMES != -999, .(quartile = quartify(R_PL_THEME4), light, medium, heavy, STATE_NAME, year)] %>%
  .[, .(
    light = sum(light, na.rm = T),
    medium = sum(medium, na.rm = T),
    heavy = sum(heavy, na.rm = T)
  ), by = . (quartile, year, STATE_NAME)] %>%
  melt.data.table(id.vars = c("quartile", "year", "STATE_NAME"), variable.name = "smoke_level") %>% mutate(svi_theme = "HousingTransportation", 
                                                                         region = ifelse(STATE_NAME %in% westStates, "west", "not west"))%>% data.table()

QTable <- data.table(bind_rows(SES, HC, MSL, HT))


QTable[quartile != "missing", .(billion_person_days = sum(as.numeric(value))/1000000000), by = .(smoke_level, quartile, svi_theme)] %>% 
  ggplot() + geom_bar(stat="identity",position = "dodge",aes(x=svi_theme, y=billion_person_days, fill=quartile)) +facet_grid(smoke_level ~ ., scales="free_y")


QTable[quartile != "missing", .(billion_person_days = sum(as.numeric(value))/1000000000), by = .(smoke_level, quartile, svi_theme, region)] %>% 
  ggplot() + geom_bar(stat="identity",position = "stack",aes(x=quartile, y=billion_person_days, fill=region)) +facet_grid( svi_theme ~ smoke_level, scales="free_x") +coord_flip()


bind_rows({
  QTable[quartile != "missing", .(region = "US", billion_person_days = sum(as.numeric(value))/1000000000), by = .(smoke_level, quartile, svi_theme)]
},
{
  QTable[quartile != "missing", .(billion_person_days = sum(as.numeric(value))/1000000000), by = .(smoke_level, quartile, svi_theme, region)]
}) %>% fwrite("~/tempZip/persondays_by_svi_raw.csv")







head( fread("/Users/jvargo/Downloads/SVI2010_US.csv"))



%>% fwrite("./tempZip/PDsBySVI_percentile.csv")



POP <- svi[F_PL_TOTAL != -999, .(SES = F_PL_THEME1, HC = R_PL_THEME2, MCL = F_PL_THEME3, HT = F_PL_THEME4, TOTPOP, STATE_NAME)] %>%
  .[, .(
    POP = sum(TOTPOP)
  ), by = . (SES,HC, MCL,HT, STATE_NAME)] %>% mutate(theme = "POP",
                                                     west = ifelse(STATE_NAME %in% westStates, "west", "not west")) %>% fwrite("./tempZip/POPBySVI.csv")


byPercentile <- merged[R_PL_THEMES != -999, .(score = R_PL_THEMES, light, medium, heavy, STATE_NAME, year)] %>%
  .[,vuln := ifelse(score > 0.75, 1, 0)] %>%
  .[, .(
    light = sum(light, na.rm = T),
    medium = sum(medium, na.rm = T),
    heavy = sum(heavy, na.rm = T)
  ), by = . (vuln, year, STATE_NAME)] %>%
  melt.data.table(id.vars = c("vuln", "year", "STATE_NAME"))  %>% mutate(theme = "Percentile", 
                                                                         west = ifelse(STATE_NAME %in% westStates,  "west", "not west"))


byPercentile %>% fwrite("./tempZip/PDstop25SVI.csv")

byMinor %>%
  ggplot() + geom_bar(stat = "identity",position = "fill",aes(x = year,y = value, fill = factor(theme))) + facet_grid(variable ~ ., scales = "free_y")

merged[F_PL_TOTAL != -999, .( light = sum(light, na.rm=T)), by=. (F_PL_TOTAL, STATE_NAME, year)] %>%
  ggplot() + geom_area(stat = "identity", position = "stack", aes(x=year, y= light, fill= factor(F_PL_TOTAL))) + facet_wrap(~STATE_NAME)
















merged[F_PL_TOTAL != -999, .( heavy = sum(heavy, na.rm=T)), by=. (F_PL_TOTAL,  year)] %>%
  ggplot() + geom_area(stat = "identity", position = "stack", aes(x=year, y= heavy, fill= factor(F_PL_TOTAL))) + ggthemes::theme_pander() + scale_fill_brewer(type = "seq")



merged %>% ggplot() + geom_point(aes(x=heavy, y=AGE65), color="red")

############# investigate season length ############
seasonOnset <- 
 smokeFile[, doy := yday(date)] %>% 
  .[heavy == 1, .(start = min(doy),
                  end = max(doy)), by = .(year, STATEFP, COUNTYFP, TRACTCE)] 

seasonOnset[, .(start = min(start, na.rm = T),
                end = max(end, na.rm = T)), by = .(year, STATEFP)] %>%
  melt.data.table(id.vars = c("year","STATEFP"),measure.vars = c("start","end"),variable.name = "timing",value.name = "DoY") %>% ggplot() + geom_line(aes(x=year, y=DoY, color = factor(timing))) + facet_wrap(~ STATEFP) + ggthemes::theme_tufte()

seasonOnset %>%
  melt.data.table(id.vars = c("year","STATEFP"),measure.vars = c("start","end"),variable.name = "timing",value.name = "DoY") %>% ggplot() + geom_line(aes(x=year, y=DoY, color = factor(timing))) + facet_wrap(~ STATEFP) + ggthemes::theme_tufte()


seasonOnset[, length := (end - start)] %>%
  ggplot() +
  geom_bar(aes(x = year, y = length, fill = factor(year)), stat = "identity") +
  facet_wrap( ~ STATEFP) +
  ggthemes::theme_fivethirtyeight()


# number of people exposed in a year National

ExposedPOP.USYear  <- smoke2[,year := year(date2)] %>% 
  replace_na(list(
    light = 0,
    medium = 0,
    heavy = 0
  )) %>%
  .[,.(lightdays = max(light, na.rm = T),
       mediumdays = max(medium, na.rm = T),
       heavydays = max(heavy, na.rm = T), 
       POP = mean(POPULATION, na.rm = T)), 
    by=.(STATEFP, COUNTYFP, TRACTCE, BLKGRPCE, year)] %>%
  .[, .(lightPOP = sum(POP*lightdays),
        mediumPOP = sum(POP*mediumdays),
        heavyPOP = sum(POP*heavydays)), by =.(year)] %>% 
  .[, totalPOP := sum(pop$totalPOP)]  # %>%
   fwrite("~/tempZip/ExposedPOP_USYear.csv")


ExposedPOP.USYear[, `:=` (light = lightPOP / totalPOP, 
                             medium = mediumPOP / totalPOP, 
                             heavy = heavyPOP / totalPOP)] %>%
  melt.data.table(measure.vars = c("light", "medium", "heavy"), id.vars = c("year"), variable.name = "Exposure", value.name = "Percent")  %>%
  fwrite("~/tempZip/ExposedPOP_USYear_percents.csv")

%>% 
  ggplot() + geom_line(aes(x= year, y= Percent, size = 3, color = factor(Exposure))) + geom_smooth(aes(x= year, y= Percent, linetype = "dash", color = factor(Exposure)))



# person-days of exposure by state and year
Exposure.stateYear  <- smoke2[,year := year(date2)] %>% 
  replace_na(list(
    light = 0,
    medium = 0,
    heavy = 0
  )) %>%
  .[,.(lightPDs = sum(light*POPULATION, na.rm = T),
       mediumPDs = sum(medium*POPULATION, na.rm = T),
       heavyPDs = sum(heavy*POPULATION, na.rm = T)), 
    by=.(STATEFP, year)]    %>%
fwrite("~/tempZip/Exposure_stateYear.csv")

Exposure.stateYear  %>%
  melt.data.table(measure.vars = c("lightPDs", "mediumPDs", "heavyPDs"), id.vars = c("year"), variable.name = "Exposure", value.name = "PersonDays") %>% 
  ggplot() + geom_bar(aes(x= year, y= PersonDays, size = 3, fill = factor(Exposure)), stat="identity", position ="dodge")



####### make heat map #######

dateList <- data.table(date = as.integer(format(seq(as.Date(start, format ="%Y%m%d" ),as.Date(end, format ="%Y%m%d"), by = "day"), format ="%Y%m%d")))
setkey(dateList, date)

merge(dateList, 
      {
        smokeFile[, .(
          lightPD = sum(POPULATION * light, na.rm = T),
          mediumPD = sum(POPULATION * medium, na.rm = T),
          heavyPD = sum(POPULATION * heavy, na.rm = T)
        ), by = .(date)]
      },
      by = c("date"), all.x = TRUE ) %>%
  .[,.(date = as.Date(as.character(date),"%Y%m%d"), 
       lightPD, 
       mediumPD,
       heavyPD) ] %>%
  .[, `:=` (year = year(date),
            julian = yday(date)
  )] %>%
  melt.data.table(id.vars = c("year", "julian"),
                  measure.vars = c("heavyPD")) %>% 
  ggplot() + 
  geom_tile(aes(x=year, y = julian, fill=value, alpha = value)) +
  scale_fill_gradient(low = "gray0", high = "red")  +
  guides(alpha=FALSE) +
  facet_grid(.~ variable) + theme_minimal()





################################################
################################################
#
# Examine burn area
#
################################################
################################################


data.table(foreign::read.dbf("~/GitHub/WFSmokeExp_US/S_USA.MTBS_BURN_AREA_BOUNDARY.dbf")) %>%
  .[, .(acres = sum(ACRES, na.rm = T)), by=.(YEAR,  FIRE_TYPE)] %>% filter(YEAR >2009) %>%  fwrite("~/tempZip/burnAreaUS.csv")
  ggplot() + geom_bar(aes(x= YEAR, y= acres/1000000, fill= FIRE_TYPE ), stat="identity", position ="stack")



%>%
    date = as.Date(test, format = "%m%d%Y"),
    monyr = paste0(YEAR,"_", ifelse(STARTMONTH<10, paste0("0",STARTMONTH),STARTMONTH)))

burn %>% 
  # filter(YEAR >2009) %>% 
  ggplot( aes(x=YEAR, y = acres, fill = FIRE_TYPE)) + geom_bar(stat="identity") + coord_flip()  

burn <- fread("~/GitHub/WFSmokeExp_US/BurnAreaCountyIntersectText.csv")

plotFile <- merge(
  {
    
    burn[STATEFP == 6,.(burnacres = sum(ACRES)), by=.(YEAR, COUNTYFP, NAMELSAD, FIRE_TYPE )]
  }, 
  {
    smokeFile[STATEFP == 6, .(smokeDays = sum(heavy, na.rm = T),
                              smokePeople = sum(heavy*POPULATION, na.rm = T),
                              YEAR = year), by=.(year, COUNTYFP)]
  }
  )


ggplot(plotFile, aes(x= smokePeople, y = burnacres, color = YEAR)) + geom_point() + facet_wrap(~FIRE_TYPE) 


plotFile <- merge({
  burn[, .(burnacres = sum(ACRES)), by = .(YEAR, FIRE_TYPE)]
},
{
  smokeFile[, .(smokeDays = sum(heavy, na.rm = T) / length(is.numeric(heavy)),
                YEAR = year), by = .(year)]
}, by = "YEAR", all.y = T)
plotFile <- merge({
  burn[, .(burnacres = sum(ACRES)), by = .(YEAR, STATEFP, COUNTYFP, NAMELSAD, FIRE_TYPE)]
},
{
  smokeFile[, .(
    smokeDays = sum(light, na.rm = T),
    smokePeople = sum(light *
                        POPULATION, na.rm = T),
    YEAR = year
  ), by = .(year, STATEFP, COUNTYFP)]
})
ggplot(plotFile, aes(x = smokeDays, y = burnacres, color = FIRE_TYPE)) + geom_point() + facet_wrap( ~
                                                                                                      YEAR)


  




############# gather data for Caitlin ############# 
############# combine with zips for EHIB ############# 
# 
# CAzips <- st_read("~/GitHub/WFSmokeExp_US/CAzips.shp") %>% st_transform(crs = 4326) 
# CAzipsBG <-st_intersection({popCentroidsBG %>% filter(STATEFP == 6, COUNTYFP %in% c(1, 5, 9, 13, 43, 45, 55, 75, 79,81, 83, 95, 97, 99, 111))}, CAzips)
# st_geometry(CAzipsBG) <- NULL

####################################
## make Tract key for Caitlin

TandHtracts <- fread("//phdeorlcsrvip01/Crossbranch/OHE-EHIB-WILDFIRE/HMS4Caitlin/tractTandH_selectedCounties.csv")

geoids <- as.data.frame(unique(TandHtracts$geoid10)) %>% 
  mutate(
    STATEFP =as.integer(substr(x, 1,1)),
    COUNTYFP =as.integer(substr(x, 2,4)),
    TRACTCE =as.integer(substr(x, 5,10))
  )

names(geoids) <- c("geoid10","STATEFP","COUNTYFP","TRACTCE")

geoids <- data.table(geoids)

names(TandHtracts)


library(data.table)

TandHtracts <- fread("//phdeorlcsrvip01/Crossbranch/OHE-EHIB-WILDFIRE/HMS4Caitlin/tractTandH_selectedCounties.csv")

TandHtracts  %>% fwrite("~/TandHtest.csv")



HI <-
  TandHtracts[, .(TEMP = (((tmmx_mean + tmmn_mean) / 2) - 273.15) * 9 / 5 + 32,
                  RH = (rmax_mean + rmin_mean) / 2), by = .(Day, geoid10)] %>% 
  .[, HIscreen := ifelse(TEMP <= 40, "T", ifelse({
    -0.3 + 1.1 * TEMP + 0.047 * RH
  } < 79, "A", "B"))] %>%
  .[, HI := ifelse(HIscreen == "B", {
    -42.379 + 2.04901523 * TEMP + 10.14333127 * RH - .22475541 * TEMP * RH - .00683783 *
      TEMP * TEMP - .05481717 * RH * RH + .00122874 * TEMP * TEMP * RH + .00085282 *
      TEMP * RH * RH - .00000199 * TEMP * TEMP * RH * RH
  }, TEMP)] %>%
  .[, HIfinal := ifelse(HIscreen == "T", TEMP,
                      ifelse(
                        HIscreen == "A", {-0.3 + 1.1 * TEMP + 0.047 * RH},
                          ifelse(RH <= 13 & TEMP >= 80 & TEMP <= 112,  HI - (((13 - RH) / 4) * ((17 - abs(TEMP - 95)) / 17)^0.5),
                            ifelse(RH > 85 & TEMP >= 80 & TEMP <= 87, HI + 0.02 * (RH - 85) * (87 - TEMP), HI)
                          )
                        )
                      )] 


HI %>% ggplot() + geom_histogram(aes(x = HIfinal),binwidth = 1)


HI %>% ggplot() + geom_point(aes(x = TEMP, y=RH, color = HIfinal), alpha=0.1) + scale_color_gradient2(mid = "orange" ,high = "red")

max(HI$TEMP, na.rm = T)
# 
# HI[is.na(HIadjusted)] %>% fwrite("//phdeorlcsrvip01/Crossbranch/OHE-EHIB-WILDFIRE/HMS4Caitlin/HeatIndexErrors.csv")

HI[HIadjusted >150] %>% fwrite("//phdeorlcsrvip01/Crossbranch/OHE-EHIB-WILDFIRE/HMS4Caitlin/HeatIndexOutliers.csv")

dates <-as.integer(format(
  seq(
    as.Date("20150101", format = "%Y%m%d"),
    as.Date("20171231", format = "%Y%m%d"),
    by = "day"
  ), format = "%Y%m%d"
))


Caitlin <-
  smokeFile[date %in% dates & STATEFP == 6 & COUNTYFP %in% c(1, 5, 9, 13, 43, 45, 55, 75, 79,81, 83, 95, 97, 99, 111)] %>% 
  merge(geoids, by = c("STATEFP","COUNTYFP","TRACTCE")) %>%
  replace_na(list(
    light = 0,
    medium = 0,
    heavy = 0
  )) %>% 
  na.omit() %>%
  .[, .(
    lightSmoke = max(light, na.rm = T),
    mediumSmoke = max(medium, na.rm = T),
    heavySmoke = max(heavy, na.rm = T)
  ), by = .(date, geoid10)] %>%
  .[, maxSmoke := ifelse(heavySmoke == 1,
                         "heavy",
                         ifelse(mediumSmoke == 1,
                               "medium",
                                ifelse(lightSmoke ==
                                         1, "light", "none")))]


setkey(Caitlin, date, geoid10)

fullGrid <- data.table({
  expand.grid(date = as.integer(format(
    seq(
      as.Date("20150101", format = "%Y%m%d"),
      as.Date("20171231", format = "%Y%m%d"),
      by = "day"
    ), format = "%Y%m%d"
  )),
  geoid10 = unique(Caitlin$geoid10))
}) %>% .[, .(date = date,
             geoid10 = as.double(geoid10))]

setkey(fullGrid, date, geoid10)

smokeDays <- merge(fullGrid, Caitlin, all.x = TRUE) %>% 
  replace_na(list(
    lightSmoke = 0,
    mediumSmoke = 0,
    heavySmoke = 0,
    maxSmoke = "none"
  )) %>%
  fwrite("//phdeorlcsrvip01/Crossbranch/OHE-EHIB-WILDFIRE/HMS4Caitlin/rawDayTract4Caitlin12-14.csv")



  .[,.(year=year(as.Date(as.character(date), "%Y%m%d")),
       geoid, anySmoke = maxSmoke)] %>%
  .[,.(smokeDays =sum(anySmoke, na.rm = T)), by = .(year, geoid)] 
  
  
  
shape <- inner_join(CAzips, smokeDays)

foo <- fread("//phdeorlcsrvip01/Crossbranch/OHE-EHIB-WILDFIRE/HMS4Caitlin/rawDayTract4Caitlin12-6.csv")
bar <- smokeDays[geoid10 =="6097152701" & date =="20150625"]

bar


bar[date =="20150625"]



library(leaflet)

sd2 <- smokeDays %>% dcast.data.table(formula = ZCTA5CE10 ~ year) 
names(sd2) <- c("ZCTA5CE10", "Y2015", "Y2016", "Y2017")

shape <- inner_join(CAzips, sd2)

shape  %>% st_write("C:/Users/jvargo/Downloads/2015_17SmokeDays.geojson")

pal <- colorNumeric(
  palette = "YlOrRd",
  domain =  0:70
)



shape %>%
  leaflet()  %>%
  # addProviderTiles(providers$CartoDB.Voyager) %>%
  addTiles()%>%
addPolygons(
  color = "#444444",
  weight = 1,
  smoothFactor = 0.1,
  fillOpacity = 0.6,
  fillColor = ~ pal(Y2015),
  highlightOptions = highlightOptions(color = "white", weight = 2,
                                      bringToFront = TRUE),
  popup = ~paste0("This is zip ", ZCTA5CE10 ," in 2015 it experienced ", Y2015," days of wildfire smoke."),
  group="2015 Smoke Days")  %>%
  addPolygons(
    color = "#444444",
    weight = 1,
    smoothFactor = 0.1,
    fillOpacity = 0.6,
    fillColor = ~ pal(Y2016),
    highlightOptions = highlightOptions(color = "white", weight = 2,
                                        bringToFront = TRUE),
    popup = ~paste0("This is zip ", ZCTA5CE10 ," in 2016 it experienced ", Y2016," days of wildfire smoke."),
                    group="2016 Smoke Days")  %>%
  addPolygons(
    color = "#444444",
    weight = 1,
    smoothFactor = 0.1,
    fillOpacity = 0.6,
    fillColor = ~ pal(Y2017),
    highlightOptions = highlightOptions(color = "white", weight = 2,
                                        bringToFront = TRUE),
    popup = ~paste0("This is zip ", ZCTA5CE10 ," in 2017 it experienced ", Y2017," days of wildfire smoke."),
                    group="2017 Smoke Days")  %>%
  addLayersControl(
    baseGroups =  c("2015 Smoke Days","2016 Smoke Days","2017 Smoke Days"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  addLegend("bottomright", pal = pal, values = ~Y2017,
              title = "Days of Smoke",
              labFormat = labelFormat(suffix = " days", transform = as.integer),
              opacity = .5
  )





  
  # fwrite("//phdeorlcsrvip01/crossbranch/OHE-EHIB-WILDFIRE/HMS4Caitlin/rawSmoke4Caitlin_15counties.csv")


# %>% .[,.(date, zip = ZCTA5CE10, maxSmoke)] %>%
#   dcast.data.table(date ~ zip, value.var = "maxSmoke") %>% fwrite("~/tempZip/maxSmoke4Caitlin.csv")

##########  plot anomolies ###############
averages <- merge(dateList, # list of all dates
              {
                smokeFile[, .(
                  lightPD = sum(POPULATION * light, na.rm = T), 
                  mediumPD = sum(POPULATION * medium, na.rm = T), 
                  heavyPD = sum(POPULATION * heavy, na.rm = T)
                ), by = .(date, STATEFP, COUNTYFP)] # make Person_day sums by day
              },
              by = c("date"), all.x = TRUE) %>% # finish merge with full date list
  .[, .(date = as.Date(as.character(date), "%Y%m%d"),
        STATEFP, 
        COUNTYFP,
        lightPD, 
        mediumPD, 
        heavyPD)] %>% # make 'date' an actual date 
  replace_na(list(
    lightPD = 0, 
    mediumPD = 0,
    heavyPD = 0
  )) %>%  # take care of zeros
  .[, `:=` (year = year(date),
            julian = yday(date),
            week = week(date),
            month = month(date),
            day = day(date))] %>%  # create new fields for day and year specifics
  melt.data.table(id.vars = c("date", "year", "month", "STATEFP","COUNTYFP"), measure.vars = c("lightPD", "mediumPD", "heavyPD")) %>% # gather the smoke levels
  # .[,average := mean(value, na.rm = T), by=.(week, variable)] 
  dcast.data.table(formula = month + STATEFP + COUNTYFP + variable ~ year, value.var = "value", fun = mean,  fill = 0) %>% 
  .[, .(average = mean(c(`2010`,`2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`, `2018`), na.rm = T)), by = .(month, variable, STATEFP, COUNTYFP)]  # calculate average for that week across all years

plotData <- merge(dateList, # list of all dates
      {
        smokeFile[, .(
          lightPD = sum(POPULATION * light, na.rm = T), 
          mediumPD = sum(POPULATION * medium, na.rm = T), 
          heavyPD = sum(POPULATION * heavy, na.rm = T)
        ), by = .(date, STATEFP, COUNTYFP)] # make Person_day sums by day
      },
      by = c("date"), all.x = TRUE) %>% # finish merge with full date list
  .[, .(date = as.Date(as.character(date), "%Y%m%d"),
        STATEFP, 
        COUNTYFP,
        lightPD, 
        mediumPD, 
        heavyPD)] %>% # make 'date' an actual date 
  replace_na(list(
    lightPD = 0, 
    mediumPD = 0,
    heavyPD = 0
  )) %>%  # take care of zeros
  .[, `:=` (year = year(date),
            julian = yday(date),
            week = week(date),
            month = month(date),
            day = day(date))] %>%  # create new fields for day and year specifics
  melt.data.table(id.vars = c("date", "year", "month", "STATEFP","COUNTYFP"), measure.vars = c("lightPD", "mediumPD", "heavyPD"))

plotData <- merge(plotData, averages)


# check Chicago numbers http://www.epa.state.il.us/air/pm25/index.html
# arb <- plotData[STATEFP == 17L & COUNTYFP == 31L, .(diff= sum(value - average, na.rm = T)), by= .(date, variable)]
arb <- plotData[, .(diff= sum(value - average, na.rm = T),
                    total = sum(value, na.rm = T)), by= .(date, variable)]
setkey(arb, date)

plotData[, `:=` (year = year(date))] %>%
  .[, .(diff= sum(value - average, na.rm = T),
        total = sum(value, na.rm = T)), by= .(year, STATEFP, variable)] %>% 
  fwrite("~/GitHub/WFSmokeExp/anomoly2.csv")





# the difference between the number of people exposed in each county on a given day and the historical average for that week and place 
ggplotly({
  ggplot(arb) +
    geom_area(aes(x = date, y = diff, fill = variable),
              stat = "identity",
              position = "stack") +
    scale_fill_manual(values = c("yellow", "orange", "red")) +
    facet_grid(variable ~ ., scales = "free_y") + 
    geom_hline(
      yintercept = 0,
      color = "gray40",
      size = 0.3,
      alpha = 0.4
    ) +
    theme_minimal()
})

############# finish for Caitlin ############# 
############# finish for EHIB ############# 

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

CA <- rbindlist(lapply(X = c(2011:2018), FUN = crunchDayCA)) 


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

