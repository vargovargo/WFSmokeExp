
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


#
############ create pct_exposed_by_year_state table ##################
#

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
  merge(pop, by = "STATEFP",all.x=T)
  

bind_rows({na.omit(ExposedPOP.stateYear)[, .(lightPOP = sum(lightPOP, na.rm=T),
                         mediumPOP = sum(mediumPOP, na.rm=T),
                         heavyPOP = sum(heavyPOP, na.rm=T),
                         totalPOP = sum(totalPOP, na.rm=T), 
                         STATEFP = 999, 
                         stateName = "US",
                         ST = "US",
                         charST = "999"), by = .(year)] %>% .[,.(STATEFP, year, lightPOP, mediumPOP, heavyPOP, totalPOP, stateName, ST, charST)]
}, ExposedPOP.stateYear) %>%
  .[,.(state_name = stateName,
       year = year,
       light = lightPOP/totalPOP,
       medium = mediumPOP/totalPOP,
       heavy = heavyPOP/totalPOP, 
       total_pop = totalPOP
       )] %>% 
  melt.data.table(id.vars = c("state_name", "year", "total_pop"), variable.name = "smoke_level", value.name = "pct_exposed") %>% na.omit() %>%
  fwrite("~/MMWR/pct_exposed_by_year_state.csv")


#
############ create persondays_by_year_state table ##################
#

pds.stateYear  <- smokeFile[,year := year(date)] %>% 
  replace_na(list(
    light = 0,
    medium = 0,
    heavy = 0
  )) %>%
  .[,.(lightdays = sum(light*POPULATION, na.rm = T),
       mediumdays = sum(medium*POPULATION, na.rm = T),
       heavydays = sum(heavy*POPULATION, na.rm = T)), 
    by=.(STATEFP, year)] %>% 
  merge(pop, by = "STATEFP",all.x=T)

bind_rows({na.omit(pds.stateYear)[, .(lightdays = sum(lightdays, na.rm=T),
                                      mediumdays = sum(mediumdays, na.rm=T),
                                      heavydays = sum(heavydays, na.rm=T),
                                    totalPOP = sum(totalPOP, na.rm=T), 
                                    STATEFP = 999, 
                                    stateName = "US",
                                    ST = "US",
                                    charST = "999"), by = .(year)] %>% .[,.(STATEFP, year, lightdays, mediumdays, heavydays, totalPOP, stateName, ST, charST)]
}, pds.stateYear) %>%
  .[,.(state_name = stateName,
       year = year,
       light = lightdays,
       medium = mediumdays,
       heavy = heavydays, 
       total_pop = totalPOP
  )] %>% 
  melt.data.table(id.vars = c("state_name", "year", "total_pop"), variable.name = "smoke_level", value.name = "persondays") %>% na.omit() %>%
  fwrite("~/MMWR/persondays_by_year_state.csv")



#
############ create persondays_by_svi_raw table ##################
#

tractAnnual<- 
  smokeFile[,year := year(date)] %>% 
  .[, .(
    light = sum(light * POPULATION, na.rm = T),
    medium = sum(medium * POPULATION, na.rm = T),
    heavy = sum(heavy * POPULATION, na.rm = T)
  ), by = .(year, STATEFP, COUNTYFP, TRACTCE)] %>% merge(tableListTract)



svi <- fread("~/MMWR/SVI2010_US.csv")[,.(GEO_ID, STATEFP = STATE_FIPS, COUNTYFP = CNTY_FIPS, TRACTCE = TRACT , STCOFIPS, FIPS,STATE_ABBR,STATE_NAME, COUNTY,TOTPOP , R_PL_THEME1, R_PL_THEME2, R_PL_THEME3, R_PL_THEME4, R_PL_THEMES)]

merged <- merge(tractAnnual, svi) # %>% fwrite("./tempZip/FullSVItracts.csv")



# function to assign quartiles
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
}) %>% fwrite("~/MMWR/persondays_by_svi_raw.csv")



#
############ create persondays_by_svi_indicators_raw table ##################
#

tractAnnual<- 
  smokeFile[,year := year(date)] %>% 
  .[, .(
    light = sum(light * POPULATION, na.rm = T),
    medium = sum(medium * POPULATION, na.rm = T),
    heavy = sum(heavy * POPULATION, na.rm = T)
  ), by = .(year, STATEFP, COUNTYFP, TRACTCE)] %>% merge(tableListTract)



svi <- fread("~/MMWR/SVI2010_US.csv")[,.(GEO_ID, STATEFP = STATE_FIPS, COUNTYFP = CNTY_FIPS, TRACTCE = TRACT , STCOFIPS, FIPS,STATE_ABBR,STATE_NAME, COUNTY, TOTPOP , PL_MINORITY, E_PL_LIMENG, E_PL_MUNIT, E_PL_MOBILE, E_PL_CROWD, E_PL_NOVEH, PL_GROUPQ)]

merged <- merge(tractAnnual, svi) # %>% fwrite("./tempZip/FullSVItracts.csv")

# function to assign quartiles
quartify <- function(values){
  
  foo <- ifelse(values == -999, "missing", 
                ifelse(values >= 0 & values <= .25, "1 least vulnerable",
                       ifelse(values > .25 & values <= .5, "2 percentile 25-50",
                              ifelse(values > 0.5 & values <= .75,"3 percentile 50-75",
                                     ifelse(values > .75 & values <= 1, "4 most vulnerable","missing")))))
  
  return(foo)
  
}


westStates <- c("California","Oregon","Washington")

minority <- merged[PL_MINORITY != -999, .(quartile = quartify(PL_MINORITY), light, medium, heavy, STATE_NAME, year)] %>%
  .[, .(
    light = sum(light, na.rm = T),
    medium = sum(medium, na.rm = T),
    heavy = sum(heavy, na.rm = T)
  ), by = . (quartile, year, STATE_NAME)] %>%
  melt.data.table(id.vars = c("quartile", "year", "STATE_NAME"), variable.name = "smoke_level")  %>% mutate(svi_theme = "minority_language", 
                                                                                                            indicator = "minority",
                                                                                                            region = ifelse(STATE_NAME %in% westStates,  "west", "not west")) %>% data.table()


limeng <- merged[E_PL_LIMENG  != -999, .(quartile = quartify(E_PL_LIMENG ), light, medium, heavy, STATE_NAME, year)] %>%
  .[, .(
    light = sum(light, na.rm = T),
    medium = sum(medium, na.rm = T),
    heavy = sum(heavy, na.rm = T)
  ), by = . (quartile, year, STATE_NAME)] %>%
  melt.data.table(id.vars = c("quartile", "year", "STATE_NAME"), variable.name = "smoke_level")  %>% mutate(svi_theme = "minority_language", 
                                                                                                            indicator = "limeng",
                                                                                                            region = ifelse(STATE_NAME %in% westStates,  "west", "not west")) %>% data.table()


munit <- merged[E_PL_MUNIT  != -999, .(quartile = quartify(E_PL_MUNIT ), light, medium, heavy, STATE_NAME, year)] %>%
  .[, .(
    light = sum(light, na.rm = T),
    medium = sum(medium, na.rm = T),
    heavy = sum(heavy, na.rm = T)
  ), by = . (quartile, year, STATE_NAME)] %>%
  melt.data.table(id.vars = c("quartile", "year", "STATE_NAME"), variable.name = "smoke_level")  %>% mutate(svi_theme = "housing_transportation", 
                                                                                                            indicator = "munit",
                                                                                                            region = ifelse(STATE_NAME %in% westStates,  "west", "not west")) %>% data.table()

mobile <- merged[E_PL_MOBILE  != -999, .(quartile = quartify(E_PL_MOBILE ), light, medium, heavy, STATE_NAME, year)] %>%
  .[, .(
    light = sum(light, na.rm = T),
    medium = sum(medium, na.rm = T),
    heavy = sum(heavy, na.rm = T)
  ), by = . (quartile, year, STATE_NAME)] %>%
  melt.data.table(id.vars = c("quartile", "year", "STATE_NAME"), variable.name = "smoke_level")  %>% mutate(svi_theme = "housing_transportation", 
                                                                                                            indicator = "mobile",
                                                                                                            region = ifelse(STATE_NAME %in% westStates,  "west", "not west")) %>% data.table()


crowd <- merged[E_PL_CROWD  != -999, .(quartile = quartify(E_PL_CROWD), light, medium, heavy, STATE_NAME, year)] %>%
  .[, .(
    light = sum(light, na.rm = T),
    medium = sum(medium, na.rm = T),
    heavy = sum(heavy, na.rm = T)
  ), by = . (quartile, year, STATE_NAME)] %>%
  melt.data.table(id.vars = c("quartile", "year", "STATE_NAME"), variable.name = "smoke_level")  %>% mutate(svi_theme = "housing_transportation", 
                                                                                                            indicator = "crowd",
                                                                                                            region = ifelse(STATE_NAME %in% westStates,  "west", "not west")) %>% data.table()


noveh <- merged[E_PL_NOVEH  != -999, .(quartile = quartify(E_PL_NOVEH), light, medium, heavy, STATE_NAME, year)] %>%
  .[, .(
    light = sum(light, na.rm = T),
    medium = sum(medium, na.rm = T),
    heavy = sum(heavy, na.rm = T)
  ), by = . (quartile, year, STATE_NAME)] %>%
  melt.data.table(id.vars = c("quartile", "year", "STATE_NAME"), variable.name = "smoke_level")  %>% mutate(svi_theme = "housing_transportation", 
                                                                                                            indicator = "noveh",
                                                                                                            region = ifelse(STATE_NAME %in% westStates,  "west", "not west")) %>% data.table()

groupq <- merged[PL_GROUPQ  != -999, .(quartile = quartify(PL_GROUPQ), light, medium, heavy, STATE_NAME, year)] %>%
  .[, .(
    light = sum(light, na.rm = T),
    medium = sum(medium, na.rm = T),
    heavy = sum(heavy, na.rm = T)
  ), by = . (quartile, year, STATE_NAME)] %>%
  melt.data.table(id.vars = c("quartile", "year", "STATE_NAME"), variable.name = "smoke_level")  %>% mutate(svi_theme = "housing_transportation", 
                                                                                                            indicator = "groupq",
                                                                                                            region = ifelse(STATE_NAME %in% westStates,  "west", "not west")) %>% data.table()




QITable <- data.table(bind_rows(minority, limeng, munit, mobile, crowd, noveh, groupq))


QITable[quartile != "missing", .(billion_person_days = sum(as.numeric(value))/1000000000), by = .(smoke_level, quartile, svi_theme, indicator)] %>% 
  ggplot() + geom_bar(stat="identity",position = "dodge",aes(x=indicator, y=billion_person_days, fill=quartile)) +facet_grid(smoke_level ~ ., scales="free_y")


QITable[quartile != "missing", .(billion_person_days = sum(as.numeric(value))/1000000000), by = .(smoke_level, quartile, svi_theme, indicator, region)] %>% 
  ggplot() + geom_bar(stat="identity",position = "stack",aes(x=quartile, y=billion_person_days, fill=region)) +facet_grid( svi_theme + indicator ~ smoke_level, scales="free_x") +coord_flip()


bind_rows({
  QITable[quartile != "missing", .(region = "US", billion_person_days = sum(as.numeric(value))/1000000000), by = .(smoke_level, quartile, svi_theme, indicator)]
},
{
  QITable[quartile != "missing", .(billion_person_days = sum(as.numeric(value))/1000000000), by = .(smoke_level, quartile, svi_theme, indicator, region)]
}) %>% fwrite("~/MMWR/persondays_by_svi_indicators_raw.csv")

