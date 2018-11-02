########### function to combine ############

# when combining several 
rm(list = ls())

library(tidyverse)
library(sf)
library(data.table)
library(scales)
library(lubridate)
library(parallel)

stateKey <- fread("~/GitHub/WFSmokeExp/stateKey.csv") %>%
  mutate(charST = ifelse(stFIPS<10, paste0("0",stFIPS), as.character(stFIPS)))
# 
# BG_list <- lapply(stateKey$charST, function(x) paste0("https://www2.census.gov/geo/docs/reference/cenpop2010/blkgrp/CenPop2010_Mean_BG",x,".txt"))
# 
# tableList <- lapply(BG_list, fread)

tableList <- fread("https://www2.census.gov/geo/docs/reference/cenpop2010/blkgrp/CenPop2010_Mean_BG.txt")

popCentroidsBG <- st_as_sf(tableList, coords = c("LONGITUDE", "LATITUDE"), crs = 4326) 

directory <- "~/data/US_BG/RDS"
allBGs <- tableList[, geoIDer := paste0(STATEFP,"_",COUNTYFP,"_",TRACTCE,"_",BLKGRPCE)]
allBGs <- copy(allBGs)
setkey(allBGs, STATEFP, COUNTYFP, geoIDer)

# convert <- function(RDSfile){
#   
#   fwrite(readRDS(RDSfile), paste0("~/data/US_BG/CSV/", sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(RDSfile)),".csv"))
#   
# }

# mclapply(list.files(directory,  full.names = T), convert)


############  update smokeFile ###################
# smokeFile <- rbindlist(mclapply(list.files("~/data/US_BG/CSV/",full.names = T), fread), fill = T) 
# smokeFile %>% fwrite("~/data/smokeFile.csv" )
# smokeFile %>% saveRDS("~/data/smokeFile.rds" )
# smokeFile[STATEFP ==6L] %>% fwrite("~/data/CAsmokeFile.csv" )
# smokeFile[STATEFP ==6L] %>% fwrite(//10.226.226.173/R-Dev/ohe/SmokeExposures/CAsmokeFile.csv)
 
# replace_na(smokeFile, list(light = 0, medium = 0, heavy= 0)) %>% .[, .(light = sum(light * POPULATION, na.rm=T),
#                                                                        medium = sum(medium * POPULATION, na.rm=T),
#                                                                        heavy = sum(heavy * POPULATION, na.rm=T),
#                                                                        POPULATION = sum(POPULATION, na.rm=T)), by=.(date, STATEFP, COUNTYFP)] %>%
#   fwrite("~/data/USsmokeFile.csv")





smokeFile <- fread("~/data/smokeFile.csv")
setkey(smokeFile, date, STATEFP, COUNTYFP, TRACTCE, BLKGRPCE) 
smokeFile <-copy(smokeFile)

start <- as.character(min(unique(smokeFile$date)))
end <- as.character(max(unique(smokeFile$date)))


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
                  measure.vars = c("lightPD", "mediumPD", "heavyPD")) %>% 
  ggplot() + 
  geom_tile(aes(x=year, y = julian, fill=value, alpha = value)) +
  scale_fill_gradient(low = "gray0", high = "red")  +
  guides(alpha=FALSE) +
  facet_grid(.~ variable) + theme_minimal()


smoke2 <- smokeFile[,date2 := as.Date(as.character(date),"%Y%m%d")] 
key(smoke2)


ExposedPOP.stateYear  <- smoke2[,year := year(date2)] %>% 
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
        heavyPOP = sum(POP*heavydays),
        totalPOP = sum(POP)), by =.(STATEFP, year)] # %>%
# fwrite("~/tempZip/ExposedPOP_stateYear.csv")

ExposedPOP.stateYear %>% 
  ggplot() + geom_area(aes(x= year, y= heavyPOP/totalPOP, fill=factor(STATEFP))) 



############# gather data for Caitlin ############# 
############# combine with zips for EHIB ############# 

CAzips <- st_read("~/GitHub/WFSmokeExp_US/CAzips.shp") %>% st_transform(crs = 4326) 
CAzipsBG <-st_intersection({popCentroidsBG %>% filter(STATEFP == 6, COUNTYFP %in% c(1, 5, 9, 13, 43, 45, 55, 75, 79,81, 83, 95, 97, 99, 111))}, CAzips)
st_geometry(CAzipsBG) <- NULL

Caitlin <-
  smokeFile[.(c(20150101:20171231), 6L)] %>% merge(select(CAzipsBG, STATEFP, COUNTYFP, TRACTCE, BLKGRPCE, ZCTA5CE10)) %>% replace_na(list(
    light = 0,
    medium = 0,
    heavy = 0
  )) %>%
  .[, .(
    lightSmoke = max(light, na.rm = T),
    mediumSmoke = max(medium, na.rm = T),
    heavySmoke = max(heavy, na.rm = T)
  ), by = .(date, ZCTA5CE10)] %>%
  .[, maxSmoke := factor(ifelse(
    heavySmoke == 1,
    "heavy",
    ifelse(
      mediumSmoke == 1,
      "medium",
      ifelse(lightSmoke ==
               1, "light", "none")
    )
  ),
  levels = c("none", "light", "medium", "heavy"))]

setkey(Caitlin, date, ZCTA5CE10)

fullGrid <- data.table({
  expand.grid(date = as.integer(format(seq(as.Date("20150101", format ="%Y%m%d" ),as.Date("20171231", format ="%Y%m%d"), by = "day"), format ="%Y%m%d")), ZCTA5CE10 = unique(CAzipsBG$ZCTA5CE10))
})

setkey(fullGrid, date, ZCTA5CE10)

merge(fullGrid, Caitlin, by = c("date","ZCTA5CE10"), all.x = TRUE ) %>% 
  replace_na(list(
    lightSmoke = 0,
    mediumSmoke = 0,
    heavySmoke = 0,
    maxSmoke = "none"
  ))  %>% fwrite("//phdeorlcsrvip01/crossbranch/OHE-EHIB-WILDFIRE/HMS4Caitlin/rawSmoke4Caitlin_15counties.csv")


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
  melt.data.table(id.vars = c("date", "year", "week", "STATEFP","COUNTYFP"), measure.vars = c("lightPD", "mediumPD", "heavyPD")) %>% # gather the smoke levels
  # .[,average := mean(value, na.rm = T), by=.(week, variable)] 
  dcast.data.table(formula = week + STATEFP + COUNTYFP + variable ~ year, value.var = "value", fun = mean,  fill = 0) %>% 
  .[, .(average = mean(c(`2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`), na.rm = T)), by = .(week, variable, STATEFP, COUNTYFP)]  # calculate average for that day across all years

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
  melt.data.table(id.vars = c("date", "year", "week", "STATEFP","COUNTYFP"), measure.vars = c("lightPD", "mediumPD", "heavyPD"))

plotData <- merge(plotData, averages)


# check Chicago numbers http://www.epa.state.il.us/air/pm25/index.html
# arb <- plotData[STATEFP == 17L & COUNTYFP == 31L, .(diff= sum(value - average, na.rm = T)), by= .(date, variable)]
arb <- plotData[, .(diff= sum(value - average, na.rm = T)), by= .(date, variable, STATEFP)]


# the difference between the number of people exposed in each county on a given day and the historical average for that week and place 
ggplotly({
  ggplot(arb) +
    geom_area(aes(x = date, y = diff, fill = variable),
              stat = "identity",
              position = "stack") +
    facet_wrap( ~ STATEFP) +
    scale_fill_manual(values = c("yellow", "orange", "red")) +
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

