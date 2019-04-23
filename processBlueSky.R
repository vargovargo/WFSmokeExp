
#################################################
#################################################
# 
# BEGIN BLUE SKY 
#
#################################################
#################################################


# install.packages("ncdf4")
library(ncdf4)
library(tidyverse)
library(sf)
library(parallel)
library(R.utils)
library(lwgeom)
library(RCurl)
library(data.table)

# 
# DIR <- tempdir()
# setwd(DIR)

dname = "PM25"
tname = "TFLAG"

CAgridSub <-
  fread("/mnt/projects/ohe/WFSmokeProcessing/blueSkyCApoints.csv") %>% .[, .(row, col)]

processBS <- function(BSday) {
  
  downloadDay <- BSday
  switchdate <- as.integer(downloadDay)
  
  DIR <- tempdir()
  setwd(tempdir())
  
  
  if (url.exists(
    paste0(
      "https://smoke.airfire.org/bluesky-daily/output/hysplit-pp/CANSAC-2km/CANSAC-2km-",
      downloadDay,
      "00.tgz"
    )
  )) {
    
    download.file(
      paste0(
        "https://smoke.airfire.org/bluesky-daily/output/hysplit-pp/CANSAC-2km/CANSAC-2km-",
        downloadDay,
        "00.tgz"
      ),
      destfile = "tempzip.tgz"
    )
    
    untar(tarfile =  "tempzip.tgz")
    
    path <-
      if (length(list.files(
        list.files(DIR, full.names = T, pattern = downloadDay),
        pattern = "forecast",
        full.names = T
      )) != 0) {
        list.files(
          list.files(
            list.files(
              list.files(DIR, full.names = T, pattern = downloadDay),
              pattern = "forecast",
              full.names = T
            ),
            pattern = "data",
            full.names = T
          ),
          pattern = ".nc",
          full.names = T
        )
      } else {
        list.files(
          list.files(
            list.files(DIR, full.names = T, pattern = downloadDay),
            pattern = "data",
            full.names = T
          ),
          pattern = ".nc",
          full.names = T
        )
      }
    
    ncin <- nc_open(path)
    # store values from variables and atributes
    # attributes(ncin$dim)$names
    
    global <- ncatt_get(ncin, varid = 0)
    nc_time <- ncvar_get(ncin, "TFLAG")
    
    # get hectares
    
    pm_array <- ncvar_get(ncin, dname)
    
    n_col <- dim(pm_array)[1]
    n_row <- dim(pm_array)[2]
    n_time <- dim(pm_array)[3]
    
    nc_close(ncin)
    #
    # fillvalue <- ncatt_get(ncin,dname,"_FillValue")
    # pm_array[pm_array==0L] <- NA
    
    bar <-  CJ(row = 1:551, col = 1:651) %>%
      .[, c("lat", "lon", "dateTXT") := list(global$YORIG + (global$YCELL * (row - 1)),
                                             global$XORIG + (global$XCELL * (col - 1)),
                                             downloadDay)] %>%
      cbind(data.table(matrix(
        as.vector(pm_array),
        nrow = n_col * n_row,
        ncol = n_time
      ))) %>% merge(CAgridSub) %>%
      melt.data.table(
        id.vars = c("row", "col", "lat", "lon", "dateTXT"),
        measure.vars = c(paste0("V", seq(1:24))),
        value.name = "PM25",
        variable.name = "hour"
      ) %>%
      .[, .(
        dailyMin = min(PM25, na.rm = T),
        dailyMax = max(PM25, na.rm = T),
        dailyMean = mean(PM25, na.rm = T)
      ), by = .(lat, lon, dateTXT)] %>%
      fwrite(paste0("/mnt/projects/ohe/WFSmokeProcessing/BSdata/rawDaily/",switchdate, "_blueSkyDaily.csv"))
    
  } else if(url.exists(
    paste0(
      "https://smoke.airfire.org/bluesky-daily/output/hysplit-pp/CANSAC-2km/CANSAC-2km-",
      as.character(switchdate-1),
      "00.tgz"
    )
  )) {
    
    dayBefore <- as.character(switchdate-1)
    
    
    download.file(
      paste0(
        "https://smoke.airfire.org/bluesky-daily/output/hysplit-pp/CANSAC-2km/CANSAC-2km-",
        dayBefore,
        "00.tgz"
      ),
      destfile = "tempzip.tgz"
    )
    
    untar(tarfile =  "tempzip.tgz")
    
    path <-
      if (length(list.files(
        list.files(DIR, full.names = T, pattern = dayBefore),
        pattern = "forecast",
        full.names = T
      )) != 0) {
        list.files(
          list.files(
            list.files(
              list.files(DIR, full.names = T, pattern = dayBefore),
              pattern = "forecast",
              full.names = T
            ),
            pattern = "data",
            full.names = T
          ),
          pattern = ".nc",
          full.names = T
        )
      } else {
        list.files(
          list.files(
            list.files(DIR, full.names = T, pattern = dayBefore),
            pattern = "data",
            full.names = T
          ),
          pattern = ".nc",
          full.names = T
        )
      }
    
    ncin <- nc_open(path)
    # store values from variables and atributes
    # attributes(ncin$dim)$names
    
    global <- ncatt_get(ncin, varid = 0)
    nc_time <- ncvar_get(ncin, "TFLAG")
    
    # get hectares
    
    pm_array <- ncvar_get(ncin, dname)
    
    n_col <- dim(pm_array)[1]
    n_row <- dim(pm_array)[2]
    n_time <- dim(pm_array)[3]
    
    nc_close(ncin)
    #
    # fillvalue <- ncatt_get(ncin,dname,"_FillValue")
    # pm_array[pm_array==0L] <- NA
    
    bar <-  CJ(row = 1:551, col = 1:651) %>%
      .[, c("lat", "lon", "dateTXT") := list(global$YORIG + (global$YCELL * (row - 1)),
                                             global$XORIG + (global$XCELL * (col - 1)),
                                             downloadDay)] %>%
      cbind(data.table(matrix(
        as.vector(pm_array),
        nrow = n_col * n_row,
        ncol = n_time
      ))) %>% merge(CAgridSub) %>%
      melt.data.table(
        id.vars = c("row", "col", "lat", "lon", "dateTXT"),
        measure.vars = c(paste0("V", seq(25:48))),
        value.name = "PM25",
        variable.name = "hour"
      ) %>%
      .[, .(
        dailyMin = min(PM25, na.rm = T),
        dailyMax = max(PM25, na.rm = T),
        dailyMean = mean(PM25, na.rm = T)
      ), by = .(lat, lon, dateTXT)] %>%
      fwrite(paste0("/mnt/projects/ohe/WFSmokeProcessing/BSdata/rawDaily/",switchdate, "_blueSkyDaily.csv"))
  } else {
    
    twoBefore <- as.character(switchdate-2)
    
    
    download.file(
      paste0(
        "https://smoke.airfire.org/bluesky-daily/output/hysplit-pp/CANSAC-2km/CANSAC-2km-",
        twoBefore,
        "00.tgz"
      ),
      destfile = "tempzip.tgz"
    )
    
    untar(tarfile =  "tempzip.tgz")
    
    path <-
      if (length(list.files(
        list.files(DIR, full.names = T, pattern = twoBefore),
        pattern = "forecast",
        full.names = T
      )) != 0) {
        list.files(
          list.files(
            list.files(
              list.files(DIR, full.names = T, pattern = twoBefore),
              pattern = "forecast",
              full.names = T
            ),
            pattern = "data",
            full.names = T
          ),
          pattern = ".nc",
          full.names = T
        )
      } else {
        list.files(
          list.files(
            list.files(DIR, full.names = T, pattern = twoBefore),
            pattern = "data",
            full.names = T
          ),
          pattern = ".nc",
          full.names = T
        )
      }
    
    ncin <- nc_open(path)
    # store values from variables and atributes
    # attributes(ncin$dim)$names
    
    global <- ncatt_get(ncin, varid = 0)
    nc_time <- ncvar_get(ncin, "TFLAG")
    
    # get hectares
    
    pm_array <- ncvar_get(ncin, dname)
    
    n_col <- dim(pm_array)[1]
    n_row <- dim(pm_array)[2]
    n_time <- dim(pm_array)[3]
    
    nc_close(ncin)
    #
    # fillvalue <- ncatt_get(ncin,dname,"_FillValue")
    # pm_array[pm_array==0L] <- NA
    
    bar <-  CJ(row = 1:551, col = 1:651) %>%
      .[, c("lat", "lon", "dateTXT") := list(global$YORIG + (global$YCELL * (row - 1)),
                                             global$XORIG + (global$XCELL * (col - 1)),
                                             downloadDay)] %>%
      cbind(data.table(matrix(
        as.vector(pm_array),
        nrow = n_col * n_row,
        ncol = n_time
      ))) %>% merge(CAgridSub) %>%
      melt.data.table(
        id.vars = c("row", "col", "lat", "lon", "dateTXT"),
        measure.vars = c(paste0("V", seq(49:71))),
        value.name = "PM25",
        variable.name = "hour"
      ) %>%
      .[, .(
        dailyMin = min(PM25, na.rm = T),
        dailyMax = max(PM25, na.rm = T),
        dailyMean = mean(PM25, na.rm = T)
      ), by = .(lat, lon, dateTXT)] %>%
      fwrite(paste0("/mnt/projects/ohe/WFSmokeProcessing/BSdata/rawDaily/",switchdate, "_blueSkyDaily.csv"))
  }
  
  unlink(paste0(tempdir(),'/*'), recursive = T )
  
}


# processBS("20160101")


dateListBlueSky <- format(seq(as.Date("2016/01/01"),as.Date("2016/04/02"), by = "day"),"%Y%m%d")
lapply(dateListBlueSky, FUN = processBS)

