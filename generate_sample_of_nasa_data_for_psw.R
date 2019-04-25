library(data.table)
library(tidyverse)

setwd("~/nasa_aq_data_fusion/csv/")

####################################################
############## Isolate Points over California ######
####################################################

library(sf)

ca_buffered <- st_read("~/shapes/state_boundary_10km_buffer2.shp") %>% st_transform(crs = 4326)

grid <- fread("daily_pm25_2007.csv")[,.(Cell_ID, Longitude, Latitude)] %>%   st_as_sf(coords = c("Longitude", "Latitude"),
                                                                                      crs = 4326)

linked <- st_intersection(grid, ca_buffered) 

linked_table <- linked
st_geometry(linked_table) <- NULL
ca_cells <- as.vector(linked_table$Cell_ID)

grab_three_days <- function(year_var) {
  foo <- fread(paste0("daily_pm25_", year_var, ".csv"))[Cell_ID %in% ca_cells]
  foo2 <- foo[, .(
    cell_id = Cell_ID,
    lon = Longitude,
    lat = Latitude,
    year = year_var,
    Day143,
    Day144,
    Day145
  )]
  
  return(foo2)
}

goo <- rbindlist(lapply(2006:2017, grab_three_days))

goo %>% fwrite("three_day_sample_pm25_for_smoothing.csv")

ggplot(goo) + geom_point(aes(x = lon, y = lat, color = Day145, alpha = Day145), size = 1) + facet_wrap(~ year)

