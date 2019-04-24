rm(list = ls())
#########################################################
#########################################################
#
#   Fucntions to process NASA PM Data Fusion Product
#   and compare/intersect with HMS
#
#   Jason Vargo jason.vargo@cdph.ca.gov
#   April 2019
#
#########################################################
#########################################################

# necessary Packages
library(tidyverse)
library(data.table)


setwd("~/nasa_aq_data_fusion/")


# annual_file <- readxl::read_excel("Daily_PM25_2006.xlsx") %>% data.table()
# annual_file %>% fwrite("daily_pm25_2006.csv")

annual_file <- fread("daily_pm25_2006.csv")
