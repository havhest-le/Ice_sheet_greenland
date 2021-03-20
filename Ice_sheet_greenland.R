rm(list = ls(all= TRUE))

library(rgdal)
library(rgeos)
library(raster)
library(data.table)
library(sp)
library(sf)
library(ncdf4)
library(rnaturalearth)
library(tidyverse)
library(glue)
library(dplyr)
library(circular)
library(scales)
library(viridis)
library(dplyr)
library(grid)
library(gridExtra)
library(cowplot)
library(ggpubr)
library(plotrix)
library(glue)

####
# Input data ####
####

setwd("C:/Users/Vivi/Documents/Uni/5. Semester/Wasser/Daten/Shape")
greenland_shape <- readOGR(".", layer = "greenland_south")
plot(greenland_shape)

files_runoff <- list.files(path = "C:/Users/Vivi/Documents/Uni/5. Semester/Wasser/Daten/Runoff", pattern = "*.tif", all.files = T, full.names = T)
files_snowfall <- list.files(path = "C:/Users/Vivi/Documents/Uni/5. Semester/Wasser/Daten/Snowfall", pattern = "*.tif", all.files = T, full.names = T)

run <- do.call("rbind", lapply(1:length(files_runoff), function(x){
  tep <- raster(files_runoff[x])
  med <- cellStats(crop(tep, greenland_shape), stat = 'mean', na.rm = TRUE)
  data.frame(med = med, month = as.numeric(substring(names(tep), 7,8)),
             year = as.numeric(substring(names(tep), 2,5)))}))
head(run)
date = as.POSIXct(glue("{run$year}-01-01"), tz = "GMT") + run$day*24*60*60


snow <- do.call("rbind", lapply(1:length(files_snowfall), function(z){
  tep <- raster(files_snowfall[z])
  med <- cellStats(crop(tep, greenland_shape), stat = 'mean', na.rm = TRUE)
  data.frame(med = med, month = as.numeric(substring(names(tep), 10,12)),
             year = as.numeric(substring(names(tep),4,7)))}))
head(snow)
date = as.POSIXct(glue("{snow$year}-01-01"), tz = "GMT") + snow$day*24*60*60


data <- full_join(snow, run, by = c("month", "year"))
data <- data[, c(2,3,1,4)]
data <- rename(data, med_snow = med.x, med_run = med.y)
data$med_snow <- data$med_snow*24*60*60*100
data$med_run <- data$med_run*24*60*60*1000
data$diff <- ((data$med_snow-data$med_run))
head(data)

for(i in 1:12){
    if(i == 1){
      data$diff*31
    } else if(i == 2){
      data$diff*28
    } else if(i == 3){
      data$diff*28
    } else if(i == 4){
      data$diff*30
    } else if(i == 5){
      data$diff*31
    } else if(i == 6){
      data$diff*30
    } else if(i == 7){
      data$diff*31
    } else if(i == 8){
      data$diff*31
    } else if(i == 9){
      data$diff*30
    } else if(i == 10){
      data$diff*31
    } else if(i == 11){
      data$diff*30
    } else if(i == 12){
      data$diff*31}
}
head(data)

