# Ice_sheet_greenland

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
  ext <- crop(tep, greenland_shape)
  stat <- quantile(ext, probs = c(0.275, 0.5, 0.975), na.rm = T)
  data.frame(stat = stat, month = as.numeric(substring(names(tep), 7,8)),
             year = as.numeric(substring(names(tep), 2,5)))}))
head(run)
date = as.POSIXct(glue("{run$year}-01-01"), tz = "GMT") + run$day*24*60*60


snow <- do.call("rbind", lapply(1:length(files_snowfall), function(z){
  tep <- raster(files_snowfall[z])
  ext <- crop(tep, greenland_shape)
  stat <- quantile(ext, probs = c(0.275, 0.5, 0.975), na.rm = T)
  data.frame(stat = stat, month = as.numeric(substring(names(tep), 10,12)),
             year = as.numeric(substring(names(tep),4,7)))}))
head(snow)
date = as.POSIXct(glue("{snow$year}-01-01"), tz = "GMT") + snow$day*24*60*60


for(y in 2000:2005){
  
cat(glue("Wir befinden uns im Jahre {y} nach Christus. Ganz Gallien ist nicht mehr von den Römern besetzt."))
  
subTab <- subset(run, as.numeric(format(date, "%Y")) %in% y &
                     as.numeric(format(date, "%m")) %in% c(1:12)) # Monat 6 bis 8 bei mehreren Jahren und Monaten



  
    
}  

