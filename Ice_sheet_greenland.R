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
for(i in 1:nrow(data)){
  if(nchar(data$month[i])== 1){
    data$date[i] <- paste(data$year[i],data$month[i],sep="-0")
  }else{
    data$date[i] <- paste(data$year[i],data$month[i],sep="-")
  }
}
head(data)


data_by_year <- data %>%
  group_by(year) %>%
  summarise("med_diff" = median(diff, na.rm = TRUE))
head(data_by_year)

ggplot(data = data_by_year, mapping = aes(x = year, y = med_diff)) +
  geom_bar(stat = 'identity', width = 0.6, colour = "black", fill = "darkseagreen1") +
  geom_text(aes(label = round(med_diff, digits = 0)), size = 2.5, vjust = -0.8) +
  theme_minimal()+
  labs(title = "The averrage mass balance of greenland ice sheet")+
  xlab("Year") +
  ylab("Median of net balance in mm/month") +
  theme(plot.title = element_text(size = 20, hjust = 0.5))



data_years <- data %>%
    filter(year == 2000 | year == 2003 | year == 2005 | year == 2008 | 
           year == 2011 | year == 2012 | year == 2015 | year == 2018 |
           year == 2020)
data_years$month <- as.numeric(data_years$month)
data_years$diff <- as.numeric(data_years$diff)
data_years$year <- factor(data_years$year)
ggplot(data = data_years) + 
  geom_line(aes(x = month, y = diff, color = year))+ 
  scale_x_continuous(breaks = 1:12)+
  theme_minimal()+
  labs(title = "The mass balance of greenland ice sheet")+
  xlab("month") +
  ylab("The median of net balance per month in mm/month") +
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        legend.title = element_text(size = 12, vjust = 1),
        legend.text = element_text(size = 8, vjust = 0.75))



for(i in 1:12) {
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
    } else {
      data$diff*31}
}


ggplot()+
  geom_line(mapping = aes(x = year, y = med_diff), data = data_by_year)+
  geom_line(mapping = aes(x = year, y = med_snow), data = data_by_year)+
  geom_line(mapping = aes(x = year, y = med_run, ), data = data_by_year)+
  scale_x_continuous(breaks = date)+
  theme_minimal()
  
  
  
ggplot(data = data_by_year, mapping = aes(x = year, y = med_diff))+
  geom_line(color = "69b3a2")+
  geom_point(color = "69b3a2", size = 2)+
  theme_minimal()+
  ylim(c(-1000, 200))
  
ggplot(data = data_by_year, mapping = aes(x = year, y = med_snow))+
  geom_line(color = "69b3a2")+
  geom_point(color = "69b3a2", size = 2)+
  theme_minimal()

ggplot(data = data_by_year, mapping = aes(x = year, y = med_run))+
  geom_line(color = "69b3a2")+
  geom_point(color = "69b3a2", size = 2)+
  theme_minimal()
