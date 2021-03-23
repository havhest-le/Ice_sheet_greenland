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
library(lubridate)

####
# Input data ####
####

setwd("C:/Users/Vivi/Documents/Uni/5. Semester/Wasser/Daten")
# setwd("~/Google Drive/Shared/Daten/")
# greenland_shape <- readOGR("greenland_south/greenland_south.shp", layer = "greenland_south")
# plot(greenland_shape)

map <- st_as_sfc(rnaturalearthdata::countries50)
ext <- st_as_sfc(as(extent(c(-55, -25, 55, 69)), "SpatialPolygons")) %>% st_set_crs(st_crs(map))
greenland <- as(st_intersection(map, ext), "Spatial")
plot(greenland)
catch <- read.table("grndrainagesystems_ekholm.txt", skip = 50)
catch5 <- subset(catch, V1==5.0)
  catch5$V3 <- ifelse(catch5$V3>180, catch5$V3-360, catch5$V3)

jpeg("catchment.jpeg", width = 1000, height = 1000)  
plot(greenland)
title("Catchment 5.0 of South Greenland", cex.main= 4, line = -4)
poly5 <- st_sfc(st_polygon(list(as.matrix(catch5[,3:2]))), crs = 4326)
plot(poly5, col = "steelblue4", add = T)
poly_run <- st_buffer(poly5, 0.4)
plot(poly_run, add = T, col = alpha("skyblue3", 0.35))
legend("right", legend = c("Catchment 5.0", "Buffer for runoff"),
       col = c("steelblue4", "skyblue3"), pch = c(15,15), pt.cex = 3,  bty = "n", cex = 3)
dev.off()  


files_runoff   <- list.files(path = "runoff", pattern = "*.tif", recursive = TRUE, full.names = T)
files_snowfall <- list.files(path = "Snowfall", pattern = "*.tif", recursive = TRUE, full.names = T)

run <- do.call("rbind", lapply(1:length(files_runoff), function(x){
  tep_run <- raster(files_runoff[x])
  med <- sum(crop(tep_run, as(poly_run, "Spatial"))[], na.rm = T)
  data.frame(med = med, month = as.numeric(substring(names(tep_run), 7,8)),
             year = as.numeric(substring(names(tep_run), 2,5)))}))
head(run)

snow <- do.call("rbind", lapply(1:length(files_snowfall), function(z){
  tep <- raster(files_snowfall[z])
  med <- sum(crop(tep, as(poly5, "Spatial"))[], na.rm = T)
  data.frame(med = med, month = as.numeric(substring(names(tep), 9,10)),
             year = as.numeric(substring(names(tep), 4,7)))}))
head(snow)


data <- full_join(snow, run, by = c("month", "year"))[, c(2,3,1,4)]
names(data)[3:4] <- c("med_snow", "med_run")
data$diff <- ((data$med_snow-data$med_run))
data$date <- as.POSIXct(glue("{data$year}-{data$month}-15"), tz = "GMT")
head(data)


data_by_year_diff <- data %>%
  group_by(year) %>%
  summarise("med_diff" = median(diff, na.rm = TRUE))
head(data_by_year_diff)


pos_bal <- data_by_year_diff %>%
  group_by(year) %>%
  filter(year == "2002" | year == "2006" | year == "2009" | year == "2011" | 
         year == "2014" | year == "2018")
head(pos_bal)

neg_bal <- data_by_year_diff %>%
  group_by(year) %>%
  filter(year == "2000" | year == "2001" | year == "2005" | year == "2008" | 
           year == "2012")
head(neg_bal)


colors_1 <- c("smaller balance" = "firebrick3", "taller balance" = "deepskyblue3")
ggplot() +
  geom_bar(data = data_by_year_diff, mapping = aes(x = year, y = med_diff, fill = med_diff), 
           stat = 'identity', width = 0.6, colour = "black", fill = "white", show.legend = FALSE)+
  geom_bar(data = pos_bal, mapping = aes(x = year, y = med_diff, fill = "smaller balance"),
           stat = 'identity', width = 0.6, colour = "black", show.legend = TRUE) +
  geom_bar(data = neg_bal, mapping = aes(x = year, y = med_diff, fill = "taller balance"),
           stat = 'identity', width = 0.6, colour = "black", show.legend = TRUE) +
  scale_fill_manual(values = colors_1) +
  theme_minimal()+
  labs(title = "The averrage mass balance of greenland ice sheet", fill = "balance")+
  xlab("Year") +
  ylab("Median of net balance [kg/s]") +
  theme(plot.title = element_text(size = 18, hjust = 0.5),
        legend.title = element_text(size = 10, vjust = 0.5),
        legend.text = element_text(size = 8, vjust = 0.5))



data_by_year <- data %>%
  group_by(year) %>%
  summarise("med_diff" = median(diff, na.rm = TRUE),
            "med_snow" = median(med_snow, na.rm = TRUE), 
            "med_run" = median(med_run, na.rm = TRUE))
head(data_by_year)


colors_2 <- c("NBW" = "firebrick4", "snowfall" = "mediumspringgreen", "runoff" = "dodgerblue4")
# data_by_year$year <- as.numeric(data_by_year$year)
ggplot() +
  geom_line(mapping = aes(x = year, y = med_diff, color = "NBW"), data = data_by_year, 
             size = 1) +
  geom_line(mapping = aes(x = year, y = med_snow, color = "snowfall"), data = data_by_year,
            size = 1) +
  geom_line(mapping = aes(x = year, y = med_run, color = "runoff"), data = data_by_year, 
            size = 1) +
  scale_color_manual(values = colors_2) +
  theme_minimal() +
  labs(title = "The mass balance of greenland ice sheet", colour = "Legend") +
  xlab("year") +
  ylab("Median of NMB [kg/s]") +
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        legend.title = element_text(size = 10, vjust = 1),
        legend.text = element_text(size = 8, vjust = 0.5))




ggplot(data = data_by_year, mapping = aes(x = year, y = med_diff)) +
  geom_point()+
  geom_line(color = "69b3a2")+
  theme_minimal()+
  labs(title = "The netto mass balance") +
  xlab("year") +
  ylab("Median of NMB [kg/s]") +
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        legend.title = element_text(size = 10, vjust = 1),
        legend.text = element_text(size = 8, vjust = 0.5))

ggplot(data = data_by_year, mapping = aes(x = year, y = med_run))+
  geom_point()+
  geom_line(color = "69b3a2")+  
  theme_minimal()+
  labs(title = "The averrage runoff") +
  xlab("year") +
  ylab("Median of runoff [kg/s]") +
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        legend.title = element_text(size = 10, vjust = 1),
        legend.text = element_text(size = 8, vjust = 0.5))

ggplot(data = data_by_year, mapping = aes(x = year, y = med_snow))+
  geom_point()+
  geom_line(color = "69b3a2")+  
  theme_minimal()+
  labs(title = "The averrage snowfall") +
  xlab("year") +
  ylab("Median of snowfall [kg/s]") +
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        legend.title = element_text(size = 10, vjust = 1),
        legend.text = element_text(size = 8, vjust = 0.5))



data$year <- as.factor(data$year)
ggplot(data = data) + 
  geom_line(aes(x = month, y = diff, color = year))+ 
  scale_x_continuous(breaks = 1:12)+
  theme_minimal()+
  labs(title = "The mass balance of greenland ice sheet")+
  xlab("month") +
  ylab("Net balance for all months [kg/s]") +
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        legend.title = element_text(size = 12, vjust = 1),
        legend.text = element_text(size = 8, vjust = 0.75))

data$year <- as.factor(data$year)
ggplot(data = data) + 
  geom_line(aes(x = month, y = diff, color = year))+ 
  scale_x_continuous(breaks = 1:12)+
  theme_minimal()+
  labs(title = "The mass balance of greenland ice sheet")+
  xlab("month") +
  ylab("Net balance for all months [kg/s]") +
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        legend.title = element_text(size = 12, vjust = 1),
        legend.text = element_text(size = 8, vjust = 0.75))



data_5 <- filter(data, year %in% 
                   c(2000:2005)) 
data_10 <- filter(data, year %in% 
                    c(2006:2010)) 
data_15 <- filter(data, year %in% 
                    c(2011:2015))
data_20 <- filter(data, year %in% 
                    c(2016:2020))

ggplot(data = data_5) + 
  geom_line(aes(x = month, y = diff, color = year))+ 
  scale_x_continuous(breaks = 1:12)+
  theme_minimal()+
  labs(title = "The mass balance of greenland ice sheet")+
  xlab("month") +
  ylab("Net balance for all months [kg/s]") +
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        legend.title = element_text(size = 12, vjust = 1),
        legend.text = element_text(size = 8, vjust = 0.75))

ggplot(data = data_10) + 
  geom_line(aes(x = month, y = diff, color = year))+ 
  scale_x_continuous(breaks = 1:12)+
  theme_minimal()+
  labs(title = "The mass balance of greenland ice sheet")+
  xlab("month") +
  ylab("Net balance for all months [kg/s]") +
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        legend.title = element_text(size = 12, vjust = 1),
        legend.text = element_text(size = 8, vjust = 0.75))

ggplot(data = data_15) + 
  geom_line(aes(x = month, y = diff, color = year))+ 
  scale_x_continuous(breaks = 1:12)+
  theme_minimal()+
  labs(title = "The mass balance of greenland ice sheet")+
  xlab("month") +
  ylab("Net balance for all months [kg/s]") +
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        legend.title = element_text(size = 12, vjust = 1),
        legend.text = element_text(size = 8, vjust = 0.75))

ggplot(data = data_20) + 
  geom_line(aes(x = month, y = diff, color = year))+ 
  scale_x_continuous(breaks = 1:12)+
  theme_minimal()+
  labs(title = "The mass balance of greenland ice sheet")+
  xlab("month") +
  ylab("Net balance for all months [kg/s]") +
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        legend.title = element_text(size = 12, vjust = 1),
        legend.text = element_text(size = 8, vjust = 0.75))

data_years <- filter(data, year %in% 
                       c(2003, 2005, 2008, 2011, 2012, 2018))

ggplot(data = data_years) + 
  geom_line(aes(x = month, y = diff, color = year))+ 
  scale_x_continuous(breaks = 1:12)+
  theme_minimal()+
  labs(title = "The mass balance of greenland ice sheet")+
  xlab("month") +
  ylab("Net balance for all months [kg/s]") +
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        legend.title = element_text(size = 12, vjust = 1),
        legend.text = element_text(size = 8, vjust = 0.75))

