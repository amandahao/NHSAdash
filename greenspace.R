# library(devtools)
# devtools::install_github("sachit27/greenR", dependencies = TRUE)
library(greenR)
library(dplyr)
library(leaflet)

setwd("./Desktop/MDI/NHSAdash/")

source("get_green_data.R")
source("modified_visualizer.R")

## combine virginia counties

virginia_green_data <- get_green_data("Washington, D.C., United States")$osm_polygons

locations <- read.csv("va_counties.csv", header=F)$V1

for (location in locations) {
  green_data <- get_green_data(location)
  virginia_green_data <- bind_rows(virginia_green_data, green_data$osm_polygons)
}

modified_visualizer(virginia_green_data)

md_green_data <- get_green_data("Allegany County, Maryland, United States")$osm_polygons
md_loc <- read.csv("md_counties.csv", header=F)$V1

for (county in md_loc) {
  green_data <- get_green_data(location)
  md_green_data <- bind_rows(md_green_data, green_data$osm_polygons)
}

dmv_green_data <- bind_rows(virginia_green_data, md_green_data)

modified_visualizer(dmv_green_data)

## save to csv - not correct sf format for mod_viz

write.csv(virginia_green_data, file = "va_green_data.csv")
va_green_data <- read.csv("va_green_data.csv", header = F)
colnames(va_green_data) <- va_green_data[1, ]
va_green_data <- va_green_data[-1, ]
va_green_data <- va_green_data[, -1]
va_green_data_sf <- st_as_sf(va_green_data, sf_column_name = "geometry")

modified_visualizer(va_green_data)

## extra greenR functions

# green_space_clustering(green_areas_data, num_clusters = 3)

# green_index <- calculate_green_index(data, 4326, 100)
# 
# map <- plot_green_index(green_index, colors = c("#FF0000", "#00FF00"), line_width = 1, line_type = "dashed")
# 
# map <- plot_green_index(green_index, interactive = TRUE, base_map = "CartoDB.DarkMatter")
# print(map)
