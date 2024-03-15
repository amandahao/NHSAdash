# library(devtools)
# devtools::install_github("sachit27/greenR", dependencies = TRUE)
library(greenR)
library(dplyr)
library(leaflet)

source("get_green_data.R")
source("modified_visualizer.R")

## combine virginia counties

virginia_green_data <- get_green_data("Washington, D.C., United States")$osm_polygons

locations <- c("Arlington County, Virginia, United States",
               "Fairfax County, Virginia, United States",
               "Loudoun County, Virginia, United States")

for (location in locations) {
  green_data <- get_green_data(location)
  virginia_green_data <- bind_rows(virginia_green_data, green_data$osm_polygons)
}

modified_visualizer(virginia_green_data)

# test <- get_green_data("")$osm_polygons

## extra greenR functions

# green_space_clustering(green_areas_data, num_clusters = 3)

# green_index <- calculate_green_index(data, 4326, 100)
# 
# map <- plot_green_index(green_index, colors = c("#FF0000", "#00FF00"), line_width = 1, line_type = "dashed")
# 
# map <- plot_green_index(green_index, interactive = TRUE, base_map = "CartoDB.DarkMatter")
# print(map)
