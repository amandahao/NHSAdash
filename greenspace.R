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

# write.csv(virginia_green_data, file = "va_green_data_2.csv")
# va_green_data_1 <- read.csv("va_green_data_1.csv", header = TRUE)

modified_visualizer(virginia_green_data)


## extra greenR functions

# green_space_clustering(green_areas_data, num_clusters = 3)

# green_index <- calculate_green_index(data, 4326, 100)
# 
# map <- plot_green_index(green_index, colors = c("#FF0000", "#00FF00"), line_width = 1, line_type = "dashed")
# 
# map <- plot_green_index(green_index, interactive = TRUE, base_map = "CartoDB.DarkMatter")
# print(map)
