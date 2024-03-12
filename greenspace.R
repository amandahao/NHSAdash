# library(devtools)
# devtools::install_github("sachit27/greenR", dependencies = TRUE)
library(greenR)

data <- get_osm_data("Washington, D.C., United States")
green_areas_data <- data$green_areas
visualize_green_spaces(green_areas_data)

## try other cities; LA, SF, Chicago, Houston

# data <- get_osm_data("")
# green_areas_data <- data$green_areas
# visualize_green_spaces(green_areas_data)

# make a function  that gets data of minimum amount of cities to load green space
# data for entirety of US, append all data in df; use visualize_green_spaces function
# on github and add it to server.R / ui.R; everything else in global.R

cities <- c("Los Angeles, California, United States",
            "Chicago, Illinois, United States",
            "Houston, Texas, United States",
            # "Phoenix, Arizona, United States",
            # "Philadelphia, Pennsylvania, United States",
            # "San Antonio, Texas, United States",
            # "San Diego, California, United States",
            # "Dallas, Texas, United States",
            # "San Jose, California, United States",
            # "Austin, Texas, United States",
            # "Jacksonville, Florida, United States",
            "San Francisco, California, United States",
            # "Columbus, Ohio, United States",
            # "Indianapolis, Indiana, United States",
            # "Fort Worth, Texas, United States",
            # "Charlotte, North Carolina, United States",
            # "Seattle, Washington, United States",
            # "Denver, Colorado, United States",
            "Washington, D.C., United States")

# Initialize an empty list to store green space data
all_green_data <- list()

# Iterate over cities to get green space data for each
for (city in cities) {
  data <- get_osm_data(city)
  green_areas_data <- data$green_areas
  all_green_data <- append(all_green_data, list(green_areas_data))
}

# Combine all the green space data into one variable
combined_green_data <- do.call(rbind, all_green_data)

# Visualize the combined green space data
visualize_green_spaces(combined_green_data)


## extra greenR functions

# green_space_clustering(green_areas_data, num_clusters = 3)

# green_index <- calculate_green_index(data, 4326, 100)
# 
# map <- plot_green_index(green_index, colors = c("#FF0000", "#00FF00"), line_width = 1, line_type = "dashed")
# 
# map <- plot_green_index(green_index, interactive = TRUE, base_map = "CartoDB.DarkMatter")
# print(map)
>>>>>>> Stashed changes
