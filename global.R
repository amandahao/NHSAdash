# install.packages('educationdata')
library(educationdata)
library(dplyr)

df <- get_education_data(level = "school-districts",
                         source = "ccd",
                         topic = "directory",
                         filters = list(year = 2021))

df$latitude <- jitter(df$latitude)
df$longitude <- jitter(df$longitude)
df$zip_location <- formatC(df$zip_location, width=5, format="d", flag="0")

# convert urban_centric_locale to string values
df <- df %>%
  mutate(locale = case_when(
    urban_centric_locale == 11 ~ "City-Large",
    urban_centric_locale == 12 ~ "City-Midsize",
    urban_centric_locale == 13 ~ "City-Small",
    urban_centric_locale == 21 ~ "Suburb-Large",
    urban_centric_locale == 22 ~ "Suburb-Midsize",
    urban_centric_locale == 23 ~ "Suburb-Small",
    urban_centric_locale == 31 ~ "Town-Fringe",
    urban_centric_locale == 32 ~ "Town-Distant",
    urban_centric_locale == 33 ~ "Town-Remote",
    urban_centric_locale == 41 ~ "Rural-Fringe",
    urban_centric_locale == 42 ~ "Rural-Distant",
    urban_centric_locale == 43 ~ "Rural-Remote",
    TRUE ~ NA_character_
  ))

df <- df[,c('lea_name','city_location','county_name','state_location',
            'zip_location','locale','lowest_grade_offered','highest_grade_offered',
            'number_of_schools','enrollment','teachers_total_fte','latitude','longitude')]

# add pop and household income
# install.packages("zipcodeR")
library(zipcodeR)

districts = merge(x = df, y = zip_code_db[,c('zipcode','population','median_household_income')],
                  by.x='zip_location', by.y='zipcode', all.x = TRUE)
districts <- districts[!is.na(districts$latitude), , drop = FALSE] # remove null values
districts$standard <- 10 # standard sizing

cleantable <- districts %>%
  dplyr::select(
    Name = lea_name,
    City = city_location,
    County = county_name,
    State = state_location,
    Zipcode = zip_location,
    Population = population,
    Income = median_household_income,
    Locale = locale,
    LowestGrade = lowest_grade_offered,
    HighestGrade = highest_grade_offered,
    NumSchools = number_of_schools,
    TotalEnrollment = enrollment,
    TotalTeachers = teachers_total_fte,
    Lat = latitude,
    Long = longitude
  )

cleantable <- cleantable[!is.na(cleantable$Lat), , drop = FALSE] # remove null values

#### Head Start locations merged by county ####

aggregated_hs <- cleantable %>%
  group_by(County, State) %>%
  summarise(TotalPopulation = sum(Population),
            TotalIncome = sum(Income),
            TotalNumSchools = sum(NumSchools),
            SumTotalEnrollment = sum(TotalEnrollment),
            SumTotalTeachers = sum(TotalTeachers),
            AllLocale = toString(unique(Locale)),
            .groups = "drop")




#### leaflet county map

library(sp)
library(sf)
library(scales)

# download county shape file from Tiger
us.map <- st_read(dsn = "./tl_2023_us_county", layer = "tl_2023_us_county")
# us.map <- st_read(dsn = "./Desktop/MDI/NHSAdash/tl_2023_us_county", layer = "tl_2023_us_county")

# Remove Alaska(2), Hawaii(15), Puerto Rico (72), Guam (66), Virgin Islands (78), American Samoa (60)
# Mariana Islands (69), Micronesia (64), Marshall Islands (68), Palau (70), Minor Islands (74)
us.map <- us.map[!us.map$STATEFP %in% c("02", "15", "72", "66", "78", "60", "69",
                                        "64", "68", "70", "74"),]
# Make sure other outling islands are removed.
us.map <- us.map[!us.map$STATEFP %in% c("81", "84", "86", "87", "89", "71", "76",
                                        "95", "79"),]

us.map <- us.map %>%
  mutate(State = case_when(
    STATEFP == "01" ~ "AL",
    STATEFP == "04" ~ "AZ",
    STATEFP == "05" ~ "AR",
    STATEFP == "06" ~ "CA",
    STATEFP == "08" ~ "CO",
    STATEFP == "09" ~ "CT",
    STATEFP == "10" ~ "DE",
    STATEFP == "11" ~ "DC",
    STATEFP == "12" ~ "FL",
    STATEFP == "13" ~ "GA",
    STATEFP == "16" ~ "ID",
    STATEFP == "17" ~ "IL",
    STATEFP == "18" ~ "IN",
    STATEFP == "19" ~ "IA",
    STATEFP == "20" ~ "KS",
    STATEFP == "21" ~ "KY",
    STATEFP == "22" ~ "LA",
    STATEFP == "23" ~ "ME",
    STATEFP == "24" ~ "MD",
    STATEFP == "25" ~ "MA",
    STATEFP == "26" ~ "MI",
    STATEFP == "27" ~ "MN",
    STATEFP == "28" ~ "MS",
    STATEFP == "29" ~ "MO",
    STATEFP == "30" ~ "MT",
    STATEFP == "31" ~ "NE",
    STATEFP == "32" ~ "NV",
    STATEFP == "33" ~ "NH",
    STATEFP == "34" ~ "NJ",
    STATEFP == "35" ~ "NM",
    STATEFP == "36" ~ "NY",
    STATEFP == "37" ~ "NC",
    STATEFP == "38" ~ "ND",
    STATEFP == "39" ~ "OH",
    STATEFP == "40" ~ "OK",
    STATEFP == "41" ~ "OR",
    STATEFP == "42" ~ "PA",
    STATEFP == "43" ~ "PR",
    STATEFP == "44" ~ "RI",
    STATEFP == "45" ~ "SC",
    STATEFP == "46" ~ "SD",
    STATEFP == "47" ~ "TN",
    STATEFP == "48" ~ "TX",
    STATEFP == "49" ~ "UT",
    STATEFP == "50" ~ "VT",
    STATEFP == "51" ~ "VA",
    STATEFP == "53" ~ "WA",
    STATEFP == "54" ~ "WV",
    STATEFP == "55" ~ "WI",
    STATEFP == "56" ~ "WY",
    TRUE ~ NA_character_
  ))

us.map <- us.map[,c('State','NAMELSAD','geometry')]

# Merge spatial df w aggregated data
merged_data <- left_join(us.map, aggregated_hs,
                         by = c("NAMELSAD" = "County", "State" = "State"))

merged_data <- st_transform(merged_data, 4326)



#### county geometry/shapefile ####

# install.packages('devtools')
# devtools::install_github("UrbanInstitute/urbnmapr")

# library(tidyverse)
# library(urbnmapr)
# library(sf)
# 
# counties_sf <- get_urbn_map("territories_counties", sf = TRUE) # %>% sf::st_transform('+proj=longlat +datum=WGS84')
# 
# # merge the two df
# # merged_df <- merge(aggregated_hs, counties_sf[,c('county_name','state_abbv','geometry')], by.x = c("County", "State"),
# #                      by.y = c("county_name", "state_abbv"), all = TRUE)
# 
# merged_data <- left_join(counties_sf,
#                          aggregated_hs,
#                          by = c("county_name" = "County", "state_abbv" = "State"))
# 
# merged_data <- st_transform(merged_data, 4326)

# install.packages("remotes")
# remotes::install_github("UrbanInstitute/urbnthemes", build_vignettes = TRUE)


## trial 3
# Get USA polygon data
# library(raster)
# USA <- getData("GADM", country = "usa", level = 2)
# USA$county <- paste(USA$NAME_2, USA$TYPE_2)
# 
# temp <- merge(USA, merged_data,
#               by.x = c("NAME_1", "county"), by.y = c("state_name", "county_name"),
#               all.x = TRUE)
