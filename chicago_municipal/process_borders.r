################################################################################
# prepping chicago geospatial boundary data
# data sources: 
#   precincts: https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Precincts-current-/uvpq-qeeq
#   2019 wards: https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-Wards-2015-2023-/sp34-6z76
#   2023 wards: https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-Wards-2023-Map/cdf7-bgn3
# last edited: 15 july 2022
################################################################################
#### initial setup ####
# clear environment and console
rm(list = ls())
cat("\014")

# standard libraries
library(tidyverse)
library(scales)
library(sf)
library(ggmap)
library(gridExtra)
library(gt)

# set directories
wd <- "~/Documents/fun/chicago_municipal"
dd <- paste(wd, "data", sep = "/")
od <- paste(wd, "output", sep = "/")

#### custom functions ####
# create dataframe of centroids for labeling
get_centroids <- function(shapefile) {
    res <- st_centroid(shapefile) %>%
        mutate(
            longitude = st_coordinates(.)[, 1],
            latitude = st_coordinates(.)[, 2]
        ) %>%
        st_set_geometry(NULL)
    return(res)
}

#### load/prep data ####
# precinct boundaries: haven't yet been updated for 2023
borders_chicago_precincts <- st_read(dsn = paste(dd, "chicago_precincts.shp", sep = "/"))

# join in precinct centers for labeling
centroids_precincts <- get_centroids(borders_chicago_precincts) %>%
    select(ward, precinct, latitude, longitude)

borders_chicago_precincts <- borders_chicago_precincts %>%
    inner_join(centroids_precincts, by = c("ward", "precinct")) %>%
    mutate(ward_precinct = paste(ward, precinct, sep = "/"))

# ward boundaries
borders_chicago_wards_2019 <- st_read(dsn = paste(dd, "wards_2015-2023.shp", sep = "/")) %>%
    select(ward, geometry)

borders_chicago_wards_2023 <- st_read(dsn = paste(dd, "wards_2023-.shp", sep = "/")) %>%
    select(ward, geometry)

# combine ward datasets into one shapefile
borders_chicago_wards <- rbind(
        borders_chicago_wards_2019 %>% mutate(year = 2019), 
        borders_chicago_wards_2023 %>% mutate(year = 2023)
    ) %>%
    mutate(year = factor(year))

# ward centroids, for fetching underlying road maps
centroid_wards <- get_centroids(borders_chicago_wards) %>%
    select(ward, year, latitude, longitude)

# join ward boundary + centroid data
borders_chicago_wards <- borders_chicago_wards %>%
    left_join(centroid_wards, by = c("ward", "year"))
