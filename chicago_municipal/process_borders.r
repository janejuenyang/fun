################################################################################
# prepping chicago geospatial boundary data
# data sources: 
#   precincts: https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Precincts-current-/uvpq-qeeq
#   2019 wards: https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-Wards-2015-2023-/sp34-6z76
#   2023 wards: https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-Wards-2023-Map/cdf7-bgn3
# last edited: 2 jan 2023
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

#### load data ####
# precinct boundaries
borders_chicago_precincts_2019 <- st_read(dsn = paste(dd, 
    "chicago_precincts_2015-2021.shp", sep = "/"))

borders_ward46_precincts_2023 <- st_read(dsn = paste(dd, 
    "chicago_precincts_2023-.shp", sep = "/")) %>%
    mutate(
        ward = 46,
        precinct = str_extract(Name, "[0-9]+") %>% as.integer()
    )

# ward boundaries and centroids
borders_chicago_wards_2019 <- st_read(dsn = paste(dd, "wards_2015-2023.shp", sep = "/")) %>%
    select(ward, geometry)

borders_chicago_wards_2023 <- st_read(dsn = paste(dd, "wards_2023-.shp", sep = "/")) %>%
    select(ward, geometry)

#### combine ward data for 2019 and 2023 ####
# combine ward datasets into one shapefile
borders_chicago_wards <- rbind(
        borders_chicago_wards_2019 %>% mutate(year = 2019), 
        borders_chicago_wards_2023 %>% mutate(year = 2023)
    ) %>%
    mutate(
        year = factor(year),
        ward = as.integer(ward)
    )

# ward centroids, for fetching underlying road maps
centroids_wards <- get_centroids(borders_chicago_wards) %>%
    select(ward, year, latitude, longitude)

# join ward boundary + centroid data
borders_chicago_wards <- borders_chicago_wards %>%
    left_join(centroids_wards, by = c("ward", "year"))

#### combine ward 46 precinct data for 2019 and 2023 ####
# add precinct centers for labeling
centroids_precincts_2019 <- get_centroids(borders_chicago_precincts_2019) %>%
    select(ward, precinct, latitude, longitude)

borders_chicago_precincts_2019 <- borders_chicago_precincts_2019 %>%
    inner_join(centroids_precincts_2019, by = c("ward", "precinct"))

centroids_ward46_precincts_2023 <- get_centroids(borders_ward46_precincts_2023) %>%
    mutate(
        ward = 46,
        precinct = str_extract(Name, "[0-9]+") %>% as.integer()
    ) %>%
    select(ward, precinct, latitude, longitude)

borders_ward46_precincts_2023 <- borders_ward46_precincts_2023 %>%
    st_transform(crs = st_crs(borders_chicago_precincts_2019)) %>%
    inner_join(centroids_ward46_precincts_2023, by = c("ward", "precinct"))

# combine data
borders_ward46_precincts <- rbind(
        borders_chicago_precincts_2019 %>% 
            filter(ward == 46) %>% 
            mutate(year = 2019) %>%
            select(year, ward, precinct, latitude, longitude, geometry), 
        borders_ward46_precincts_2023 %>% 
            mutate(year = 2023) %>%
            select(year, ward, precinct, latitude, longitude, geometry)
    ) %>%
    mutate(year = as.factor(year))

centroids_ward46_precincts <- rbind(
        centroids_precincts_2019 %>% filter(ward == 46) %>% mutate(year = 2019), 
        centroids_ward46_precincts_2023 %>% mutate(year = 2023)
    ) %>%
    mutate(year = as.factor(year))
