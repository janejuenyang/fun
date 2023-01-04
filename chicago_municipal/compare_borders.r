################################################################################
# compare the ward boundaries for 2019 vs. 2023 municipal elections
# last edited: 15 july 2022
################################################################################
#### initial setup ####
# load data, directories, libraries
source("~/Documents/fun/chicago_municipal/process_borders.r")

#### city-wide ####
g_chicago <- get_googlemap(
    center = "chicago",
    zoom = 10,
    maptype = "roadmap"
)

g_compare_chicago <- ggmap(
        g_chicago, 
        base_layer = ggplot(data = borders_chicago_wards)
    ) +
    geom_sf(
        aes(color = year),
        size = 0.25,
        alpha = 0.5
    ) +
    geom_text(
        mapping = aes(
            label = ward,
            x = longitude,
            y = latitude,
            color = year
        ),
        size = 2,
        fontface = "bold"
    ) +
    # general formatting
    labs(
        title = "2019 vs. 2023 Chicago ward boundaries",
        subtitle = "ward labels indicate geographic center",
        caption = "created by Jane Yang: https://github.com/janejuenyang",
        x = "",
        y = "",
        color = ""
    ) +
    scale_color_manual(values = c("black", "red")) +
    theme(legend.position = "top")
g_compare_chicago
ggsave(paste(od, "compare_chicago.jpg", sep = "/"),
    units = "in", height = 10, width = 10)

#### zoom in on ward 46 ####
# get base street map
g_ward46 <- get_googlemap(
    center = centroids_wards %>% 
        filter(ward == 46, year == 2023) %>% 
        select(longitude, latitude) %>% 
        unlist(),
    zoom = 14,
    maptype = "roadmap"
)

# do overlap map
g_compare_ward46_overlap <- ggmap(
        g_ward46, 
        base_layer = ggplot(borders_ward46_precincts %>% 
            filter(ward == 46))
    ) +
    # precincts
    geom_sf(
        aes(
            color = year,
            linetype = year
        ),
        size = 0.5,
        alpha = 0
    ) +
    # ward boundaries
    geom_sf(
        data = borders_chicago_wards %>% filter(ward == 46),
        aes(
            color = year,
            linetype = year
        ),
        size = 1,
        alpha = 0
    ) +
    # precinct labels
    geom_text(
        data = centroids_ward46_precincts %>% filter(ward == 46),
        mapping = aes(
            color = year,
            label = precinct,
            x = longitude,
            y = latitude,
        ),
        size = 2
    ) +
    # general formatting
    labs(
        title = "2019 vs. 2023 Chicago Ward 46 Precinct Boundaries",
        subtitle = paste(
            "consolidated from", 
            max(centroids_ward46_precincts %>% filter(year == 2019) %>% select(precinct)),
            "to",
            max(centroids_ward46_precincts %>% filter(year == 2023) %>% select(precinct)),
            "precincts"),
        caption = "created by Jane Yang: https://github.com/janejuenyang",
        x = "",
        y = "",
        color = "",
        linetype = ""
    ) +
    scale_color_manual(values = c("black", "red")) +
    scale_linetype_manual(values = c("solid", "twodash")) +
    theme(legend.position = "top")
g_compare_ward46_overlap # hmmm, hard to parse
ggsave(paste(od, "compare_ward46_overlap.jpg", sep = "/"),
    units = "in", height = 10, width = 10)

# do side-by-side map for improved readability
g_compare_ward46_faceted <- ggmap(
        g_ward46, 
        base_layer = ggplot(borders_ward46_precincts %>% 
            filter(ward == 46))
    ) +
    # precincts
    geom_sf(
        size = 0.5,
        alpha = 0
    ) +
    # ward boundaries
    geom_sf(
        data = borders_chicago_wards %>% filter(ward == 46),
        size = 1,
        alpha = 0
    ) +
    # precinct labels
    geom_text(
        data = centroids_ward46_precincts %>% filter(ward == 46),
        mapping = aes(
            label = precinct,
            x = longitude,
            y = latitude,
        ),
        size = 2
    ) +
    # do side-by-side format
    facet_wrap(vars(year)) +
    # general formatting
    labs(
        title = "2019 vs. 2023 Chicago Ward 46 Precinct Boundaries",
        subtitle = paste(
            "consolidated from", 
            max(centroids_ward46_precincts %>% filter(year == 2019) %>% select(precinct)),
            "to",
            max(centroids_ward46_precincts %>% filter(year == 2023) %>% select(precinct)),
            "precincts"),
        caption = "created by Jane Yang: https://github.com/janejuenyang",
        x = "",
        y = "",
        color = "",
        linetype = ""
    ) +
    theme(legend.position = "top")
g_compare_ward46_faceted
ggsave(paste(od, "compare_ward46_faceted.jpg", sep = "/"),
    units = "in", height = 6, width = 12)
