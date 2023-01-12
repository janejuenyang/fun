###############################################################################
# PURPOSE: identify 2019 Angela Clay vote share against 2023 precincts
# LAST EDITED: 9 jan 2023
###############################################################################

#### initial setup ####
# load data, directories, libraries
source("~/Documents/fun/chicago_municipal/process_borders.r")

# load 2019 ward 46 vote data ####
d_votes_raw <- read_csv(paste(dd, "ward46_2019_combined.csv", sep = "/"))

#### prep data ####
# focus vote data
glimpse(d_votes_raw)

d_votes <- d_votes_raw %>%
    select(
        ward,
        precinct,
        neighborhood,
        general_total_registered,
        general_total_ballots,
        general_total_votes = general_total,
        general_angela_clay,
        runoff_total_registered,
        runoff_total_ballots,
        runoff_total_votes = runoff_total,
    ) %>%
    mutate(year = as.factor(2019))
glimpse(d_votes)

# combine vote data with 2019 precinct borders
precincts_2019 <- borders_chicago_precincts_2019 %>%
    select(ward, precinct, latitude, longitude, geometry) %>%
    inner_join(d_votes, by = c("ward", "precinct")) %>%
    # order by precinct to allow proper overlap analysis (based on indices)
    arrange(precinct)
glimpse(precincts_2019)

# focus 2023 precinct border data
precincts_2023 <- borders_ward46_precincts_2023 %>%
    select(ward, precinct, latitude, longitude, geometry) %>%
    # order by precinct to allow proper overlap analysis (based on indices)
    arrange(precinct)

# scale down 2023 precinct borders to avoid erroneous "overlaps" caused by
# common borders
geom_2023 <- st_geometry(precincts_2023)
centroids_2023 <- st_centroid(precincts_2023_geom)
geom_2023_scaled <- (geom_2023 - centroids_2023) * 0.98 + centroids_2023

precincts_2023_scaled <- precincts_2023
st_geometry(precincts_2023_scaled) <- geom_2023_scaled
precincts_2023_scaled <- st_set_crs(precincts_2023_scaled, st_crs(precincts_2023))

precincts_2023_scaled

# identify intersections between 2023 and 2019 precincts
overlaps <- st_overlaps(precincts_2023_scaled, precincts_2019)
glimpse(overlaps)

#### calculate turnout and angela vote share per 2023 precinct ####
# create table of all 2019 -> 2023 precinct overlaps
d_overlaps <- as.data.frame(overlaps) %>%
    rename(
        precinct_2023 = row.id,
        precinct_2019 = col.id,
    ) %>%
    # add 2023 mapping of 2019 precinct 7; TO DO: debug initial overlap calc
    rbind(c(15, 7)) %>%
    # bring in vote data
    full_join(d_votes, by = c("precinct_2019" = "precinct")) %>%
    # add back in empty precinct 9 (2023); TO DO: find more elegant way
    rbind(c(9, rep(NA, 11)))
glimpse(d_overlaps)

# summarize at 2023 precinct level
s_overlaps <- d_overlaps %>%
    group_by(precinct_2023) %>%
    summarize(
        n_overlapping_2019_precincts = n(),
        general_pct_turnout = sum(general_total_votes) /
            sum(general_total_registered),
        general_pct_voted_angela = sum(general_angela_clay) /
            sum(general_total_votes),
        runoff_pct_turnout = sum(runoff_total_votes) /
            sum(runoff_total_registered),
    )
glimpse(s_overlaps)

write_csv(s_overlaps, paste(od, "ward_blended.csv", sep = "/"))

# transform to long format for easier plotting
s_overlaps_long <- s_overlaps %>%
    pivot_longer(
        cols = contains("pct"),
        names_to = "metric"
    )
glimpse(s_overlaps_long)

# join with 2023 precinct shapefile
precincts_2023_overlain <- precincts_2023 %>%
    left_join(s_overlaps_long, by = c("precinct" = "precinct_2023"))
glimpse(precincts_2023_overlain)

# join voter data with borders_ward46_precincts for detailed view
s_detail <- borders_ward46_precincts %>%
    left_join(d_votes, by = c("year", "precinct", "ward")) %>%
    mutate(
        general_pct_turnout = general_total_votes / general_total_registered,
        general_pct_voted_angela = general_angela_clay / general_total_votes,
        runoff_pct_turnout = runoff_total_votes / runoff_total_registered,
    )
glimpse(s_detail)

s_detail_long <- s_detail %>%
    pivot_longer(
        cols = contains("pct"),
        names_to = "metric"
    )
glimpse(s_detail_long)

#### generate maps ####
# get base street map
g_ward46 <- get_googlemap(
    center = centroids_wards %>% 
        filter(ward == 46, year == 2023) %>% 
        select(longitude, latitude) %>% 
        unlist(),
    zoom = 14,
    maptype = "roadmap"
)

# create label map
metric_labels <- c(
    "general: turnout", 
    "general: voted for Angela", 
    "runoff: turnout"
)
names(metric_labels) <- unique(precincts_2023_overlain$metric)

# create other formatting mappings
color_mappings <- c("black", "white", "white", "black")
names(color_mappings) <- c("dark", "light", "2019", "2023")

alpha_levels <- c(0.85, 0)
names(alpha_levels) <- c("opaque", "transparent")

# ward 46: % angela votes and turnout with 2019 blended rates
g_ward_blended <- ggmap(
        g_ward46, 
        base_layer = ggplot(precincts_2023_overlain)
    ) +
    # precincts
    geom_sf(
        aes(
            geometry = geometry,
            fill = value
        ),
        alpha = 0.85
    ) +
    geom_text(
        mapping = aes(
            label = precinct,
            x = longitude,
            y = latitude,
            color = if_else(value > 0.35, "light", "dark")
        ),
        size = 3,
    ) +
    facet_wrap(
        facets = vars(metric),
        ncol = 2,
        labeller = labeller(metric = metric_labels)
    ) +
    # general formatting
    scale_fill_viridis_c(
        direction = -1, 
        label = percent_format(accuracy = 1)
    ) +
    scale_color_manual(
        values = color_mappings,
        guide = "none",
    ) +
    labs(
        title = "2019 municipal election results mapped to 2023 precincts",
        subtitle = "percents calculated across all overlapping precincts",
        x = "",
        y = "",
        fill = "%"
    )
g_ward_blended
ggsave(paste(od, "ward_blended.jpg", sep = "/"),
    units = "in", height = 12, width = 10)

# ward 46: % angela votes and turnout with 2019 detail
g_ward_detail <- ggmap(
        g_ward46, 
        base_layer = ggplot(s_detail_long)
    ) +
    # precincts
    geom_sf(
        aes(
            color = as.character(year),
            fill = value,
            alpha = if_else(year == 2019, "opaque", "transparent")
        ),
        size = 0.5,
    ) +
    # 2023 precinct labels
    geom_label(
        data = s_detail_long %>% filter(year == 2023), 
        mapping = aes(
            label = precinct,
            x = longitude,
            y = latitude,
        ),
        color = "black",
        fill = "white",
        label.size = 0,
        alpha = 0.5,
        size = 2,
    ) +
    # general formatting
    facet_wrap(
        facets = vars(metric),
        ncol = 2,
        labeller = labeller(metric = metric_labels)
    ) +
    # general formatting
    scale_fill_viridis_c(
        direction = -1, 
        label = percent_format(accuracy = 1)
    ) +
    scale_color_manual(
        values = color_mappings,
        guide = "none",
    ) +
    scale_alpha_manual(
        values = alpha_levels,
        guide = "none",
    ) +
    labs(
        title = "2019 municipal election results",
        subtitle = "with overlain 2023 precinct borders",
        x = "",
        y = "",
        fill = "%"
    )
g_ward_detail
ggsave(paste(od, "ward_detail.jpg", sep = "/"),
    units = "in", height = 12, width = 10)

# by precinct: with 2019 detail

# by precinct: with blended rates
