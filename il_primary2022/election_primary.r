################################################################################
# IL 2022 primary election results
# data sources: 
#   votes: https://www.chicagoelections.gov/en/election-results.html
#   boundaries: https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Precincts-current-/uvpq-qeeq
# last edited: 10 july 2022
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
wd <- "~/Documents/Fun/il_primary2022"
dd <- paste(wd, "data", sep = "/")
od <- paste(wd, "output", sep = "/")

#### load/prep data ####
# precinct boundaries
borders_chicago_precincts <- st_read(dsn = paste(dd, "chicago_precincts.shp", sep = "/"))
plot(borders_chicago_precincts)

# join in precinct centers for labeling
centroids_precincts <- st_centroid(borders_chicago_precincts) %>%
    mutate(
        longitude = st_coordinates(.)[, 1],
        latitude = st_coordinates(.)[, 2]
    ) %>%
    st_set_geometry(NULL) %>%
    select(ward, precinct, latitude, longitude)

borders_chicago_precincts <- borders_chicago_precincts %>%
    inner_join(centroids_precincts, by = c("ward", "precinct")) %>%
    mutate(ward_precinct = paste(ward, precinct, sep = "/"))

# ward boundaries
borders_chicago_wards <- borders_chicago_precincts %>%
    group_by(ward) %>%
    summarize(geometry = st_union(geometry)) %>%
    ungroup()

# IL-13 ward labels
labels_il13_wards <- data.frame(
    ward = c(40, 44, 46, 47, 48),
    longitude = c(-87.715, -87.65, -87.63, -87.68, -87.64),
    latitude = c(41.98, 41.94, 41.95, 41.955, 41.975)
)

# IL-13 race votes
d_votes_il13_raw <- read_csv(paste(dd, "votes_il13.csv", sep = "/"))
d_votes_il13 <- d_votes_il13_raw %>%
    pivot_longer(
        cols = matches("votes_[[:alpha:]]"),
        names_to = "candidate",
        values_to = "votes",
        names_prefix = "votes_"
    ) %>%
    mutate(
        votes_pct = votes/votes_13,
        candidate = factor(candidate, levels = c(
            "hoan", "eileen", "sergio", "andy", "joe"
        ))
    )
glimpse(d_votes_il13)

d_votes_il13_spread <- d_votes_il13_raw %>%
    mutate_at(vars(matches("votes_[[:alpha:]]+")), .funs = c(pct = ~./votes_13)) %>%
    mutate(
        spread_votes = votes_hoan - votes_eileen,
        spread_pp = votes_hoan_pct - votes_eileen_pct
    )
glimpse(d_votes_il13_spread)

# combine IL-13 precinct-level data
d_il13_precincts <- borders_chicago_precincts %>% 
    inner_join(d_votes_il13, by = c("ward", "precinct")) %>%
    mutate(label_color = if_else(votes_pct >= 0.6, "white", "black"))
glimpse(d_il13_precincts)

d_votes_il13_spread <- borders_chicago_precincts %>% 
    inner_join(d_votes_il13_spread, by = c("ward", "precinct")) %>%
    mutate(
        label_color_pct = if_else(spread_pp >= 0.45, "white", "black"),
        label_color_n = if_else(spread_votes >= 80, "white", "black"),
        lost = spread_votes < 0
    )
glimpse(d_votes_il13_spread)

# centroid of IL-13, for fetching underlying road maps
centroid_il13 <- st_centroid(st_union(d_votes_il13_spread)) %>% 
    st_coordinates() %>% 
    as.vector()

# centroid of ward 46, for fetching underlying road maps
centroid_wards <- st_centroid(borders_chicago_wards) %>%
    mutate(
        longitude = st_coordinates(.)[, 1],
        latitude = st_coordinates(.)[, 2]
    ) %>%
    st_set_geometry(NULL) %>%
    select(ward, latitude, longitude)

# city-wide voter turnout
d_turnout_raw <- read_csv(paste(dd, "turnout.csv", sep = "/"))
d_turnout <- borders_chicago_precincts %>%
    inner_join(d_turnout_raw, by = c("ward", "precinct")) %>%
    mutate(
        ward_precinct = paste(ward, precinct, sep = "/"),
        pct_turnout = ballots_cast/voters_registered,
        label_color_city = if_else(pct_turnout > 0.5, "white", "black"),
        label_color_ward46 = if_else(pct_turnout > 0.35, "white", "black"),
        label_color_il13 = if_else(pct_turnout > 0.38, "white", "black"),
        in_il13 = ward_precinct %in% d_votes_il13_spread$ward_precinct
    )
glimpse(d_turnout)

#### il house district 13: democratic primary ####
# plot base road map
g_il13 <- get_googlemap(
        center = centroid_il13,
        zoom = 13,
        maptype = "roadmap"
    )

# IL-13 house district wards
borders_il13_wards <- borders_chicago_wards %>%
    filter(ward %in% d_il13_precincts$ward)

# create plotting function to handle repeated layers
gg_il13 <- function(
        precinct_data, # dataset with precinct-level geometry + values
        precinct_fill_var, # variable in precinct_data to use for map fill
        precinct_border_var = factor(1), # variable in precinct_data to use for border color
        precinct_label_color_var, # variable in precinct_data to use for label color
        lab_title, # title for map
        lab_subtitle = "data downloaded July 9, 2022 9am CDT",
        lab_caption = "created by Jane Yang: https://github.com/janejuenyang",
        lab_fill = "% won of votes cast"
    ) {
        
        precinct_fill <- enquo(precinct_fill_var)
        precinct_label_color <- enquo(precinct_label_color_var)
        precinct_border <- enquo(precinct_border_var)
    
        plot <- ggmap(g_il13, base_layer = ggplot(data = precinct_data)) +
        # precinct-level data
        geom_sf(
            mapping = aes(fill = !!precinct_fill, color = !!precinct_border),
            alpha = 0.85
        ) +
        geom_text(
            mapping = aes(
                label = precinct,
                x = longitude,
                y = latitude,
            ),
            size = 1,
            color = precinct_data %>% pull(!!precinct_label_color)
        ) +
        # ward-level data
        geom_sf(
            data = borders_il13_wards,
            alpha = 0,
            size = 1,
            color = "black"
        ) +
        geom_text(
            data = labels_il13_wards,
            mapping = aes(x = latitude, y = longitude, label = ward),
            fontface = "bold"
        ) +
        # general formatting
        scale_fill_viridis_c(
            direction = -1, 
            label = percent_format(accuracy = 1)
        ) +
        scale_color_manual(
            values = c("grey50", "red"),
            guide = "none"
        ) +
        labs(
            title = lab_title,
            subtitle = lab_subtitle,
            caption = lab_caption,
            x = "",
            y = "",
            fill = lab_fill
        )
            
        return(plot)
    }

# % won by candidate
g_il13_candidates <- gg_il13(
        precinct_data = d_il13_precincts,
        precinct_fill_var = votes_pct,
        precinct_label_color_var = label_color,
        lab_title = "Hoan Huynh: IL-13 state rep democratic primary 2022",
        lab_fill = "% won of votes cast"
    ) + 
    facet_wrap(~candidate, ncol = 2)
g_il13_candidates
ggsave(paste(od, "votes_il13.jpg", sep = "/"),
    units = "in", height = 10, width = 10)

# zoom in on just hoan
g_il13_hoan <- gg_il13(
        precinct_data = d_il13_precincts %>% filter(candidate == "hoan"),
        precinct_fill_var = votes_pct,
        precinct_label_color_var = label_color,
        lab_title = "IL-13 state rep democratic primary 2022",
        lab_fill = "% won of votes cast"
    )
g_il13_hoan
ggsave(paste(od, "votes_il13_hoan.jpg", sep = "/"),
    units = "in", height = 10, width = 10)

# hoan - eileen spread
g_il13_spread_votes <- gg_il13(
        precinct_data = d_votes_il13_spread,
        precinct_fill_var = spread_votes,
        precinct_border_var = lost,
        precinct_label_color_var = label_color_n,
        lab_title = "Hoan Huynh vs. Eileen Dordek, IL-13 Democratic Primary 2022",
        lab_subtitle = "precincts Huynh lost are outlined in red",
        lab_caption = "data downloaded July 9, 2022 9am CDT",
        lab_fill = "votes spread"
    )  +
    theme(
        legend.position = "top",
        plot.subtitle = element_text(color = "red")
    ) +
    scale_fill_viridis_c(
        direction = -1, 
        label = comma_format(accuracy = 1)
    )

g_il13_spread_pp <- gg_il13(
        precinct_data = d_votes_il13_spread,
        precinct_fill_var = spread_pp,
        precinct_border_var = lost,
        precinct_label_color_var = label_color_pct,
        lab_title = "",
        lab_subtitle = "",
        lab_fill = "percentage point spread"
    )  +
    theme(legend.position = "top")
g_il13_spread <- grid.arrange(g_il13_spread_votes, g_il13_spread_pp, ncol = 2)
ggsave(
    plot = g_il13_spread, 
    filename = paste(od, "votes_il13_spread.jpg", sep = "/"),
    units = "in", height = 8, width = 12
)

# ward-level summary
s_ward <- d_votes_il13 %>%
    group_by(ward, candidate) %>%
    summarize(
        precincts = n(),
        votes_13 = sum(votes_13),
        votes = sum(votes),
        votes_pct = votes/votes_13
    ) %>%
    ungroup() %>%
    pivot_wider(
        names_from = candidate,
        values_from = c(votes, votes_pct)
    ) %>%
    mutate(ward = as.character(ward))

s_total <- d_votes_il13 %>%
    group_by(candidate) %>%
    summarize(
        ward = "Total",
        precincts = n_distinct(paste(ward, precinct, "-")),
        votes_13 = sum(votes_13),
        votes = sum(votes),
        votes_pct = votes/votes_13
    ) %>%
    ungroup() %>%
    pivot_wider(
        names_from = "candidate",
        values_from = c(votes, votes_pct)
    )

s_il13 <- bind_rows(s_ward, s_total) %>%
    gt() %>%
    tab_header(
        title = "IL House District 13 Democratic Primary Results", 
        subtitle = "by Chicago ward"
    ) %>%
    tab_spanner(
        label = md("**Hoan**"),
        columns = contains("hoan")
    ) %>%
    tab_spanner(
        label = "Eileen",
        columns = contains("eileen")
    ) %>%
    tab_spanner(
        label = "Sergio",
        columns = contains("sergio")
    ) %>%
    tab_spanner(
        label = "Andy",
        columns = contains("andy")
    ) %>%
    tab_spanner(
        label = "Joe",
        columns = contains("joe")
    ) %>%
    tab_source_note(source_note = "created by Jane Yang: https://github.com/janejuenyang") %>%
    tab_footnote(
      footnote = "data downloaded July 9, 2022 9am CDT",
      locations = cells_column_labels(columns = votes_13)
    ) %>%
    cols_label(
        ward = "Ward",
        precincts = "Number of Precincts",
        votes_13 = "Total Votes Cast",
        votes_hoan = "#",
        votes_eileen = "#",
        votes_sergio = "#",
        votes_andy = "#",
        votes_joe = "#",
        votes_pct_hoan = "%",
        votes_pct_eileen = "%",
        votes_pct_sergio = "%",
        votes_pct_andy = "%",
        votes_pct_joe = "%"
    ) %>%
    fmt_percent(
        columns = contains("pct"),
        decimals = 0
    ) %>%
    fmt_number(
        columns = contains("votes"),
        decimals = 0
    ) %>%
    tab_style(
        style = list(cell_fill(color = "lightskyblue2")),
        locations = cells_body(columns = contains("hoan"))
    ) %>%
    tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_body(rows = ward == "Total")
    ) %>%
    tab_style(
        style = cell_borders(
            sides = c("top", "bottom"),
            color = "lightgrey",
            weight = px(2),
            style = "solid"
        ),
        locations = cells_body(
            columns = everything(),
            rows = ward == "Total"
        )
    )
s_il13
gtsave(s_il13, paste(od, "votes_il13_by_ward.png", sep = "/"))

# precincts Hoan lost
s_lost <- d_votes_il13_spread %>% 
    filter(spread_votes < 0) %>%
    arrange(spread_votes) %>%
    select(
        ward, precinct, votes_13, 
        contains("hoan"), contains("eileen"), contains("spread")
    ) %>% 
    st_set_geometry(NULL) %>%
    gt() %>%
    tab_header(
        title = "Hoan Huynh: Precincts Lost", 
        subtitle = "IL House District 13 Democratic Primary Results"
    ) %>%
    tab_spanner(
        label = md("Hoan"),
        columns = contains("hoan")
    ) %>%
    tab_spanner(
        label = "Eileen",
        columns = contains("eileen")
    ) %>%
    tab_source_note(source_note = "created by Jane Yang: https://github.com/janejuenyang") %>%
    tab_footnote(
      footnote = "data downloaded July 9, 2022 9am CDT",
      locations = cells_column_labels(columns = votes_13)
    ) %>%
    cols_label(
        ward = "Ward",
        precinct = "Precinct",
        votes_13 = "Total Votes Cast",
        votes_hoan = "#",
        votes_eileen = "#",
        votes_hoan_pct = "%",
        votes_eileen_pct = "%",
        spread_votes = "Votes Spread",
        spread_pp = "p.p. Spread"
    ) %>%
    fmt_percent(
        columns = c(contains("pct"), "spread_pp"),
        decimals = 0
    ) %>%
    fmt_number(
        columns = contains("votes"),
        decimals = 0
    )
s_lost
gtsave(s_lost, paste(od, "votes_il13_lost_precincts.png", sep = "/"))

#### voter turnout ####
# il house district 13
g_il13_turnout_votes <- gg_il13(
        precinct_data = d_turnout %>% filter(in_il13),
        precinct_fill_var = ballots_cast,
        precinct_label_color_var = label_color_il13,
        lab_title = "Voter Turnout",
        lab_subtitle = "IL-13 Democratic Primary 2022",
        lab_caption = "data downloaded July 9, 2022 9am CDT",
        lab_fill = "ballots cast"
    )  +
    theme(legend.position = "top") +
    scale_fill_viridis_c(
        direction = -1, 
        label = comma_format(accuracy = 1)
    )

g_il13_turnout_pct <- gg_il13(
        precinct_data = d_turnout %>% filter(in_il13),
        precinct_fill_var = pct_turnout,
        precinct_label_color_var = label_color_il13,
        lab_title = "",
        lab_subtitle = "",
        lab_fill = "% turnout"
    )  +
    theme(legend.position = "top")
g_il13_turnout <- grid.arrange(g_il13_turnout_votes, g_il13_turnout_pct, ncol = 2)
ggsave(
    plot = g_il13_turnout, 
    filename = paste(od, "turnout_il13.jpg", sep = "/"),
    units = "in", height = 8, width = 12
)

# ward 46
g_uptown <- get_googlemap(
        center = centroid_wards %>% 
            filter(ward == 46) %>% 
            select(longitude, latitude) %>% 
            unlist(),
        zoom = 14,
        maptype = "roadmap"
    )

g_turnout_ward46 <- ggmap(
        g_uptown, 
        base_layer = ggplot(data = d_turnout %>% filter(ward == 46))
    ) +
    # precinct-level data
    geom_sf(
        mapping = aes(fill = pct_turnout),
        size = 0.25,
        alpha = 0.85
    ) +
    geom_text(
        mapping = aes(
            label = precinct,
            x = latitude,
            y = longitude
        ),
        size = 2,
        color = d_turnout %>% filter(ward == 46) %>% pull(label_color_ward46)
    ) +
    # general formatting
    scale_fill_viridis_c(
        direction = -1, 
        label = percent_format(accuracy = 1)
    ) +
    labs(
        title = "Ward 46 Voter Turnout, 2022 Democratic Primary",
        subtitle = "data downloaded July 9, 2022 9am CDT",
        caption = "created by Jane Yang: https://github.com/janejuenyang",
        x = "",
        y = "",
        fill = "% turnout"
    )
g_turnout_ward46
ggsave(paste(od, "turnout_ward46.jpg", sep = "/"),
    units = "in", height = 10, width = 10)

# chicago
g_chicago <- get_googlemap(
        center = "chicago",
        zoom = 10,
        maptype = "roadmap"
    )

g_turnout <- ggmap(g_chicago, base_layer = ggplot(data = d_turnout)) +
    # precinct-level data
    geom_sf(
        mapping = aes(fill = pct_turnout),
        size = 0.25,
        alpha = 0.85
    ) +
    geom_text(
        mapping = aes(
            label = precinct,
            x = latitude,
            y = longitude
        ),
        size = 1,
        color = d_turnout$label_color_city
    ) +
    # ward-level data
    geom_sf(
        data = borders_chicago_wards,
        alpha = 0,
        size = 0.5,
        color = "black"
    ) +
    # general formatting
    scale_fill_viridis_c(
        direction = -1, 
        label = percent_format(accuracy = 1)
    ) +
    labs(
        title = "Chicago Voter Turnout, 2022 Democratic Primary",
        subtitle = "data downloaded July 9, 2022 9am CDT",
        caption = "created by Jane Yang: https://github.com/janejuenyang",
        x = "",
        y = "",
        fill = "% turnout"
    )
g_turnout
ggsave(paste(od, "turnout_chicago.jpg", sep = "/"),
    units = "in", height = 10, width = 10)
