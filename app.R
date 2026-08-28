# =============================================================================
# Illinois O-D Trip Flow Dashboard
# -----------------------------------------------------------------------------
# Maps origin-destination trip flows between Illinois census tracts, with a
# single "what to map" selector (category + metric) driving line color and
# thickness, plus a trip-count threshold and a hard cap on lines drawn.
#
# DATA SHAPE NOTE: the real OD table (str(il_full_od)) has one row per
# (origin tract, destination tract) pair, with trip counts broken out three
# separate ways — by mode, by demographic (race/age/income), and by trip
# purpose. These are independent MARGINAL totals, not a joint cross-tab:
# e.g. race_black_not_hispanic_or_latino is total Black-traveler trips on
# that OD pair across ALL modes and purposes combined, not "Black + transit
# + work" trips specifically. That's why the UI below is a single metric
# selector rather than three simultaneously-intersecting filters — the
# source data doesn't support a true 3-way intersection without the
# underlying microdata.
#
# Toggle USE_SYNTHETIC_DATA below to run this against your real data. With
# it TRUE (default) the app runs standalone on made-up data, same as before.
# =============================================================================
# 
# setwd("C:/Users/mdunst/OneDrive - Cambridge Systematics/Documents/GitHub/IDOT_ICB_Study")
# rsconnect::writeManifest()

library(shiny)
library(tidyr)
library(leaflet)
library(dplyr)
library(tibble)
library(sf)
library(scales)
library(data.table)  # fast read for the real 2M+ row OD table
library(readxl)       # tract -> UZA crosswalk spreadsheet
library(ggplot2)      # builds the bar chart geometry, then handed to plotly
library(plotly) 

# -----------------------------------------------------------------------------
# 1. CONFIG
# -----------------------------------------------------------------------------

USE_SYNTHETIC_DATA <- FALSE   # set FALSE once OD_DATA_PATH / CENTROID_PATH are real

OD_DATA_PATH <- "data\\full_od_weekday_082826.rds"           # .rds (recommended) or .csv — your 53-col table
CENTROID_PATH <- "data\\CenPop2020_Mean_TR17.txt" # Census 2020 Centers of Population, IL rows — STATEFP/COUNTYFP/TRACTCE/LATITUDE/LONGITUDE
UZA_CROSSWALK_PATH <- "data\\Tract - County - UZA Lookup.xlsx"

EXCLUDE_INTRATRACT_TRIPS <- TRUE  # drop origin_fips == destination_fips (degenerate zero-length lines)

ILLINOIS_CENTER_LON <- -89.15
ILLINOIS_CENTER_LAT <- 40.0
ILLINOIS_DEFAULT_ZOOM <- 6

MAX_LINES_ON_MAP <- 10000   # default cap on lines drawn — see Section 6 note
MIN_LINE_WEIGHT <- 1
MAX_LINE_WEIGHT <- 10
LINE_COLOR_PALETTE <- "YlOrRd"

# Category -> column-name lookup, grouped for the UI's cascading dropdown.
# Populates a named nested list so selectInput can render optgroups.
METRIC_CHOICES <- list(
  "Overall" = c(
    "Total Trips" = "total_count"
  ),
  "Mode" = c(
    "Walking"                    = "walking_count",
    "Biking"                     = "biking_count",
    "Public Transit"             = "public_transit_count",
    "On-Demand Auto (rideshare)" = "on_demand_auto_count",
    "Private Auto"               = "private_auto_count",
    "Auto Passenger"             = "auto_passenger_count",
    "Commercial"                 = "commercial_count",
    "Other Mode"                 = "other_travel_mode_count"
  ),
  "Race / Ethnicity" = c(
    "White, non-Hispanic"      = "race_white_not_hispanic_or_latino",
    "Black, non-Hispanic"      = "race_black_not_hispanic_or_latino",
    "Hispanic or Latino"       = "race_hispanic_or_latino_origin",
    "Two+ Races, non-Hispanic" = "race_two_races_not_hispanic_or_latino",
    "AIAN, non-Hispanic"       = "race_aian_not_hispanic_or_latino",
    "NHOPI, non-Hispanic"      = "race_nhopi_not_hispanic_or_latino",
    "Asian, non-Hispanic"      = "race_asian_not_hispanic_or_latino",
    "Other race, non-Hispanic" = "race_other_not_hispanic_or_latino"
  ),
  "Age" = c(
    "Under 5"  = "age_under_5",
    "5 to 11"  = "age_5_to_11",
    "12 to 17" = "age_12_to_17",
    "18 to 34" = "age_18_to_34",
    "35 to 49" = "age_35_to_49",
    "50 to 64" = "age_50_to_64",
    "Over 65"  = "age_over_65"
  ),
  "Household Income" = c(
    "Under $15k"  = "income_under_15k",
    "$15k-$25k"   = "income_15k_to_25k",
    "$25k-$50k"   = "income_25k_to_50k",
    "$50k-$75k"   = "income_50k_to_75k",
    "$75k-$100k"  = "income_75k_to_100k",
    "$100k-$150k" = "income_100k_to_150k",
    "$150k-$200k" = "income_150k_to_200k",
    "Over $200k"  = "income_over_200k"
  ),
  "Trip Purpose" = c(
    "Home"        = "home_count",
    "Work"        = "work_count",
    "School"      = "school_count",
    "Shop"        = "shop_count",
    "Eat"         = "eat_count",
    "Social"      = "social_count",
    "Recreation"  = "recreation_count",
    "Maintenance" = "maintenance_count",
    "Lodging"     = "lodging_count",
    "Other Purpose" = "other_activity_type_count"
  )
)

ALL_METRIC_COLS <- unname(unlist(METRIC_CHOICES))

# -----------------------------------------------------------------------------
# 2. DATA LOADING & CLEANING
# -----------------------------------------------------------------------------
# A few things worth double-checking in your extract before trusting it at
# face value:
#   - total_count.x, total_count.y, and total_count all look identical in
#     the sample rows - collapsed to one `total_count` column below.
#   - commercial_count.x / commercial_count.y likewise - collapsed to one.
#   - race_visitor / age_visitor / income_visitor look identical to each
#     other row-by-row in the sample - worth confirming what these actually
#     represent before using them; deliberately left out of METRIC_CHOICES.

load_od_data <- function(path) {
  
  print(paste("Loading OD data from", path))
  
  if (grepl("\\.rds$", path, ignore.case = TRUE)) {
    readRDS(path)
  } else {
    data.table::fread(path) %>% as_tibble()
  }
}

clean_od_data <- function(raw_df) {
  
  raw_df <- raw_df %>% as_tibble()
  
  print(paste("Raw OD data:", nrow(raw_df), "rows,", ncol(raw_df), "columns"))
  
  if (!"total_count" %in% names(raw_df) && "total_count.x" %in% names(raw_df)) {
    raw_df <- raw_df %>% mutate(total_count = total_count.x)
  }
  if (!"commercial_count" %in% names(raw_df) && "commercial_count.x" %in% names(raw_df)) {
    raw_df <- raw_df %>% mutate(commercial_count = commercial_count.x)
  }
  
  keep_cols <- c(
    "origin_fips", "destination_fips",
    intersect(ALL_METRIC_COLS, names(raw_df)),
    intersect("network_distance_miles", names(raw_df))
  )
  
  df <- raw_df %>%
    mutate(
      origin_fips = as.character(origin_fips),
      destination_fips = as.character(destination_fips)
    ) %>%
    select(all_of(keep_cols))
  
  print(paste("Cleaned OD data:", nrow(df), "rows,", ncol(df), "columns"))
  
  df
}

load_tract_centroids <- function(path) {
  # Census 2020 Centers of Population layout: STATEFP, COUNTYFP, TRACTCE,
  # POPULATION, LATITUDE, LONGITUDE. Adjust column names below if your
  # extract differs.
  
  print(paste("Loading tract centroids from", path))
  
  raw <- data.table::fread(path, colClasses = "character") %>% as_tibble()
  
  raw %>%
    transmute(
      GEOID = paste0(STATEFP, COUNTYFP, TRACTCE),
      lon = as.numeric(LONGITUDE),
      lat = as.numeric(LATITUDE),
      population = as.numeric(POPULATION)
    )
}

load_uza_crosswalk <- function(path) {
  # Expects one row per tract with a GEOID column, a County column, and a
  # UZA category column (rural/small/large/Chicago). Update the right-hand
  # column names below to match your actual spreadsheet headers.
  # NOTE: if GEOID was stored as a number in Excel rather than text, leading
  # zeros in the county/tract portion may already be dropped on read - check
  # a few values after loading, and pad with formatC(GEOID, width = 11,
  # flag = "0") here if so.
  
  print(paste("Loading UZA crosswalk from", path))
  
  raw <- readxl::read_excel(path)
  
  raw %>%
    transmute(
      GEOID = as.character(GEOID),          # <- your tract GEOID column
      county = as.character(County),        # <- your County column
      uza_category = as.character(`UZA grouping`) # <- your UZA category column
    ) %>%
    distinct(GEOID, .keep_all = TRUE)
}

haversine_miles <- function(lon1, lat1, lon2, lat2) {
  # Straight-line distance, in miles - used only for synthetic demo data.
  # Real data already carries a genuine network_distance_miles column
  # (computed upstream by compute_tract_network_distances.R and joined
  # onto the OD table before it's saved).
  R <- 3958.8  # Earth radius, miles
  to_rad <- pi / 180
  dlat <- (lat2 - lat1) * to_rad
  dlon <- (lon2 - lon1) * to_rad
  a <- sin(dlat / 2)^2 + cos(lat1 * to_rad) * cos(lat2 * to_rad) * sin(dlon / 2)^2
  2 * R * asin(pmin(1, sqrt(a)))
}

join_centroids <- function(od_df, centroids_df) {
  
  print(paste("Joining centroids: od rows =", nrow(od_df), "| centroid rows =", nrow(centroids_df)))
  
  out <- od_df %>%
    left_join(centroids_df %>% select(GEOID, lon, lat) %>% rename(o_lon = lon, o_lat = lat), by = c("origin_fips" = "GEOID")) %>%
    left_join(centroids_df %>% select(GEOID, lon, lat) %>% rename(d_lon = lon, d_lat = lat), by = c("destination_fips" = "GEOID"))
  
  n_unmatched <- out %>% filter(is.na(o_lon) | is.na(d_lon)) %>% nrow()
  if (n_unmatched > 0) {
    print(paste("WARNING:", n_unmatched, "OD rows dropped - origin or destination FIPS not found in centroid file"))
  }
  
  out %>% filter(!is.na(o_lon), !is.na(d_lon))
}

generate_synthetic_od_and_centroids <- function(n_tracts = 60, n_pairs = 2000) {
  
  print(paste("Generating synthetic centroids for", n_tracts, "tracts"))
  
  synthetic_counties <- c("Cook", "DuPage", "Will", "Lake", "Kane", "McHenry", "Sangamon", "Champaign")
  synthetic_uza_categories <- c("Chicago", "Large", "Small", "Rural")
  
  centroids <- tibble(
    GEOID = sprintf("17SYN%06d", seq_len(n_tracts)),
    lon = runif(n_tracts, -91.4, -87.3),
    lat = runif(n_tracts, 37.0, 42.4),
    population = round(runif(n_tracts, 500, 8000))
  )
  
  uza_crosswalk <- tibble(
    GEOID = centroids$GEOID,
    county = sample(synthetic_counties, n_tracts, replace = TRUE),
    uza_category = sample(synthetic_uza_categories, n_tracts, replace = TRUE)
  )
  
  print(paste("Generating", n_pairs, "synthetic OD pairs across", length(ALL_METRIC_COLS), "metric columns"))
  
  od <- tibble(
    origin_fips = sample(centroids$GEOID, n_pairs, replace = TRUE),
    destination_fips = sample(centroids$GEOID, n_pairs, replace = TRUE),
    total_count = round(rlnorm(n_pairs, meanlog = 5, sdlog = 1.2))
  ) %>%
    filter(origin_fips != destination_fips)
  
  # Sub-category columns as a random share of total_count - NOT constrained
  # to actually sum to total_count. Real data won't have that limitation.
  sub_cols <- setdiff(ALL_METRIC_COLS, "total_count")
  for (col in sub_cols) {
    od[[col]] <- round(od$total_count * runif(nrow(od), 0, 0.4))
  }
  
  # Straight-line stand-in for network_distance_miles in synthetic mode -
  # real data already carries this column natively (see clean_od_data()).
  od <- od %>%
    left_join(centroids %>% select(GEOID, lon, lat) %>% rename(o_lon = lon, o_lat = lat), by = c("origin_fips" = "GEOID")) %>%
    left_join(centroids %>% select(GEOID, lon, lat) %>% rename(d_lon = lon, d_lat = lat), by = c("destination_fips" = "GEOID")) %>%
    mutate(network_distance_miles = haversine_miles(o_lon, o_lat, d_lon, d_lat)) %>%
    select(-o_lon, -o_lat, -d_lon, -d_lat)
  
  list(od = od, centroids = centroids, uza_crosswalk = uza_crosswalk)
}

# -----------------------------------------------------------------------------
# 3. LOAD DATA (once, at app start - shared across all Shiny sessions)
# -----------------------------------------------------------------------------

if (USE_SYNTHETIC_DATA) {
  synthetic <- generate_synthetic_od_and_centroids()
  od_raw <- synthetic$od
  centroids <- synthetic$centroids
  uza_crosswalk <- synthetic$uza_crosswalk
} else {
  od_raw <- clean_od_data(load_od_data(OD_DATA_PATH))
  centroids <- load_tract_centroids(CENTROID_PATH)
  uza_crosswalk <- load_uza_crosswalk(UZA_CROSSWALK_PATH)
}

od_data_full <- join_centroids(od_raw, centroids)

if (EXCLUDE_INTRATRACT_TRIPS) {
  n_before <- nrow(od_data_full)
  od_data_full <- od_data_full %>% filter(origin_fips != destination_fips)
  print(paste("Excluded", n_before - nrow(od_data_full), "intra-tract (origin == destination) rows"))
}

print(paste("od_data_full ready:", nrow(od_data_full), "OD pairs with valid centroids"))

od_data_full <- od_data_full %>%
  left_join(
    uza_crosswalk %>% rename(origin_county = county, origin_uza_category = uza_category),
    by = c("origin_fips" = "GEOID")
  )

n_missing_uza <- od_data_full %>% filter(is.na(origin_county)) %>% nrow()
if (n_missing_uza > 0) {
  print(paste("WARNING:", n_missing_uza, "OD rows have no UZA/County match on origin_fips"))
}

N_COUNTIES <- n_distinct(uza_crosswalk$county, na.rm = TRUE)  # sizes the "By County" chart in the UI below

# Tract-level population rolled up to county and to UZA type - computed
# once, since population doesn't depend on the selected metric.
tract_population <- centroids %>%
  select(GEOID, population) %>%
  left_join(uza_crosswalk %>% select(GEOID, county, uza_category), by = "GEOID")

n_missing_population <- tract_population %>% filter(is.na(population)) %>% nrow()
if (n_missing_population > 0) {
  print(paste("WARNING:", n_missing_population, "tracts have no population value"))
}

county_population <- tract_population %>%
  filter(!is.na(county), !is.na(population)) %>%
  group_by(county) %>%
  summarise(population = sum(population, na.rm = TRUE), .groups = "drop")

uza_type_population <- tract_population %>%
  filter(!is.na(uza_category), !is.na(population)) %>%
  group_by(uza_category) %>%
  summarise(population = sum(population, na.rm = TRUE), .groups = "drop") %>%
  mutate(pop_share = population / sum(population))

# A county can technically span more than one UZA category at the tract
# level - this assigns each county its population-weighted MAJORITY
# category, used only to color the By County chart below.
county_uza_type <- tract_population %>%
  filter(!is.na(county), !is.na(uza_category), !is.na(population)) %>%
  group_by(county, uza_category) %>%
  summarise(population = sum(population, na.rm = TRUE), .groups = "drop") %>%
  group_by(county) %>%
  slice_max(order_by = population, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(county, uza_category)

# network_distance_miles arrives already merged into the OD table itself
# (see clean_od_data() / the synthetic generator above) - nothing left to
# compute or join here, just confirm it made it through intact.
n_missing_distance <- od_data_full %>% filter(is.na(network_distance_miles)) %>% nrow()
if (n_missing_distance > 0) {
  print(paste("WARNING:", n_missing_distance, "OD rows have no network_distance_miles value"))
}

print(paste(
  "Network distance summary (miles) - min:", round(min(od_data_full$network_distance_miles, na.rm = TRUE), 1),
  "| median:", round(median(od_data_full$network_distance_miles, na.rm = TRUE), 1),
  "| max:", round(max(od_data_full$network_distance_miles, na.rm = TRUE), 1)
))

# -----------------------------------------------------------------------------
# 4. METRIC RANGES (precomputed once, used to set slider bounds instantly)
# -----------------------------------------------------------------------------

compute_metric_ranges <- function(df, metric_cols) {
  
  print(paste("Computing metric ranges for", length(metric_cols), "columns"))
  
  ranges <- lapply(metric_cols, function(col) {
    v <- df[[col]]
    c(min = min(v, na.rm = TRUE), max = max(v, na.rm = TRUE))
  })
  names(ranges) <- metric_cols
  ranges
}

metric_ranges <- compute_metric_ranges(od_data_full, intersect(ALL_METRIC_COLS, names(od_data_full)))

# -----------------------------------------------------------------------------
# 5. LINE GEOMETRY HELPER
# -----------------------------------------------------------------------------
# Only ever called on the small, already-filtered-and-capped subset (see
# Section 7) - never on the full od_data_full table.

build_od_linestrings <- function(df) {
  
  df <- df %>% ungroup()
  
  if (nrow(df) == 0) return(df)
  
  geoms <- lapply(seq_len(nrow(df)), function(i) {
    st_linestring(matrix(
      c(df$o_lon[i], df$d_lon[i], df$o_lat[i], df$d_lat[i]),
      ncol = 2
    ))
  })
  
  df %>%
    mutate(geometry = st_sfc(geoms, crs = 4326)) %>%
    st_as_sf()
}

# -----------------------------------------------------------------------------
# 6. UI
# -----------------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Illinois O-D Trip Flow Explorer"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput("metric_category", "Category",
                  choices = names(METRIC_CHOICES), selected = "Overall"),
      uiOutput("metric_selector"),
      sliderInput("min_count", "Minimum trip count per line",
                  min = 0, max = 100, value = 1),
      numericInput("max_lines", "Max lines to draw",
                   value = MAX_LINES_ON_MAP, min = 50, max = 5000, step = 50),
      sliderInput("distance_range", "Trip distance (miles)",
                  min = 0,
                  max = ceiling(max(od_data_full$network_distance_miles, na.rm = TRUE)),
                  value = c(20, ceiling(max(od_data_full$network_distance_miles, na.rm = TRUE))),
                  step = 1),
      helpText("The threshold filters candidate OD pairs first; only the",
               "top N by the selected metric are then drawn, to keep the",
               "map responsive against a multi-million-row table."),
      hr(),
      strong("Filtered summary"),
      textOutput("summary_text")
    ),
    mainPanel(
      width = 9,
      leafletOutput("od_map", height = "80vh")
    )
  ),
  hr(),
  h4("Trip Starts by Urbanized Area"),
  tabsetPanel(
    tabPanel("By County", plotlyOutput("county_bar_chart", height = paste0(max(400, N_COUNTIES * 18), "px"))),
    tabPanel("By UZA Type", plotlyOutput("uza_type_bar_chart", height = "320px"))
  )
)

# -----------------------------------------------------------------------------
# 7. SERVER
# -----------------------------------------------------------------------------

server <- function(input, output, session) {
  
  output$metric_selector <- renderUI({
    choices <- METRIC_CHOICES[[input$metric_category]]
    selectInput("metric_col", "Metric", choices = choices)
  })
  
  observeEvent(input$metric_col, {
    req(input$metric_col)
    rng <- metric_ranges[[input$metric_col]]
    updateSliderInput(session, "min_count",
                      min = 0, max = ceiling(unname(rng["max"])), value = 1)
  }, ignoreNULL = TRUE)
  
  # Core mechanism: filter the full OD table by the selected metric column,
  # threshold, and distance range, then cap to the top N pairs by that
  # metric so downstream geometry-building and map rendering stay cheap
  # regardless of table size.
  filtered_od <- reactive({
    
    req(input$metric_col)
    metric_col <- input$metric_col
    
    df_all <- od_data_full %>%
      mutate(metric_value = .data[[metric_col]]) %>%
      filter(
        metric_value >= input$min_count,
        !is.na(network_distance_miles),
        network_distance_miles >= input$distance_range[1],
        network_distance_miles <= input$distance_range[2]
      )
    
    n_total_matching <- nrow(df_all)
    total_trips_matching <- sum(df_all$metric_value)
    
    df_capped <- df_all %>%
      slice_max(order_by = metric_value, n = input$max_lines, with_ties = FALSE)
    
    print(paste(
      n_total_matching, "OD pairs match metric =", metric_col,
      "| min_count =", input$min_count,
      "| drawing top", nrow(df_capped)
    ))
    
    list(
      capped = df_capped,
      n_total_matching = n_total_matching,
      total_trips_matching = total_trips_matching
    )
  })
  
  # County/UZA-type summary tables react to the selected metric and distance
  # range only (not to the line threshold/cap, which are map-rendering
  # controls) - these show the full picture for whichever metric and
  # distance band is selected.
  county_summary <- reactive({
    req(input$metric_col)
    metric_col <- input$metric_col
    
    od_data_full %>%
      mutate(metric_value = .data[[metric_col]]) %>%
      filter(
        !is.na(origin_county),
        !is.na(network_distance_miles),
        network_distance_miles >= input$distance_range[1],
        network_distance_miles <= input$distance_range[2]
      ) %>%
      group_by(origin_county) %>%
      summarise(total_trips = sum(metric_value, na.rm = TRUE), .groups = "drop") %>%
      left_join(county_population, by = c("origin_county" = "county")) %>%
      left_join(county_uza_type, by = c("origin_county" = "county")) %>%
      filter(!is.na(population), population > 0) %>%
      mutate(trips_per_capita = total_trips / population) %>%
      arrange(desc(trips_per_capita))
  })
  
  uza_type_summary <- reactive({
    req(input$metric_col)
    metric_col <- input$metric_col
    
    trip_shares <- od_data_full %>%
      mutate(metric_value = .data[[metric_col]]) %>%
      filter(
        !is.na(origin_uza_category),
        !is.na(network_distance_miles),
        network_distance_miles >= input$distance_range[1],
        network_distance_miles <= input$distance_range[2]
      ) %>%
      group_by(origin_uza_category) %>%
      summarise(total_trips = sum(metric_value, na.rm = TRUE), .groups = "drop") %>%
      mutate(trip_share = total_trips / sum(total_trips))
    
    trip_shares %>%
      left_join(uza_type_population, by = c("origin_uza_category" = "uza_category")) %>%
      select(origin_uza_category, total_trips, trip_share, population, pop_share) %>%
      pivot_longer(
        cols = c(trip_share, pop_share),
        names_to = "share_type",
        values_to = "share_value"
      ) %>%
      mutate(share_type = recode(share_type,
                                 trip_share = "Actual Trip Share",
                                 pop_share = "Expected (Population) Share"
      ))
  })
  
  # Base map built exactly once. All filter-driven redraws go through
  # leafletProxy() below, so the user's pan/zoom isn't reset on every filter.
  output$od_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = ILLINOIS_CENTER_LON, lat = ILLINOIS_CENTER_LAT,
              zoom = ILLINOIS_DEFAULT_ZOOM)
  })
  
  observe({
    
    res <- filtered_od()
    od_sf <- build_od_linestrings(res$capped)
    
    proxy <- leafletProxy("od_map") %>%
      clearGroup("od_lines") %>%
      clearControls()
    
    if (nrow(od_sf) == 0) {
      return(invisible(NULL))
    }
    
    # Draw weakest -> strongest, so heavy OD pairs render on top of the
    # dense pile of lighter, overlapping lines instead of being buried
    # underneath them.
    od_sf <- od_sf %>% arrange(metric_value)
    
    # Color/thickness on a geometric (log) scale rather than linear - equal
    # visual steps now represent equal RATIOS in trip count, so the mass of
    # lower-volume pairs stops collapsing into visually-identical lines.
    geo_value <- log1p(od_sf$metric_value)  # log1p handles metric_value == 0 safely
    
    pal <- colorNumeric(LINE_COLOR_PALETTE, domain = geo_value)
    
    line_weights <- scales::rescale(
      geo_value,
      to = c(MIN_LINE_WEIGHT, MAX_LINE_WEIGHT),
      from = range(geo_value)
    )
    
    proxy %>%
      addPolylines(
        data = od_sf,
        group = "od_lines",
        color = pal(geo_value),
        weight = line_weights,
        opacity = 0.7,
        label = ~paste0(origin_fips, " -> ", destination_fips, " (", metric_value, " trips)"),
        popup = ~paste0(
          "<b>", origin_fips, " &rarr; ", destination_fips, "</b><br>",
          "Selected metric: ", metric_value, " trips<br>",
          "Total (all modes/purposes): ", total_count
        )
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = geo_value,
        title = "Trip count",
        opacity = 0.7,
        labFormat = labelFormat(transform = function(x) round(expm1(x)))
      )
  })
  
  output$summary_text <- renderText({
    res <- filtered_od()
    paste0(
      format(res$n_total_matching, big.mark = ","), " OD pairs match this filter (",
      format(nrow(res$capped), big.mark = ","), " drawn), ",
      format(res$total_trips_matching, big.mark = ","), " total trips"
    )
  })
  
  output$county_bar_chart <- renderPlotly({
    df <- county_summary()
    
    p <- ggplot(df, aes(
      x = reorder(origin_county, trips_per_capita),
      y = trips_per_capita,
      fill = uza_category,
      text = paste0(
        origin_county, "<br>",
        round(trips_per_capita, 2), " trips per capita<br>",
        format(total_trips, big.mark = ","), " trips / ",
        format(population, big.mark = ","), " population<br>",
        "UZA type: ", uza_category
      )
    )) +
      geom_col() +
      coord_flip() +
      scale_fill_brewer(palette = "Set2", name = "UZA Type") +
      labs(x = NULL, y = "Trips per capita", title = "Trip Starts per Capita by County") +
      theme_minimal(base_size = 12)
    
    ggplotly(p, tooltip = "text")
  })
  
  output$uza_type_bar_chart <- renderPlotly({
    df <- uza_type_summary()
    
    p <- ggplot(df, aes(
      x = origin_uza_category,
      y = share_value,
      fill = share_type,
      text = paste0(
        origin_uza_category, "<br>",
        share_type, ": ", scales::percent(share_value, accuracy = 0.1)
      )
    )) +
      geom_col(position = "dodge") +
      scale_y_continuous(labels = scales::percent) +
      scale_fill_manual(values = c(
        "Actual Trip Share" = "#c0392b",
        "Expected (Population) Share" = "#7f8c8d"
      )) +
      labs(x = NULL, y = "Share of total", fill = NULL,
           title = "Trip Share vs. Population Share by UZA Type") +
      theme_minimal(base_size = 12)
    
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", y = -0.15))
  })
}

# -----------------------------------------------------------------------------
# 8. RUN APP
# -----------------------------------------------------------------------------

shinyApp(ui = ui, server = server)

