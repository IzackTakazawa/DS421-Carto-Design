# app.R – Minimal Shiny app for DS421 final project
# Global Shark Attack Incidents by Country

library(shiny)
library(tidyverse)
library(lubridate)
library(janitor)
library(leaflet)
library(RColorBrewer)
library(htmlwidgets)
library(scales)
library(countrycode)
library(rnaturalearth)
library(htmltools)
library(RKaggle)

# ---- Data loading and preparation (run once at startup) ----

# Download and load dataset
shark_data <- RKaggle::get_dataset("thedevastator/global-shark-attack-incidents")
shark <- shark_data[[1]]

# Identify the exact fatal column name
fatal_col <- colnames(shark)[str_detect(tolower(colnames(shark)), "fatal")][1]

# If you know it's exactly "Fatal (Y/N)", you can hard-code:
# fatal_col <- "Fatal (Y/N)"

# Clean and select variables
shark_clean <- shark |>
  select(
    `Case Number`,
    Date,
    Type,
    Country,
    Area,
    Location,
    Activity,
    Injury,
    !!sym(fatal_col),
    Time,
    Species
  ) |>
  mutate(
    injury_cat = case_when(
      !!sym(fatal_col) == "Y" | str_detect(tolower(Injury), "fatal") ~ "Fatal",
      str_detect(tolower(Injury), "severe")                           ~ "Severe",
      str_detect(tolower(Injury), "minor")                            ~ "Minor",
      TRUE                                                            ~ "No injury / Unknown"
    )
  )

# Aggregate to country level
shark_country <- shark_clean |>
  filter(!is.na(Country), Country != "") |>
  mutate(
    fatal_flag = if_else(!!sym(fatal_col) == "Y", 1L, 0L),
    Country = as.character(trimws(Country))
  ) |>
  group_by(Country) |>
  summarise(
    n_incidents = n(),
    n_fatal     = sum(fatal_flag),
    fatal_rate  = n_fatal / n_incidents,
    .groups = "drop"
  ) |>
  mutate(
    iso3 = countrycode(
      Country,
      origin = "country.name",
      destination = "iso3c",
      warn = FALSE
    )
  ) |>
  filter(!is.na(iso3))

# Load world polygons
world_sf <- ne_countries(scale = "medium", returnclass = "sf")

# Join shark data to world polygons
world_shark <- world_sf |>
  left_join(shark_country, by = c("iso_a3" = "iso3")) |>
  mutate(
    log_incidents = log1p(n_incidents)
  )

# Create robust breaks for cut()
breaks <- unique(
  quantile(world_shark$log_incidents,
           probs = seq(0, 1, by = 0.25),
           na.rm = TRUE)
)

if (length(breaks) < 2) {
  breaks <- c(min(world_shark$log_incidents, na.rm = TRUE),
              max(world_shark$log_incidents, na.rm = TRUE))
}

breaks <- c(min(breaks), breaks, max(breaks))
breaks <- unique(breaks)

world_shark <- world_shark |>
  mutate(
    inc_cat = cut(
      log_incidents,
      breaks = breaks,
      include.lowest = TRUE,
      labels = paste0("Q", seq_len(length(breaks) - 1))
    )
  )

# Color palette
inc_pal <- colorFactor(
  palette = brewer.pal(min(length(levels(world_shark$inc_cat)), 5), "Blues"),
  domain  = levels(world_shark$inc_cat),
  levels  = levels(world_shark$inc_cat)
)

# Popup content
popup_country <- paste0(
  "<strong>Country:</strong> ", world_shark$name, "<br/>",
  "<strong>Incidents:</strong> ", ifelse(is.na(world_shark$n_incidents), 0, world_shark$n_incidents), "<br/>",
  "<strong>Fatal:</strong> ", ifelse(is.na(world_shark$n_fatal), 0, world_shark$n_fatal), "<br/>",
  "<strong>Fatal rate:</strong> ",
  scales::percent(ifelse(is.na(world_shark$fatal_rate), 0, world_shark$fatal_rate), accuracy = 0.1)
)

# Build the map object
m_country <- leaflet(world_shark) |>
  addProviderTiles("CartoDB.Positron") |>
  addPolygons(
    fillColor = ~inc_pal(inc_cat),
    fillOpacity = 0.8,
    color = "white",
    weight = 0.5,
    smoothFactor = 0.5,
    popup = popup_country,
    highlightOptions = highlightOptions(
      color = "yellow",
      weight = 2,
      bringToFront = TRUE
    )
  ) |>
  addLegend(
    pal = inc_pal,
    values = ~inc_cat,
    title = "Shark incidents (log scale)",
    position = "bottomright",
    opacity = 0.9
  ) |>
  setView(lng = -150, lat = 20, zoom = 3)

# Add title
title_html <- tags$div(
  tags$h3(
    "Global Shark Attack Incidents by Country",
    style = "position:absolute; top:10px; left:50px; z-index:1000;
           background: rgba(255,255,255,0.85); padding: 6px 10px;
           border-radius: 4px; font-family: sans-serif; margin: 0;"
  )
)

m_country <- m_country |> addControl(title_html, position = "topleft")

# ---- Shiny UI and server ----

ui <- fluidPage(
  titlePanel("DS421 Final Project – Global Shark Attack Incidents"),
  leafletOutput("sharkMap", height = "700px")
)

server <- function(input, output, session) {
  output$sharkMap <- renderLeaflet({
    m_country
  })
}

shinyApp(ui, server)


#rsconnect::writeManifest()
