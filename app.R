
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
library(shinydashboard)

#LOADING DATA INTO R

shark_raw <- RKaggle::get_dataset("thedevastator/global-shark-attack-incidents")
shark <- shark_raw[[1]]

#CLEANING DATA

fatal_col_candidates <- colnames(shark)[str_detect(tolower(colnames(shark)), "fatal")]
if (length(fatal_col_candidates) == 0) {
  stop("No 'fatal' column found in the dataset.")
}
fatal_col <- fatal_col_candidates[1]

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
    ),
    Type = as.character(Type),
    Country = as.character(Country)
  )

#WORLD MAP BASE ALL INCIDENTS

shark_country_all <- shark_clean |>
  filter(!is.na(Country), Country != "") |>
  mutate(
    fatal_flag = if_else(!!sym(fatal_col) == "Y", 1L, 0L),
    Country = trimws(Country)
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

world_sf <- ne_countries(scale = "medium", returnclass = "sf")

world_shark_base <- world_sf |>
  left_join(shark_country_all, by = c("iso_a3" = "iso3")) |>
  mutate(
    n_incidents = replace_na(n_incidents, 0L),
    n_fatal     = replace_na(n_fatal, 0L),
    fatal_rate  = replace_na(fatal_rate, 0),
    log_incidents = log1p(n_incidents)
  )

breaks <- unique(
  quantile(world_shark_base$log_incidents,
           probs = seq(0, 1, by = 0.25),
           na.rm = TRUE)
)

if (length(breaks) < 2) {
  breaks <- c(
    min(world_shark_base$log_incidents, na.rm = TRUE),
    max(world_shark_base$log_incidents, na.rm = TRUE)
  )
}

breaks <- c(min(breaks), breaks, max(breaks))
breaks <- unique(breaks)

world_shark_base <- world_shark_base |>
  mutate(
    inc_cat = cut(
      log_incidents,
      breaks = breaks,
      include.lowest = TRUE,
      labels = paste0("Q", seq_len(length(breaks) - 1))
    )
  )

inc_levels <- levels(world_shark_base$inc_cat)
inc_pal <- colorFactor(
  palette = brewer.pal(min(length(inc_levels), 5), "Blues"),
  domain  = inc_levels,
  levels  = inc_levels
)

#SHINY ----HELP WITH BUILD CONTAINERS

ui <- fluidPage(
  theme = bslib::bs_theme(version = 5),
  
  tags$style(HTML("
    body { font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif; }
    .leaflet-container { border-radius: 8px; }
  ")),
  
  titlePanel(
    tagList(
      tags$span("DS421 Final Project"),
      tags$span("– Global Shark Attack Incidents by Country",
                style = "font-weight: normal;")
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      h4("About this project"),
      p(
        "This app visualizes global shark attack incidents by country, ",
        "including totals, fatal counts, and fatal rates. Data are from a ",
        "Kaggle dataset on global shark attacks."
      ),
      
      hr(),
      
      h4("Filters"),
      
      selectInput(
        inputId = "type_filter",
        label   = "Incident type",
        choices = c("All", sort(unique(shark_clean$Type))),
        selected = "All"
      ),
      
      checkboxInput(
        inputId = "fatal_only",
        label   = "Show only fatal incidents",
        value   = FALSE
      ),
      
      p(
        style = "font-size: 0.85em; color: #555;",
        "Note: Incident counts are shown on a log scale to improve contrast between countries."
      )
    ),
    
    mainPanel(
      leafletOutput("sharkMap", height = "700px")
    )
  )
)

#SERVER SHINY

server <- function(input, output, session) {
  
  filtered_data <- reactive({
    d <- shark_clean
    
    if (input$type_filter != "All") {
      d <- d |> filter(Type == input$type_filter)
    }
    
    if (input$fatal_only) {
      d <- d |> filter(!!sym(fatal_col) == "Y")
    }
    
    d
  })
  
  shark_country_filtered <- reactive({
    d <- filtered_data() |>
      filter(!is.na(Country), Country != "") |>
      mutate(
        fatal_flag = if_else(!!sym(fatal_col) == "Y", 1L, 0L),
        Country = trimws(Country)
      ) |>
      group_by(Country) |>
      summarise(
        n_incidents = n(),
        n_fatal     = sum(fatal_flag),
        fatal_rate  = if_else(n_incidents > 0, n_fatal / n_incidents, 0),
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
    
    d
  })
  
  world_shark <- reactive({
    country_data <- shark_country_filtered()
    
    world_sf |>
      left_join(country_data, by = c("iso_a3" = "iso3")) |>
      mutate(
        n_incidents = replace_na(n_incidents, 0L),
        n_fatal     = replace_na(n_fatal, 0L),
        fatal_rate  = replace_na(fatal_rate, 0),
        log_incidents = log1p(n_incidents)
      ) |>
      mutate(
        inc_cat = cut(
          log_incidents,
          breaks = breaks,
          include.lowest = TRUE,
          labels = paste0("Q", seq_len(length(breaks) - 1))
        )
      )
  })
  
  output$sharkMap <- renderLeaflet({
    ws <- world_shark()
    
    popup_content <- paste0(
      "<strong>Country:</strong> ", ws$name, "<br/>",
      "<strong>Incidents:</strong> ", ws$n_incidents, "<br/>",
      "<strong>Fatal:</strong> ", ws$n_fatal, "<br/>",
      "<strong>Fatal rate:</strong> ",
      scales::percent(ws$fatal_rate, accuracy = 0.1)
    )
    
    leaflet(ws) |>
      addProviderTiles("CartoDB.Positron") |>
      addPolygons(
        fillColor = ~inc_pal(inc_cat),
        fillOpacity = 0.85,
        color = "#ffffff",
        weight = 0.7,
        smoothFactor = 0.5,
        popup = popup_content,
        highlightOptions = highlightOptions(
          color = "#f1c40f",
          weight = 2,
          bringToFront = TRUE,
          opacity = 0.9
        )
      ) |>
      addLegend(
        pal = inc_pal,
        values = ~inc_cat,
        title = "Incidents (log₁₀ scale)",
        position = "bottomright",
        opacity = 0.9,
        labFormat = labelFormat(prefix = "")
      ) |>
      setView(lng = -150, lat = 20, zoom = 3)
  })
}

shinyApp(ui, server)