# =============================================================================
# America's Health Divide: Mortality Patterns Across HHS Regions
# DS421 - Cartographic Design | Final Project
# Deployed via Posit Connect Cloud
# =============================================================================

library(shiny)
library(shinythemes)
#install.packages("shinythemes")
library(shinydashboard)   # needed for valueBox / valueBoxOutput
library(bslib)             # needed for bs_theme
library(tidyverse)
library(ggplot2)
library(plotly)
library(DT)
library(viridis)
library(RColorBrewer)
library(leaflet)
library(sf)
library(here)

# here::here() finds the project root regardless of what folder the R
# session's working directory happens to be in, so this won't break if
# you run chunks from a different working directory than the file lives in.
mortality_data <- read_csv(here::here("USRegionalMortality.csv"), show_col_types = FALSE)

# Clean column names
names(mortality_data) <- c("rownames", "Region", "Status", "Sex", "Cause", "Rate", "SE")

# Remove rownames column
mortality_data <- mortality_data %>% select(-rownames)

hhs_regions <- tibble(
  Region = c(
    "HHS Region 01",
    "HHS Region 02",
    "HHS Region 03",
    "HHS Region 04",
    "HHS Region 05",
    "HHS Region 06",
    "HHS Region 07",
    "HHS Region 08",
    "HHS Region 09",
    "HHS Region 10"
  ),
  Region_Short = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10"),
  City = c("Boston", "New York", "Philadelphia", "Atlanta", "Chicago",
           "Dallas", "Kansas City", "Denver", "San Francisco", "Seattle"),
  States = c(
    "CT, ME, MA, NH, RI, VT",
    "NJ, NY, PR, VI",
    "DE, DC, MD, PA, VA, WV",
    "AL, FL, GA, KY, MS, NC, SC, TN",
    "IL, IN, MI, MN, OH, WI",
    "AR, LA, NM, OK, TX",
    "IA, KS, MO, NE",
    "CO, MT, ND, SD, UT, WY",
    "AZ, CA, HI, NV, AS, CNMI, FM, GU, MH, PW",
    "AK, ID, OR, WA"
  ),
  # Approximate centroids for mapping
  lon = c(-71.06, -74.01, -75.17, -84.39, -87.63,
          -96.80, -94.58, -104.99, -122.42, -122.33),
  lat = c(42.36, 40.71, 39.95, 33.75, 41.88,
          32.78, 39.10, 39.74, 37.77, 47.61)
)

outlying_areas <- tibble(
  Area = c(
    "Alaska",
    "Hawaii",
    "American Samoa",
    "Guam",
    "Commonwealth of the Northern Mariana Islands",
    "Federated States of Micronesia",
    "Marshall Islands",
    "Republic of Palau"
  ),
  Region = c(
    "HHS Region 10",  # Alaska -> Seattle region
    "HHS Region 09",  # Hawaii -> San Francisco region
    "HHS Region 09",
    "HHS Region 09",
    "HHS Region 09",
    "HHS Region 09",
    "HHS Region 09",
    "HHS Region 09"
  ),
  lon = c(-152.4044, -155.5828, -170.7183, 144.7937,
          145.6739, 150.5508, 171.1845, 134.5825),
  lat = c(61.3707, 19.8968, -14.2710, 13.4443,
          15.0979, 6.8874, 7.1315, 7.5150)
)

mortality_full <- mortality_data %>%
  left_join(hhs_regions, by = "Region")

# Summary by cause and region (overall)
cause_region_summary <- mortality_full %>%
  group_by(Region, Region_Short, City, States, Cause) %>%
  summarise(
    Avg_Rate = mean(Rate, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Region, desc(Avg_Rate))

# Summary by sex
sex_region_summary <- mortality_full %>%
  group_by(Region, Sex) %>%
  summarise(
    Avg_Rate = mean(Rate, na.rm = TRUE),
    .groups = "drop"
  )

# Summary by urban/rural
ur_region_summary <- mortality_full %>%
  group_by(Region, Status) %>%
  summarise(
    Avg_Rate = mean(Rate, na.rm = TRUE),
    .groups = "drop"
  )

# Top causes overall
top_causes_overall <- mortality_full %>%
  group_by(Cause) %>%
  summarise(
    Avg_Rate = mean(Rate, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(Avg_Rate))

# Urban-rural disparity by cause
ur_disparity <- mortality_full %>%
  group_by(Cause, Status) %>%
  summarise(
    Avg_Rate = mean(Rate, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = Status, values_from = Avg_Rate) %>%
  mutate(
    Disparity = Rural - Urban,
    Pct_Difference = (Rural - Urban) / Urban * 100
  ) %>%
  arrange(desc(Disparity))

# Regional ranking
regional_ranking <- mortality_full %>%
  group_by(Region, Region_Short, City) %>%
  summarise(
    Overall_Rate = mean(Rate, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Overall_Rate)

ui <- fluidPage(
  theme = bs_theme(version = 4, bootswatch = "flatly"),

  tags$head(
    tags$style(HTML("
      .navbar-default { background-color: #2C3E50; border-color: #1A252F; }
      .well { background-color: #F8F9FA; border: 1px solid #DEE2E6; }
      .info-box {
        background-color: rgba(44, 62, 80, 0.08);
        padding: 15px;
        border-radius: 8px;
        margin-bottom: 15px;
      }
      .region-high { color: #E74C3C; font-weight: bold; }
      .region-low { color: #2ECC71; font-weight: bold; }
      .urban-label { color: #3498DB; }
      .rural-label { color: #E67E22; }
      .male-label { color: #2980B9; }
      .female-label { color: #E91E63; }
    "))
  ),

  # ---- Header ----
  div(
    style = "text-align: center; padding: 20px 0; background: linear-gradient(135deg, #2C3E50, #34495E); color: white; border-radius: 8px; margin-bottom: 20px;",
    h1("America's Health Divide"),
    h4("Mortality Patterns Across HHS Regions (2011-2013)"),
    p("Exploring how geography, sex, and urban/rural status shape health outcomes",
      style = "font-size: 16px; opacity: 0.9;")
  ),

  # ---- Navigation Tabs ----
  navbarPage(
    title = "Explore",
    selected = "Interactive Map",
    collapsible = TRUE,

    # ========================================================================
    # TAB 1: INTERACTIVE MAP
    # ========================================================================
    tabPanel(
      "Interactive Map",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          h4("Map Controls"),
          br(),

          selectInput(
            "map_cause",
            "Select Cause of Death:",
            choices = sort(unique(mortality_full$Cause)),
            selected = "Heart disease"
          ),

          selectInput(
            "map_sex",
            "Select Sex:",
            choices = c("Both", "Male", "Female"),
            selected = "Both"
          ),

          selectInput(
            "map_status",
            "Select Urban/Rural:",
            choices = c("Both", "Urban", "Rural"),
            selected = "Both"
          ),

          hr(),
          div(
            class = "info-box",
            h5("What You're Seeing"),
            p("This map shows mortality rates (per 100,000 population) across HHS regions.",
              "Darker colors indicate higher mortality rates.")
          ),
          div(
            class = "info-box",
            h5("Key Finding"),
            p("The Southern regions (04-Atlanta, 06-Dallas) consistently show",
              "higher mortality rates across most causes of death.")
          )
        ),
        mainPanel(
          width = 9,
          leafletOutput("mortality_map", height = "600px"),
          br(),
          fluidRow(
            valueBoxOutput("highest_region"),
            valueBoxOutput("lowest_region"),
            valueBoxOutput("avg_rate")
          ),
          br(),
          h4("Regional Mortality Rankings"),
          DTOutput("ranking_table")
        )
      )
    ),

    # ========================================================================
    # TAB 2: CAUSE COMPARISON
    # ========================================================================
    tabPanel(
      "Cause Comparison",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          h4("Explore Causes"),
          br(),

          selectInput(
            "cause_select",
            "Select Cause:",
            choices = sort(unique(mortality_full$Cause)),
            selected = "Heart disease"
          ),

          checkboxGroupInput(
            "cause_sex",
            "Sex:",
            choices = c("Male", "Female"),
            selected = c("Male", "Female")
          ),

          checkboxGroupInput(
            "cause_status",
            "Urban/Rural:",
            choices = c("Urban", "Rural"),
            selected = c("Urban", "Rural")
          ),

          hr(),
          div(
            class = "info-box",
            h5("Insights"),
            p("Toggle different causes and demographic groups to discover",
              "patterns in mortality across regions.")
          )
        ),
        mainPanel(
          width = 9,
          plotlyOutput("cause_bar_plot", height = "500px"),
          br(),
          fluidRow(
            column(6, plotlyOutput("sex_comparison", height = "300px")),
            column(6, plotlyOutput("ur_comparison", height = "300px"))
          ),
          br(),
          h4("Detailed Cause Data"),
          DTOutput("cause_table")
        )
      )
    ),

    # ========================================================================
    # TAB 3: URBAN-RURAL DISPARITY
    # ========================================================================
    tabPanel(
      "Urban-Rural Disparity",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          h4("Urban vs. Rural Health"),
          br(),

          selectInput(
            "ur_region",
            "Select Region:",
            choices = c("All Regions", sort(unique(mortality_full$Region))),
            selected = "All Regions"
          ),

          selectInput(
            "ur_sex",
            "Select Sex:",
            choices = c("Both", "Male", "Female"),
            selected = "Both"
          ),

          hr(),
          div(
            class = "info-box",
            h5("Key Finding"),
            p("Rural areas consistently show higher mortality rates than urban areas",
              "for most causes of death. The disparity is largest for:",
              tags$br(),
              tags$span(style = "color: #E74C3C;", "- Unintentional injuries"),
              tags$br(),
              tags$span(style = "color: #E67E22;", "- Suicide"),
              tags$br(),
              tags$span(style = "color: #3498DB;", "- Diabetes")
            )
          )
        ),
        mainPanel(
          width = 9,
          plotlyOutput("ur_disparity_plot", height = "450px"),
          br(),
          fluidRow(
            column(6, plotlyOutput("ur_region_plot", height = "400px")),
            column(6, plotlyOutput("ur_cause_plot", height = "400px"))
          ),
          br(),
          h4("Urban-Rural Disparity by Cause"),
          DTOutput("ur_table")
        )
      )
    ),

    # ========================================================================
    # TAB 4: EXPLORE DATA
    # ========================================================================
    tabPanel(
      "Explore Data",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          h4("Filter Data"),
          br(),

          selectInput(
            "filter_region",
            "Region:",
            choices = c("All", sort(unique(mortality_full$Region))),
            selected = "All"
          ),

          selectInput(
            "filter_cause",
            "Cause:",
            choices = c("All", sort(unique(mortality_full$Cause))),
            selected = "All"
          ),

          selectInput(
            "filter_sex",
            "Sex:",
            choices = c("All", "Male", "Female"),
            selected = "All"
          ),

          selectInput(
            "filter_status",
            "Urban/Rural:",
            choices = c("All", "Urban", "Rural"),
            selected = "All"
          ),

          hr(),
          downloadButton("download_data", "Download Data", class = "btn-primary")
        ),
        mainPanel(
          width = 9,
          h4("Mortality Data"),
          DTOutput("full_data_table", height = "600px")
        )
      )
    ),

    # ========================================================================
    # TAB 5: ABOUT
    # ========================================================================
    tabPanel(
      "About",
      fluidRow(
        column(8, offset = 2,
               div(
                 style = "padding: 30px;",
                 h2("About This Project"),
                 br(),

                 h4("Objective"),
                 p("This project visualizes mortality patterns across the 10 HHS regions",
                   "of the United States, exploring how cause of death, sex, and",
                   "urban/rural status intersect with geography to reveal health",
                   "disparities."),
                 br(),

                 h4("Data Source"),
                 p("Data represents mortality rates (per 100,000 population) for",
                   "the period 2011-2013, sourced from the National Center for",
                   "Health Statistics."),
                 br(),

                 h4("Key Variables"),
                 tags$ul(
                   tags$li(tags$strong("Region"), " - HHS administrative region (10 regions)"),
                   tags$li(tags$strong("Status"), " - Urban or Rural classification"),
                   tags$li(tags$strong("Sex"), " - Male or Female"),
                   tags$li(tags$strong("Cause"), " - Cause of death (10 categories)"),
                   tags$li(tags$strong("Rate"), " - Death rate per 100,000 population"),
                   tags$li(tags$strong("SE"), " - Standard error")
                 ),
                 br(),

                 h4("Design Philosophy"),
                 p("The cartographic and data visualization design follows principles of:"),
                 tags$ul(
                   tags$li(tags$strong("Clarity"), " - Clean layouts and intuitive navigation"),
                   tags$li(tags$strong("Interactivity"), " - Users can explore patterns independently"),
                   tags$li(tags$strong("Storytelling"), " - Visual hierarchy guides viewers to key insights"),
                   tags$li(tags$strong("Accessibility"), " - Color palettes chosen for universal readability")
                 ),
                 br(),

                 h4("Reproducibility"),
                 p("This project is fully reproducible. The complete code is available on GitHub."),
                 br(),

                 h4("Course Information"),
                 p(tags$strong("DS421 - Cartographic Design"), " - Final Project"),
                 p("University of Hawaii")
               )
        )
      )
    )
  ),

  # ---- Footer ----
  tags$footer(
    style = "text-align: center; padding: 20px; margin-top: 20px; border-top: 1px solid #DEE2E6; color: #7F8C8D;",
    p("DS421 - Cartographic Design | Final Project | Data: NCHS 2011-2013")
  )
)

server <- function(input, output, session) {

  # ---- 4a. Reactive Data Filters ----

  filtered_data <- reactive({
    df <- mortality_full

    if (!is.null(input$filter_region) && input$filter_region != "All") {
      df <- df %>% filter(Region == input$filter_region)
    }
    if (!is.null(input$filter_cause) && input$filter_cause != "All") {
      df <- df %>% filter(Cause == input$filter_cause)
    }
    if (!is.null(input$filter_sex) && input$filter_sex != "All") {
      df <- df %>% filter(Sex == input$filter_sex)
    }
    if (!is.null(input$filter_status) && input$filter_status != "All") {
      df <- df %>% filter(Status == input$filter_status)
    }

    df
  })

  # ---- 4b. Map Data ----

  map_data <- reactive({
    df <- mortality_full

    # Filter by cause
    df <- df %>% filter(Cause == input$map_cause)

    # Filter by sex
    if (input$map_sex != "Both") {
      df <- df %>% filter(Sex == input$map_sex)
    }

    # Filter by status
    if (input$map_status != "Both") {
      df <- df %>% filter(Status == input$map_status)
    }

    # Aggregate to region level
    df %>%
      group_by(Region, Region_Short, City, States, lon, lat) %>%
      summarise(
        Avg_Rate = mean(Rate, na.rm = TRUE),
        .groups = "drop"
      )
  })

  # Outlying areas (AK, HI, and the Pacific territories) get the same
  # Avg_Rate as the HHS region they belong to, since the data doesn't go
  # below region-level resolution.
  outlying_map_data <- reactive({
    outlying_areas %>%
      left_join(map_data() %>% select(Region, Avg_Rate), by = "Region")
  })

  # ---- 4c. Render Map ----

  output$mortality_map <- renderLeaflet({
    df <- map_data()
    outlying_df <- outlying_map_data()

    # Shared color palette across both the mainland-city markers and the
    # outlying-area markers, so a region reads as the same color everywhere
    # it appears on the map.
    pal <- colorNumeric(
      palette = "RdYlBu",
      domain = c(df$Avg_Rate, outlying_df$Avg_Rate),
      reverse = TRUE
    )

    leaflet(df, options = leafletOptions(worldCopyJump = TRUE, minZoom = 2)) %>%
      addProviderTiles("CartoDB.Positron") %>%
      # Widened to include Alaska and Hawaii by default. American Samoa,
      # Guam, CNMI, FSM, the Marshall Islands, and Palau sit on the far
      # side of the Pacific (across the date line) - drag/scroll the map
      # west to reach them; worldCopyJump lets that panning wrap smoothly.
      fitBounds(lng1 = -172, lat1 = 17, lng2 = -65, lat2 = 72) %>%
      addCircleMarkers(
        lng = ~lon,
        lat = ~lat,
        radius = ~sqrt(Avg_Rate) * 3,
        color = ~pal(Avg_Rate),
        fillOpacity = 0.8,
        stroke = TRUE,
        weight = 2,
        popup = ~paste(
          "<b>", Region, "</b><br>",
          "<b>City:</b>", City, "<br>",
          "<b>States:</b>", States, "<br>",
          "<b>Mortality Rate:</b>", round(Avg_Rate, 1), "per 100,000"
        ),
        label = ~paste(Region_Short, "-", round(Avg_Rate, 1)),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "12px",
          direction = "auto"
        )
      ) %>%
      addCircleMarkers(
        data = outlying_df,
        lng = ~lon,
        lat = ~lat,
        radius = ~sqrt(Avg_Rate) * 2,
        color = ~pal(Avg_Rate),
        fillOpacity = 0.8,
        stroke = TRUE,
        weight = 2,
        dashArray = "3",
        popup = ~paste(
          "<b>", Area, "</b><br>",
          "<b>Part of:</b>", Region, "<br>",
          "<b>Mortality Rate:</b>", round(Avg_Rate, 1),
          "per 100,000 (region average)"
        ),
        label = ~paste(Area, "-", round(Avg_Rate, 1)),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "12px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        pal = pal,
        values = c(df$Avg_Rate, outlying_df$Avg_Rate),
        title = "Mortality Rate<br>(per 100,000)",
        position = "bottomright",
        labFormat = labelFormat(digits = 1)
      )
  })

  # ---- 4d. Value Boxes ----

  output$highest_region <- renderValueBox({
    df <- map_data()
    highest <- df %>% arrange(desc(Avg_Rate)) %>% slice(1)

    valueBox(
      value = paste(highest$Region_Short, "-", highest$City),
      subtitle = "Highest Mortality Rate",
      icon = icon("arrow-up"),
      color = "red"
    )
  })

  output$lowest_region <- renderValueBox({
    df <- map_data()
    lowest <- df %>% arrange(Avg_Rate) %>% slice(1)

    valueBox(
      value = paste(lowest$Region_Short, "-", lowest$City),
      subtitle = "Lowest Mortality Rate",
      icon = icon("arrow-down"),
      color = "green"
    )
  })

  output$avg_rate <- renderValueBox({
    df <- map_data()
    avg <- mean(df$Avg_Rate, na.rm = TRUE)

    valueBox(
      value = paste0(round(avg, 1)),
      subtitle = "Average Rate (per 100,000)",
      icon = icon("chart-line"),
      color = "blue"
    )
  })

  # ---- 4e. Ranking Table ----

  output$ranking_table <- renderDT({
    df <- map_data() %>%
      arrange(desc(Avg_Rate)) %>%
      mutate(
        Rank = row_number(),
        Rate = round(Avg_Rate, 1)
      ) %>%
      select(Rank, Region, City, States, Rate) %>%
      rename(
        "Rank" = Rank,
        "Region" = Region,
        "City" = City,
        "States" = States,
        "Rate (per 100,000)" = Rate
      )

    datatable(
      df,
      options = list(
        pageLength = 10,
        dom = 'Bfrtip',
        scrollX = TRUE
      ),
      class = 'display compact',
      rownames = FALSE
    ) %>%
      formatStyle(
        'Rate (per 100,000)',
        background = styleColorBar(range(df$`Rate (per 100,000)`), 'lightblue'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })

  # ---- 4f. Cause Bar Plot ----

  output$cause_bar_plot <- renderPlotly({
    df <- mortality_full %>%
      filter(
        Sex %in% input$cause_sex,
        Status %in% input$cause_status
      ) %>%
      group_by(Region, Cause) %>%
      summarise(
        Avg_Rate = mean(Rate, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(Cause == input$cause_select)

    p <- ggplot(df, aes(x = reorder(Region, Avg_Rate), y = Avg_Rate, fill = Avg_Rate)) +
      geom_col() +
      scale_fill_gradient(low = "#2ECC71", high = "#E74C3C") +
      coord_flip() +
      labs(
        title = paste("Mortality Rates for", input$cause_select),
        x = "Region",
        y = "Rate (per 100,000)",
        caption = "Data: NCHS 2011-2013"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "none"
      )

    ggplotly(p) %>%
      layout(hoverlabel = list(bgcolor = "white"))
  })

  # ---- 4g. Sex Comparison ----

  output$sex_comparison <- renderPlotly({
    df <- mortality_full %>%
      filter(Cause == input$cause_select) %>%
      group_by(Region, Sex) %>%
      summarise(
        Avg_Rate = mean(Rate, na.rm = TRUE),
        .groups = "drop"
      )

    p <- ggplot(df, aes(x = Region, y = Avg_Rate, fill = Sex)) +
      geom_col(position = "dodge") +
      scale_fill_manual(values = c("Male" = "#2980B9", "Female" = "#E91E63")) +
      labs(
        title = "Sex Comparison",
        x = "Region",
        y = "Rate (per 100,000)"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        legend.position = "bottom"
      )

    ggplotly(p) %>%
      layout(hoverlabel = list(bgcolor = "white"))
  })

  # ---- 4h. Urban-Rural Comparison ----

  output$ur_comparison <- renderPlotly({
    df <- mortality_full %>%
      filter(Cause == input$cause_select) %>%
      group_by(Region, Status) %>%
      summarise(
        Avg_Rate = mean(Rate, na.rm = TRUE),
        .groups = "drop"
      )

    p <- ggplot(df, aes(x = Region, y = Avg_Rate, fill = Status)) +
      geom_col(position = "dodge") +
      scale_fill_manual(values = c("Urban" = "#3498DB", "Rural" = "#E67E22")) +
      labs(
        title = "Urban vs. Rural",
        x = "Region",
        y = "Rate (per 100,000)"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        legend.position = "bottom"
      )

    ggplotly(p) %>%
      layout(hoverlabel = list(bgcolor = "white"))
  })

  # ---- 4i. Cause Table ----

  output$cause_table <- renderDT({
    df <- mortality_full %>%
      filter(Cause == input$cause_select) %>%
      group_by(Region, Sex, Status) %>%
      summarise(
        Rate = mean(Rate, na.rm = TRUE),
        SE = mean(SE, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(Rate = round(Rate, 1))

    datatable(
      df,
      options = list(
        pageLength = 15,
        dom = 'Bfrtip'
      ),
      class = 'display compact',
      rownames = FALSE
    )
  })

  # ---- 4j. Urban-Rural Disparity Plot ----

  output$ur_disparity_plot <- renderPlotly({
    df <- mortality_full

    if (input$ur_region != "All Regions") {
      df <- df %>% filter(Region == input$ur_region)
    }

    if (input$ur_sex != "Both") {
      df <- df %>% filter(Sex == input$ur_sex)
    }

    df_agg <- df %>%
      group_by(Cause, Status) %>%
      summarise(
        Avg_Rate = mean(Rate, na.rm = TRUE),
        .groups = "drop"
      )

    p <- ggplot(df_agg, aes(x = reorder(Cause, Avg_Rate), y = Avg_Rate, fill = Status)) +
      geom_col(position = "dodge") +
      scale_fill_manual(values = c("Urban" = "#3498DB", "Rural" = "#E67E22")) +
      coord_flip() +
      labs(
        title = "Urban vs. Rural Mortality by Cause",
        x = "Cause of Death",
        y = "Rate (per 100,000)"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "bottom"
      )

    ggplotly(p) %>%
      layout(hoverlabel = list(bgcolor = "white"))
  })

  # ---- 4k. Urban-Rural Region Plot ----

  output$ur_region_plot <- renderPlotly({
    df <- mortality_full

    if (input$ur_region != "All Regions") {
      df <- df %>% filter(Region == input$ur_region)
    }

    if (input$ur_sex != "Both") {
      df <- df %>% filter(Sex == input$ur_sex)
    }

    df_agg <- df %>%
      group_by(Region, Status) %>%
      summarise(
        Avg_Rate = mean(Rate, na.rm = TRUE),
        .groups = "drop"
      )

    p <- ggplot(df_agg, aes(x = Region, y = Avg_Rate, fill = Status)) +
      geom_col(position = "dodge") +
      scale_fill_manual(values = c("Urban" = "#3498DB", "Rural" = "#E67E22")) +
      labs(
        title = "Urban vs. Rural by Region",
        x = "Region",
        y = "Rate (per 100,000)"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom"
      )

    ggplotly(p) %>%
      layout(hoverlabel = list(bgcolor = "white"))
  })

  # ---- 4l. Urban-Rural Cause Plot ----

  output$ur_cause_plot <- renderPlotly({
    df <- mortality_full

    if (input$ur_region != "All Regions") {
      df <- df %>% filter(Region == input$ur_region)
    }

    if (input$ur_sex != "Both") {
      df <- df %>% filter(Sex == input$ur_sex)
    }

    df_agg <- df %>%
      group_by(Cause, Status) %>%
      summarise(
        Avg_Rate = mean(Rate, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      pivot_wider(names_from = Status, values_from = Avg_Rate) %>%
      mutate(
        Disparity = Rural - Urban,
        Pct_Increase = (Disparity / Urban) * 100
      ) %>%
      arrange(desc(Disparity))

    p <- ggplot(df_agg, aes(x = reorder(Cause, Disparity), y = Disparity,
                            fill = Disparity > 0)) +
      geom_col() +
      scale_fill_manual(values = c("TRUE" = "#E74C3C", "FALSE" = "#2ECC71"),
                        labels = c("Rural Higher", "Urban Higher")) +
      coord_flip() +
      labs(
        title = "Rural-Urban Mortality Disparity",
        x = "Cause of Death",
        y = "Rural - Urban Rate Difference",
        fill = "Disparity Direction"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "bottom"
      )

    ggplotly(p) %>%
      layout(hoverlabel = list(bgcolor = "white"))
  })

  # ---- 4m. Urban-Rural Table ----

  output$ur_table <- renderDT({
    df <- mortality_full

    if (input$ur_region != "All Regions") {
      df <- df %>% filter(Region == input$ur_region)
    }

    if (input$ur_sex != "Both") {
      df <- df %>% filter(Sex == input$ur_sex)
    }

    df_agg <- df %>%
      group_by(Cause, Status) %>%
      summarise(
        Rate = mean(Rate, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      pivot_wider(names_from = Status, values_from = Rate) %>%
      mutate(
        Disparity = Rural - Urban,
        Pct_Increase = (Disparity / Urban) * 100
      ) %>%
      arrange(desc(Disparity)) %>%
      mutate(
        Urban = round(Urban, 1),
        Rural = round(Rural, 1),
        Disparity = round(Disparity, 1),
        Pct_Increase = round(Pct_Increase, 1)
      )

    datatable(
      df_agg,
      options = list(
        pageLength = 10,
        dom = 'Bfrtip',
        scrollX = TRUE
      ),
      class = 'display compact',
      rownames = FALSE
    ) %>%
      formatStyle(
        'Disparity',
        background = styleColorBar(range(df_agg$Disparity), 'lightblue'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })

  # ---- 4n. Full Data Table ----

  output$full_data_table <- renderDT({
    df <- filtered_data() %>%
      select(Region, City, States, Cause, Sex, Status, Rate, SE) %>%
      mutate(
        Rate = round(Rate, 1),
        SE = round(SE, 2)
      )

    datatable(
      df,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        dom = 'Bfrtip'
      ),
      class = 'display compact',
      rownames = FALSE
    ) %>%
      formatStyle(
        'Rate',
        background = styleColorBar(range(df$Rate), 'lightblue'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })

  # ---- 4o. Download Handler ----

  output$download_data <- downloadHandler(
    filename = function() {
      paste0("mortality_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      df <- filtered_data() %>%
        select(Region, City, States, Cause, Sex, Status, Rate, SE)
      write_csv(df, file)
    }
  )
}

shinyApp(ui = ui, server = server)
