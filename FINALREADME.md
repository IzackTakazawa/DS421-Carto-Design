# DS421 Cartographic Design – Final Project

## Global Shark Attack Incidents by Country

For my final project I wanted to build an interactive map of reported shark attack incidents by country. This past weekend I went to go throw pole at Kewalos and caught a white tip shark. I've been familiar with how common sharks swim around the shore break and have heard multiple stories of people getting bit there, which really sparked my interest when thinking about a topic for the final project.

### Motivation

Growing up, I've spent my whole life playing in the water and grew up with two parents who met at the beach surfing together so ocean activities are a core part of life. Shark incidents are often sensationalized in media, but the actual risk varies widely by location and activity. This map can hopefully help provide a global perspective on incident frequency and severity.

### Where I got the Dataset

- **Source:** [thedevastator/global-shark-attack-incidents](https://www.kaggle.com/datasets/thedevastator/global-shark-attack-incidents) on Kaggle\
- **Access:** Instead of downloading the dataset how I would regularly do it I didn't have enough storage and programmed in R using the `RKaggle` package.

### Map features

- Interactive map with visualizing the incidents by country\
- Color encodes total number of incidents (log‑scaled) using a sequential Blues palette\
- Click on any country to see:
  - Total incidents
  - Number of fatal incidents
  - Fatality rate

### Reproduce the project

1.  Clone or download this repository

2.  **Open the project** in RStudio

3.  **Install required packages** (run once in the Console): —-This was extremely helpful when I switched computers to work on

    ``` r
    install.packages(c(
      "tidyverse", "lubridate", "janitor", "leaflet",
      "RColorBrewer", "htmlwidgets", "scales", "countrycode",
      "rnaturalearth", "htmltools", "remotes"
    ))
    remotes::install_github("benyamindsmith/RKaggle")
    ```

4.  **Render the Quarto document**:

    - Open app.R in RStudio.
    - Click **Run App**
    - This will download the dataset via `RKaggle`, process it, and generate the interactive map in Shiny
