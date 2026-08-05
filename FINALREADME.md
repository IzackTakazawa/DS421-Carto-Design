# DS421 Cartographic Design – Final Project

## Global Shark Attack Incidents by Country

This project builds an interactive map of reported shark attack incidents by country using R, Quarto, and Leaflet. The map is designed to show where incidents have occurred and how deadly they have been, supporting conversations about ocean safety and risk perception.

### Motivation

As a data science undergraduate and NCAA student‑athlete based in Hawaii, ocean activities are a core part of life. Shark incidents are often sensationalized in media, but the actual risk varies widely by location and activity. This map provides a global perspective on incident frequency and severity.

### Dataset

- **Source:** [thedevastator/global-shark-attack-incidents](https://www.kaggle.com/datasets/thedevastator/global-shark-attack-incidents) on Kaggle\
- **Description:** Compiled reports of shark incidents worldwide, including date, country, activity, injury, and fatality information. [cite:web:26][cite:web:35]\
- **Access:** Downloaded programmatically in R using the `RKaggle` package.

### Map features

- Interactive Leaflet map with country‑level aggregation\
- Color encodes total number of incidents (log‑scaled) using a sequential Blues palette\
- Click on any country to see:
  - Total incidents
  - Number of fatal incidents
  - Fatality rate\
- Clean basemap (CartoDB Positron), legend, and title for clarity

### How to reproduce this project

1.  **Clone or download this repository** to your local machine.

2.  **Open the project** in RStudio (open `ds421-shark-map.Rproj` or the folder).

3.  **Install required packages** (run once in the Console):

    ``` r
    install.packages(c(
      "tidyverse", "lubridate", "janitor", "leaflet",
      "RColorBrewer", "htmlwidgets", "scales", "countrycode",
      "rnaturalearth", "htmltools", "remotes"
    ))
    remotes::install_github("benyamindsmith/RKaggle")
    ```

4.  **Render the Quarto document**:

    - Open `final.qmd` in RStudio.
    - Click **Render** (or run `quarto::render("final.qmd")` in the Console).
    - This will download the dataset via `RKaggle`, process it, and generate an HTML file with the interactive map.

5.  **(Optional) Run the Shiny app**:

    - Open `app.R` in RStudio.
    - Click **Run App**.
    - The app will render the same interactive map in a Shiny interface.

### Deployment

- Push this repository to GitHub.
- Connect the repo to Posit Connect / ShinyApps.io and deploy the `app.R` Shiny app.
- Share the resulting URL in your presentation and project submission.

### Files in this repository

- `final.qmd` – Main analysis and map (Quarto document).\
- `app.R` – Minimal Shiny app that displays the map.\
- `README.md` – This file: project summary and instructions.\
- `.gitignore` – Optional; ignores large generated files like HTML outputs.

### 
