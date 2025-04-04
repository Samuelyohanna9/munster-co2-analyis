Analysis of CO₂ Emissions and Energy Consumption in Münster

This project analyzes CO₂ emissions and energy consumption trends across districts in Münster, Germany, to support the city’s goal of achieving climate neutrality by 2030. The analysis includes spatial, temporal, and machine learning approaches to disaggregate city-level data, forecast future emissions, and identify high-emission districts for targeted interventions.
Key Features

    Spatial Analysis: Disaggregates city-level CO₂ emissions and energy consumption to the district level using proxy variables (residential area, population density, commercial area, industrial area, and road density).

    Time Series Forecasting: Uses ARIMA models to forecast future CO₂ emissions based on historical data.

    Machine Learning: Employs Random Forest models to predict CO₂ emissions based on spatial and demographic predictors.

    Interactive Dashboard: A Shiny app provides an interactive interface for exploring emissions and energy consumption trends.

Data Sources

    Spatial Data: District boundaries, land use (industrial, residential, commercial), and road networks from OpenStreetMap (OSM).

    CO₂ Emissions Data: Annual CO₂ emissions by sector (residential, commercial, industrial, transport) from Münster Open Data Portal.

    Energy Consumption Data: Annual energy consumption by sector from Münster Open Data Portal.

    Population Data: District-level population statistics (2000–2021) from Münster Open Data Portal.

Methodology

    Disaggregation:

        City-level emissions and energy consumption are disaggregated to the district level using proxy variables.

        Weights are calculated based on the proportion of each proxy variable (e.g., residential area, population density) within a district.

    Spatial Analysis:

        Spatial autocorrelation is assessed using Moran’s I to identify clustering or dispersion of emissions and energy consumption.

    Time Series Analysis:

        ARIMA models are used to forecast future CO₂ emissions based on historical trends.

    Machine Learning:

        Random Forest models predict CO₂ emissions based on spatial and demographic predictors.

    Visualization:

        An interactive Shiny app visualizes emissions and energy consumption trends across districts.

Results

    Disaggregated Emissions: Districts with high industrial activity and road density exhibit the highest CO₂ emissions.

    Forecasts: ARIMA models predict a continued decline in emissions, with the largest reductions in the residential and transport sectors by 2030.

    Spatial Autocorrelation: No significant clustering of emissions or energy consumption was found, suggesting an even distribution across districts.

    Model Performance: Random Forest models achieved high predictive accuracy (R² = 0.85, RMSE = 120 tons).

Setup Instructions
Prerequisites

    R (version 4.2 or higher)

    RStudio (recommended)

    Required R packages:
    R
    Copy

    install.packages(c("sf", "dplyr", "readxl", "spdep", "units", "purrr", "ggplot2", "tidyr", "gstat", "caret", "randomForest", "spatialreg", "CAST", "automap", "zoo", "forecast", "viridis", "stringr", "shiny", "shinydashboard", "leaflet", "plotly"))

Running the Project

    Clone the repository:

    git clone https://github.com/Samuelyohanna9/munster-co2-analysis.git

    Run the main script (main.R) to perform the analysis and generate results.

    Launch the Shiny app:

    shiny::runApp("shiny_app")

File Structure
Copy

munster-co2-analysis/
├── data/                     # Raw and processed data files
│   ├── spatial/              # Shapefiles and spatial data
│   ├── co2_emissions.xls     # CO₂ emissions data
│   ├── energy_consumption.xls# Energy consumption data
│   └── population.csv        # Population data
├── scripts/                  # R scripts for analysis
│   ├── main.R                # Main analysis script
│   ├── spatial_analysis.R    # Spatial analysis script
│   ├── time_series.R         # Time series forecasting script
│   └── machine_learning.R    # Machine learning script
├── shiny_app/                # Shiny app for interactive visualization
│   ├── ui.R                  # User interface
│   ├── server.R              # Server logic
│   └── app.R                 # Main app file
├── results/                  # Output files (plots, forecasts, etc.)
├── README.md                 # Project overview
└── munster-co2-analysis.Rproj# R project file

Docker Setup

To ensure reproducibility, a Dockerfile is provided to containerize the project. Follow these steps to build and run the Docker image:

    Run the Docker container:


    docker run -p 3838:3838 munster-co2-analysis

    Access the Shiny app at http://localhost:3838.

Contributing

Contributions are welcome! Please follow these steps:

    Fork the repository.

    Create a new branch for your feature or bugfix.

    Submit a pull request with a detailed description of your changes.

License

This project is licensed under the MIT License. See the LICENSE file for details.
Contact

For questions or feedback, please contact:

    Samuel Yohanna
    Email: samuel.yohanna@uni-muenster.de
    GitHub: samuelyohanna9
