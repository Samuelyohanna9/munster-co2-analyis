
library(sf)
library(dplyr)
library(readxl)
library(spdep)
library(units)
library(purrr)
library(ggplot2)
library(tidyr)
library(gstat)
library(caret)
library(randomForest)
library(spatialreg)
library(CAST)
library(automap)
library(zoo)
library(forecast)
library(viridis)
library(stringr)

# ------ DATA LOADING & PREPROCESSING ------
districts <- st_read("munster_districts/stadtbezirk.shp") %>%
  st_transform(25832) %>%
  mutate(
    district_id = row_number(),
    area = st_area(.) %>% set_units("km²")
  )


industrial_areas <- st_read("munster_landuse/munster_landuse.shp") %>%
  st_transform(25832) %>%
  filter(fclass == "industrial") %>%
  st_intersection(districts) %>%
  mutate(industrial_area = st_area(.) %>% set_units("km²")) %>%
  group_by(district_id) %>%
  summarise(industrial_area = sum(industrial_area, na.rm = TRUE)) %>%
  st_drop_geometry()


residential_areas <- st_read("munster_landuse/munster_landuse.shp") %>%
  st_transform(25832) %>%
  filter(fclass == "residential") %>%
  st_intersection(districts) %>%
  mutate(residential_area = st_area(.) %>% set_units("km²")) %>%
  group_by(district_id) %>%
  summarise(residential_area = sum(residential_area, na.rm = TRUE)) %>%
  st_drop_geometry()


commercial_areas <- st_read("munster_landuse/munster_landuse.shp") %>%
  st_transform(25832) %>%
  filter(fclass == "commercial") %>%
  st_intersection(districts) %>%
  mutate(commercial_area = st_area(.) %>% set_units("km²")) %>%
  group_by(district_id) %>%
  summarise(commercial_area = sum(commercial_area, na.rm = TRUE)) %>%
  st_drop_geometry()


road_density <- st_read("munster_roads/munster_roads.shp") %>%
  st_transform(25832) %>%
  st_intersection(districts) %>%
  mutate(road_length = st_length(.)) %>%
  group_by(district_id) %>%
  summarise(total_road_length = sum(road_length, na.rm = TRUE)) %>%
  st_drop_geometry() %>%
  left_join(st_drop_geometry(districts) %>% select(district_id, area), by = "district_id") %>%
  mutate(road_density = as.numeric(total_road_length / area)) %>%
  select(-area, -total_road_length)


population_data <- read.csv("new_munster_districts_population.csv") %>%
  mutate(Year = as.integer(Year)) %>%
  complete(Year = full_seq(c(2000, 2005, 2010, 2015:2021), 1), NAME_STADT,
           fill = list(Population = 0)) %>%
  left_join(st_drop_geometry(districts) %>% select(NAME_STADT, district_id), by = "NAME_STADT")


years <- c(2000, 2005, 2010, 2015:2021)
population_data_filtered <- population_data %>%
  filter(Year %in% years) %>%
  mutate(Population = ifelse(Population == 0, NA, Population)) %>%
  group_by(district_id) %>%
  arrange(Year) %>%
  mutate(Population = na.approx(Population, na.rm = FALSE)) %>%
  ungroup()


co2_data <- read_excel("Muenster-CO2-Emissionen_2021.xls") %>% mutate(Year = as.integer(Year))
energy_data <- read_excel("energy_consumption.xls") %>% mutate(Year = as.integer(Year))

# Combine spatial predictors
districts <- districts %>%
  left_join(industrial_areas, by = "district_id") %>%
  left_join(residential_areas, by = "district_id") %>%
  left_join(commercial_areas, by = "district_id") %>%
  left_join(road_density, by = "district_id") %>%
  mutate(
    industrial_area = replace_na(industrial_area, 0),
    residential_area = replace_na(residential_area, 0),
    commercial_area = replace_na(commercial_area, 0),
    road_density = replace_na(road_density, 0)
  )

# ------ HELPER FUNCTIONS ------
scale_safe <- function(x) {
  x <- replace_non_finite(x)
  if (sd(x, na.rm = TRUE) < 1e-6) return(rep(0, length(x)))
  scale(x, center = TRUE, scale = TRUE) %>% 
    replace_non_finite() %>% 
    as.numeric()
}

replace_non_finite <- function(x) {
  x[!is.finite(x)] <- 0
  x
}

# ------ DISAGGREGATION FUNCTION ------
disaggregate_sector <- function(city_total, districts, sector, year) {
  predictors <- switch(sector,
                       "residential" = c("residential_area", "pop_density"),
                       "commercial" = c("commercial_area"),
                       "industry" = c("industrial_area"),
                       "transport" = c("road_density")
  )
  
  districts <- districts %>%
    mutate(
      across(all_of(predictors), ~ scale_safe(.x)),
      raw_weight = rowSums(pick(all_of(predictors)), na.rm = TRUE) %>%
        pmax(0.001) %>% replace_non_finite()
    )
  
  districts %>%
    mutate(
      weight = raw_weight / sum(raw_weight, na.rm = TRUE),
      !!sym(sector) := city_total * weight
    ) %>%
    st_drop_geometry() %>%
    select(district_id, all_of(sector), weight)
}

# ------ MAIN WORKFLOW ------
results <- map(years, ~ {
  yearly_data <- districts %>%
    left_join(population_data_filtered %>% filter(Year == .x), by = "district_id") %>%
    mutate(pop_density = (Population / as.numeric(area)) %>% 
             replace_non_finite() %>% 
             pmax(0.001))
  
  co2_sectors <- list(
    residential = co2_data %>% filter(Year == .x) %>% pull(`Private Haushalte`),
    commercial = co2_data %>% filter(Year == .x) %>% pull(`Gewerbe + Sonstiges`),
    industry = co2_data %>% filter(Year == .x) %>% pull(Industrie),
    transport = co2_data %>% filter(Year == .x) %>% pull(Verkehr)
  )
  
  co2_disaggregated <- imap(co2_sectors, ~ disaggregate_sector(.x, yearly_data, .y, .x)) %>%
    reduce(full_join, by = "district_id") %>%
    rename_with(~ paste0("co2_", .x), -district_id)
  
  energy_sectors <- list(
    residential = energy_data %>% filter(Year == .x) %>% pull(`Private Haushalte`),
    commercial = energy_data %>% filter(Year == .x) %>% pull(`Gewerbe + Sonstiges`),
    industry = energy_data %>% filter(Year == .x) %>% pull(Industrie),
    transport = energy_data %>% filter(Year == .x) %>% pull(Verkehr)
  )
  
  energy_disaggregated <- imap(energy_sectors, ~ disaggregate_sector(.x, yearly_data, .y, .x)) %>%
    reduce(full_join, by = "district_id") %>%
    rename_with(~ paste0("energy_", .x), -district_id)
  
  yearly_data %>%
    left_join(co2_disaggregated, by = "district_id") %>%
    left_join(energy_disaggregated, by = "district_id") %>%
    mutate(Year = .x) %>%
    st_as_sf()
})

final_results <- bind_rows(results) %>% st_make_valid()

# ------ EXTRACT CO2 EMISSIONS DATA BY SECTOR ------
co2_data <- final_results %>%
  st_drop_geometry() %>%
  select(district_id, Year, co2_residential, co2_commercial, co2_industry, co2_transport) %>%
  pivot_longer(cols = starts_with("co2_"), names_to = "Sector", values_to = "CO2_Emissions") %>%
  mutate(Sector = str_remove(Sector, "co2_")) %>%
  group_by(district_id, Sector) %>%
  arrange(Year) %>%
  ungroup()

# ------ FORECASTING ------
# Function to fit ARIMA and forecast
forecast_co2_sector <- function(historical_data, future_years) {
  fit <- auto.arima(historical_data$CO2_Emissions)
  forecast_result <- forecast(fit, h = length(future_years))
  tibble(Year = future_years, predicted_co2 = forecast_result$mean)
}

future_years <- 2022:2030


co2_sector_totals <- co2_data %>%
  group_by(Sector, Year) %>%
  summarise(CO2_Emissions = sum(CO2_Emissions, na.rm = TRUE)) %>%
  ungroup()


co2_sector_forecast <- co2_sector_totals %>%
  group_by(Sector) %>%  # Group by sector
  group_modify(~ forecast_co2_sector(.x, future_years)) %>%
  ungroup()


full_co2_sector_data <- co2_data %>%
  bind_rows(co2_sector_forecast) %>%
  arrange(district_id, Sector, Year)


full_co2_sector_data <- full_co2_sector_data %>%
  group_by(district_id, Sector) %>%
  mutate(CO2_Emissions = na.approx(CO2_Emissions, na.rm = FALSE)) %>%
  ungroup()

# ------ VISUALIZATION ------
time_series_plot <- ggplot(full_co2_sector_data, aes(x = Year, y = CO2_Emissions, color = as.factor(district_id))) +
  geom_line() +
  facet_wrap(~Sector, scales = "free_y") +
  labs(
    title = "Historical and Forecasted CO2 Emissions by District and Sector",
    x = "Year",
    y = "CO2 Emissions (tons)",
    color = "District ID"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(time_series_plot)

# ------ MODEL EVALUATION ------
set.seed(123)
trainIndex <- createDataPartition(co2_data$CO2_Emissions, p = 0.8, list = FALSE)
train_data <- co2_data[trainIndex, ]
test_data <- co2_data[-trainIndex, ]

train_models <- train_data %>%
  group_by(district_id, Sector) %>%
  group_modify(~ tibble(model = list(auto.arima(.x$CO2_Emissions)))) %>%
  ungroup()

test_predictions <- test_data %>%
  group_by(district_id, Sector) %>%
  group_modify(~ {
    model <- train_models %>% filter(district_id == .y$district_id, Sector == .y$Sector) %>% pull(model) %>% .[[1]]
    tibble(Year = .x$Year, predicted_co2 = forecast(model, h = nrow(.x))$mean)
  }) %>%
  ungroup()

evaluation_data <- test_data %>%
  left_join(test_predictions, by = c("district_id", "Sector", "Year"))

mae <- mean(abs(evaluation_data$CO2_Emissions - evaluation_data$predicted_co2), na.rm = TRUE)
rmse <- sqrt(mean((evaluation_data$CO2_Emissions - evaluation_data$predicted_co2)^2, na.rm = TRUE))
r_squared <- cor(evaluation_data$CO2_Emissions, evaluation_data$predicted_co2, use = "complete.obs")^2

cat("MAE:", mae, "\nRMSE:", rmse, "\nR-squared:", r_squared, "\n")

# Plot predicted vs. actual CO2 emissions
ggplot(evaluation_data, aes(x = CO2_Emissions, y = predicted_co2)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "Predicted vs. Actual CO2 Emissions",
    x = "Actual CO2 Emissions (tons)",
    y = "Predicted CO2 Emissions (tons)"
  ) +
  theme_minimal()




# ------ SPATIAL ANALYSIS ------
final_results <- final_results %>%
  mutate(co2_total = co2_residential + co2_commercial + co2_industry + co2_transport)


final_results <- final_results %>%
  mutate(energy_total = energy_residential + energy_commercial + energy_industry + energy_transport)


neighbors <- poly2nb(final_results)
weights <- nb2listw(neighbors, style = "W")

# Moran's I test for CO2 emissions
moran_co2 <- moran.test(final_results$co2_total, weights)
print(moran_co2)

# Moran's I test for energy consumption
moran_energy <- moran.test(final_results$energy_total, weights)
print(moran_energy)

#RSHINY APPLICATION DASHBOARD

# ------ LIBRARIES ------
library(shiny)
library(shinydashboard)
library(leaflet)
library(leaflet.extras)
library(ggplot2)
library(plotly)
library(dplyr)
library(sf)
library(forecast)
library(randomForest)
library(ggcorrplot)
library(stringr)
library(caret)
library(zoo)
library(scales)
library(tidyr)

# ------ DATA PREPARATION ------

final_results <- final_results %>%
  st_transform(4326)


final_results <- final_results %>%
  filter(st_is(geometry, c("POLYGON", "MULTIPOLYGON"))) %>%
  st_cast("MULTIPOLYGON") 

# Calculate co2_total if it doesn't exist
if (!"co2_total" %in% names(final_results)) {

  co2_columns <- final_results %>%
    st_drop_geometry() %>%  
    select(starts_with("co2_")) %>%
    select(where(is.numeric))  
  

  if (ncol(co2_columns) > 0) {
    final_results <- final_results %>%
      mutate(co2_total = rowSums(co2_columns, na.rm = TRUE))
  } else {
    stop("No valid numeric columns starting with 'co2_' found in final_results.")
  }
}

# Calculate energy_total if it doesn't exist
if (!"energy_total" %in% names(final_results)) {
  # Select only numeric columns that start with "energy_"
  energy_columns <- final_results %>%
    st_drop_geometry() %>%  # Drop geometry if it exists
    select(starts_with("energy_")) %>%
    select(where(is.numeric))  # Select only numeric columns
  
  # Calculate energy_total if there are valid columns
  if (ncol(energy_columns) > 0) {
    final_results <- final_results %>%
      mutate(energy_total = rowSums(energy_columns, na.rm = TRUE))
  } else {
    stop("No valid numeric columns starting with 'energy_' found in final_results.")
  }
}


co2_district_totals <- final_results %>%
  st_drop_geometry() %>%
  select(district_id, NAME_STADT.x, Year, co2_total) %>%
  group_by(district_id, NAME_STADT.x, Year) %>%
  summarise(CO2_Emissions = sum(co2_total, na.rm = TRUE)) %>%
  ungroup()

# ------ FORECASTING ------

forecast_co2_district <- function(historical_data, future_years) {
  fit <- auto.arima(historical_data$CO2_Emissions)
  forecast_result <- forecast(fit, h = length(future_years))
  tibble(Year = future_years, predicted_co2 = forecast_result$mean)
}


future_years <- 2022:2030


co2_district_forecast <- co2_district_totals %>%
  group_by(district_id, NAME_STADT.x) %>%  # Group by district
  group_modify(~ forecast_co2_district(.x, future_years)) %>%
  ungroup()


full_co2_district_data <- co2_district_totals %>%
  bind_rows(co2_district_forecast) %>%
  arrange(district_id, Year)


full_co2_district_data <- full_co2_district_data %>%
  group_by(district_id) %>%
  mutate(CO2_Emissions = na.approx(CO2_Emissions, na.rm = FALSE)) %>%
  ungroup()

# ------ SHINY APP UI ------
ui <- dashboardPage(
  dashboardHeader(title = "Münster CO2 Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Map", tabName = "map_tab", icon = icon("map")),
      menuItem("Graphs", tabName = "graphs_tab", icon = icon("chart-bar")),
      menuItem("Forecast", tabName = "forecast_tab", icon = icon("line-chart")),
      menuItem("Correlation", tabName = "correlation_tab", icon = icon("chart-area")),
      menuItem("Time Series", tabName = "timeseries_tab", icon = icon("chart-line")),
      menuItem("Model Evaluation", tabName = "modeleval_tab", icon = icon("check-circle"))
    )
  ),
  dashboardBody(
    tabItems(
      # Map Tab
      tabItem(tabName = "map_tab",
              fluidRow(
                box(width = 12,
                    selectInput("map_year", "Select Year:",
                                choices = c(2000, 2005, 2010, 2015:2021),
                                selected = 2021),
                    selectInput("map_metric", "Select Metric:",
                                choices = c("CO2 Emissions" = "co2_total",
                                            "Energy Consumption" = "energy_total"),
                                selected = "co2_total"),
                    leafletOutput("map")
                )
              )
      ),
      # Graphs Tab
      tabItem(tabName = "graphs_tab",
              fluidRow(
                box(width = 12,
                    selectInput("graph_type", "Select Graph Type:",
                                choices = c("Sector-wise Emissions" = "sector",
                                            "Trends Over Time" = "trend"),
                                selected = "sector"),
                    selectInput("district", "Select District:",
                                choices = unique(final_results$NAME_STADT.x),
                                selected = unique(final_results$NAME_STADT.x)[1]),
                    plotlyOutput("plot")
                )
              )
      ),
      # Forecast Tab
      tabItem(tabName = "forecast_tab",
              fluidRow(
                box(width = 12,
                    plotlyOutput("forecast_plot")
                )
              )
      ),
      # Correlation Tab
      tabItem(tabName = "correlation_tab",
              fluidRow(
                box(width = 12,
                    plotOutput("correlation_heatmap")
                )
              )
      ),
      # Time Series Tab
      tabItem(tabName = "timeseries_tab",
              fluidRow(
                box(width = 12,
                    plotlyOutput("timeseries_plot")
                )
              )
      ),
      # Model Evaluation Tab
      tabItem(tabName = "modeleval_tab",
              fluidRow(
                box(width = 12,
                    h3("Model Evaluation Metrics"),
                    verbatimTextOutput("model_metrics"),
                    plotlyOutput("pred_vs_actual_plot")
                )
              )
      )
    )
  )
)

# ------ SHINY APP SERVER ------
server <- function(input, output, session) {
  
  map_data <- reactive({
    final_results %>%
      filter(Year == input$map_year) %>%
      st_as_sf()
  })
  
  # Interactive Map
  output$map <- renderLeaflet({
    leaflet(map_data()) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~colorNumeric("YlOrRd", get(input$map_metric))(get(input$map_metric)),
        weight = 2,
        opacity = 1,
        color = "white",
        fillOpacity = 0.7,
        popup = ~paste(
          "District:", NAME_STADT.x, "<br>",
          "CO2 Emissions:", round(co2_total, 2), "tons<br>",
          "Energy Consumption:", round(energy_total, 2), "GWh"
        ),
        group = "Districts"
      ) %>%
      addSearchOSM(
        options = searchOptions(autoCollapse = TRUE, minLength = 2)
      ) %>%
      addScaleBar(position = "bottomleft") %>%
      addResetMapButton() %>%
      addLegend(
        position = "bottomright",
        pal = colorNumeric("YlOrRd", map_data()[[input$map_metric]]),
        values = ~get(input$map_metric),
        title = input$map_metric,
        opacity = 1
      )
  })
  
  # Dynamic Graphs
  output$plot <- renderPlotly({
    if (input$graph_type == "sector") {
      district_data <- final_results %>%
        filter(NAME_STADT.x == input$district) %>%
        select(Year, co2_residential, co2_commercial, co2_industry, co2_transport) %>%
        pivot_longer(cols = starts_with("co2_"), 
                     names_to = "Sector", 
                     values_to = "CO2_Emissions") %>%
        mutate(Sector = str_remove(Sector, "co2_"))
      
      p <- ggplot(district_data, aes(x = Year, y = CO2_Emissions, fill = Sector)) +
        geom_bar(stat = "identity") +
        scale_y_continuous(labels = scales::comma) +
        labs(title = paste("Sector-wise CO2 Emissions in", input$district),
             x = "Year", y = "CO2 Emissions (tons)") +
        theme_minimal()
    } else {
      district_data <- final_results %>%
        filter(NAME_STADT.x == input$district) %>%
        select(Year, co2_total)
      
      p <- ggplot(district_data, aes(x = Year, y = co2_total)) +
        geom_line(color = "steelblue", size = 1) +
        scale_y_continuous(labels = scales::comma) +
        labs(title = paste("CO2 Emissions Over Time in", input$district),
             x = "Year", y = "CO2 Emissions (tons)") +
        theme_minimal()
    }
    ggplotly(p)
  })
  
  # Forecasting Tool
  output$forecast_plot <- renderPlotly({
    full_co2_sector_data <- final_results %>%
      st_drop_geometry() %>%
      select(Year, co2_residential, co2_commercial, co2_industry, co2_transport) %>%
      pivot_longer(cols = starts_with("co2_"), 
                   names_to = "Sector", 
                   values_to = "CO2_Emissions") %>%
      mutate(Sector = str_remove(Sector, "co2_")) %>%
      group_by(Sector, Year) %>%
      summarise(CO2_Emissions = sum(CO2_Emissions, na.rm = TRUE)) %>%
      ungroup()
    
    future_years <- 2022:2030
    forecast_data <- full_co2_sector_data %>%
      group_by(Sector) %>%
      group_modify(~ forecast_co2_district(.x, future_years)) %>%
      ungroup()
    
    full_data <- full_co2_sector_data %>%
      bind_rows(forecast_data) %>%
      arrange(Sector, Year)
    
    forecast_plot <- ggplot(full_data, aes(x = Year, y = CO2_Emissions, color = Sector)) +
      geom_line(size = 1) +
      geom_line(data = filter(full_data, Year >= 2022), aes(y = predicted_co2), linetype = "dashed", size = 1) +
      geom_vline(xintercept = 2021, linetype = "dashed", color = "red") +
      scale_y_continuous(labels = scales::comma) +
      labs(
        title = "Historical and Forecasted CO2 Emissions by Sector",
        x = "Year",
        y = "CO2 Emissions (tons)",
        color = "Sector"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(forecast_plot)
  })
  
  # Correlation Heatmap
  output$correlation_heatmap <- renderPlot({
    correlation_data <- final_results %>%
      st_drop_geometry() %>%
      select(Year, co2_residential, co2_commercial, co2_industry, co2_transport,
             energy_residential, energy_commercial, energy_industry, energy_transport) %>%
      pivot_longer(cols = starts_with("co2_") | starts_with("energy_"),
                   names_to = c("Metric", "Sector"),
                   names_pattern = "(co2|energy)_(.*)") %>%
      pivot_wider(names_from = Metric, values_from = value, values_fn = list(value = sum)) %>%
      group_by(Sector) %>%
      summarise(Correlation = cor(co2, energy, use = "complete.obs"))
    
    ggplot(correlation_data, aes(x = Sector, y = "Correlation", fill = Correlation)) +
      geom_tile() +
      scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, limits = c(-1, 1)) +
      labs(
        title = "Correlation between CO2 Emissions and Energy Consumption by Sector",
        x = "Sector",
        y = NULL,
        fill = "Correlation"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Time Series Plot
  output$timeseries_plot <- renderPlotly({
    historical_data <- full_co2_district_data %>%
      filter(Year <= 2021)
    
    forecast_data <- full_co2_district_data %>%
      filter(Year >= 2022)
    
    time_series_plot <- ggplot(historical_data, aes(x = Year, y = CO2_Emissions, color = NAME_STADT.x)) +
      geom_line(size = 1) +
      geom_line(data = forecast_data, aes(y = predicted_co2), linetype = "dashed", size = 1) +
      geom_vline(xintercept = 2021, linetype = "dashed", color = "red") +
      scale_y_continuous(labels = scales::comma) +
      labs(
        title = "Historical and Forecasted CO2 Emissions by District",
        x = "Year",
        y = "CO2 Emissions (tons)",
        color = "District"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(time_series_plot)
  })
  
  # Model Evaluation Metrics
  output$model_metrics <- renderPrint({
    mae <- mean(abs(evaluation_data$CO2_Emissions - evaluation_data$predicted_co2), na.rm = TRUE)
    rmse <- sqrt(mean((evaluation_data$CO2_Emissions - evaluation_data$predicted_co2)^2, na.rm = TRUE))
    r_squared <- cor(evaluation_data$CO2_Emissions, evaluation_data$predicted_co2, use = "complete.obs")^2
    
    cat("MAE:", mae, "\nRMSE:", rmse, "\nR-squared:", r_squared, "\n")
  })
  
  # Predicted vs. Actual Plot
  output$pred_vs_actual_plot <- renderPlotly({
    p <- ggplot(evaluation_data, aes(x = CO2_Emissions, y = predicted_co2)) +
      geom_point() +
      geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
      scale_y_continuous(labels = scales::comma) +
      scale_x_continuous(labels = scales::comma) +
      labs(
        title = "Predicted vs. Actual CO2 Emissions",
        x = "Actual CO2 Emissions (tons)",
        y = "Predicted CO2 Emissions (tons)"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
}

# ------ RUN SHINY APP ------
shinyApp(ui, server)

