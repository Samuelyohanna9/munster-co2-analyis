library(sf)
library(dplyr)
library(readxl)
library(spdep)
library(gstat)
library(spatialreg)
library(units)
library(purrr)
library(ggplot2)


# Load spatial data
districts <- st_read("munster_districts/stadtbezirk.shp") %>%
  st_transform(25832) %>%
  mutate(
    area = st_area(.),
    district_id = row_number(),
    NAME_STADT = trimws(NAME_STADT)  
  )

landuse_shapefile <- st_read("munster_landuse/munster_landuse.shp") %>%
  st_transform(st_crs(districts)) %>%
  mutate(fclass = as.character(fclass))  

roads_shapefile <- st_read("munster_roads/munster_roads.shp") %>%
  st_transform(st_crs(districts))

# Load tabular data
co2_data <- read_excel("Muenster-CO2-Emissionen_2021.xls") %>%
  mutate(Year = as.integer(Year)) %>%
  filter(Year >= 2000) %>%
  rename(
    co2_industry = `Industrie`,
    co2_transport = `Verkehr`,
    co2_residential = `Private Haushalte`,
    co2_commercial = `Gewerbe + Sonstiges`
  )


population_data <- read.csv("new_munster_districts_population.csv") %>%
  mutate(
    Year = as.integer(Year),
    NAME_STADT = trimws(NAME_STADT),
    Population = as.numeric(Population)
  ) %>%
  complete(Year = full_seq(min(Year):max(Year), 1), NAME_STADT, fill = list(Population = 0))  # Ensure complete time series

# Load energy data 
energy_data <- read_excel("energy_consumption.xls") %>%
  mutate(Year = as.integer(Year)) %>%
  rename(
    energy_industry = `Industrie`,
    energy_transport = `Verkehr`,
    energy_residential = `Private Haushalte`,
    energy_commercial = `Gewerbe + Sonstiges`
  )


# Calculate industrial areas 
industrial_areas <- landuse_shapefile %>%
  filter(fclass == "industrial") %>%
  st_intersection(districts) %>%
  mutate(industrial_area = st_area(.)) %>%
  group_by(district_id) %>%
  summarise(industrial_area = sum(industrial_area))

# Calculate road density
road_density <- roads_shapefile %>%
  st_intersection(districts) %>%
  mutate(
    road_length = st_length(.),
    road_density = road_length / districts$area
  ) %>%
  group_by(district_id) %>%
  summarise(road_density = sum(road_density))

# Merge static spatial features
districts_static <- districts %>%
  st_join(industrial_areas, by = "district_id") %>%
  st_join(road_density, by = "district_id") %>%
  mutate(
    industrial_area = replace_na(industrial_area, set_units(0, m^2)),
    road_density = replace_na(road_density, set_units(0, m/m^2))
  )


# Create yearly district data 
district_years <- population_data %>%
  split(.$Year) %>%
  map(~{
    districts_static %>%
      left_join(.x, by = "NAME_STADT") %>%
      mutate(
        population_density = Population / as.numeric(area),
        population_density = set_units(population_density, 1/km^2))
  })


districts_nb <- poly2nb(districts_static)
weights <- nb2listw(districts_nb, style = "W")


disaggregate_emissions <- function(city_data, districts, year) {
  annual_data <- city_data %>%
    filter(Year == year) %>%
    left_join(
      districts %>%
        st_drop_geometry() %>%
        select(district_id, industrial_weight, transport_weight, residential_weight),
      by = "district_id",
      relationship = "many-to-many"  # Explicitly handle many-to-many relationships
    ) %>%
    mutate(
      industry_emission = co2_industry * industrial_weight,
      transport_emission = co2_transport * transport_weight,
      residential_emission = co2_residential * residential_weight,
      total_emission = set_units(
        industry_emission + transport_emission + residential_emission,
        tonnes
      )
    )
  
  st_as_sf(annual_data)
}


years <- c(2000, 2005, 2010, 2015:2021)

# Calculate weights and disaggregate for each year
emission_results <- years %>%
  map_dfr(function(y) {
    # Get district data for this year
    d <- district_years[[as.character(y)]] %>%
      mutate(
        industrial_weight = as.numeric(industrial_area)/sum(as.numeric(industrial_area)),
        transport_weight = as.numeric(road_density)/sum(as.numeric(road_density)),
        residential_weight = Population/sum(Population)
      )
    
    # Disaggregate emissions
    co2_data %>%
      filter(Year == y) %>%
      left_join(st_drop_geometry(d), by = "Year", relationship = "many-to-many") %>%
      left_join(d %>% select(district_id, geometry), by = "district_id", relationship = "many-to-many") %>%  # Reattach geometry
      st_as_sf() %>%
      mutate(
        industry_emission = co2_industry * industrial_weight,
        transport_emission = co2_transport * transport_weight,
        residential_emission = co2_residential * residential_weight,
        total_emission = industry_emission + transport_emission + residential_emission,
        Year = y
      )
  })



emission_results <- emission_results %>%
  filter(district_id %in% districts_static$district_id)  


emission_results_nb <- poly2nb(emission_results)
emission_results_weights <- nb2listw(emission_results_nb, style = "W")

# Perform Moran's I test
moran_test <- moran.mc(
  as.numeric(emission_results$total_emission),
  emission_results_weights,
  nsim = 999
)


ggplot(emission_results) +
  geom_sf(aes(fill = as.numeric(total_emission))) +
  scale_fill_viridis_c(name = "CO2 Emissions (tonnes)") +
  facet_wrap(~Year) +
  theme_minimal() +
  labs(title = "Disaggregated CO2 Emissions by District")

