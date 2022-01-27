library(bcaquiferdata)
library(ggplot2)
library(dplyr)
library(sf)
library(readr)

### Extra

# Load cleaned data (will fetch if doesn't already exist)
wells <- data_read("wells")

# Return lithology by well
wells %>%
  select(well_tag_number, well_depth_m, contains("lith")) %>%
  arrange(!is.na(lith_category)) %>%
  write_csv("lith_categorization.csv")


### Workflow

# Load a shape file defining the region of interest
creek <- st_read("misc/data/Clinton_Creek.shp")

# Fetch LiDAR DEM
creek_lidar <- lidar_region(creek)

plot(creek_lidar)

# Collect wells in this region with added elevation from LiDAR
creek_wells <- wells_elev(creek, creek_lidar)

ggplot() +
  geom_sf(data = creek) +
  geom_sf(data = creek_wells, size= 0.5, colour = "dark blue",
          fill="NA", show.legend = FALSE) +
 coord_sf(datum = st_crs(3005)) # BC Albers

# Export data for Strater and Voxler
wells_export(creek_wells, id = "clinton")


wells <- data_read("wells")
