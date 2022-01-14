library(bcaquiferdata)
library(sf)

# Load a shape file defining the region of interest
creek <- st_read("misc/data/Clinton_Creek.shp")

# Fetch LiDAR DEM
creek_lidar <- lidar_region(creek)

# Collect wells in this region with added elevation from LiDAR
creek_wells <- wells_elev(creek, creek_lidar)

# Export data for Strater and Voxler
wells_export(creek_wells, id = "clinton")
