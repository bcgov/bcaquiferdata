data_update(download = FALSE)

mill <- st_read("misc/data/MillBayWatershed.shp")
mill_lidar <- lidar_region(mill)
mill_wells <- wells_elev(mill, mill_lidar)
readr::write_rds(mill_wells, "mills.rds")
