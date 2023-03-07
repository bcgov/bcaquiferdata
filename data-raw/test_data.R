data_update(download = FALSE)

mill <- sf::st_read("misc/data/MillBayWatershed.shp")
mill_lidar <- lidar_region(mill)

mill_wells <- wells_subset(mill)

mill_wells <- wells_elev(mill, mill_lidar)

y <- wells_yield(mill_wells)

readr::write_rds(mill_wells, "mills.rds")
