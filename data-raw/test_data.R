# Local tests --------------------------------------------
data_update(download = FALSE)

mill <- sf::st_read("misc/data/MillBayWatershed.shp")
mill_lidar <- lidar_region(mill)

mill_wells <- wells_subset(mill)
mill_wells <- wells_elev(mill_wells, mill_lidar)

readr::write_rds(mill_wells, "mills.rds")


y <- wells_yield(mill_wells)


# Package tests ------------------------------------
# Small area, with at least 5 obs, requires only one lidar
creek_sf <- sf::st_read("misc/data/Clinton_Creek.shp") |>
  sf::st_crop(c(xmin = 1308000, ymin = 683000, xmax = 1310000, ymax = 684000))

usethis::use_data(creek_sf, overwrite = TRUE)


# Small set of wells with lithology
mill <- sf::st_read("misc/data/MillBayWatershed.shp")
mill_lidar <- lidar_region(mill)
mill_wells <- wells_subset(mill)
mill_elev <- wells_elev(mill_wells, mill_lidar)
#mill_yield <- wells_yield(mill_elev) # For getting tag numbers
mill_elev <- dplyr::filter(mill_elev, well_tag_number %in% c(
  921, 84493, 84499, 84498, 86675, 94353, 119112))
usethis::use_data(mill_elev, overwrite = TRUE)

