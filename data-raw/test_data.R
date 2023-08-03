# Copyright 2023 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations under
# the License.

# Local tests --------------------------------------------
data_update(download = FALSE)

mill <- sf::st_read("misc/data/MillBayWatershed.shp")
mill_lidar <- dem_region(mill)

mill_wells <- wells_subset(mill)
mill_wells <- wells_elev(mill_wells, mill_lidar)

readr::write_rds(mill_wells, "misc/mills.rds")


y <- wells_yield(mill_wells)


# Package tests ------------------------------------
# Small area, with at least 5 obs, requires only one lidar
#creek_sf <- sf::st_read("misc/data/Clinton_Creek.shp") |>
#  sf::st_crop(c(xmin = 1308000, ymin = 683000, xmax = 1310000, ymax = 684000))
# usethis::use_data(creek_sf, overwrite = TRUE)


# Small set of wells with lithology
mill <- sf::st_read("misc/data/MillBayWatershed.shp")
mill_lidar <- dem_region(mill)
mill_wells <- wells_subset(mill)
#mill_yield <- wells_yield(mill_elev) # For getting tag numbers

withr::with_seed(
  111,
  {
    mill_elev <- wells_elev(mill_wells, mill_lidar) |>
      dplyr::filter(well_tag_number %in% c(
        921, 84493, 84499, 84498, 86675, 94353, 119112)) |>
      dplyr::mutate(
        well_tag_number = as.numeric(paste0("99999999999",
                                            as.numeric(as.factor(well_tag_number)))),
        utm_northing = jitter(utm_northing),
        utm_easting = jitter(utm_easting)) |>
      sf::st_jitter()
  })


usethis::use_data(mill_elev, overwrite = TRUE)

