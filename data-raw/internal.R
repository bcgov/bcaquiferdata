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


# Fields to keep --------------------------------------------------------------
fields_wells <- c(
  "well_tag_number", "longitude_decdeg", "latitude_decdeg",
  "utm_northing", "utm_easting",
  "water_depth_m", "well_depth_m",
  "total_depth_drilled_ft_bgl", "finished_well_depth_ft_bgl",
  "bedrock_depth_ft_bgl", "ground_elevation_ft_asl",
  "static_water_level_ft_btoc", "well_yield_usgpm", "well_yield_unit_code",
  "artesian_flow_usgpm", "artesian_pressure_psi", "water_quality_colour",
  "water_quality_odour", "yield_estimation_rate_usgpm",
  "static_level_before_test_ft_btoc", "drawdown_ft_btoc", "aquifer_id",
  "storativity", "transmissivity_m_2_s", "hydraulic_conductivity_m_s",
  "specific_storage_1_m", "specific_yield", "aquifer_lithology_code",
  "artesian_pressure_head_ft_agl", "artesian_conditions")

fields_lith_gwells <- c(
  "well_tag_number", "lithology_from_ft_bgl", "lithology_to_ft_bgl",
  "lithology_raw_data", "lithology_description_code", "lithology_material_code",
  "lithology_hardness_code", "lithology_colour_code",
  "water_bearing_estimated_flow_usgpm", # "well_yield_unit_code",
  "lithology_observation")

fields_lith_new <- c(
  "lithology_from_m", "lithology_to_m",
  "flag_no_end", "flag_zero_zero", "flag_missing", "flag_bedrock_position", "lith_clean",
  "lith_primary", "lith_secondary", "lith_tertiary", "lith_flag",
  "lith_extra", "lith_yield", "lith_category", "bedrock_depth_m",
  "digits_extra", "yield", "depth")


# Test data ------------------------------------------------------------------
# Local tests --------------------------------------------
#data_update(download = FALSE)

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


usethis::use_data(mill_elev, fields_wells, fields_lith_gwells,
                  overwrite = TRUE, internal = TRUE)

