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

# Lidar Tiles -----------------------------------------------------------------
lidar_url <- "https://nrs.objectstore.gov.bc.ca/gdwuts"

#bcdata::bcdc_get_record("bcgs-1-20-000-grid")
#bcdata::bcdc_tidy_resources('a61976ac-d8e8-4862-851e-d105227b6525')

tiles <- bcdata::bcdc_query_geodata('a61976ac-d8e8-4862-851e-d105227b6525') %>%
  dplyr::collect() %>%
  janitor::clean_names() %>%
  dplyr::select(map_tile) %>%
  dplyr::mutate(map_tile = tolower(map_tile))

tiles_utm <- tiles %>%
  sf::st_set_agr("constant") %>%# Suppress warnings about constant geometries
  sf::st_centroid() %>%
  sf::st_transform(4326) %>%
  dplyr::mutate(coords = purrr::map(geometry, ~as.data.frame(sf::st_coordinates(.)))) %>%
  tidyr::unnest(coords) %>%
  dplyr::mutate(utm = (floor((.data$X + 180)/6) %% 60) + 1) %>%
  sf::st_drop_geometry() %>%
  dplyr::select(map_tile, utm)

tiles <- dplyr::left_join(tiles, tiles_utm, by = "map_tile")

usethis::use_data(lidar_url, tiles, overwrite = TRUE)

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
  "water_bearing_estimated_flow_usgpm", "well_yield_unit_code",
  "lithology_observation")

fields_lith_new <- c(
  "lithology_from_m", "lithology_to_m",
  "flag_no_end", "flag_zero_zero", "flag_missing", "flag_bedrock_position", "lith_clean",
  "lith_primary", "lith_secondary", "lith_tertiary", "lith_flag",
  "lith_extra", "lith_yield", "lith_category", "depth_to_bedrock",
  "digits_extra", "yield", "depth")


usethis::use_data(fields_wells, fields_lith_gwells,
                  overwrite = TRUE, internal = TRUE)

