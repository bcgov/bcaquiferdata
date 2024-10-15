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

# Fields to combine -----------------------------------------------------------
# GWELLS Lithology fields which are combined into `lithology_raw_combined` by
# `lith_desc_combine()` in `lith_prep()`
fields_lith_combine <- c(
  "lithology_raw_data",
  "lithology_description_code",
  "lithology_material_code",
  "lithology_colour_code",
  "lithology_hardness_code",
  "lithology_observation")

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
  "lithology_hardness_code", "lithology_colour_code", "lithology_observation",
  "water_bearing_estimated_flow_usgpm" # "well_yield_unit_code",
  )

usethis::use_data(fields_lith_combine, fields_wells, fields_lith_gwells,
                  overwrite = TRUE, internal = TRUE)
