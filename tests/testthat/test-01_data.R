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

test_that("clean_wells()", {
  f <- system.file("extdata", "test_gwells_wells.csv", package = "bcaquiferdata")
  expect_silent(w <- clean_wells(file = f))
  expect_true(all(c("well_depth_m", "water_depth_m") %in% names(w)))
})

test_that("wells", {
  skip_if(!file.exists(m <- test_path("../../misc/data/Clinton_Creek.shp")))
  creek_sf <- sf::st_read(m, quiet = TRUE)

  # Get lidar
  expect_message(creek_lidar <- dem_region(creek_sf)) |>
    suppressMessages()

  # Subset to region
  expect_message(creek_wells <- wells_subset(creek_sf)) |>
    suppressMessages()

  # Add Lidar
  expect_message(creek_wells <- wells_elev(creek_wells, creek_lidar)) |>
    suppressMessages()

})

test_that("fix_bottom_layers", {
  expect_silent(m2 <- fix_bottom_layers(mill_elev))
  expect_equal(m2$lithology_to_m[m2$flag_int_bottom],
               mill_elev$lithology_to_m[mill_elev$flag_int_bottom] + 1)
  expect_equal(m2$well_depth_m[m2$flag_int_bottom],
               mill_elev$well_depth_m[mill_elev$flag_int_bottom] + 1)
})
