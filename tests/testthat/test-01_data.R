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

test_that("fix_bottom_intervals", {
  # Unfix examples
  w <- wells_eg |>
    dplyr::select("well_tag_number", dplyr::contains("depth"),
                  dplyr::contains("lith"), dplyr::contains("flag")) |>
    sf::st_drop_geometry() |>
    dplyr::mutate(
      well_depth_m = dplyr::if_else(flag_int_bottom, well_depth_m - 1, well_depth_m),
      lithology_to_m = dplyr::if_else(flag_int_bottom, lithology_to_m - 1, lithology_to_m))

  # Fix
  expect_message(w2 <- fix_bottom_intervals(w), "Fixing wells")
  expect_equal(w2$lithology_to_m[w2$flag_int_bottom],
               w$lithology_to_m[w$flag_int_bottom] + 1)
  expect_equal(w2$well_depth_m[w2$flag_int_bottom],
               w$well_depth_m[w$flag_int_bottom] + 1)
  expect_true("fix_int_bottom" %in% names(w2))
  expect_true(all(w2$fix_int_bottom[w2$flag_int_bottom]))

  # Message only
  expect_message(w2 <- fix_bottom_intervals(w, fix = FALSE), "Some wells have a bottom")
  expect_equal(w2$lithology_to_m[w2$flag_int_bottom],
               w$lithology_to_m[w$flag_int_bottom])
  expect_equal(w2$well_depth_m[w2$flag_int_bottom],
               w$well_depth_m[w$flag_int_bottom])
  expect_true("fix_int_bottom" %in% names(w2))
  expect_true(all(!w2$fix_int_bottom[w2$flag_int_bottom]))
})
