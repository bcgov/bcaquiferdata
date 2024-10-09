# Copyright 2024 Province of British Columbia
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

test_that("wells", {
  skip_if(!file.exists(m <- test_path("../../misc/data/Clinton_Creek.shp")))
  r <- sf::st_read(m, quiet = TRUE)

  # Get lidar
  expect_message(elev <- dem_region(r)) |>
    suppressMessages()

  # Subset to region
  expect_message(wells <- wells_subset(r)) |>
    suppressMessages()

  # Add Lidar
  expect_message(wells_elev <- wells_elev(wells, elev)) |>
    suppressMessages()

  # Add yield
  expect_silent(wells_yield <- wells_yield(wells_elev))

  # Flags are consistent - Must update local lithology data first!
  expect_true(all(flags$Flag %in% names(wells_yield)))
  expect_equal(sort(stringr::str_subset(names(wells_yield), "^flag_|^fix_")),
               sort(flags$Flag))

})

test_that("fix_bottom_intervals", {
  # Unfix examples
  w <- dplyr::select(wells_eg_unfixed, -"fix_int_bottom")

  # Fix
  expect_message(w2 <- fix_bottom_intervals(w), "Fixing wells")
  expect_equal(nrow(w), nrow(w2))
  expect_equal(sf::st_geometry(w), sf::st_geometry(w2))
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

test_that("fix_depth_missing", {
  w <- dplyr::select(wells_eg_unfixed, -"fix_depth_missing")

  # Fix
  expect_message(w2 <- fix_depth_missing(w), "Fixing wells")
  expect_equal(nrow(w), nrow(w2))
  expect_equal(sf::st_geometry(w), sf::st_geometry(w2))
  expect_true(all(!is.na(w2$well_depth_m[w2$fix_depth_missing])))
  expect_equal(w2$well_depth_m[w2$flag_depth_missing & w2$lith_n == w2$lith_rec],
               w$lithology_to_m[w$flag_depth_missing & w$lith_n == w$lith_rec])

  expect_true("fix_depth_missing" %in% names(w2))
  expect_true(all(w2$fix_depth_missing[w2$flag_depth_missing]))

  # Message only
  expect_message(w2 <- fix_depth_missing(w, fix = FALSE), "Some wells are missing well depth")
  expect_true(all(is.na(w2$well_depth_m[w2$flag_depth_missing])))
  expect_true("fix_depth_missing" %in% names(w2))
  expect_true(all(!w2$fix_depth_missing[w2$flag_depth_missing]))
})
