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
