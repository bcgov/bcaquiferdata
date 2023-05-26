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
  creek_sf <- st_read("misc/data/Clinton_Creek.shp")


  Fetch Lidar DEM (this may take a while the first time)

  creek_lidar <- dem_region(creek_sf)
  plot(creek_lidar)
  creek_wells <- creek_sf |>
    wells_subset() |>        # Subset to region
    wells_elev(creek_lidar)  # Add Lidar


})
