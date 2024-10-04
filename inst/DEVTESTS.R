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


# library(bcaquiferdata) # Use Ctrl-l to load all functions when developing
library(ggplot2)
library(dplyr)
library(sf)
library(readr)

# Load cleaned data (will fetch if doesn't already exist)
#wells <- data_read("wells")

# Update local data
data_update()
data_update(download = FALSE)


# Shiny tests -----------------------------

# Test app
aq_app()

mod_test("export_data")



# Silver creek -----------------

ws_sf <- st_read("misc/data/SilverdaleCreekWatershed2/SilverdaleCreekWatershed2.shp")

ws_lidar <- dem_region(ws_sf)
ws_lidar <- dem_region(ws_sf, "trim")

ws_lidar_sf <- stars::st_downsample(ws_lidar, n = 12) |> # Downsample first
  st_as_sf(as_points = FALSE, merge = TRUE)

ggplot() +
  geom_sf(data = ws_sf) +
  geom_sf(data = ws_lidar_sf, aes(fill = elev), colour = NA)


ws_wells <- ws_sf |>
  wells_subset() |>        # Subset to region
  wells_elev(ws_lidar)     # Add Lidar

ggplot() +
  geom_sf(data = ws_sf) +
  geom_sf(data = ws_wells, aes(colour = elev))



# Tsolumn Watershed -------------
library(sf)
library(ggplot2)

ws_sf <- st_read("misc/data/TsolumWatershedBdy/TsolumWatershedBdy.shp")
ws_lidar <- dem_region(ws_sf)
plot(ws_lidar)

ws_wells <- ws_sf |>
  wells_subset() |>        # Subset to region
  wells_elev(ws_lidar)  # Add Lidar


ggplot() +
  geom_sf(data = ws_sf) +
  geom_sf(data = ws_wells, size= 1, aes(colour = elev))


ws_lidar_sf <- stars::st_downsample(ws_lidar, n = 12) |> # Downsample first
  st_as_sf(as_points = FALSE, merge = TRUE)         # Convert to polygons

ggplot() +
  geom_sf(data = ws_sf) +
  geom_sf(data = ws_lidar_sf, aes(fill = elev), colour = NA) +
  geom_sf(data = ws_wells, size= 1, aes(colour = elev))

# Clinton Creek Watershed ----------------
library(sf)
library(dplyr)
ws_sf <- st_read("misc/data/Clinton_Creek.shp")
ws_lidar <- dem_region(ws_sf)
ws_trim <- dem_region(ws_sf, type = "trim")



ws_wells <- ws_sf |>
  wells_subset() |>        # Subset to region
  wells_elev(ws_lidar)     # Add Lidar

filter(ws_wells, well_tag_number %in% c(20593, 111562)) |>
  select("well_tag_number", "lithology_raw_combined")

w <- data_read("lithology")
filter(w, well_tag_number %in% c(20593, 111562)) |>
  select("well_tag_number", "lithology_raw_combined")

# Koksilah Watershed ---------------------------------------
library(bcaquiferdata)
ko_sf <- sf::st_read("misc/data/Koksilah_watershed4/Koksilah_watershed4.shp")
ko_lidar <- dem_region(ko_sf)

# Exports ----------------------------------------
ws_sf <- sf::st_read("misc/data/TsolumWatershedBdy/TsolumWatershedBdy.shp")
ws_lidar <- dem_region(ws_sf)
ws_wells <- ws_sf |>
  wells_subset() |>        # Subset to region
  wells_elev(ws_lidar)  # Add Lidar

wells_export(ws_wells, id = "well1", type = "leapfrog")

# A single well -------------------------------------------------
data_read("wells") |>
  dplyr::filter(well_tag_number == 14068) |>
  dplyr::select(1:3) |>
  as.data.frame()

m <- rnaturalearth::ne_states("Canada", returnclass = "sf") |>
  dplyr::filter(name == "British Columbia") |>
  dplyr::select(name) |>
  sf::st_crop(xmin = -123.637 - 0.1, xmax = -123.637 + 0.1,
              ymin =  48.72952 - 0.1, ymax = 48.72952 + 0.1)

m_lidar <- dem_region(m)
m_wells <- m |>
  wells_subset() |>        # Subset to region
  wells_elev(m_lidar)  # Add Lidar

m_wells |>
  wells_yield() |>
  dplyr::select(dplyr::contains("flag"))
#dplyr::filter(well_tag_number == 14068) |>
wells_export(id = "test", type = "archydro")


# Lithology tests ---------------------------------------
library(dplyr)
library(stringr)

lith_fix(desc = "bedrock with sand")
lith_fix(desc = "fractured bedrock")
lith_fix(desc = "fracturing bedrock")
lith_fix(desc = "sand & bedrock")

lith_fix(desc = "boulders")
lith_fix(desc = "boulders with sand")
lith_fix(desc = "sand with boulders")
lith_fix(desc = "bouldery sand")
lith_fix(desc = "sand and boulders")

lith_fix(desc = "blue hardpan, sand and broken gravel")

wells_lith <- data_read("lithology") |>
  select(lithology_raw_combined, lithology_clean, lith_primary, lith_secondary, lith_tertiary, lithology_category, lithology_extra)

filter(wells_lith, lith_tertiary == "fractured")

filter(wells_lith, str_detect(lithology_clean, "faulted|weathered"))

filter(wells_lith, lith_secondary == "bedrock")
lith_fix(desc = "sand with bedrock")


filter(wells_lith, lithology_extra != "")

filter(wells_lith, str_detect(lithology_raw_combined, "-[a-zA-Z]+")) |>
  filter(str_detect(lithology_raw_combined, "water-bearing", negate = TRUE))


filter(wells_lith, lithology_category == "Compact")
