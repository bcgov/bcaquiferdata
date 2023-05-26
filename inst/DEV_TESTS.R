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


library(bcaquiferdata)
library(ggplot2)
library(dplyr)
library(sf)
library(readr)

### Extra

# Load cleaned data (will fetch if doesn't already exist)
#wells <- data_read("wells")

data_update(which = "all")

# Return lithology by well
# wells %>%
#   dplyr::select(well_tag_number, well_depth_m, contains("lith")) %>%
#   dplyr::arrange(!is.na(lith_category)) %>%
#   readr::write_csv("lith_categorization.csv")

#l <- clean_lithology(wells)
p <- lith_prep(wells)

dplyr::filter(p, stringr::str_detect(lithology_raw_data, "'"))

l <- lith_fix()

# Check for immediate problems
test_desc <- dplyr::filter(l, is.na(lith_category), lith_clean != "", lith_yield == "") %>%
  dplyr::pull(lithology_raw_data)

lith_fix(desc = test_desc) %>%
  dplyr::as_tibble() %>%
  dplyr::filter(
    is.na(lith_category),
    lith_clean != "",
    lith_yield == "",
    lith_clean != lith_extra,
    stringr::str_detect(lith_clean, "(flow)|(traces)|(seepage)|(overburden)", negate = TRUE)) %>%
  dplyr::select(lith_primary, lith_secondary, lith_tertiary) %>%
  dplyr::distinct() %>%
  as.data.frame()


y <- dplyr::filter(l, stringr::str_detect(lithology_raw_data, "425 feet 1 3/4 gpm")) %>%
  dplyr::select(lithology_raw_data, lith_clean, lith_yield) %>%
  lith_yield()

dplyr::filter(y, lith_yield == "gpm, gph")


lith_fix(desc = "med. to hard granite")

lith_fix(desc = "low plasticity silt, silty clay ")

lith_fix(desc = "very hard clay, silt came up in good")
lith_fix(desc = "tilly silt")
lith_fix(desc = "clay till")
lith_fix(desc = "tilly clay")
lith_fix(desc = "silt hardpacked bedrock")

t <- lith_fix(desc = "soil'") %>%
  lith_yield()

lith_fix(desc = "soft bedrock - 6 gpm") %>%
  lith_yield()

lith_fix(desc = "soft bedrock - 8-10 gpm") %>%
  lith_yield()



#' Questions -----------------------------------
lith_fix(desc = "hard packed gravel & boulders")


# Checks - all NAs
"very hard clay, silt came up in good" - Organics?
  "silty clay (cemented)"

### Workflow

# Load a shape file defining the region of interest
creek <- st_read("misc/data/Clinton_Creek.shp")

# Fetch Lidar DEM
creek_lidar <- dem_region(creek)

plot(creek_lidar)

# Collect wells in this region with added elevation from Lidar
creek_wells <- wells_elev(creek, creek_lidar)

ggplot() +
  geom_sf(data = creek) +
  geom_sf(data = creek_wells, size= 0.5, colour = "dark blue",
          fill="NA", show.legend = FALSE) +
 coord_sf(datum = st_crs(3005)) # BC Albers

# Export data for Strater and Voxler
wells_export(creek_wells, id = "clinton")


wells <- data_read("wells")


# Plotting --------------------------------------
library(sf)
library(ggplot2)
library(bcaquiferdata)
library(ggspatial)

creek <- st_read("misc/data/Clinton_Creek.shp")
creek_lidar <- dem_region(creek)

g <- ggplot() +
  geom_sf(data = creek, fill = NA, linewidth = 1.5)
g

ds <- nrow(creek_lidar) / 100
temp <- stars::st_downsample(creek_lidar, n = ds) %>%
  st_transform(crs = st_crs(creek))

g + geom_stars(data = temp, aes(x = x, y = y))



