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

# Lidar Tiles - Spatial ------------------------------------------------------

#bcdata::bcdc_get_record("bcgs-1-20-000-grid")
#bcdata::bcdc_tidy_resources('a61976ac-d8e8-4862-851e-d105227b6525')

tiles <- bcdata::bcdc_query_geodata('a61976ac-d8e8-4862-851e-d105227b6525') %>%
  dplyr::collect() %>%
  janitor::clean_names() %>%
  dplyr::select(map_tile) %>%
  dplyr::mutate(map_tile = tolower(map_tile))

tile_utm <- tiles %>%
  sf::st_set_agr("constant") %>%# Suppress warnings about constant geometries
  sf::st_centroid() %>%
  sf::st_transform(4326) %>%
  dplyr::mutate(coords = purrr::map(geometry, ~as.data.frame(sf::st_coordinates(.)))) %>%
  tidyr::unnest(coords) %>%
  dplyr::mutate(utm = (floor((.data$X + 180)/6) %% 60) + 1) %>%
  sf::st_drop_geometry() %>%
  dplyr::select(map_tile, utm)


# Get LIDAR tile download locations ------------------------------------------
#
# Go to https://governmentofbc.maps.arcgis.com/apps/MapSeries/index.html?appid=d06b37979b0c4709b7fcf2a1ed458e03
#
# - Click Discovery and Download
# - Click on the small grey up arrow at the bottom of the screen to show the spreadsheet viewer
# - Click on "DEM Index - 1:20,000 Grid (the last tab)
# - Click Options and remove columns for Project Name and Spacing
# - Click on File Name and sort
# - Click on a row and then select all (Ctrl-A)
# - Scroll down the spreadsheet until the next section appears (just a little)
# - Copy and paste to `data-raw/lidar_tiles.txt`
# - Keep scrolling down two sections at a time and repeat the last three steps
# - Run this scripts (then re-run internal.R)

lidar_url <- "https://nrs.objectstore.gov.bc.ca/gdwuts"

n_tiles <- readr::read_lines("data-raw/lidar_tiles.txt") |>
  stringr::str_subset("^\\d+(?= features)") |>
  stringr::str_extract("^\\d+(?= features)") |>
  unique() |>
  as.numeric()

tile_files <- readr::read_lines("data-raw/lidar_tiles.txt") |>
  # Extract file names from tiles
  stringr::str_subset("bc_") |>
  stringr::str_remove("^\\t") |>
  paste(collapse = "\n") |>
  readr::read_tsv(col_names = c("file", "file2", "map_tile", "scale", "year", "proj"),
                  col_types = "c_ccnc") |>
  unique() |>
  # Ensure we copied all the tiles
  assertr::verify(expr = length(map_tile) == n_tiles) |>
  # Keep only the most recent tif per tile
  dplyr::slice_max(year, by = "map_tile") |>
  # Create download links
  dplyr::mutate(tile_part1 = stringr::str_extract(map_tile, "^\\d+"),
                tile_part2 = stringr::str_extract(map_tile, "^\\d+[a-z]+"),
                url = file.path(lidar_url, tile_part1, tile_part2, year,
                                 "dem", file),
                url = stringr::str_replace_all(url, " ", "%20"))

# Fix/Document errors
tile_files <- tile_files |>
  dplyr::mutate(
    # Deal with capital "K"s and "L"s in listing
    url = tolower(url),
    url = dplyr::case_when(
      map_tile == "092i071" ~ "https://nrs.objectstore.gov.bc.ca/gdwuts/092/092j/2019/dem/bc_092i071_xli1m_utm10_2019.tif",
      TRUE ~ url)
  )

# Find any other problems
#test <- tile_files |>
#  dplyr::mutate(test = purrr::map_lgl(url, ~identical(httr::status_code(httr::HEAD(.x)), 200L)))
#test <- verify(test, test)
#dplyr::filter(test, !test)

# Join and finish ----------------------------------------
tiles <- tiles |>
  dplyr::left_join(tile_utm, by = "map_tile") |>
  dplyr::left_join(dplyr::select(tile_files, "map_tile", "tile_name" = "file", "url"), by = "map_tile")

usethis::use_data(tiles, overwrite = TRUE)
