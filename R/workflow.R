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

#' Fetch and trim DEM of a region
#'
#' This function takes a shape file of a region and creates a DEM of the region.
#' Lidar data is stored locally as tiles. Tiles are only downloaded if they
#' don't already exist unless `only_new = FALSE`. TRIM data is obtained via the
#' `bcmaps` package and stored locally as tiles. **Note:** TRIM elevation is
#' coarser than Lidar Use Lidar unless it is missing for your region of
#' interest.
#'
#' Lidar tiles are the newest tile available. If you have reason to need a
#' historical file, contact the team to discuss your use case.
#'
#' @param type Character. Type of DEM to download, either "lidar" or "trim". Use
#'  Lidar unless unavailable.
#' @param buffer Numeric. Percent buffer to apply to the `region` spatial file
#'   before cropping the DEM data to match. Increase this value if you find
#'   that wells on the edge of your area aren't been matched to elevations when
#'   using `wells_elev()`.
#' @param lidar_dir Character. File path of where Lidar tiles should be stored.
#'   Defaults to the cache directory. Only applies when `type = "lidar"`.
#' @param only_new Logical. Whether to download all Lidar tiles, or only new
#'   tiles that don't exist locally. Defaults to TRUE. Only apples when `type =
#'   "lidar"`.

#' @param progress Function. Progress bar to use. Generally leave as is.
#'
#' @inheritParams common_docs
#'
#' @section Data Source:
#'
#' Lidar data is obtained programatically from the BC government portal
#' `r lidar_url` based on overlap between map tiles and the provided shapefile (`region`).
#' These Lidar tiles can be browsed and downloaded manually via the
#' [LidarBC Open LiDAR Data Portal](https://governmentofbc.maps.arcgis.com/apps/MapSeries/index.html?appid=d06b37979b0c4709b7fcf2a1ed458e03)
#'
#' The grid of map tiles is obtained from the BC Data Catalogue,
#' [BCGS 1:20,000 Grid](https://catalogue.data.gov.bc.ca/dataset/a61976ac-d8e8-4862-851e-d105227b6525)
#'
#' TRIM data is obtained via the `bcmaps` package from the BC government portal
#' https://catalogue.data.gov.bc.ca/dataset/7b4fef7e-7cae-4379-97b8-62b03e9ac83d
#' based on overlap between map tiles and the provided shapefile (`region`).

#' @return stars spatiotemporal array object
#' @export
#'
#' @examplesIf interactive()
#'
#' library(sf)
#'
#' # Load a shape file defining the region of interest
#' creek_sf <- st_read("misc/data/Clinton_Creek.shp")
#'
#' # Fetch Lidar DEM
#' creek_lidar <- dem_region(creek_sf)
#'
#' plot(creek_lidar)
#'
#' # Fetch TRIM DEM
#' creek_trim <- dem_region(creek_sf, type = "trim")
#'
#' plot(creek_trim)
#'
#'
dem_region <- function(region, type = "lidar", buffer = 1,
                       lidar_dir = NULL, only_new = TRUE,
                       progress = httr::progress()) {

  type <- tolower(type)
  if(!type %in% c("lidar", "trim")) {
    stop("`type` must be one of 'lidar' or 'trim'", call. = FALSE)
  }

  # Add Buffer
  region <- sf::st_buffer(region, sqrt(sf::st_area(region)) * buffer/100)

  # Load DEM raster as combined (mosaic)
  if(type == "lidar") {
    message("Get Lidar data")
    dem  <- lidar_fetch(region, out_dir = lidar_dir, progress = progress) %>%
      dplyr::pull(.data$out_file)

  } else if(type == "trim") {
    message("Get TRIM data")
    dem <- bcmaps::cded(region)
  }

  dem <- dem %>%
    normalizePath() %>%
    stars::st_mosaic() %>%
    stars::read_stars(proxy = TRUE) %>%
    stats::setNames("elev")

  message("Cropping DEM to region\n")

  # Match regional crs to dem
  region <- sf::st_transform(region, crs = sf::st_crs(dem))

  # Clip dem to region
  sf::st_crop(dem, region)
}

#' Subset wells to region
#'
#' Filter the GWELLS data returning only wells within the provided shapefile.
#'
#' @inheritParams common_docs
#'
#' @examplesIf interactive()
#'
#' library(sf)
#'
#' # Load a shape file defining the region of interest
#' creek_sf <- st_read("misc/data/Clinton_Creek.shp")
#'
#' # Get wells within this region
#' creek_wells <- wells_subset(creek_sf)
#'
#' @export
wells_subset <- function(region, update = FALSE) {

  if(!"sf" %in% class(region)) {
    stop("'region' must be an sf spatial object (see examples)",
         call. = FALSE)
  }

  # Subset wells to creek area
  message("Subset wells")
  data_read(type = "wells_sf", update = update) %>%
    sf::st_transform(sf::st_crs(region)) %>%
    sf::st_filter(region) %>%
    dplyr::left_join(
      data_read("lithology") |> dplyr::select(-"well_yield_unit_code"),
      by = "well_tag_number") #by = c("well_tag_number", "well_yield_unit_code"))
}


#' Subset wells and add elevation
#'
#' This function takes a region shape file and the DEM of a region (output of
#' `dem_region()`), subsets the wells data (from GWELLS) to this region and adds
#' the elevation data.
#'
#' @param dem stars simple features object. Output of `dem_region()`.
#'
#' @inheritParams common_docs
#'
#' @return sf spatial data frame
#' @export
#'
#' @examplesIf interactive()
#'
#' library(sf)
#' library(ggplot2)
#'
#' # Load a shape file defining the region of interest
#' creek_sf <- st_read("misc/data/Clinton_Creek.shp")
#'
#' # Get wells within this region
#' creek_wells <- wells_subset(creek_sf)
#'
#' # Fetch Lidar DEM
#' creek_lidar <- dem_region(creek_sf)
#'
#' # Collect wells in this region with added elevation from Lidar
#' creek_wells <- wells_elev(creek_wells, creek_lidar)
#'
#' ggplot() +
#'   geom_sf(data = creek_sf) +
#'   geom_sf(data = creek_wells, aes(colour = elev), size = 0.5,
#'           fill = "NA", show.legend = FALSE) +
#'  coord_sf(datum = st_crs(3005)) # BC Albers
#'
#' # OR Fetch TRIM DEM
#' creek_trim <- dem_region(creek_sf, type = "trim")
#'
#' # Collect wells in this region with added elevation from Lidar
#' creek_wells <- wells_elev(creek_wells, creek_trim)
#'
#' ggplot() +
#'   geom_sf(data = creek_sf) +
#'   geom_sf(data = creek_wells, aes(colour = elev), size = 0.5,
#'           fill = "NA", show.legend = FALSE) +
#'  coord_sf(datum = st_crs(3005)) # BC Albers
#'
wells_elev <- function(wells_sub, dem, update = FALSE) {

  # Checks
  if(!"sf" %in% class(wells_sub)) {
    stop("'wells_sub' must be an sf spatial object output by `wells_subset()`",
         call. = FALSE)
  }

  if(!"well_tag_number" %in% names(wells_sub)) {
    stop("`well_tag_number` is not a column in `wells_sub`. ",
         "`wells_sub` should be the output of `wells_subset()`", call. = FALSE)
  }

  if(!"stars" %in% class(dem)) {
    stop("'dem' must be a stars object output from `dem_region()`",
         call. = FALSE)
  }

  message("Add elevation")
  wells_sub <- wells_sub %>%
    sf::st_transform(sf::st_crs(dem)) %>%
    dplyr::mutate(elev = round(stars::st_extract(dem, .)[[1]], 2)) %>%
    sf::st_transform(crs = 3005) # Transform to BC albers
}


#' Add yield lithology data to wells subset
#'
#' Yield records are extracted from lithology observations and added to the
#' wells data.
#'
#' @inheritParams common_docs
#'
#' @return Data frame or sf spatial data frame with wells data and added yield
#'   from lithology.
#' @export
#'
#' @examplesIf interactive()
#'
#' library(sf)
#'
#' # Load a shape file defining the region of interest
#' creek_sf <- st_read("misc/data/Clinton_Creek.shp")
#'
#' # Get wells within this region
#' creek_wells <- wells_subset(creek_sf)
#'
#' # Get yield data for these wells
#' creek_yield <- wells_yield(creek_wells)

wells_yield <- function(wells_sub) {
  wells_sub %>%
    dplyr::mutate(
      fractured =
        .data$lithology_category == "Weathered, Fractured or Faulted Bedrock") %>%
    dplyr::select(
      "well_tag_number", dplyr::any_of("elev"), "well_depth_m",
      "lithology_from_m", "lithology_to_m",
      "well_yield_usgpm", "well_yield_unit_code",
      "fractured", "yield_units",
      "lithology_raw_data", dplyr::starts_with("flag")) %>%
    lith_yield() %>%
    tidyr::unnest(cols = c("yield", "depth"), keep_empty = TRUE)
}

