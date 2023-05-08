# Copyright 2021 Province of British Columbia
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

#' Create LiDAR DEM of a region
#'
#' This function takes a shape file of a region and creates a LiDAR DEM of the
#' region. LiDAR data is stored locally as tiles. Tiles are only downloaded if
#' they don't already exist unless `only_new = FALSE`.
#'
#' @param lidar_dir Character. File path of where LiDAR tiles should be stored.
#'   Defaults to the cache directory.
#' @param only_new Logical. Whether to download all LiDAr tiles, or only new
#'   tiles that don't exist locally. Defaults to TRUE.
#' @param progress Function. Progress bar to use. Generally leave as is.
#'
#' @inheritParams common_docs
#'
#' @section Data Source:
#'
#' LiDAR data is obtained programatically from the BC government portal
#' `r lidar_url` based on overlap between map tiles and the provided shapefile (`region`).
#' These LiDAR tiles can be browsed and downloaded manually via the
#' [LiDarBC Open LiDAR Data Portal](https://governmentofbc.maps.arcgis.com/apps/MapSeries/index.html?appid=d06b37979b0c4709b7fcf2a1ed458e03)
#'
#' The grid of map tiles is obtained from the BC Data Catalogue,
#' [BCGS 1:20,000 Grid](https://catalogue.data.gov.bc.ca/dataset/a61976ac-d8e8-4862-851e-d105227b6525)
#'
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
#' # Fetch LiDAR DEM
#' creek_lidar <- lidar_region(creek_sf)
#'
#' plot(creek_lidar)
#'
lidar_region <- function(region, lidar_dir = NULL, only_new = TRUE,
                         progress = httr::progress()) {
  # Load lidar raster as combined (mosaic)
  message("Get LiDAR data")
  lidar <- lidar_fetch(region, out_dir = lidar_dir, progress = progress) %>%
    dplyr::pull(.data$out_file) %>%
    normalizePath() %>%
    stars::st_mosaic() %>%
    stars::read_stars() %>%
    stats::setNames("elev")

  # Match regional crs to lidar
  region <- sf::st_transform(region, crs = sf::st_crs(lidar))

  # Clip lidar to region
  sf::st_crop(lidar, region)
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
    dplyr::left_join(data_read("lithology"),
                     by = c("well_tag_number", "well_yield_unit_code"))
}


#' Subset wells and add elevation
#'
#' This function takes a region shape file and LiDAR object of a region (output
#' of `lidar_region()`), subsets the wells data (from GWELLS) to this region and
#' adds LiDAR elevation data.
#'
#' @param lidar stars simple features object. Output of `lidar_region()`.
#'
#' @inheritParams common_docs
#'
#' @return sf spatial data frame
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
#' # Fetch LiDAR DEM
#' creek_lidar <- lidar_region(creek_sf)
#'
#' # Collect wells in this region with added elevation from LiDAR
#' creek_wells <- wells_elev(creek_wells, creek_lidar)
#'
#' library(ggplot2)
#' ggplot() +
#'   geom_sf(data = creek) +
#'   geom_sf(data = creek_wells, size=0.5, colour = "dark blue",
#'           fill="NA",show.legend=FALSE) +
#'  coord_sf(datum = st_crs(3005)) # BC Albers
#'
wells_elev <- function(wells_sub, lidar, update = FALSE) {

  # Checks
  if(!"sf" %in% class(wells_sub)) {
    stop("'wells_sub' must be an sf spatial object output by `wells_subset()`",
         call. = FALSE)
  }

  if(!"well_tag_number" %in% names(wells_sub)) {
    stop("`well_tag_number` is not a column in `wells_sub`. ",
         "`wells_sub` should be the output of `wells_subset()`", call. = FALSE)
  }

  if(!"stars" %in% class(lidar)) {
    stop("'lidar' must be a stars object output from `lidar_region()`",
         call. = FALSE)
  }

  message("Add Lidar")
  wells_sub <- wells_sub %>%
    sf::st_transform(sf::st_crs(lidar)) %>%
    dplyr::mutate(elev = stars::st_extract(lidar, .)[[1]]) %>%
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
    sf::st_drop_geometry() %>%
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
    tidyr::unnest(cols = c("yield", "depth"), keep_empty = TRUE) %>%
    dplyr::filter(!is.na(.data$yield) | !is.na(.data$well_yield_usgpm))
}

