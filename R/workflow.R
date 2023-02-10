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
#' @param region sf simple features object. Shape file of the region of
#'   interest.
#' @param lidar_dir Character. File path of where LiDAR tiles should be stored.
#'   Defaults to the cache directory.
#' @param only_new Logical. Whether to download all LiDAr tiles, or only new
#'   tiles that don't exist locally. Defaults to TRUE.
#'
#' @return
#' @export
#'
#' @examplesIf interactive()
#'
#' library(sf)
#'
#' # Load a shape file defining the region of interest
#' creek <- st_read("misc/data/Clinton_Creek.shp")
#'
#' # Fetch LiDAR DEM
#' creek_lidar <- lidar_region(creek)
#'
#' plot(creek_lidar)
#'
lidar_region <- function(region, lidar_dir = NULL, only_new = TRUE) {
  # Load lidar raster as combined (mosaic)
  message("Get LiDAR data")
  lidar <- lidar_fetch(region, out_dir = lidar_dir) %>%
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

#' Subset wells and add elevation
#'
#' This function takes a region shape file and LiDAR object of a region (output
#' of `lidar_region()`), subsets the wells data (from GWELLS) to this region and
#' adds LiDAR elevation data.
#'
#' @param region sf simple features object. Shape file of the region of
#'   interest.
#' @param lidar stars simple features object. Output of `lidar_region()`.
#'
#' @return
#' @export
#'
#' @examplesIf interactive()
#'
#' library(sf)
#'
#' # Load a shape file defining the region of interest
#' creek <- st_read("misc/data/Clinton_Creek.shp")
#'
#' # Fetch LiDAR DEM
#' creek_lidar <- lidar_region(creek)
#'
#' # Collect wells in this region with added elevation from LiDAR
#' creek_wells <- wells_elev(creek, creek_lidar)
#'
#' library(ggplot2)
#' ggplot() +
#'   geom_sf(data = creek) +
#'   geom_sf(data = creek_wells, size=0.5, colour = "dark blue",
#'           fill="NA",show.legend=FALSE) +
#'  coord_sf(datum = st_crs(3005)) # BC Albers
#'
wells_elev <- function(region, lidar, type = "wells_lith", update = FALSE) {

  if(!"sf" %in% class(region)) {
    stop("'region' must be an sf spatial object (see examples)",
         call. = FALSE)
  }
  if(!"stars" %in% class(lidar)) {
    stop("'lidar' must be a stars object output from `lidar_region()`",
         call. = FALSE)
  }

  # Subset wells to creek area, extract elevation from LiDAR
  message("Subset wells")

  region <- sf::st_transform(region, sf::st_crs(lidar))

  wells_sub <- data_read(type = paste0(type, "_sf"), update = update) %>%
    sf::st_transform(sf::st_crs(lidar)) %>%
    sf::st_filter(region)

  message("Add Lidar")
  wells_sub %>%
    dplyr::mutate(elev = stars::st_extract(lidar, .)[[1]]) %>%
    sf::st_transform(crs = 3005) # Transform to BC albers

}

#' Export wells data for use in Strater and Voxler
#'
#' @param wells_sub Data frame. Output of `wells_elev()`
#' @param id Character. Id to prepend to all output files e.g., "id_lith.csv"
#' @param dir Character. Directory where files should be exported to. Defaults
#'   to working directory.
#'
#' @return
#' @export
#'
#' @examplesIf interactive()
#'
#' library(sf)
#'
#' # Load a shape file defining the region of interest
#' creek <- st_read("misc/data/Clinton_Creek.shp")
#'
#' # Fetch LiDAR DEM
#' creek_lidar <- lidar_region(creek)
#'
#' # Collect wells in this region with added elevation from LiDAR
#' creek_wells <- wells_elev(creek, creek_lidar)
#'
#' # Export data for Strater and Voxler
#' wells_export(creek_wells, id = "clinton")

wells_export <- function(wells_sub, id, dir = ".") {

  wells_sub <- wells_sub %>%
    dplyr::bind_cols(as.data.frame(sf::st_coordinates(.))) %>%
    sf::st_drop_geometry()

  # Strater Lithology
  wells_sub %>%
    dplyr::select("Hole_ID" = "well_tag_number",
                  "From" = "lithology_from_m",
                  "To" = "lithology_to_m",
                  "Lithology_Keyword" = "lith_category",
                  "Lithology_Description" = "lithology_raw_data") %>%
    readr::write_csv(file.path(dir, paste0(id, "_lith.csv")))

  # Strater Collars
  wells_sub %>%
    dplyr::group_by(.data$well_tag_number, .data$X, .data$Y, .data$elev) %>%
    dplyr::summarize(Starting_Depth = min(.data$lithology_from_m),
                     Ending_Depth = max(.data$lithology_to_m),
                     .groups = "drop") %>%
    dplyr::select("Hole_ID" = "well_tag_number",
                  "Easting_Albers" = "X",
                  "Northing_Albers" = "Y",
                  "Starting_Depth", "Ending_Depth",
                  "Elevation" = "elev") %>%
    readr::write_csv(file.path(dir, paste0(id, "_collars.csv")))

  wells_sub %>%
    dplyr::select("well_tag_number", "water_depth_m") %>%
    readr::write_csv(file.path(dir, paste0(id, "_wls.csv")))


  voxler <- wells_sub %>%
    dplyr::mutate(Water_Elevation = .data$elev - .data$water_depth_m,
                  Component = 0) %>%
    dplyr::filter(!is.na(.data$Water_Elevation)) %>%
    dplyr::select("well_tag_number",
                  "Easting_Albers" = "X", "Northing_Albers" = "Y",
                  "Water_Elevation", "Component") %>%
    dplyr::distinct()


  voxler %>%
    dplyr::mutate(Component = 2, Water_Elevation = .data$Water_Elevation + 1) %>%
    dplyr::bind_rows(voxler) %>%
    readr::write_csv(file.path(dir, paste0(id, "_vox")))
}

