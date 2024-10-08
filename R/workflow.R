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
#' Lidar data is obtained from the LidarBC portal. The `tiles` data frame
#' contains is an internally created data frame listing tiles and their
#' respective download locations. Tiles to download are selected based on
#' overlap between map tiles and the provided shapefile (`region`). These Lidar
#' tiles can be browsed and downloaded manually via the
#' [LidarBC Open LiDAR Data Portal](https://governmentofbc.maps.arcgis.com/apps/MapSeries/index.html?appid=d06b37979b0c4709b7fcf2a1ed458e03)
#'
#' The grid of map tiles is obtained from the BC Data Catalogue,
#' [BCGS 1:20,000 Grid](https://catalogue.data.gov.bc.ca/dataset/a61976ac-d8e8-4862-851e-d105227b6525)
#'
#' TRIM data is obtained via the `bcmaps` package from the BC government
#' [Data Catalogue](https://catalogue.data.gov.bc.ca/dataset/7b4fef7e-7cae-4379-97b8-62b03e9ac83d)
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
    dem <- bcmaps::cded(region, ask = FALSE)
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
#' @param fix_bottom Logical. Whether to add 1m to bottom lithology intervals that
#'   has no thickness (identified by `flat_int_bottom`). Default `TRUE`.
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
wells_subset <- function(region, fix_bottom = TRUE, update = FALSE) {

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
      by = "well_tag_number") |>
    dplyr::mutate(flag_lith_missing = dplyr::if_else(is.na(.data$lithology_from_m), TRUE, FALSE)) |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("flag_"), \(x) tidyr::replace_na(x, FALSE))) |>
    fix_bottom_intervals(fix = fix_bottom)
}


#' Subset wells and add elevation
#'
#' This function takes a region shape file and the DEM of a region (output of
#' `dem_region()`), subsets the wells data (from GWELLS) to this region and adds
#' the elevation data.
#'
#' @param dem stars simple features object. Output of `dem_region()`. Primary
#'   source of elevation data.
#' @param dem_extra stars simple features object. Output of `dem_region()`.
#'   Optional secondary source of elevation data. Useful in situations where the
#'   primary source is incomplete. **Use with caution: Combining elevations
#'   measured though different techniques may introduce artifacts (See
#'   Details)**.
#'
#' @details
#' Because combining elevation data measured from different sources can
#' introduce artifacts, if two sources of elevation are provided (i.e. both
#' `dem` and `dem_extra`), the data will contain extra columns for assessment.
#' Specifically, there will be three elevation columns, rather than just one.
#' `elev1` contains elevations from the primary source (`dem`), `elev2` contains
#' elevations from the secondary source (`dem_extra`), `elev` contains the
#' combined elevation data, `elev1` unless missing, then `elev2`.
#'
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
#' # Dealing with missing data
#' mill_sf <- st_read("misc/data/MillBayWatershed.shp")
#' mill_wells <- wells_subset(mill_sf)
#'
#' mill_lidar <- dem_region(mill_sf)
#' mill_trim <- dem_region(mill_sf, type = "trim")
#'
#' mill_wells <- wells_elev(mill_wells, dem = mill_lidar, dem_extra = mill_trim)
#'
#' # See how the elevation data is combined, `dem` (elev1) is the primary source.
#' select(mill_wells, well_tag_number, elev1, elev2, elev)
#'
wells_elev <- function(wells_sub, dem, dem_extra = NULL, update = FALSE) {

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
  e1 <- wells_sub %>%
    sf::st_transform(sf::st_crs(dem)) %>%  # Faster to transform wells than dem
    dplyr::mutate(elev = round(stars::st_extract(dem, .)[[1]], 2)) %>%
    sf::st_transform(crs = 3005) # Transform to BC albers

  if(!is.null(dem_extra)) {
    warning("Combining elevations measured through different techniques may ",
            "introduce artifacts into your measure of elevation. ",
            "Use with caution.", call. = FALSE)

    e2 <- wells_sub %>%
      dplyr::select("well_tag_number", "geometry") %>%
      sf::st_transform(sf::st_crs(dem_extra)) %>%
      dplyr::mutate(elev2 = round(stars::st_extract(dem_extra, .)[[1]], 2)) %>%
      sf::st_drop_geometry()

    e1 <- e1 %>%
      dplyr::rename(elev1 = "elev") %>%
      dplyr::left_join(e2, by = "well_tag_number") |>
      dplyr::mutate(elev = dplyr::coalesce(.data[["elev1"]], .data[["elev2"]]))
  }
  e1
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
      "lithology_raw_combined", dplyr::starts_with("flag")) %>%
    lith_yield() %>%
    dplyr::mutate(flag_yield = tidyr::replace_na(.data$flag_yield, FALSE))
}



#' Fix the depth of the final interval if missing
#'
#' The `flag_int_bottom` flag identifies the bottom interval if it is depthless,
#' but otherwise okay. Fixing these intervals means adding 1m to them and to the
#' final depth of the well.
#'
#' @param wells_sub The subsetted wells data frame (combined with lithology)
#'
#' @return Fixed wells_sub data frame
#' @noRd
fix_bottom_intervals <- function(wells_sub, fix = TRUE) {

  if(!"fix_int_bottom" %in% names(wells_sub)) wells_sub$fix_int_bottom <- FALSE

  # Which wells need to be fixed and haven't been?
  w <- which(wells_sub$flag_int_bottom & !wells_sub$fix_int_bottom)
  w_pretty <- unique(wells_sub$well_tag_number[w]) |> paste0(collapse = ", ")

  if(length(w) > 0) {
    if(fix) {
      message("Fixing wells with a bottom lithology interval of zero thickness: ",
              w_pretty)

      wells_sub$lithology_to_m[w] <- wells_sub$lithology_to_m[w] + 1
      wells_sub$lithology_to_ft_bgl[w] <- wells_sub$lithology_to_ft_bgl[w] + 3.28084
      wells_sub$well_depth_m[w] <- wells_sub$well_depth_m[w] + 1
      wells_sub$finished_well_depth_ft_bgl[w] <- wells_sub$finished_well_depth_ft_bgl[w] + 3.28084
      wells_sub$fix_int_bottom[w] <- TRUE

    } else {
      message("Some wells have a bottom lithology interval of zero thickness.\n",
              "Consider either using `fix_bottom = TRUE` in `wells_subset()` or ",
              "fixing the original record in GWELLS\n",
              "Wells: ", w_pretty)
    }
  }

  wells_sub
}
