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


#' Fetch Lidar tiles corresponding to a region
#'
#' @param region Sf object of region
#' @param out_dir Character. Output folder, defaults to working directory
#' @param only_new Logical. Only download new, missing Lidar tiles, default to
#'   TRUE
#' @param verbose Logical. Show extra output. Can be useful to check on
#'   progress.
#' @param progress Function. Type of progress bar to use. Only change to use
#'   Shiny bar in Apps
#'
#' @return Data frame describing tiles and location on disk
#' @noRd

lidar_fetch <- function(region, out_dir = NULL, only_new = TRUE, verbose = FALSE,
                        quiet = FALSE,
                        progress = httr::progress()) {

  if(is.null(out_dir)) {
    cache_check()
    out_dir <- file.path(cache_dir(), "tiles")
    if(!dir.exists(out_dir)) dir.create(out_dir)
    if(!quiet) message("Saving new tiles to cache directory: ", cache_dir())
  }

  if(!"sf" %in% class(region)) {
    stop("region must be an sf spatial object (see examples)", call. = FALSE)
  }

  if(only_new) local_tiles <- list.files(out_dir) else local_tiles <- vector()
  # Check for tifs on each call, checking all tiles takes too long
  if(!quiet) message("Checking for matching tifs")

  region <- sf::st_transform(region, sf::st_crs(bcaquiferdata::tiles))

  fetch <- sf::st_filter(bcaquiferdata::tiles, region)

  if(nrow(fetch) == 0) {
    stop("Region does not overlap any BC tiles. ",
         "Are you sure this is a region in British Columbia?", call. = FALSE)
  }

  fetch <- fetch %>%
    sf::st_drop_geometry() %>%
    dplyr::mutate(
      tif_good = purrr::map_lgl(.data$url, url_exists),
      out_file = file.path(.env$out_dir, .data$tile_name))

  # Warn if cannot find a tile
  if(any(!fetch$tif_good)) {
    problems <- stringr::str_remove(fetch$map_tile[!fetch$tif_good], "^0") %>%
      stringr::str_replace("(.*)(\\d{3})", "\\1.\\2") %>%
      paste0("\n- ", fetch$map_tile[!fetch$tif_good], " (", ., ")")

    problems <- paste(
      "Could not find a lidar image for map tile(s):", paste0(problems, collapse = ""),
      "\n\nConfirm lidar for tile is missing by seaching 'LidarBC Discovery and Download' ",
      "by name(s) in brackets (DEM 1:20,000):",
      "\nhttps://governmentofbc.maps.arcgis.com/apps/MapSeries/index.html?appid=d06b37979b0c4709b7fcf2a1ed458e03",
      "\nIf not missing, report as a package error.")

    fetch <- dplyr::filter(fetch, .data$tif_good) # Only get the ones that exist

    if(nrow(fetch) == 0) {
      stop("No non-missing lidar tiles available, try TRIM data\n",
           "  `dem_region(region, type = 'trim')`\n\n",
           problems,
           call. = FALSE)
    } else {
      warning(problems, call. = FALSE)
    }
  }

  # Download tiles
  for(i in seq_len(nrow(fetch))) {
    if(!quiet) msg <- paste0("Fetching ", fetch$tile_name[i])
    if(only_new && file.exists(fetch$out_file[i])) {
      if(!quiet) message(paste0(msg, " - skipping (new_only = TRUE)"))
      next
    }
    message(msg)

    resp <- httr::GET(fetch$url[i], progress)
    writeBin(httr::content(resp, "raw"), fetch$out_file[i])
  }

  fetch
}


url_exists <- function(url) {
  identical(httr::status_code(httr::HEAD(url)), 200L)
}
