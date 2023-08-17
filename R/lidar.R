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
                        progress = httr::progress()) {

  if(is.null(out_dir)) {
    cache_check()
    out_dir <- file.path(cache_dir(), "tiles")
    if(!dir.exists(out_dir)) dir.create(out_dir)
    message("Saving tiles to cache directory: ", cache_dir())
  }

  if(!"sf" %in% class(region)) {
    stop("region must be an sf spatial object (see examples)", call. = FALSE)
  }

  if(only_new) local_tiles <- list.files(out_dir) else local_tiles <- vector()
  # Check for tifs on each call, checking all tiles takes too long
  message("Checking for matching tifs")

  region <- sf::st_transform(region, sf::st_crs(bcaquiferdata::tiles))

  fetch <- sf::st_filter(bcaquiferdata::tiles, region) %>%
    sf::st_drop_geometry() %>%
    create_url() %>%
    tidyr::nest(tiles = -"map_tile") %>%
    dplyr::mutate(tiles = purrr::map(
      .data$tiles, ~lidar_check_urls(.x, local_tiles, verbose))) %>%
    tidyr::unnest("tiles") %>%
    dplyr::group_by(.data$map_tile) %>%
    dplyr::mutate(n_good = sum(.data$tif_good, na.rm = TRUE)) %>%
    dplyr::arrange(dplyr::desc(.data$tif_good), dplyr::desc(.data$tile),
                   .by_group = TRUE) %>%
    dplyr::slice(1) %>%
    dplyr::mutate(out_file = file.path(.env$out_dir, .data$tile))

  # Warn if cannot find a tile
  if(any(!fetch$tif_good)) {
    problems <- stringr::str_remove(fetch$map_tile[!fetch$tif_good], "^0") %>%
      stringr::str_replace("(.*)(\\d{3})", "\\1.\\2") %>%
      paste0("\n- ", fetch$map_tile[!fetch$tif_good], " (", ., ")")
    warning("Could not find a lidar image for map tile(s):", problems,
            "\nConfirm lidar for tile is missing by seaching 'LidarBC Discovery and Download' ",
            "by name(s) in brackets:",
            "\nhttps://governmentofbc.maps.arcgis.com/apps/MapSeries/index.html?appid=d06b37979b0c4709b7fcf2a1ed458e03",
            "\nIf not missing, report as a package error.",
            call. = FALSE)
    fetch <- dplyr::filter(fetch, .data$tif_good) # Only get the ones that exist
  }

  for(i in seq_len(nrow(fetch))) {
    msg <- paste0("Fetching ", fetch$tile[i])
    if(only_new && file.exists(fetch$out_file[i])) {
      message(paste0(msg, " - skipping (new_only = TRUE)"))
      next
    }
    message(msg)

    resp <- httr::GET(fetch$url[i], progress)
    writeBin(httr::content(resp, "raw"), fetch$out_file[i])
  }

  fetch
}


#' Create url for tiles
#'
#' Based on the interal `tiles` data frame, create all the possible year/id
#' combinations and then return the most recent which can be found.
#'
#' `tiles` are created in data-raw/internal.R based
#'
#' @noRd
create_url <- function(tiles) {
  t <- tiles %>%
    dplyr::mutate(n = stringr::str_extract(.data$map_tile, "^[0-9]{3}"),
                  l = stringr::str_extract(.data$map_tile, "[[:alpha:]]"),
                  year = list(seq(lubridate::year(Sys.Date()), by = -1,
                                  length.out = 10))) %>%
    tidyr::unnest("year") %>%
    dplyr::mutate(
      tile = paste0("bc_", .data$map_tile, "_xli1m_utm", .data$utm,
                    "_", .data$year, ".tif"),
      url = file.path(.env$lidar_url, .data$n, paste0(.data$n, .data$l),
                      .data$year, "dem", .data$tile)) %>%
    dplyr::select(-"utm", -"n", -"l", -"year")

  t %>%
    dplyr::mutate(url = stringr::str_replace(.data$url, "xli1m", "xl1m"),
                  tile = stringr::str_replace(.data$tile, "xli1m", "xl1m")) %>%
    dplyr::bind_rows(t) %>%
    dplyr::arrange(dplyr::desc(.data$url))
}


url_exists <- function(url) {
  identical(httr::status_code(httr::HEAD(url)), 200L)
}

#' Check for urls
#'
#' Loops until finds one then breaks
#'
#' @noRd
lidar_check_urls <- function(x, local_tiles, verbose) {

  x$tif_good <- NA
  x$tif_good[x$tile %in% local_tiles] <- TRUE

  if(!any(x$tif_good, na.rm = TRUE)) {
    for(i in seq_len(nrow(x))) {
      if(verbose) message("  Checking for ", x$tile[i], "...")
      x$tif_good[i] <- url_exists(x$url[i])
      if(x$tif_good[i]) {
        if(verbose) message("    found online")
        break
      }
    }
  } else {
    t <- x$tile[x$tif_good][1]
    if(verbose) message("  Checking for ", t, "...\n    found locally")
  }
  x
}
