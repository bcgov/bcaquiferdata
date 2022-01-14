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


#' Fetch LiDAR tiles corresponding to a sf object
#'
#' @param region sf object
#' @param out_dir Character. Output folder, defaults to working directory
#' @param only_new Logical. Only download new, missing LiDAR tiles, default to
#'   TRUE
#' @param verbose Logical. Show extra output. Can be useful to check on
#'   progress.
#'
#' @return Data frame describing tiles and location on disk
#' @export
#'
#' @examplesIf interactive()
#'
#' creek <- sf::st_read("misc/data/Clinton_Creek.shp")
#' lidar_fetch(creek)
#'
lidar_fetch <- function(region, out_dir = NULL, only_new = TRUE, verbose = FALSE) {

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
  fetch <- sf::st_filter(tiles, region) %>%
    sf::st_drop_geometry() %>%
    dplyr::mutate(locs = purrr::map(locs, lidar_check_urls, local_tiles, verbose)) %>%
    tidyr::unnest(locs) %>%
    dplyr::group_by(map_tile) %>%
    dplyr::mutate(n_good = sum(tif_good, na.rm = TRUE)) %>%
    dplyr::arrange(dplyr::desc(tif_good), dplyr::desc(tile), .by_group = TRUE) %>%
    dplyr::slice(1) %>%
    dplyr::mutate(out_file = file.path(out_dir, tile))

  for(i in seq_len(nrow(fetch))) {
    msg <- paste0("Fetching ", fetch$tile[i])
    if(only_new && file.exists(fetch$out_file[i])) {
      message(paste0(msg, " - skipping (new_only = TRUE)"))
      next
    }
    message(msg)
    resp <- httr::GET(fetch$url[i], httr::progress())
    writeBin(content(resp, "raw"), fetch$out_file[i])
  }

  fetch
}

url_exists <- function(url) {
  identical(httr::status_code(httr::HEAD(url)), 200L)
}


lidar_check_urls <- function(x, local_tiles, verbose) {

  if(verbose) message("  Checking for ", x$tile[i], "...")

  x$tif_good <- NA
  x$tif_good[x$tile %in% local_tiles] <- TRUE

  if(all(is.na(x$tif_good)) || !any(x$tif_good)) {
    for(i in seq_len(nrow(x))) {
      x$tif_good[i] <- url_exists(x$url[i])
      if(x$tif_good[i]) {
        if(verbose) message("    found online")
        break
      }
    }
  } else {
    if(verbose) message("    found locally")
  }
  x
}
