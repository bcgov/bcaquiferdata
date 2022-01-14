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

data_read <- function(type, update = FALSE) {
  cache_check()

  f <- file.path(cache_dir(), paste0(type, "_nice.rds"))

  if(update || !file.exists(f)) {
    data_update(type = dplyr::case_when(
      type %in% c("wells", "wells_sf", "lithology") ~ "gwells"))
  }

  readr::read_rds(f)
}

data_update <- function(type = "gwells") {

  cache_check()

  if(type == "gwells") {

    # Download the data
    message("Downloading GWELLS data")
    fetch_gwells()

    # Clean and Save wells
    message("Wells - Cleaning")
    wells <- clean_wells()

    # Clean and Save lithology
    message("Lithology")
    message("  - Cleaning")
    lith <- clean_lithology(wells)
    message("  - Standardizing")
    lith <- fix_lithology(lith)      # Standardize lithology

    wells <- dplyr::left_join(wells, lith,
                              by = c("well_tag_number", "well_depth_m"))

    wells_sf <- sf::st_as_sf(wells,
                             coords = c("longitude_decdeg", "latitude_decdeg"),
                             crs = 4326)

    # Saving files
    message("Saving data to cache")
    readr::write_rds(wells_sf, file.path(cache_dir(), "wells_sf_nice.rds"))
    readr::write_rds(wells, file.path(cache_dir(), "wells_nice.rds"))

  }
}

fetch_gwells <- function() {
  "https://s3.ca-central-1.amazonaws.com/gwells-export/export/v2/gwells.zip" %>%
    httr::GET(httr::write_disk(file.path(cache_dir(), "gwells.zip"),
                               overwrite = TRUE),
              httr::progress())
  unzip(file.path(cache_dir(), "gwells.zip"), exdir = cache_dir(),
        files = c("well.csv", "lithology.csv"), overwrite = TRUE)
  unlink(file.path(cache_dir(), "gwells.zip"))
}

clean_wells <- function(file = "well.csv") {
  readr::read_csv(file.path(cache_dir(), file),
                  guess_max = Inf, show_col_types = FALSE) %>%
    janitor::clean_names() %>%
    dplyr::filter(!is.na(.data$latitude_decdeg),
                  !is.na(.data$longitude_decdeg)) %>%
    dplyr::mutate(water_depth_m = static_water_level_ft_btoc * 0.3048,
                  well_depth_m = finished_well_depth_ft_bgl * 0.3048)
}

clean_lithology <- function(wells, file = "lithology.csv") {
  readr::read_csv(file.path(cache_dir(), file),
                  guess_max = Inf, show_col_types = FALSE) %>%
    janitor::clean_names() %>%
    # Convert to metric
    dplyr::mutate(
      lithology_from_m = round(.data$lithology_from_ft_bgl * 0.3048, 2),
      lithology_to_m = round(.data$lithology_to_ft_bgl * 0.3048, 2),
      lithology_raw_data = stringr::str_to_lower(lithology_raw_data)) %>%
    dplyr::select("well_tag_number", "lithology_from_m",
                  "lithology_to_m", "lithology_raw_data") %>%
    dplyr::left_join(
      dplyr::select(wells, "well_tag_number", "well_depth_m"),
      by = "well_tag_number") %>%
    dplyr::group_by(.data$well_tag_number) %>%

    # Fix lithology where only 1 entry
    dplyr::mutate(lithology_to_m =
                    dplyr::if_else(dplyr::n() == 1 & .data$lithology_to_m == 0,
                                   .data$well_depth_m,
                                   .data$lithology_to_m)) %>%

    # Fix overflow lithology (zero to zero)
    dplyr::mutate(zerozero = lithology_from_m == 0 & lithology_to_m == 0) %>%
    dplyr::arrange(well_tag_number, zerozero, lithology_from_m) %>%
    dplyr::mutate(
      lithology_raw_data = dplyr::if_else(
        dplyr::lead(.data$zerozero, default = FALSE),
        paste(.data$lithology_raw_data, dplyr::lead(.data$lithology_raw_data)),
        .data$lithology_raw_data)) %>%
    dplyr::filter(!zerozero) %>%

    # Fix incorrect lithology_to_m
    dplyr::mutate(
      lithology_to_m = dplyr::if_else(
        # SHOULD THIS BE == TO SAME lithology_from_m? I.e. not lead? (what about the last, really + 0.01?
        .data$lithology_to_m == 0,
        dplyr::lead(.data$lithology_from_m),
        .data$lithology_to_m)) %>%
    dplyr::mutate(lithology_to_m = dplyr::if_else(
      .data$lithology_to_m == max(.data$lithology_from_m),
      .data$lithology_to_m + 0.01,
      .data$lithology_to_m)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-"zerozero")
}

cache_check <- function() {
  # Ask for permission to save data
  if(!dir.exists(cache_dir())) {
    cont <- utils::askYesNo(
      paste0("bcaquiferdata would like to store data for the reports ",
             "in: \n", cache_dir(), "\nIs that okay? ",
             "(You can always use cache_clean() to remove it)"))

    if(!cont) {
      stop("Can't store data. Stopping.", call. = FALSE)
    } else {
      dir.create(file.path(cache_dir()), recursive = TRUE)
    }
  }
}

cache_dir <- function() {
  rappdirs::user_data_dir("bcaquiferdata")
}

#' Clean cache
#'
#' Removes data cache
#'
#' @examples
#'
#' # cache_clean()
#'
#' @export

cache_clean <- function() {
  unlink(cache_dir(), recursive = TRUE)
}
