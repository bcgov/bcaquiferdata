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

#' Download, Update, and/or load data
#'
#' This function downloads, updates or loads locally stored data. Currently this
#' function returns `wells`, `wells_sf`, or `lithology` data. Note that these
#' data are originally from GWELLS, but are cleaned and summarized for use in
#' the bcaquiferdata package. For example `wells_sf` is a spatial version of the
#' data, and `lithology` is a cleaned and standardized version of lithology.
#' `wells` also contains the new standardized `lithology` data, along with the
#' original lithology observations and intermediate classification steps to
#' simplify error tracing.
#'
#' Under normal circumstances, users will not need to use this function as it is
#' used internally by the main workflow functions. However, users may wish to
#' overview entire datasets.
#'
#' Bear in mind that the lithology cleaning and
#' standardizing, while better than the original data, will almost certainly
#' still have errors!
#'
#' @param type Character. Type of data to return, one of `wells`, `wells_sf`, or
#'   `lithology`
#' @param update Logical. Force update of the data?
#'
#' @return
#' @export
#'
#' @examples
#'
#' \dontrun{
#' wells <- data_read("wells")
#' }
data_read <- function(type, update = FALSE, permission = FALSE) {
  t <- c("lithology", "wells", "wells_sf", "wells_lith", "wells_lith_sf")
  if(!type %in% t) {
    stop(
      "`type` must be one of ",
      paste0(t, collapse = ", "), call. = FALSE)
  }
  cache_check(permission)

  f <- file.path(cache_dir(), paste0(type, "_nice.rds"))

  if(update || !file.exists(f)) data_update(which = "all")

  readr::read_rds(f)
}


#' @export

data_update <- function(type = "all", download = TRUE, permission = FALSE) {

  if(!type %in% c("all", "wells", "lithology")) {
    stop("`type` must be one of ",
         paste0(c("all", "wells", "lithology"), collapse = ", "),
         call. = FALSE)
  }

  cache_check(permission)

  meta <- cache_meta()


  # Download the data
  if(download && type %in% c("all", "wells", "lith")) {
    message("Downloading GWELLS data")
    fetch_gwells()
    meta$GWELLS_downloaded <- as.character(Sys.time())
  }

  # Clean and Save wells
  if(type %in% c("all", "wells")) {
    message("Wells - Cleaning")
    wells <- clean_wells()
    wells_sf <- sf::st_as_sf(wells,
                             coords = c("longitude_decdeg", "latitude_decdeg"),
                             crs = 4326)

    message("Wells - Saving data to cache")
    readr::write_rds(wells_sf, file.path(cache_dir(), "wells_sf_nice.rds"))
    readr::write_rds(wells, file.path(cache_dir(), "wells_nice.rds"))
    meta$wells_processed <- as.character(Sys.time())
  }

  # Clean and Standardize lithology
  if(type %in% c("all", "lithology")) {
    lith <- clean_lithology()
    meta$lith_processed <- as.character(Sys.time())
  }

  # Save updated metadata
  readr::write_csv(meta, file.path(cache_dir(), "meta.csv"))

  # message("Wells with Lithology - Saving data to cache")
  # if(!exists("wells")) wells <- data_read("wells")
  # if(!exists("lith")) lith <- data_read("lithology")
  #
  # wells_lith <- dplyr::left_join(
  #   wells, lith,
  #   by = c("well_tag_number", "well_yield_unit_code"))
  #
  # wells_lith_sf <- sf::st_as_sf(wells_lith,
  #                               coords = c("longitude_decdeg", "latitude_decdeg"),
  #                               crs = 4326)
  #
  # readr::write_rds(wells_lith_sf, file.path(cache_dir(), "wells_lith_sf_nice.rds"))
  # readr::write_rds(wells_lith, file.path(cache_dir(), "wells_lith_nice.rds"))

}

fetch_gwells <- function() {
  "https://s3.ca-central-1.amazonaws.com/gwells-export/export/v2/gwells.zip" %>%
    httr::GET(httr::write_disk(file.path(cache_dir(), "GWELLS", "gwells.zip"),
                               overwrite = TRUE),
              httr::progress())
  utils::unzip(file.path(cache_dir(), "GWELLS", "gwells.zip"),
               exdir = file.path(cache_dir(), "GWELLS"),
               files = c("well.csv", "lithology.csv"), overwrite = TRUE)
  unlink(file.path(cache_dir(), "GWELLS", "gwells.zip"))
}

clean_wells <- function(file = "GWELLS/well.csv") {
  readr::read_csv(file.path(cache_dir(), file),
                  guess_max = Inf, show_col_types = FALSE) %>%
    janitor::clean_names() %>%
    dplyr::filter(!is.na(.data$latitude_decdeg),
                  !is.na(.data$longitude_decdeg)) %>%
    dplyr::mutate(water_depth_m = .data$static_water_level_ft_btoc * 0.3048,
                  well_depth_m = .data$finished_well_depth_ft_bgl * 0.3048) %>%
    dplyr::select(dplyr::all_of(fields_wells))
}


clean_lithology <- function(file = "GWELLS/lithology.csv") {

  message("Lithology - Cleaning")
  l_prep <- lith_prep(file)

  message("Lithology - Standardizing")
  l_std <- lith_fix(file)

  #l_std <- lith_yield(l_std)

  l <- dplyr::left_join(l_prep, l_std, by = "lithology_raw_data")

  message("Lithology - Saving data to cache")
  readr::write_rds(l, file.path(cache_dir(), "lithology_nice.rds"))

  l
}

data_ready <- function() {
  meta <- cache_meta()
  m <- meta$wells_processed != "" & meta$lith_processed != ""
  f <- file.exists(file.path(cache_dir(),
                             c("wells_nice.rds", "lithology_nice.rds")))
  all(m & f)
}


cache_check <- function(permission = FALSE) {
  # Ask for permission to save data
  if(!dir.exists(cache_dir())) {

    if(!permission) {
      permission <- utils::askYesNo(
        paste0("bcaquiferdata would like to store data ",
               "in: \n", cache_dir(), "\nIs that okay? ",
               "(You can always use cache_clean() to remove it)"))
    }

    if(!permission) {
      stop("Can't store data. Stopping.", call. = FALSE)
    } else {
      dir.create(file.path(cache_dir(), "GWELLS"), recursive = TRUE)
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
  if(dir.exists(cache_dir())) {
    message("Removing cache directory: ", cache_dir(), appendLF = FALSE)
    unlink(cache_dir(), recursive = TRUE)
    if(length(list.files(cache_dir())) == 0) {
      message("... Successful")
    } else message("... Unsuccessful")
  } else {
    message("No cache directory to remove")
  }
}

cache_meta <- function() {

  if(file.exists(f <- file.path(cache_dir(), "meta.csv"))) {
    m <- read.csv(f)
  } else {
    m <- data.frame(
      bcaquiferdata_version = as.character(packageVersion("bcaquiferdata")),
      GWELLS_downloaded = "No",
      wells_processed = "No",
      lith_processed = "No")
  }
  m
}
