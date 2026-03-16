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

data_types <- function() {
  c("lithology", "wells", "wells_testing", "wells_sf", "aquifers")
}


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
#'
#' @inheritParams common_docs
#'
#' @return Data frame or spatial features object of the requested data.
#' @export
#'
#' @examplesIf interactive()
#' wells <- data_read("wells")

data_read <- function(type, update = FALSE, permission = FALSE) {
  if (!type %in% data_types()) {
    stop(
      "`type` must be one of ",
      paste0(data_types(), collapse = ", "),
      call. = FALSE
    )
  }
  cache_check(permission)

  f <- file.path(cache_dir(), paste0(type, "_nice.rds"))

  if (update || !file.exists(f) || !data_ready()) {
    data_update(type)
  }

  readr::read_rds(f)
}

#' Update cached data
#'
#' Update the GWELLs data stored locally.
#'
#' @param type Character. Type of data to update. One of "all", "wells",
#'   "lithology"
#' @param download Logical. Whether to re-download and process the data
#'   (`TRUE`), or just re-process it (`FALSE`).
#'
#' @inheritParams common_docs
#'
#' @export
#'
#' @examplesIf interactive()
#' data_update(type = "wells")
#' data_update(type = "lithology")

data_update <- function(
  type = c("wells", "lithology"),
  download = TRUE,
  permission = FALSE
) {
  opts <- c("all", data_types())
  if (!any(type %in% opts)) {
    stop("`type` must be one of ", paste0(opts, collapse = ", "), call. = FALSE)
  }

  cache_check(permission)

  meta <- cache_meta()
  if (
    meta$bcaquiferdata_version != packageVersion("bcaquiferdata") &&
      type[1] != "all"
  ) {
    message(
      "Cache data was processed with a different version of bcaquiferdata",
      "must update all data..."
    )
    type <- "all"
  }

  # Download data
  if (download) {
    # GWELLS
    if (any(type %in% c("all", "wells", "lithology"))) {
      message("Downloading GWELLS data")
      fetch_gwells()
      meta$GWELLS_downloaded <- as.character(Sys.time())
    }
    # Aquifers
    if (any(type %in% c("all", "aquifers"))) {
      message("Downloading Aquifers")

      message("  Standard data")
      download.file(
        "https://apps.nrs.gov.bc.ca/gwells/api/v1/aquifers/csv",
        file.path(cache_dir(), "aquifers.csv")
      )

      meta$aquifers_downloaded <- as.character(Sys.time())

      message("  Spatial data (Note: this may take several minutes)")
      #bcdata::bcdc_tidy_resources("099d69c5-1401-484d-9e19-c121ccb7977c")
      #d <- bcdata::bcdc_get_data(record = "099d69c5-1401-484d-9e19-c121ccb7977c",
      #                           resource = "8f421e3a-ccd3-4fab-8198-53ad6e9e2af2")
      d <- bcdata::bcdc_query_geodata("099d69c5-1401-484d-9e19-c121ccb7977c")
      message("  Collecting spatial data...")
      d <- dplyr::collect(d)
      sf::write_sf(d, file.path(cache_dir(), "aquifers_spatial.gpkg"))
      message("  Done!")
    }
  }

  # Clean and Save Aquifers
  if (any(type %in% c("all", "aquifers"))) {
    message("Aquifers - Cleaning")
    clean_aquifers()
    meta$aquifers_processed <- as.character(Sys.time())
  }

  # Clean and Save wells
  if (any(type %in% c("all", "wells"))) {
    message("Wells - Cleaning")
    wells <- clean_wells()
    wells_testing <- clean_wells_testing()
    meta$wells_processed <- as.character(Sys.time())
  }

  # Clean and Standardize lithology
  if (any(type %in% c("all", "lithology"))) {
    lith <- clean_lithology()
    meta$lith_processed <- as.character(Sys.time())
  }

  # Update package version
  meta$bcaquiferdata_version <- as.character(utils::packageVersion(
    "bcaquiferdata"
  ))

  # Save updated metadata
  readr::write_csv(meta, file.path(cache_dir(), "meta.csv"), progress = FALSE)
}

fetch_gwells <- function() {
  "https://s3.ca-central-1.amazonaws.com/gwells-export/export/v2/gwells.zip" %>%
    httr::GET(
      httr::write_disk(
        file.path(cache_dir(), "GWELLS", "gwells.zip"),
        overwrite = TRUE
      ),
      httr::progress()
    )
  utils::unzip(
    file.path(cache_dir(), "GWELLS", "gwells.zip"),
    exdir = file.path(cache_dir(), "GWELLS"),
    files = c("well.csv", "lithology.csv", "pt_aquifer_parameters.csv"),
    overwrite = TRUE
  )
  #unlink(file.path(cache_dir(), "GWELLS", "gwells.zip"))
}

fetch_aquifers <- function() {}

clean_wells <- function(file = NULL) {
  if (is.null(file)) {
    file <- file.path(cache_dir(), "GWELLS/well.csv")
  }

  wells <- readr::read_csv(
    file,
    guess_max = Inf,
    show_col_types = FALSE,
    progress = FALSE
  ) %>%
    janitor::clean_names() %>%
    dplyr::filter(
      !is.na(.data$latitude_decdeg),
      !is.na(.data$longitude_decdeg)
    ) %>%
    # Convert to metric
    convert_m(
      cols = c(
        "well_depth_m" = "finished_well_depth_ft_bgl",
        "water_depth_m" = "static_water_level_ft_btoc"
      ),
      digits = 1
    ) %>%
    dplyr::select(dplyr::all_of(fields_wells))

  wells_sf <- sf::st_as_sf(
    wells,
    coords = c("longitude_decdeg", "latitude_decdeg"),
    crs = 4326
  )

  message("Wells - Saving data to cache")
  readr::write_rds(wells_sf, file.path(cache_dir(), "wells_sf_nice.rds"))
  readr::write_rds(wells, file.path(cache_dir(), "wells_nice.rds"))
}

clean_wells_testing <- function(file = NULL) {
  if (is.null(file)) {
    file <- file.path(cache_dir(), "GWELLS/pt_aquifer_parameters.csv")
  }

  testing <- readr::read_csv(
    file,
    guess_max = Inf,
    show_col_types = FALSE,
    progress = FALSE
  ) %>%
    janitor::clean_names()

  message("Wells Testing - Saving data to cache")
  readr::write_rds(testing, file.path(cache_dir(), "wells_testing_nice.rds"))
}

clean_lithology <- function(file = NULL) {
  if (is.null(file)) {
    file <- file.path(cache_dir(), "GWELLS/lithology.csv")
  }

  message("Lithology - Cleaning")
  l_prep <- lith_prep(file)

  message("Lithology - Standardizing")
  l_std <- lith_fix(l_prep$lithology_raw_combined)

  #l_std <- lith_yield(l_std)

  l <- dplyr::left_join(l_prep, l_std, by = "lithology_raw_combined")
  message("Lithology - Calculating depth to bedrock")
  l <- lith_bedrock(l)

  message("Lithology - Saving data to cache")
  readr::write_rds(l, file.path(cache_dir(), "lithology_nice.rds"))

  l
}

clean_aquifers <- function(files = NULL) {
  if (is.null(files)) {
    file <- c(
      file.path(cache_dir(), "aquifers.csv"),
      file.path(cache_dir(), "aquifers_spatial.gpkg")
    )
  }

  aq <- readr::read_csv(
    file[1],
    guess_max = Inf,
    show_col_types = FALSE,
    progress = FALSE
  ) %>%
    janitor::clean_names()

  sf::st_read(file[2], quiet = TRUE) %>%
    janitor::clean_names() %>%
    dplyr::select("aquifer_id") |>
    dplyr::mutate(aquifer_id = as.numeric(aquifer_id)) %>%
    dplyr::left_join(aq, by = "aquifer_id") |>
    readr::write_rds(file.path(cache_dir(), "aquifers_nice.rds"))
}

data_ready <- function() {
  meta <- cache_meta()
  m <- as.character(meta$wells_processed) != "" &
    as.character(meta$lith_processed) != ""
  v <- meta$bcaquiferdata_version == utils::packageVersion("bcaquiferdata")
  f <- file.exists(file.path(
    cache_dir(),
    c("wells_nice.rds", "lithology_nice.rds")
  ))

  if (!v) {
    message(
      "Your version of the data was cleaned using a different ",
      "version of `bcaquiferdata`.\nUpdating data..."
    )
  }

  all(m & f & v)
}


cache_check <- function(permission = FALSE) {
  # Ask for permission to save data
  if (!dir.exists(cache_dir())) {
    if (!permission) {
      permission <- utils::askYesNo(
        paste0(
          "bcaquiferdata would like to store data ",
          "in: \n",
          cache_dir(),
          "\nIs that okay? ",
          "(You can always use cache_clean() to remove it)"
        )
      )
    }

    if (!permission) {
      stop("Can't store data. Stopping.", call. = FALSE)
    } else {
      message("Creating cache directory: ", cache_dir())
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
#' @param bcmaps_cded Logical. Whether or not to also remove CDED files cached with
#'   the bcmaps package. These are used by bcaquifertools for acquiring TRIM
#'   data, but may also be cached for use by other workflows.
#'
#' @examples
#'
#' # cache_clean()
#' # cache_clean(bcmaps_cded = TRUE)
#'
#' @export

cache_clean <- function(bcmaps_cded = FALSE) {
  if (dir.exists(cache_dir())) {
    message("Removing cache directory: ", cache_dir(), appendLF = FALSE)
    unlink(cache_dir(), recursive = TRUE)
    if (length(list.files(cache_dir())) == 0) {
      message("... Successful")
    } else {
      message("... Unsuccessful")
    }
  } else {
    message("No bcaquiferdata cache directory to remove")
  }

  if (bcmaps_cded) {
    f <- list.files(file.path(bcmaps:::data_dir(), "cded"), recursive = TRUE)
    if (length(f) > 0) {
      message(
        "Removing bcmaps cache files related to CDED: \n",
        paste0(paste0(" - ", f), sep = "\n")
      )
      unlink(file.path(bcmaps:::data_dir(), "cded"), recursive = TRUE)
    } else {
      message("No bcmaps CDED cache directory to remove")
    }
  }
}

cache_meta <- function() {
  f <- file.path(cache_dir(), "meta.csv")
  if (file.exists(f)) {
    m <- readr::read_csv(f, show_col_types = FALSE, progress = FALSE) %>%
      dplyr::mutate(dplyr::across(
        dplyr::where(lubridate::is.POSIXct),
        ~ round(.x, units = "secs")
      ))
  } else {
    m <- data.frame(
      bcaquiferdata_version = as.character(utils::packageVersion(
        "bcaquiferdata"
      )),
      GWELLS_downloaded = "No",
      wells_processed = "No",
      lith_processed = "No"
    )
  }
  m
}
