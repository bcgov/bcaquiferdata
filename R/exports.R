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

#' Export wells data for use in Strater and Voxler
#'
#' @param wells_sub Data frame. Output of `wells_elev()`
#' @param id Character. Id to prepend to all output files e.g., "id_lith.csv"
#' @param dir Character. Directory where files should be exported to. Defaults
#'   to working directory.
#' @param type Character. Format in which to export. One of "strater", "voxler",
#'   "archydro", "leapfrog", or "surfer" (case-insensitive).
#' @param preview Logical. Whether to preview the exports (`TRUE`, return a list
#'   of data frames) or to actually export the data (`FALSE`, write the
#'   necessary files to the `dir` folder, default).
#' @param zip Logical. Whether to export a zip archive of the files to `dir`.
#'
#' @return If `preview = FALSE`, a vector of file names (or if `zip = TRUE`, a
#' single filename of the zipped archive); if `preview = TRUE`, a list of data
#' frames.
#'
#' @export
#'
#' @examplesIf interactive()
#'
#' library(sf)
#'
#' # Load a shape file defining the region of interest
#' creek <- st_read("misc/data/Clinton_Creek.shp")
#'
#' # Get wells within this region
#' creek_wells <- wells_subset(creek)
#'
#' # Fetch Lidar DEM
#' creek_lidar <- dem_region(creek)
#'
#' # Collect wells in this region with added elevation from Lidar
#' creek_wells <- wells_elev(creek_wells, creek_lidar)
#'
#' # Preview data for Strater
#' p <- wells_export(creek_wells, id = "clinton", type = "strater", preview = TRUE)
#' names(p)
#' p[["strater_lith"]]
#' p[["strater_collars"]]
#' p[["strater_wells"]]
#'
#' # Export data for Strater
#' wells_export(creek_wells, id = "clinton", type = "strater", zip = TRUE)
#'
#' # Export data for Voxler
#' wells_export(creek_wells, id = "clinton", type = "voxler", zip = TRUE)
#'
#' # Export Arc Hydro
#' wells_export(creek_wells, id = "clinton", type = "archydro", zip = TRUE)
#'
#' # Export Surver
#' wells_export(creek_wells, id = "clinton", type = "surfer", zip = TRUE)
#'
#' wells_export(creek_wells, id = "clinton", type = "leapfrog", zip = TRUE)

wells_export <- function(
  wells_sub,
  id,
  type,
  dir = ".",
  zip = FALSE,
  preview = FALSE
) {
  # TODO: Checks
  # Check for elev and well tag number etc.
  if (!dir.exists(dir)) {
    stop(
      "`dir` (",
      dir,
      " doesn't not exist relative to current working directory\n(",
      getwd(),
      ")",
      call. = FALSE
    )
  }

  type <- tolower(type)

  if (!preview && missing(id)) {
    stop("Must provide `id` in order to export data", call. = FALSE)
  }

  opts <- c("strater", "voxler", "archydro", "leapfrog", "surfer")
  if (missing(type) || !type %in% opts) {
    stop(
      "`type` must be one of '",
      paste0(opts, collapse = "', '"),
      "'",
      call. = FALSE
    )
  }

  if (!missing(id)) {
    id <- stringr::str_replace_all(tolower(id), " ", "_")
  }

  wells_sub <- wells_sub %>%
    dplyr::bind_cols(as.data.frame(sf::st_coordinates(.))) %>%
    sf::st_drop_geometry()

  # Export with appropriate function
  get(paste0("export_", type))(wells_sub, id, dir, zip, preview)
}

export_strater <- function(wells_sub, id, dir, zip, preview) {
  # Strater Lithology
  f1 <- wells_sub %>%
    dplyr::select(
      "Hole_ID" = "well_tag_number",
      "From" = "lithology_from_m",
      "To" = "lithology_to_m",
      "Lithology_Keyword" = "lithology_category",
      "Lithology_Description" = "lithology_raw_combined"
    )

  # Strater Collars
  f2 <- wells_sub %>%
    dplyr::group_by(.data$well_tag_number, .data$X, .data$Y, .data$elev) %>%
    dplyr::summarize(
      Starting_Depth = min(.data$lithology_from_m),
      Ending_Depth = max(.data$lithology_to_m),
      .groups = "drop"
    ) %>%
    dplyr::select(
      "Hole_ID" = "well_tag_number",
      "Easting_Albers" = "X",
      "Northing_Albers" = "Y",
      "Starting_Depth",
      "Ending_Depth",
      "Elevation" = "elev"
    )

  f3 <- wells_sub %>%
    dplyr::select("well_tag_number", "water_depth_m")

  dfs <- exp_name_dfs(list(f1, f2, f3), "strater", c("lith", "collars", "wls"))

  if (preview) {
    r <- dfs
  } else {
    r <- exp_save("Strater", dfs, id, dir, zip)
  }

  r
}

export_voxler <- function(wells_sub, id, dir, zip, preview) {
  voxler <- wells_sub %>%
    dplyr::mutate(
      Water_Elevation = .data$elev - .data$water_depth_m,
      Component = 0
    ) %>%
    dplyr::filter(!is.na(.data$Water_Elevation)) %>%
    dplyr::select(
      "well_tag_number",
      "Easting_Albers" = "X",
      "Northing_Albers" = "Y",
      "Water_Elevation",
      "Component"
    ) %>%
    dplyr::distinct()

  f1 <- voxler %>%
    dplyr::mutate(
      Component = 2,
      Water_Elevation = .data$Water_Elevation + 1
    ) %>%
    dplyr::bind_rows(voxler)

  dfs <- exp_name_dfs(list(f1), "voxler")

  if (preview) {
    r <- dfs
  } else {
    r <- exp_save("Voxler", dfs, id, dir, zip)
  }

  r
}


export_archydro <- function(wells_sub, id, dir, zip, preview) {
  w <- wells_sub %>%
    dplyr::mutate(
      HydroID = .data$well_tag_number,
      HydroCode = paste0("w", .data$well_tag_number),
      LandElev = .data$elev,
      X = .data$X,
      Y = .data$Y,
      WellDepth = .data$well_depth_m,
      FromDepth = .data$lithology_from_m,
      ToDepth = .data$lithology_to_m,
      TopElev = .data$LandElev - .data$FromDepth,
      BottomElev = .data$LandElev - .data$ToDepth,
      Description = .data$lithology_category,
      HGUName = .data$lithology_category,
      OriginalLithology = .data$lithology_raw_combined
    )

  f1 <- dplyr::select(
    w,
    "HydroID",
    "HydroCode",
    "X",
    "Y",
    "LandElev",
    "WellDepth"
  ) |>
    dplyr::distinct()

  f2 <- w %>%
    dplyr::select("Description", "HGUName") %>%
    dplyr::distinct() %>%
    dplyr::mutate(HGUID = 1:dplyr::n(), HGUCode = .data$HGUID) %>%
    dplyr::relocate("HGUID", "HGUCode", .before = "Description")

  f3 <- w %>%
    dplyr::left_join(dplyr::select(f2, "HGUName", "HGUID"), by = "HGUName") %>%
    dplyr::select(
      "WellID" = "HydroID",
      "WellCode" = "HydroCode",
      "Material" = "HGUName",
      "HGUID",
      "RefElev" = "LandElev",
      "FromDepth",
      "ToDepth",
      "TopElev",
      "BottomElev",
      "OriginalLithology"
    )

  dfs <- exp_name_dfs(
    list(f1, f2, f3),
    "archydro",
    names = c("well", "hguid", "bh")
  )

  if (preview) {
    r <- dfs
  } else {
    r <- exp_save("ArcHydro", dfs, id, dir, zip)
  }

  r
}

export_leapfrog <- function(wells_sub, id, dir, zip, preview) {
  # Check for un-fixed problems
  wells_sub <- wells_sub %>%
    fix_bottom_intervals() %>%
    fix_depth_missing() %>%
    fix_depth_mismatch()

  # Collars File
  f1 <- wells_sub %>%
    dplyr::select(
      "Hole ID" = "well_tag_number",
      "East (X)" = "X",
      "North (Y)" = "Y",
      "Elev (Z)" = "elev",
      "Max Depth (m)" = "well_depth_m",
      # Extra fields
      "Water Depth (m)" = "water_depth_m",
      "Well Yield" = "well_yield_usgpm",
      "Artesian Conditions" = "artesian_conditions",
      "Artesian Pressure (Head Ft AGL)" = "artesian_pressure_head_ft_agl",
      "Aquifer ID" = "aquifer_id"
    ) %>%
    dplyr::distinct()

  # Intervals File
  f2 <- wells_sub %>%
    dplyr::select(
      "Hole ID" = "well_tag_number",
      "From" = "lithology_from_m",
      "To" = "lithology_to_m",
      "Lithology" = "lithology_category",
      "Lithology Raw" = "lithology_raw_combined"
    ) %>%
    dplyr::distinct()

  dfs <- exp_name_dfs(list(f1, f2), "leapfrog", c("collars", "intervals"))

  if (preview) {
    r <- dfs
  } else {
    r <- exp_save("Leapfrog", dfs, id, dir, zip)
  }

  r
}

export_surfer <- function(wells_sub, id, dir, zip, preview) {
  f1 <- wells_sub %>%
    dplyr::select(
      "well_tag_number",
      "X",
      "Y",
      "bedrock_depth_m",
      "water_depth_m"
    ) %>%
    dplyr::distinct()

  dfs <- exp_name_dfs(list(f1), "surfer")

  if (preview) {
    r <- dfs
  } else {
    r <- exp_save("Surfer", dfs, id, dir, zip)
  }

  r
}

exp_name_dfs <- function(l, type, names = NULL) {
  nm <- type
  if (!is.null(names)) {
    nm <- paste0(nm, "_", names)
  }
  stats::setNames(l, nm)
}

exp_save <- function(type, dfs, id, dir, zip) {
  f <- paste0(id, "_", names(dfs))

  fzip <- stringr::str_replace(
    f[1],
    paste0("(?<=", tolower(type), ").+"),
    ".zip"
  )
  f <- paste0(f, ".csv")

  if (!zip || length(dfs) == 1) {
    f <- file.path(dir, f)
  } else {
    f <- file.path(tempdir(), f)
  }
  fzip <- file.path(dir, fzip)

  message("Writing ", type, " file(s) ", paste0(f, collapse = ", "))
  for (i in seq_along(dfs)) {
    readr::write_csv(dfs[[i]], f[i])
  }

  if (zip) {
    # Override for Shiny Downloads - write to where Shiny wants it
    shiny_dl <- Sys.getenv("bcaquiferdata_shiny_export_path")
    if (shiny_dl != "") {
      fzip <- shiny_dl
    }

    if (length(f) == 1) {
      message("Skipping zip for single ", type, " file")
      if (shiny_dl != "") {
        # Write the single csv to temp file to accessible by Shiny downloads
        readr::write_csv(dfs[[1]], fzip)
        return(fzip)
      } else {
        return(f)
      }
    }

    message("Zipping files...")
    zip(fzip, files = f)
    unlink(f) # Should only be in temp folder
    return(fzip)
  }

  f
}
