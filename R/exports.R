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
#'   "archydro" (case-insensitive).
#' @param preview Logical. Whether to preview the exports (`TRUE`, return a list
#'   of data frames) or to actually export the data (`FALSE`, write the
#'   necessary files to the `dir` folder.
#'
#' @return If `preview = FALSE`, a vector of file names, if `preview = TRUE`,
#'   a list of data frames.
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
#' wells_export(creek_wells, id = "clinton", type = "strater")
#'
#' # Export Arc Hydro
#' wells_export(creek_wells, id = "clinton", type = "archydro")


wells_export <- function(wells_sub, id, type, dir = ".", preview = FALSE) {

  # TODO: Checks
  # Check for elev and well tag number etc.
  if(!dir.exists(dir)) {
    stop("`dir` (", dir,
         " doesn't not exist relative to current working directory\n(",
         getwd(), ")", call. = FALSE)
  }

  type <- tolower(type)

  if(!preview && missing(id)) {
    stop("Must provide `id` in order to export data", call. = FALSE)
  }

  if(missing(type) || !type %in% c("strater", "voxler", "archydro")) {
    stop("`type` must be one of 'strater', 'voxler', or 'archydro'",
         call. = FALSE)
  }

  if(!missing(id)) id <- stringr::str_replace_all(tolower(id), " ", "_")

  wells_sub <- wells_sub %>%
    dplyr::bind_cols(as.data.frame(sf::st_coordinates(.))) %>%
    sf::st_drop_geometry()

  if(type == "strater") r <- export_strater(wells_sub, id, dir, preview)
  if(type == "voxler") r <- export_voxler(wells_sub, id, dir, preview)
  if(type == "archydro") r <- export_archydro(wells_sub, id, dir, preview)

  r
}

export_strater <- function(wells_sub, id, dir, preview) {

  if(!preview) {
    f <- file.path(dir, paste0(id, c("_lith.csv", "_collars.csv", "_wls.csv")))
  }

  # Strater Lithology
  f1 <- wells_sub %>%
    dplyr::select("Hole_ID" = "well_tag_number",
                  "From" = "lithology_from_m",
                  "To" = "lithology_to_m",
                  "Lithology_Keyword" = "lithology_category",
                  "Lithology_Description" = "lithology_raw_data")

  # Strater Collars
  f2 <- wells_sub %>%
    dplyr::group_by(.data$well_tag_number, .data$X, .data$Y, .data$elev) %>%
    dplyr::summarize(Starting_Depth = min(.data$lithology_from_m),
                     Ending_Depth = max(.data$lithology_to_m),
                     .groups = "drop") %>%
    dplyr::select("Hole_ID" = "well_tag_number",
                  "Easting_Albers" = "X",
                  "Northing_Albers" = "Y",
                  "Starting_Depth", "Ending_Depth",
                  "Elevation" = "elev")

  f3 <- wells_sub %>%
    dplyr::select("well_tag_number", "water_depth_m")

  if(preview) {
    r <- list("strater_lith" = f1,
              "strater_collars" = f2,
              "strater_wells" = f3)
  } else {
    message("Writing Strater files ", paste0(f, collapse = ", "))
    readr::write_csv(f1, f[1])
    readr::write_csv(f2, f[2])
    readr::write_csv(f3, f[3])
    r <- f
  }

  r
}

export_voxler <- function(wells_sub, id, dir, preview) {

  if(!preview) f <- file.path(dir, paste0(id, "_vox"))

  voxler <- wells_sub %>%
    dplyr::mutate(Water_Elevation = .data$elev - .data$water_depth_m,
                  Component = 0) %>%
    dplyr::filter(!is.na(.data$Water_Elevation)) %>%
    dplyr::select("well_tag_number",
                  "Easting_Albers" = "X", "Northing_Albers" = "Y",
                  "Water_Elevation", "Component") %>%
    dplyr::distinct()

  f1 <- voxler %>%
    dplyr::mutate(Component = 2, Water_Elevation = .data$Water_Elevation + 1) %>%
    dplyr::bind_rows(voxler)

  if(preview) {
    r <- list("voxler" = f1)
  } else {
    message("Writing Voxler file ", f)
    readr::write_csv(f1, f)
    r <- f
  }
  r
}


export_archydro <- function(wells_sub, id, dir, preview) {

  if(!preview) {
    f <- file.path(
      dir,
      paste0(id, c("_arc_well.csv", "_arc_hguid.csv", "_arc_bh.csv")))
  }

  w <- wells_sub %>%
    dplyr::mutate(
      HydroID = .data$well_tag_number,
      HydroCode = paste0("w", .data$well_tag_number),
      LandElev = .data$elev,
      X = .data$utm_easting,
      Y = .data$utm_northing,
      WellDepth = .data$finished_well_depth_ft_bgl,
      FromDepth = .data$lithology_from_ft_bgl,
      ToDepth = .data$lithology_to_ft_bgl,
      TopElev = .data$LandElev - .data$FromDepth,
      BottomElev = .data$LandElev - .data$ToDepth,
      Description = .data$lithology_category,
      HGUName = .data$lithology_category,
      OriginalLithology = .data$lithology_clean)

  f1 <- dplyr::select(w,
                      "HydroID", "HydroCode",
                      "X", "Y",
                      "LandElev", "WellDepth")

  f2 <- w %>%
    dplyr::select("Description", "HGUName") %>%
    dplyr::distinct() %>%
    dplyr::mutate(HGUID = 1:dplyr::n(),
                  HGUCode = .data$HGUID) %>%
    dplyr::relocate("HGUID", "HGUCode", .before = "Description")

  f3 <- w %>%
    dplyr::left_join(dplyr::select(f2, "HGUName", "HGUID"), by = "HGUName") %>%
    dplyr::select("WellID" = "HydroID",
                  "WellCode" = "HydroCode",
                  "Material" = "HGUName",
                  "HGUID",
                  "RefElev" = "LandElev",
                  "FromDepth", "ToDepth",
                  "TopElev", "BottomElev", "OriginalLithology")

  if(preview) {
    r <- list("archydro_well" = f1,
              "archydro_hguid" = f2,
              "archydro_bh" = f3)
  } else {
    message("Writing ArcHydro files ", paste0(f, collapse = ", "))
    readr::write_csv(f1, f[1])
    readr::write_csv(f2, f[2])
    readr::write_csv(f3, f[3])
    r <- f
  }

  r
}


