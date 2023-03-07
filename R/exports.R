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

wells_export <- function(wells_sub, id, type, dir = ".") {

  wells_sub <- wells_sub %>%
    dplyr::bind_cols(as.data.frame(sf::st_coordinates(.))) %>%
    sf::st_drop_geometry()

  if(type == "strater") export_strater(wells_sub, id, dir)
  if(type == "voxler") export_voxler(wells_sub, id, dir)
  if(type == "archydro") export_archydro(wells_sub, id, dir)
}

export_strater <- function(wells_sub, id, dir) {

  # Strater Lithology
  wells_sub %>%
    dplyr::select("Hole_ID" = "well_tag_number",
                  "From" = "lithology_from_m",
                  "To" = "lithology_to_m",
                  "Lithology_Keyword" = "lithology_category",
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
}

export_voxler <- function(wells_sub, id, dir) {

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


export_archydro <- function(wells_sub, id, dir) {

  w <- wells_sub %>%
    dplyr::mutate(
      WellID = well_tag_number,
      WellCode = paste0("w", well_tag_number),
      HydroID = well_tag_number,
      HydroCode = paste0("w", well_tag_number),
      RefElev = elev,
      LandElev = elev,
      X = utm_easting,
      Y = utm_northing,
      FromDepth = lithology_from_ft_bgl,
      ToDepth = lithology_to_ft_bgl,
      TopElev = RefElev - FromDepth,
      BottomElev = RefElev - ToDepth,
      Description = lithology_category,
      HGUName = lithology_category,
      OriginalLithology = lithology_clean)

  arc_well <- dplyr::select(w,
                            "HydroID", "HydoCode",
                            "X", "Y",
                            "LandElev", "WellDepth")

  readr::write_csv(arc_well, file.path(dir, paste0(id, "_arc_well.csv")))

  arc_hguid <- w %>%
    dplyr::select("Description", "HGUName") %>%
    dplyr::distinct() %>%
    dplyr::mutate(HGUID = 1:dplyr::n(),
                  HGUCode = .data$HydroID) %>%
    dplyr::relocate(HGUID, HGUCode, .before = "Description")

  readr::write_csv(arc_hguid, file.path(dir, paste0(id, "_arc_hguid.csv")))


  arc_bh <- w %>%
    dplyr::select("WellID", "WellCode", "HGUName", "RefElev", "FromDepth", "ToDepth",
                  "TopElev", "BottomElev", "OriginalLithology") %>%
    dplyr::left_join(dplyr::select(arc_hguid, "HGUName", "HGUID"), by = "HGUName") %>%
    dplyr::rename("Material" = "HGUName")

  readr::write_csv(arc_bh, file.path(dir, paste0(id, "_arc_bh.csv")))
}


