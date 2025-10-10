
#' Create Excel Drawdown template file
#'
#' Creates an Excel file with formulas for calculating drawdown. File is
#' pre-filled with wells within 100km of `location` (well or coordinates).
#'
#' @param location Numeric. Either a vector with longitude/latitude, or a well
#'   tag number.
#' @param rate Numeric. The pumping rate in m3/day (defaults to `NA`, fillable in Excel file).
#' @param duration Numeric. The duration of pumping in days (defaults to `NA`, fillable in Excel file).
#' @param overwrite Logical. Overwrite existing file?
#'
#' @inheritParams common_docs
#'
#' @return Creates excel file
#' @export
#'
#' @examples
#' drawdown(85199, rate = 343, duration = 180)
#' drawdown(85199, rate = 343, duration = 180, overwrite = TRUE)
#' drawdown(22966, rate = 343, duration = 180)
#' drawdown(22966)
#'
#' drawdown(c(-123.5593, 48.647), rate = 3.97, duration = 180)
#' drawdown(c(-123.5593, 48.647))

drawdown <- function(location, rate = NA, duration = NA, overwrite = FALSE,
                     file_name = NULL, update = FALSE) {

  if(!is.numeric(location)) {
    stop("`location` must be a number. Either a pair of longitude/latitude, ",
         "or a well tag number", call. = FALSE)
  }

  #TODO: Well status code and well intended use from GWELLS
  #TODO: Check consistency between wells testing info and pt_aquifer_parameters testing info
  #TODO: Hydrogeologic Setting should have all wells from all aquifers in the data set?
  #TODO: Hydrogeologic columns, move comments to middle and merge, then values as smaller to end? 
  #      MAke URL smaller hyperlink? Remove codes from from aquifer values to reduce size? Wrap Aquifer values?

  # Organization
  space <- 1 + 1 + 1 # Title + well + space

  # Data sets
  wells <- dd_wells(location, update)
  wells_testing <- dd_wells_testing(wells, update)
  aquifers <- dd_aquifers(wells$aquifer_id[wells$focal], wells$aquifer_id, update)
  inputs <- dd_inputs(rate, duration) 

  # Setup Sheets -------------------------------------------------------------
  # Get static value locations for the sheets
  locs <- dd_sheets_locs(inputs, space)

  dd <- dd_drawdown(wells, locs)

  wb <- openxlsx::createWorkbook()

  # Write inputs
  wb <- dd_sheet_inputs(wb, location, inputs, locs, space)

  # Write drawdowns
  wb <- dd_sheet_drawdowns(wb, dd)

  # Write Hydrogeologic
  wb <- dd_sheet_hydrogeologic(wb, aquifers, wells_testing)

  # Write Supporting
  wb <- dd_sheet_limitations(wb)
  wb <- dd_sheet_glossary(wb)
  wb <- dd_sheet_metadata(wb)

  # Save
  if(is.null(file_name)) {
    if(length(location) == 2) {
    } else location <- paste0("well_", location)
    file_name <- paste0("drawdown_", location, "_", Sys.Date(), ".xlsx")
  }
  openxlsx::saveWorkbook(wb, file_name, overwrite = overwrite)
}

col_nms <- function(wb = NULL, sheet = NULL, types = NULL) {
  cols <- dplyr::tribble(
    ~name_nice, ~name, ~sheet, ~numFmt, ~width,
    "Well Tag Number", "well_tag_number", "dd", "TEXT", 7,
    "Well Status", "status", "dd", "TEXT", NA,
    "Intended Well Use", "use", "dd", "TEXT", NA,
    "Well Details URL", "url", "dd", "TEXT", 35,
    
    "Distance to Well (m)", "dist", "dd", "0", NA,
    "Top of Bedrock Depth (m)", "bedrock_depth_m", "dd", "0.00", NA,
    "Top of Screen Depth (m)", "screen_depth", "dd", "0.00", NA,
    "Finished Depth of Well (m)", "well_depth_m", "dd", "0.00", NA,
    "Depth to Water (m)", "static_water_m", "dd", "0.00", NA,
    "Yield (US gpm)", "well_yield_usgpm", "dd", "0", NA,
    
    "Transmissivity (m2/d)", "transmissivity_m_2_d", "ref", "0", NA,
    "Storativity", "storativity", "ref",  "0.000", NA,
    
    "Aquifer ID", "aquifer_id", "dd", "0", NA,
    "Aquifer Subtype", "aquifer_lithology_code", "all", "TEXT", 15,
    "Bedrock / Unconsolidated", "aquifer_lithology_reassigned", "dd", "TEXT", 15,
    
    "Top of Fracture or Aquifer or Finished Well Depth [m]", "final_depth", "dd", "0.00", 10,
    "Drawdown Impact (m)", "drawdown", "dd", "0.00", 7,
    "Safe (70%)\nAvailable\nDrawdown (m)", "safe", "dd", "0.00", 15,
    "Impact as a\nPercentage of SAD (red>30%)", "impact", "dd", "0%", 15,
    
    # Hydrogeologic Settings
    "Reference", "reference", "aq", "TEXT", 20,
    "Aquifer ID", "aquifer_id", "aq", "0", 10,
    "Well Tag Number", "well_tag_number", "aq", "TEXT", 20,
    "Aquifer Name", "aquifer_name", "aq", "TEXT", 30,
    "Material Type", "material", "aq", "TEXT", 20,
    "Subtype", "subtype", "aq", "TEXT", 40,
    "Vulnerability", "vulnerability", "aq", "TEXT", 15,
    "Lithostratigraphic Unit", "litho_stratographic_unit", "aq", "TEXT", 35,
    "Descriptive Location", "location_description", "aq", "TEXT", 30,
    "Aquifer Details URL", "aquifer_details_url", "aq", "TEXT", 35,
    "Average Depth to Water (m)", "avg_water_depth_m", "aq", "0.0", 15,
    "Average Well Depth (m)", "avg_well_depth_m", "aq", "0.0", 15,
    "Test Duration (min)", "test_duration", "hg", "0", NA,
    "Hydraulic Conductivity (m/day)", "hydraulic_conductivity", "hg", "0.0", NA,
    "Specific Capacity (L/s/m)", "specific_capacity", "hg", "0.0", NA
  ) |>
  dplyr::mutate(
    style = purrr::map(numFmt, \(x) openxlsx::createStyle(numFmt = x)),
    width = tidyr::replace_na(width, 5),
    sheet = dplyr::case_when(
      sheet == "dd" ~ "Drawdown",
      sheet == "aq" ~ "Hydrogeologic Setting",
      .default = sheet))
    
    if (!is.null(wb) && !is.null(sheet)) {
      if (!is.numeric(sheet)) s <- which(names(wb) == sheet) else s <- sheet
      cols <- cols |>
        dplyr::filter(.data$sheet %in% c("all", .env$sheet)) |>
        dplyr::mutate(
          col_n = match(.data$name_nice,
            stringr::str_replace_all(
              openxlsx::get_worksheet_entries(.env$wb, .env$s), "&gt;", ">")
          )
        ) |>
        tidyr::drop_na(.data$col_n)
    }
    cols
}

dd_pretty_names <- function(.data) {
  nms <- col_nms()

  .data |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), as.numeric)) |>
    dplyr::rename_with(\(x) {
      n <- nms$name_nice[match(x, nms$name)]
      n[is.na(n)] <- x[is.na(n)] |>
        stringr::str_replace_all("_", " ") |>
        stringr::str_to_title()
      n
    })
}



aq_subtypes <- function() {

  dplyr::tribble(
    ~"subtype", ~"transmissivity", ~"storativity",
    "1a - Unconfined sand and gravel - large river system", 4500, 0.3,
    "1b - Unconfined sand and gravel aquifer - medium stream system", 1300, 0.3,
    "1c - Unconfined sand and gravel aquifer - small stream system", 200, 0.02,
    "2 - Unconfined sand and gravel - deltaic", 1049, 0.02,
    "3 - Unconfined sand and gravel - alluvial or colluvial fan", 710, 0.019,
    "4a - Unconfined sand and gravel - late glacial outwash", 690, 0.02,
    "4b - Confined sand and gravel - glacial", 250, 0.005,
    "4c - Confined sand and gravel - glacio-marine", 150, 0.005,
    "5a - Fractured sedimentary rock", 4, 0.00003,
    "5b - Karstic limestone", NA, NA,
    "6a - Flat-lying to gently-dipping volcanic bedrock", 23, 0.00064,
    "6b - Fractured crystalline bedrock", 1.7, 0.00064
  )
}

#' Excel Columns
cloc <- function(df, name) {
  LETTERS[which(names(df) == name)]
}

#' Excel locations
eloc <- function(df, name, row = 2) {
  paste0(cloc(df, name), seq(row, dplyr::n() + row - 1))
}

dd_inputs <- function(rate, duration) {
  
  duration <- units::set_units(duration, "d")
  rate <- units::set_units(rate, "m3/d")

  dplyr::tribble(
    ~Parameter,       ~Symbol,  ~Units, ~Value,
    "Transmissivity", "T", "m2/day", NA,
    "Storativity",    "S", "-", NA,
    "Pumping rate",   "Q", "m3/day", as.numeric(rate),
    "Duration",       "t", "days",   as.numeric(duration),
    "Distance",       "r", "m", NA)
}

#' Prepare aquifer details
#'
#' @param aquifer_ids List of aquifer ids to include
#'
#' @returns Formated aquifer details data frame
#' @noRd
dd_aquifers <- function(focal_aquifer, aquifer_ids, update) {
  aquifers <- data_read(type = "aquifers", update = update) |>
    sf::st_drop_geometry() |>
    dplyr::select(
      "aquifer_id", "aquifer_name", "material", "subtype", 
      "vulnerability", "litho_stratographic_unit", "location_description") |>
    dplyr::filter(.data$aquifer_id %in% unique(.env$aquifer_ids)) |>
    dplyr::mutate(
      aquifer_details_url = paste0(
        "https://apps.nrs.gov.bc.ca/gwells/aquifers/",
        .data$aquifer_id
      )
    )

  # Reload wells data to get *all* wells in an aquifer, not just those close to the focal
  w <- data_read(type = "wells", update = update) |>
    dplyr::select("aquifer_id", "water_depth_m", "well_depth_m") |>
    dplyr::semi_join(aquifers, by = "aquifer_id") |>
    dplyr::summarize(
      avg_water_depth_m = mean(.data$water_depth_m, na.rm = TRUE),
      avg_well_depth_m = mean(.data$well_depth_m, na.rm = TRUE),
      .by = "aquifer_id"
    )

  dplyr::left_join(aquifers, w, by = "aquifer_id") |>
    dplyr::mutate(reference = .data$aquifer_id %in% .env$focal_aquifer) |>
    dplyr::arrange(dplyr::desc(.data$reference), .data$aquifer_id) |>
    dplyr::relocate("reference") |>
    dplyr::mutate(
      reference = dplyr::if_else(
        .data$reference,
        "Pumping Well Aquifer",
        "Other Aquifer"
      )
    ) |>
    dd_pretty_names()
}

#' Prepare well data
#'
#' @returns Data frame of well data 
#' @noRd
#' Title
#'
#' @param location
#' @param wells
#'
#' @noRd
#'
#' @examples
#' dd_focal(85199, data_read("wells_sf"))
#' dd_focal(c(-123.5593, 48.647), data_read("wells_sf"))

dd_wells <- function(location, update = FALSE) {

  wells <- data_read(type = "wells_sf", update = update) |>
    sf::st_transform(3005) |>
    dplyr::mutate(aquifer_id = dplyr::na_if(.data$aquifer_id, 1143))

  # Get focal location as well or point
  if(length(location) == 1) {
    wells <- dplyr::mutate(wells, focal = well_tag_number == .env$location)
    if(sum(wells$focal) == 0) {
      stop("`location` seemed to be a well tag number but was not ",
           "found in GWELLS", call. = FALSE)
    }
  } else if(length(location) == 2) {
    focal <- sf::st_point(location) |>
      sf::st_sfc(crs = 4326) |>
      sf::st_transform(crs = 3005) |>
      sf::st_as_sf() |>
      dplyr::mutate(focal = TRUE) |>
      dplyr::rename("geometry" = "x")

    wells <- dplyr::bind_rows(wells, focal) |>
      dplyr::mutate(focal = tidyr::replace_na(.data$focal, FALSE))
  }

  focal_dist <- sf::st_distance(x = wells[wells$focal,], y = wells)

  wells |>
    dplyr::mutate(
      static_water_m = units::set_units(.data$static_water_level_ft_btoc, "ft") |>
        units::set_units("m"),
      well_depth_m = units::set_units(.data$finished_well_depth_ft_bgl, "ft") |>
        units::set_units("m"),
      bedrock_depth_m = units::set_units(.data$bedrock_depth_ft_bgl, "ft") |>
        units::set_units("m"),
      transmissivity_m_2_s = units::set_units(.data$transmissivity_m_2_s, "m2/s"),
      transmissivity_m_2_d = units::set_units(.data$transmissivity_m_2_s, "m2/d")) |>
    dplyr::mutate(dist = drop(.env$focal_dist)) |>
    dplyr::filter(.data$dist < units::set_units(1000, "m")) |>
    dplyr::arrange(.data$dist) |>
    dplyr::mutate(
      screen_depth = NA,
      status = NA,
      use = NA,
      url = paste0("https://apps.nrs.gov.bc.ca/gwells/well/", .data$well_tag_number),
      final_depth = NA,
      drawdown = NA,
      safe = NA,
      impact = NA,
      aquifer_lithology_reassigned = dplyr::case_when(
        is.na(.data$bedrock_depth_m) & is.na(.data$aquifer_lithology_code) ~ "Unassigned",
        is.na(.data$bedrock_depth_m) ~ .data$aquifer_lithology_code,
        .data$well_depth_m - .data$bedrock_depth_m > units::set_units(5, "ft") ~ "Bedrock",
        TRUE ~ "Unconsolidated")) |>
    dplyr::arrange(.data$well_tag_number) |>
    sf::st_drop_geometry()
  }

dd_wells_testing <- function(wells, update = FALSE) {
  data_read("wells_testing", update) |>
    dplyr::right_join(dplyr::select(wells, "well_tag_number", "aquifer_id"), by = "well_tag_number") |>
    dplyr::select(-"testing_number") |>
      dplyr::rename_with(\(x) stringr::str_remove_all(x, "_?pumping_test_?")) |>
    dplyr::relocate("aquifer_id", .after = "well_tag_number") |>
    dplyr::relocate("description", .before = "start_date") |>    
      dplyr::relocate("boundary_effect", .after = "start_date") |>    
    tidyr::drop_na("start_date") |>
    dd_pretty_names()
}

#' Title
#'
#' @param location
#' @param wells
#'
#' @noRd
#'
#' @examples
#'
#' f <- dd_focal(85199, data_read("wells_sf"))
#' dd_trans_store(f)
#'
#' f <- dd_focal(512, data_read("wells_sf"))
#' dd_trans_store(f)
#'
#' f <- dd_focal(c(-123.5593, 48.647), data_read("wells_sf"))
#' dd_trans_store(f)

dd_trans_store <- function(focal, update = FALSE) {

  # Get from Wells if possible
  if(!inherits(focal, "sfc") &&
     !is.na(focal$transmissivity_m_2_s) &&
     !is.na(focal$storativity)) {
    message("Transmissivity and storativity from GWELLS")
    return(c(focal$transmissivity_m_2_s, focal$storativity))
  }

  if(!inherits(focal, "sfc")) message("Well missing transmissivity and/or storativity")

  message("Obtaining transmissivity and storativity from aquifer subtype")

  aq <- data_read(type = "aquifers", update = update)

  if(!is.null(focal$aquifer_id) && !is.na(focal$aquifer_id)) {
    aq <- dplyr::filter(aq, aquifer_id == focal$aquifer_id)
  } else {
    aq <- aq |>
      sf::st_filter(focal) |>
      dplyr::select("aquifer_id", "subtype")
  }

  if(nrow(aq) > 1) {
    message("  `location` overlaps more than one aquifer (",
            paste0(aq$aquifer_id, collapse = ", "), ") using aquifer ",
            aq$aquifer_id[1])
    aq <- aq[1, ]
  } else if(nrow(aq) == 1) {
    message("  Using aquifer ", aq$aquifer_id)
  }

  if(nrow(aq) == 0 || is.na(aq$subtype)) {
    stop("Cannot find aquifer or missing aquifer subtype.",
         " Supply `transmissivity` and `storativity` manually.", call. = TRUE)
  }

  aq <- dplyr::left_join(aq, aq_subtypes(), by = "subtype")

  message("  Sub type: ", aq$subtype, "\n  Transmissivity of ",
          aq$transmissivity, " and Storativity of ", aq$storativity)

  c(aq$transmissivity, aq$storativity)
}

dd_drawdown <- function(wells, locs) {
  
  nms <- col_nms()

  # Drawdown columns and formulae
  dd <- wells |>
    dplyr::arrange(dist) |>
    dplyr::select(dplyr::all_of(nms$name[nms$sheet %in% c("all", "Drawdown")]))

  dd <- dd |>
    dplyr::mutate(
      final_depth = paste0(
        "=IF(", eloc(dd, "screen_depth"), "<>\"\", ", eloc(dd, "screen_depth"), ", ", eloc(dd, "well_depth_m"), ")"),
      drawdown = paste0(
        "Inputs!", locs$EQ1, "*LOG10(Inputs!", locs$EQ2, "/(", eloc(dd, "dist"), "*", eloc(dd, "dist"), "))"),
      drawdown = dplyr::if_else(as.numeric(dist) == 0, stringr::str_replace(drawdown, "[A-Z]{1}\\d\\*[A-Z]{1}\\d", "0.1*0.1"), drawdown),
      
      safe = glue::glue(
        "=IF({eloc(dd, 'final_depth')} <> \"\", ",
           "IF({eloc(dd, 'static_water_m')} <> \"\", ",
             "({eloc(dd, 'final_depth')} - {eloc(dd, 'static_water_m')}) * 0.7, ",
           "\"no Water Level\"), \"no Well Depth\")"),
      impact = glue::glue("=IF(ISNUMBER({eloc(dd, 'safe')}),{eloc(dd, 'drawdown')}/{eloc(dd, 'safe')}, \"\")")
      ) |>
    dplyr::select(-dplyr::starts_with("loc"))

  # Pretty names and remove unit class
  dd <- dd_pretty_names(dd)

  # Apply formula class
  for(x in nms$name_nice[nms$name %in% c("final_depth", "drawdown", "safe", "impact")]) {
    class(dd[[x]]) <- c(class(dd[[x]]), "formula")
  }

  dd
}


dd_sheets_locs <- function(inputs, space) {
  dplyr::tibble(
    `T`   = space + 1 + which(inputs[1] == "Transmissivity"),
    `S`   = space + 1 + which(inputs[1] == "Storativity"),
    `Q`   = space + 1 + which(inputs[1] == "Pumping rate"),
    `t`   = space + 1 + which(inputs[1] == "Duration"),
    `EQ1` = space + 3 + nrow(inputs) + 1 + 1,
    `EQ2` = space + 3 + nrow(inputs) + 1 + 2,
  ) |>
    dplyr::summarize(dplyr::across(
      dplyr::everything(),
      \(x) paste0("$", LETTERS[which(names(inputs) == "Value")], "$", x)
    ))
}


dd_sheet_inputs <- function(wb, location, inputs, locs, space, s = "Inputs") {

  # EQ formulas
  eqs <- dplyr::tribble(
    ~Name,       ~Equation,  ~` `, ~Value,
    "EQ1", "2.303Q/4PiT", NA, NA,
    "EQ2", "2.25Tt/S", NA, NA,
    "EQ Drawdown", "2.303Q/4PiT*log10(2.25Tt/Sr^2)", NA, NA) |>
    dplyr::mutate(
      Value = dplyr::case_when(
        Name == "EQ1" ~ paste0("=2.303*", locs$Q, "/(4*PI()*", locs$T),
        Name == "EQ2" ~ paste0("=2.25*", locs$T, "*", locs$t, "/", locs$S),
        .default = Value))

  class(eqs$Value) <- c(class(eqs$Value), "formula")

  openxlsx::addWorksheet(wb, s)

  openxlsx::addStyle(wb, s, cols = 1:10, rows = 1:20, gridExpand = TRUE,
                     style = s_body(), stack = TRUE)

  # Metadata
  openxlsx::writeData(wb, s, x = "Calculation of impact to adjacent wells from a pumping well")
  openxlsx::addStyle(wb, s, cols = 1, rows = 1, style = s_heading(), stack = TRUE)
  openxlsx::mergeCells(wb, s, cols = 1:6, rows = 1)
  openxlsx::writeData(wb, s, x = "Well Tag #", startRow = 2)
  openxlsx::addStyle(wb, s, row = 2, col = 1, style = s_emph(), stack = TRUE)
  openxlsx::setRowHeights(wb, s, rows = 1:2, heights = c(30, 20))
  openxlsx::writeData(wb, s, x = location, startRow = 2, startCol = 2)
  openxlsx::writeData(wb, s, x = "", startRow = space)

  # Inputs
  openxlsx::writeData(wb, s, x = inputs, startCol = 1, startRow = space + 1)
  openxlsx::writeData(wb, s, x = "'Distance to Well' in Drawdown worksheet", startCol = ncol(inputs), startRow = nrow(inputs) + space + 1)
  openxlsx::addStyle(wb, s, col = ncol(inputs), row = nrow(inputs) + space + 1,
                     style = s_it("left"))
  openxlsx::addStyle(wb, s, style = s_head(), cols = seq_len(ncol(inputs)), rows = space + 1, stack = TRUE)
  openxlsx::addStyle(wb, s, style = s_input(), cols = ncol(inputs), rows = space + 1 + which(is.na(inputs$Value[inputs$Parameter != "Distance"])), stack = TRUE)

  openxlsx::writeData(wb, s, x = "User Input Required",
                      startCol = ncol(inputs) + 2,
                      startRow = space + 1)
  openxlsx::addStyle(wb, s, style = s_input(), cols = ncol(inputs) + 2, rows = space + 1, stack = TRUE)
  openxlsx::addStyle(wb, s, style = s_it(), cols = ncol(inputs) + 2, rows = space + 1, stack = TRUE)


  # Equations
  openxlsx::writeData(wb, s, x = "Equations for calculating Drawdown",
                      startCol = 1, startRow = space + 1 + nrow(inputs) + 1 + 1)
  openxlsx::mergeCells(wb, s, cols = 1:5, rows = space + 1 + nrow(inputs) + 1 + 1)
  openxlsx::addStyle(wb, s, style = s_emph("left"), col = 1, row = space + 1 + nrow(inputs) + 1 + 1,
                     stack = TRUE)

  openxlsx::writeData(wb, s, x = eqs, startCol = 1, startRow = space + 1 + nrow(inputs) + 1 + 1 + 1)
  openxlsx::addStyle(wb, s, style = s_head(), cols = seq_len(ncol(eqs)),
                     rows = space + 1 + nrow(inputs) + 1 + 1 + 1, stack = TRUE)
  openxlsx::addStyle(wb, s, style = openxlsx::createStyle(numFmt = "0.00000"),
                     cols = ncol(eqs), rows = space + 1 + nrow(inputs) + 1 + 1 + 1 + 1, stack = TRUE)
  purrr::map(seq(space + 1 + nrow(inputs) + 1 + 1 + 1, length.out = nrow(eqs) + 1),
            \(x) openxlsx::mergeCells(wb, s, cols = 2:3, rows = x))
  openxlsx::writeData(wb, s, x = "'Drawdown Impact' in Drawdown worksheet", startCol = 4,
                      startRow = space + 1 + nrow(inputs) + 1 + 1 + 1 + 3)
  openxlsx::addStyle(wb, s, col = 4, row = space + 1 + nrow(inputs) + 1 + 1 + 1 + 3,
                     style = s_it("left"))

  openxlsx::setColWidths(wb, s, cols = 1:6, widths = c(15, 13, 13, 13, 13, 13))

  wb
}

dd_sheet_drawdowns <- function(wb, dd, s = "Drawdown") {

  startRow <- 1

  # Add sheet and data
  openxlsx::addWorksheet(wb, s)
  openxlsx::writeData(wb, s, x = dd, startRow = startRow)
  openxlsx::freezePane(wb, s, firstRow = TRUE)

  # Get col/row locations
  cols <- col_nms(wb, s, types = c("all", "dd"))
  rows <- seq(startRow + 1, nrow(dd) + startRow)

  # Set styles
  col_wrap <- which(nchar(names(dd)) > 30)
  col_rotate <- which(nchar(names(dd)) <= 30)

  s_rotate <- openxlsx::createStyle(textRotation = 90)
  s_wrap <- openxlsx::createStyle(wrapText = TRUE)

  # No stack option for conditional styles (s_focal not conditional, so needs fgFill)
  s_focal <- openxlsx::createStyle(fgFill = "#afd095")
  s_aq_focal <- openxlsx::createStyle(bgFill = "#afd095", fontSize = 10, halign = "center")
  s_aq_diff <- openxlsx::createStyle(bgFill = "#b4c7dc", fontSize = 10, halign = "center")
  s_aq_na <- openxlsx::createStyle(bgFill = "#ec9ba4", fontSize = 10, halign = "center")

  # Apply styles
  openxlsx::setRowHeights(wb, s, rows = 1, heights = 140)

  openxlsx::addStyle(wb, s, style = s_head(), cols = cols$col_n, rows = 1)
  openxlsx::addStyle(wb, s, style = s_rotate, cols = col_rotate, rows = 1, stack = TRUE)
  openxlsx::addStyle(wb, s, style = s_wrap, cols = col_wrap, rows = 1, stack = TRUE)
  openxlsx::addStyle(wb, s, style = s_body(), cols = cols$col_n,
                     rows = rows, gridExpand = TRUE, stack = TRUE)

  # Add colour
  aid <- cloc(dd, "Aquifer ID")
  openxlsx::addStyle(wb, s, style = s_focal, cols = cols$col_n, rows = 2, stack = TRUE)
  openxlsx::conditionalFormatting(
    wb, s,
    style = s_aq_focal, cols = cols$col_n[cols$name == "aquifer_id"],
    rows = rows, rule = paste0(aid, "2==$", aid, "$2"), stack = TRUE)
  openxlsx::conditionalFormatting(
    wb, s,
    style = s_aq_diff, cols = cols$col_n[cols$name == "aquifer_id"],
    rows = rows, rule = paste0(aid, "2!=$", aid, "$2"), stack = TRUE)
  openxlsx::conditionalFormatting(
    wb, s,
    style = s_aq_na, cols = cols$col_n[cols$name == "aquifer_id"],
    rows = rows, type = "blanks")

  # Apply formatting and column widths
  s_apply(wb, s, rows, cols)

  wb
}

dd_sheet_hydrogeologic <- function(wb, aquifers, wells_testing, s = "Hydrogeologic Setting") {
  
  # Add sheet data
  openxlsx::addWorksheet(wb, s)

  rows_aq <- seq(2, length.out = nrow(aquifers) + 1)
  rows_wt <- seq(rows_aq[length(rows_aq)] + 3, length.out = nrow(wells_testing) + 1)
   
  # Add data
  openxlsx::writeData(wb, s, x = aquifers, startRow = rows_aq[1])
  openxlsx::writeData(wb, s, x = wells_testing, startRow = rows_wt[1])

  # Get header information
  cols <- col_nms(wb, s)
  cols_aq <- cols[match(names(aquifers), cols$name_nice), ]
  cols_wt <- cols[match(names(wells_testing), cols$name_nice), ]

  # Add section headings (after cols, so doesn't interfere with headers)
  openxlsx::writeData(wb, s, x = "Aquifer Information")
  openxlsx::addStyle(wb, s, style = s_heading(), rows = 1, col = 1)
  openxlsx::writeData(wb, s, startRow = rows_wt[1] - 1 , x = "Hydraulic Parameters from Well Testing")
  openxlsx::addStyle(wb, s, style = s_heading(), rows = rows_wt[1]-1, col = 1)
  
  # Set styles
  #col_wrap <- which(nchar(names(aquifers)) > 30)
  #col_rotate <- which(nchar(names(aquifers)) <= 30)
  
  s_rotate <- openxlsx::createStyle(textRotation = 90)
  s_wrap <- openxlsx::createStyle(wrapText = TRUE)
  
  # Apply styles
  #openxlsx::setRowHeights(wb, s, rows = rows_aq[1], heights = 140)
  
  openxlsx::addStyle(wb, s, style = s_head(), cols = seq_len(ncol(aquifers)), rows = rows_aq[1])
  #openxlsx::addStyle(wb, s, style = s_rotate, cols = col_rotate, rows = rows_aq[1], stack = TRUE)
  openxlsx::addStyle(
    wb, s, style = s_body(), cols = seq_len(ncol(aquifers)),
    rows = rows_aq[-1], gridExpand = TRUE, stack = TRUE)
  
  openxlsx::addStyle(wb, s, style = s_head(), cols = seq_len(ncol(wells_testing)), rows = rows_wt[1])
  #openxlsx::addStyle(wb, s, style = s_rotate, cols = col_rotate, rows = rows_wt[1], stack = TRUE)
  openxlsx::addStyle(
    wb, s, style = s_body(), cols = seq_len(ncol(aquifers)),
    rows = rows_wt[-1], gridExpand = TRUE, stack = TRUE)
  
  openxlsx::addStyle(
    wb, s, style = s_wrap, cols = stringr::str_which(names(aquifers), "Depth"),
    rows = rows_aq[1], stack = TRUE)
  
  # Apply formatting and column widths
  s_apply(wb, s, rows = rows_aq, cols = cols_aq)
  s_apply(wb, s, rows = rows_wt, cols = tidyr::drop_na(cols_wt))
  
  wb
}

dd_sheet_limitations <- function(wb, s = "Limitations") {

  startRow <- 1

  # Add sheet and data
  openxlsx::addWorksheet(wb, s)
  openxlsx::addStyle(wb, s, col = 1:2, row = 1:40,
                     style = s_body("left"), gridExpand = TRUE, stack = TRUE)
  openxlsx::writeData(wb, s, startRow = startRow, x = "Limitations")
  openxlsx::addStyle(wb, s, row = startRow, col = 1, style = s_heading())
  openxlsx::writeData(wb, s, startRow = startRow + 2,
                      "This tool is based on Cooper (1946). Assumptions of this solution include infinite aquifer extent, homogeneous, isotropic and uniform thickness, fully penetrating pumping well, horizontal flow, a nonleaky confined aquifer, and neglects well bore storage. This tool does not replace the guidance or advice of a qualified professional. Hydrogeology is a reserved practice that may only be carries out by or under the supervision of an individual registered with Engineers and Geoscientists with competence in hydrogeology.")
  openxlsx::addStyle(wb, s, row = startRow + 2, col = 1,
                     style = openxlsx::createStyle(wrapText = TRUE, indent = 1),
                     stack = TRUE)
  openxlsx::setColWidths(wb, s, cols = 1, widths = 100)

  wb
}

dd_sheet_glossary <- function(wb, s = "Glossary") {
  
  startRow <- 1
  g <- dplyr::tribble(
    ~Term, ~Definition,
    "SAD", "Safe Available Drawdown",
    #"WL", "Water Level (Depth to Water (m))",
    "US gpm", "Imperial Gallons Per Minute",
    "m", "metres",
    "m2/d", "metres squared per day"
  )

  exp <- dplyr::tribble(
    ~`Concept/Column`, ~Explanation, 
    "Bedrock / Unconsolidated", 
    paste0(
      "Reassigned lithology:\n",
      "If missing both bedrock depth and lithology, becomes 'Unassigned'\n",
      "If missing bedrock depth, uses same value as 'Aquifer Subtype'\n",
      "Otherwise, if well depth is > 5ft below the bedrock depth, becomes 'Bedrock'")
  )

  # Add sheet and data
  openxlsx::addWorksheet(wb, s)
  openxlsx::writeData(wb, s, startRow = startRow, x = g)
  openxlsx::writeData(wb, s, startRow = startRow + nrow(g) + 2, x = exp)

  openxlsx::addStyle(wb, s, row = c(startRow, startRow + nrow(g) + 2), col = 1:2, style = s_head(), gridExpand = TRUE)

  openxlsx::setColWidths(wb, s, cols = 1:2, widths = c(25, 80))

  wb
}

dd_sheet_metadata <- function(wb, s = "Metadata") {

  startRow <- 1

  meta <- tidyr::pivot_longer(
    cache_meta(), cols = dplyr::everything(),
    names_to = "key", values_transform = as.character) |>
    dplyr::mutate(key = stringr::str_replace_all(key, "_", " "),
                  key = stringr::str_to_title(key),
                  key = stringr::str_replace_all(key, "Bcaquiferdata", "bcaquiferdata"))

  # Add sheet and data
  openxlsx::addWorksheet(wb, s)
  openxlsx::addStyle(wb, s, col = 1:2, row = 1:40,
                     style = s_body("left"), gridExpand = TRUE, stack = TRUE)
  openxlsx::writeData(wb, s, startRow = startRow, x = "Metadata")
  openxlsx::addStyle(wb, s, row = startRow, col = 1, style = s_heading())

  openxlsx::writeData(
    wb, s, startRow = startRow + 2,
    x = paste0("Excel template created with the bcaquiferdata R package on ", Sys.Date()))

  openxlsx::writeData(wb, s, startRow = startRow + 4, x = meta, colNames = FALSE)
  openxlsx::addStyle(wb, s, row = (startRow + 4):(startRow + 4 + nrow(meta)),
                     col = 1, style = openxlsx::createStyle(textDecoration = "bold"),
                     stack = TRUE)

  openxlsx::setColWidths(wb, s, cols = 1:2, widths = c(25, 25))

  wb
}

s_heading <- function() {
  openxlsx::createStyle(
    textDecoration = "bold", fontSize = 14, valign = "center", halign = "left",
    indent = 1)
}

s_head <- function() {
  openxlsx::createStyle(
    textDecoration = "bold", fontSize = 10, valign = "center", halign = "center",
    border = "TopBottomLeftRight", borderStyle = "thin")
}

s_body <- function(halign = "center") {
  openxlsx::createStyle(fontSize = 10, halign = halign)
}

s_emph <- function(halign = "center") {
  openxlsx::createStyle(fontSize = 11, textDecoration = "bold", indent = 1,
                        halign = halign)
}

s_input <- function() {
  openxlsx::createStyle(fgFill = "#fff2cc")
}

s_it <- function(halign = "center") {
  openxlsx::createStyle(fontSize = 9, textDecoration = "italic", halign = halign)
}


s_apply <- function(wb, s, rows, cols) {
  purrr::walk(seq_len(nrow(cols)), \(n) {
    openxlsx::addStyle(
      wb,
      s,
      cols = cols$col_n[n],
      style = cols$style[[n]],
      rows = rows,
      stack = TRUE
    )
  })

  # Column widths - Cannot use Auto and then override, one or the other
  openxlsx::setColWidths(wb, s, cols = cols$col_n, widths = cols$width)

  wb
}