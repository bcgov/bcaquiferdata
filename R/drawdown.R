
#' Title
#'
#' @param location
#' @param rate
#' @param duration
#' @param transmissivity
#' @param storativity
#' @param update
#'
#' @return
#' @export
#'
#' @examples
#'
#' d <- drawdown(85199, rate = 343, duration = 180)
#'
#' drawdown(85199, rate = 3.97, duration = 180, transmissivity = 208, storativity = 0.0048)
#'
#' drawdown(c(-123.5593, 48.647), rate = 3.97, duration = 180)
#' drawdown(c(-123.5593, 48.647), rate = 3.97, duration = 180,
#'          transmissivity = 208, storativity = 0.0048)

drawdown <- function(location, rate, duration,
                     update = FALSE) {

  if(!is.numeric(location)) {
    stop("`location` must be a number. Either a pair of longitude/latitude, ",
         "or a well tag number", call. = FALSE)
  }

  # Organization
  space <- 1 + 1 + 1 # Title + well + space

  # Get and format data
  duration <- units::set_units(duration, "d")
  rate <- units::set_units(rate, "m3/d")

  wells <- data_read(type = "wells_sf", update = update) |>
    sf::st_transform(3005) |>
    dplyr::mutate(aquifer_id = dplyr::na_if(.data$aquifer_id, 1143))

  focal <- dd_focal(location, wells)
  focal_dist <- sf::st_distance(x = focal, y = wells)

  wells <- wells |>
    dplyr::mutate(
      static_water_m = units::set_units(static_water_level_ft_btoc, "ft") |>
        units::set_units("m"),
      well_depth_m = units::set_units(finished_well_depth_ft_bgl, "ft") |>
        units::set_units("m"),
      bedrock_depth_m = units::set_units(bedrock_depth_ft_bgl, "ft") |>
        units::set_units("m"),
      transmissivity_m_2_s = units::set_units(transmissivity_m_2_s, "m2/s"),
      transmissivity_m_2_d = units::set_units(transmissivity_m_2_s, "m2/d")) |>
    dplyr::mutate(dist = drop(.env$focal_dist)) |>
    dplyr::filter(dist < units::set_units(1000, "m")) |>
    dplyr::arrange(dist) |>
    dplyr::mutate(
      screen_depth = NA,
      status = NA,
      use = NA,
      url = paste0("https://apps.nrs.gov.bc.ca/gwells/well/", well_tag_number),
      drawdown = NA,
      safe = NA,
      impact = NA,
      aquifer_lithology_reassigned = dplyr::case_when(
        is.na(bedrock_depth_m) & is.na(aquifer_lithology_code) ~ "Unassigned",
        is.na(bedrock_depth_m) ~ aquifer_lithology_code,
        well_depth_m - bedrock_depth_m > units::set_units(5, "ft") ~ "Bedrock",
        TRUE ~ "Unconsolidated")) |>
    dplyr::arrange(well_tag_number) |>
    sf::st_drop_geometry()

  inputs <- dplyr::tribble(
    ~Parameter,       ~Symbol,  ~Units, ~Value,
    "Transmissivity", "T", "m2/day", NA,
    "Storativity",    "S", "-", NA,
    "Pumping rate",   "Q", "m3/day", as.numeric(rate),
    "Duration",       "t", "days",   as.numeric(duration),
    "Distance",       "r", "m", NA)


  # Setup Sheets -------------------------------------------------------------
  # Get static value locations for the sheets
  locs <- dd_sheets_locs(inputs, space)

  nms <- col_nms()

  refs <- wells |>
    dplyr::select(dplyr::all_of(nms$name[nms$sheet != "dd"])) |>
    dplyr::rename_with(\(x) nms$name_nice[match(x, nms$name)])

  # Drawdown columns and formulae
  dd <- wells |>
    dplyr::arrange(dist) |>
    dplyr::select(dplyr::all_of(nms$name))

  dd <- dd |>
    dplyr::mutate(
      drawdown = paste0(
        "Inputs!", locs$EQ1, "*LOG10(Inputs!", locs$EQ2, "/(", eloc(dd, "dist"), "*", eloc(dd, "dist"), "))"),
      # TODO: screen depth not actually used in any calculations....?
      #well_depth_m = paste0(
      #  "=IF(", eloc(dd, "screen_depth"), "<>'', ", eloc(dd, "screen_depth"), ", ", eloc(dd, "well_depth_m"), ")"),
      safe = glue::glue(
        "=IF({eloc(dd, 'well_depth_m')} <> \"\", ",
           "IF({eloc(dd, 'static_water_m')} <> \"\", ",
             "({eloc(dd, 'well_depth_m')} - {eloc(dd, 'static_water_m')}) * 0.7, ",
           "\"no NPL\"), \"no Well Depth\")"),
      impact = glue::glue("=IF(ISNUMBER({eloc(dd, 'safe')}),{eloc(dd, 'drawdown')}/{eloc(dd, 'safe')}, \"n.a.\")")
      ) |>
    dplyr::select(-dplyr::starts_with("loc"))

  # Pretty names and remove unit class
  dd <- dd |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), as.numeric)) |>
    dplyr::rename_with(\(x) nms$name_nice[match(x, nms$name)])

  # Apply formula class
  for(x in nms$name_nice[nms$name %in% c("drawdown", "safe", "impact")]) {
    class(dd[[x]]) <- c(class(dd[[x]]), "formula")
  }

  # TODO: NExt is get the screen vs. depth formula

  wb <- openxlsx::createWorkbook()

  # Write inputs
  wb <- dd_sheet_inputs(wb, location, inputs, locs, space)

  # Write drawdowns
  wb <- dd_sheet_drawdowns(wb, dd)

  # Save
  openxlsx::saveWorkbook(wb, "testing.xlsx", overwrite = TRUE)
}

col_nms <- function(wb = NULL, sheet = NULL) {
  cols <- dplyr::tribble(
    ~name_nice, ~name, ~sheet, ~numFmt, ~width,
    "Well Tag Number", "well_tag_number", "all", "TEXT", 7,
    "Well Status", "status", "dd", "TEXT", NA,
    "Intended Well Use", "use", "dd", "TEXT", NA,
    "Well Details URL", "url", "dd", "TEXT", 35,

    "Distance to Well (m)", "dist", "all", "0", NA,
    "Top of Bedrock Depth (m)", "bedrock_depth_m", "all", "0.00", NA,
    "Top of Screen Depth (m)", "screen_depth", "all", "0.00", NA,
    "Finished Depth of Well (m)", "well_depth_m", "all", "0.00", NA,
    "Depth to Water (m)", "static_water_m", "all", "0.00", NA,

    "Yield (US gpm)", "well_yield_usgpm", "ref", "0", NA,
    "Transmissivity (m2/d)", "transmissivity_m_2_d", "ref", "0", NA,
    "Storativity", "storativity", "ref",  "0.000", NA,

    "Aquifer ID", "aquifer_id", "all", "0", NA,
    "Aquifer Subtype", "aquifer_lithology_code", "all", "TEXT", 15,
    "Bedrock / Unconsolidated", "aquifer_lithology_reassigned", "all", "TEXT", 15,

    "Drawdown Impact (m)", "drawdown", "dd", "0.00", 7,
    "Safe (70%)\nAvailable\nDrawdown (m)", "safe", "dd", "0.00", 15,
    "Impact as a\nPercentage of SAD (red>30%)", "impact", "dd", "PERCENTAGE", 15,
  ) |>
    dplyr::mutate(
      style = purrr::map(numFmt, \(x) openxlsx::createStyle(numFmt = x)),
      width = tidyr::replace_na(width, 5))

  if(!is.null(wb) && !is.null(sheet)) {
    cols <- dplyr::mutate(cols, col_n = match(
      .data$name_nice,
      stringr::str_replace_all(openxlsx::get_worksheet_entries(.env$wb, .env$sheet), "&gt;", ">")))
  }
  cols
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
eloc <- function(df, name, row = 6) {
  paste0(cloc(df, name), seq(row, dplyr::n() + row - 1))
}


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

dd_focal <- function(location, wells) {
  # Get focal location as well or point
  if(length(location) == 1) {
    focal <- wells |>
      dplyr::filter(well_tag_number == .env$location) |>
      sf::st_transform(crs = 3005)
    if(nrow(focal) == 0) {
      stop("`location` seemed to be a well tag number but was not ",
           "found in GWELLS", call. = FALSE)
    }
  } else if(length(location) == 2) {
    focal <- sf::st_point(location) |>
      sf::st_sfc(crs = 4326) |>
      sf::st_transform(crs = 3005)
  }

  focal
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

dd_sheets_locs <- function(inputs, space) {
  dplyr::tibble(
    `T`   = space + 1 + which(inputs[1] == "Transmissivity"),
    `S`   = space + 1 + which(inputs[1] == "Storativity"),
    `Q`   = space + 1 + which(inputs[1] == "Pumping rate"),
    `t`   = space + 1 + which(inputs[1] == "Duration"),
    `EQ1` = space + 1 + nrow(inputs) + 1 + 1,
    `EQ2` = space + 1 + nrow(inputs) + 1 + 2,
  ) |>
    dplyr::summarize(dplyr::across(
      dplyr::everything(),
      \(x) paste0("$", LETTERS[which(names(inputs) == "Value")], "$", x)
    ))
}


dd_sheet_inputs <- function(wb, location, inputs, locs, space) {

  # EQ formulas
  eqs <- dplyr::tribble(
    ~Parameter,       ~Symbol,  ~Units, ~Value,
    "EQ1", "2.303Q/4PiT", NA, NA,
    "EQ2", "2.25Tt/S", NA, NA,
    "EQ Drawdown", "2.303Q/4PiT*log10(2.25Tt/Sr^2)", NA, NA) |>
    dplyr::mutate(
      Value = dplyr::case_when(
        Parameter == "EQ1" ~ paste0("=2.303*", locs$Q, "/(4*PI()*", locs$T),
        Parameter == "EQ2" ~ paste0("=2.25*", locs$T, "*", locs$t, "/", locs$S),
        .default = Value))

  class(eqs$Value) <- c(class(eqs$Value), "formula")

  openxlsx::addWorksheet(wb, "Inputs")
  openxlsx::writeData(wb, "Inputs", x = "Calculation of impact to adjacent wells from a pumping well")
  openxlsx::writeData(wb, "Inputs", x = "Well", startRow = 2)
  openxlsx::writeData(wb, "Inputs", x = location, startRow = 2, startCol = 2)
  openxlsx::writeData(wb, "Inputs", x = "", startRow = space)
  openxlsx::writeData(wb, "Inputs", x = inputs, startCol = 1, startRow = space + 1)
  openxlsx::writeData(wb, "Inputs", x = eqs, startCol = 1, startRow = space + 1 + nrow(inputs) + 1 + 1,
                      colNames = FALSE)
  openxlsx::setColWidths(wb, "Inputs", cols = seq_len(ncol(inputs)), widths = "auto")
  openxlsx::setColWidths(wb, "Inputs", cols = 1, widths = 20)

  wb
}

dd_sheet_drawdowns <- function(wb, dd) {

  startRow <- 1

  # Add sheet and data
  openxlsx::addWorksheet(wb, "Drawdown")
  openxlsx::writeData(wb, "Drawdown", x = dd, startRow = startRow)

  # Get col/row locations
  cols <- col_nms(wb, 2)
  rows <- seq(startRow + 1, nrow(dd) + startRow)

  # Set styles

  col_wrap <- which(nchar(names(dd)) > 30)
  col_rotate <- which(nchar(names(dd)) <= 30)

  s_head <- openxlsx::createStyle(
    textDecoration = "bold", fontSize = 10, valign = "center", halign = "center",
    border = "TopBottomLeftRight", borderStyle = "thin")

  s_rotate <- openxlsx::createStyle(textRotation = 90)
  s_wrap <- openxlsx::createStyle(wrapText = TRUE)
  s_body <- openxlsx::createStyle(fontSize = 10, halign = "center")

  # No stack option for conditional styles (s_focal not conditional, so needs fgFill)
  s_focal <- openxlsx::createStyle(fgFill = "#afd095")
  s_aq_focal <- openxlsx::createStyle(bgFill = "#afd095", fontSize = 10, halign = "center")
  s_aq_diff <- openxlsx::createStyle(bgFill = "#b4c7dc", fontSize = 10, halign = "center")
  s_aq_na <- openxlsx::createStyle(bgFill = "#ec9ba4", fontSize = 10, halign = "center")

  # Apply styles
  openxlsx::setRowHeights(wb, "Drawdown", rows = 1, heights = 140)

  openxlsx::addStyle(wb, 2, style = s_head, cols = cols$col_n, rows = 1)
  openxlsx::addStyle(wb, 2, style = s_rotate, cols = col_rotate, rows = 1, stack = TRUE)
  openxlsx::addStyle(wb, 2, style = s_wrap, cols = col_wrap, rows = 1, stack = TRUE)
  openxlsx::addStyle(wb, 2, style = s_body, cols = cols$col_n,
                     rows = rows, gridExpand = TRUE, stack = TRUE)

  # Add colour
  aid <- cloc(dd, "Aquifer ID")
  openxlsx::addStyle(wb, 2, style = s_focal, cols = cols$col_n, rows = 2, stack = TRUE)
  openxlsx::conditionalFormatting(
    wb, 2,
    style = s_aq_focal, cols = cols$col_n[cols$name == "aquifer_id"],
    rows = rows, rule = paste0(aid, "2==$", aid, "$2"), stack = TRUE)
  openxlsx::conditionalFormatting(
    wb, 2,
    style = s_aq_diff, cols = cols$col_n[cols$name == "aquifer_id"],
    rows = rows, rule = paste0(aid, "2!=$", aid, "$2"), stack = TRUE)
  openxlsx::conditionalFormatting(
    wb, 2,
    style = s_aq_na, cols = cols$col_n[cols$name == "aquifer_id"],
    rows = rows, type = "blanks")


  purrr::walk(seq_len(nrow(cols)), \(n) {
    openxlsx::addStyle(wb, 2, cols = cols$col_n[n],
                       style = cols$style[[n]], rows = rows, stack = TRUE)
  })

  # Column widths - Cannot use Auto and then override, one or the other
  openxlsx::setColWidths(wb, 2, cols = cols$col_n,
                         widths = cols$width)

  # TODO: Fix column widths for where based on 'unrotated' column names
  # TODO: highlight the focal well
  # TODO: Add colour highlights for wells

  openxlsx::saveWorkbook(wb, "testing.xlsx", overwrite = TRUE)
  wb
}

ox_col <- function(wb, col) {
  browser()
  openxlsx::get_worksheet_entries(wb, 2)
}


