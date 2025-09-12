
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
#' d <- drawdown(85199, rate = 3.97, duration = 180)
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

  # Load wells data
  wells <- data_read(type = "wells_sf", update = update) |>
    sf::st_transform(3005)

  # Format focal location
  focal <- dd_focal(location, wells)

  duration <- units::set_units(duration, "d")

  rate <- units::set_units(3.97, "L/s") |>
    units::set_units("m3/d")


  info <- wells |>
    dplyr::mutate(
      static_water_m = units::set_units(static_water_level_ft_btoc, "ft") |>
        units::set_units("m"),
      well_depth_m = units::set_units(finished_well_depth_ft_bgl, "ft") |>
        units::set_units("m"),
      bedrock_depth_m = units::set_units(bedrock_depth_ft_bgl, "ft") |>
        units::set_units("m"),
      transmissivity_m_2_s = units::set_units(transmissivity_m_2_s, "m2/s")) |>

    #dplyr::select("well_tag_number", "aquifer_id", "well_depth_m", "bedrock_depth_m",
    #              "well_yield_usgpm", "static_water_m", "aquifer_lithology_code",
    #              "transmissivity) |>
    dplyr::mutate(dist = drop(sf::st_distance(x = focal, y = wells))) |>
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
    #  drawdown = as.numeric(eq1 * log10(eq2/dist^2)),
    #  drawdown = units::set_units(drawdown, "m"),
      aquifer_lithology_reassigned = dplyr::case_when(
        is.na(bedrock_depth_m) & is.na(aquifer_lithology_code) ~ "Unassigned",
        is.na(bedrock_depth_m) ~ aquifer_lithology_code,
        well_depth_m - bedrock_depth_m > units::set_units(5, "ft") ~ "Bedrock",
        TRUE ~ "Unconsolidated")) |> #,
    #  safe_drawdown = (well_depth_m - static_water_m) * 0.7,
    #  impact = units::set_units(drawdown / safe_drawdown, "%")) |>
    dplyr::arrange(well_tag_number) |>
    sf::st_drop_geometry()


  nms <- dplyr::tribble(
    ~name_nice, ~name, ~sheet,
    "Well Tag Number", "well_tag_number", "all",
    "Well Status", "status", "dd",
    "Intended Well Use", "use", "dd",
    "Well Details URL", "url", "dd",

    "Distance to Well", "dist", "all",
    "Top of Bedrock Depth (m)", "bedrock_depth_m", "all",
    "Screen Depth (m)", "screen_depth", "all",
    "Finished Depth of Well (m)", "well_depth_m", "all",
    "Depth to Water (m)", "static_water_m", "all",

    "Yield (US gpm)", "well_yield_usgpm", "ref",
    "Transmissivity (m2/s)", "transmissivity_m_2_s", "ref",
    "Storativity", "storativity", "ref",

    "Aquifer ID", "aquifer_id", "all",
    "Aquifer Subtype", "aquifer_lithology_code", "all",
    "Bedrock / Unconsolidated", "aquifer_lithology_reassigned", "all",

    "Drawdown Impact (m)", "drawdown", "dd",
    "Safe (70%)\nAvailable Drawdown (m)", "safe", "dd",
    "Impact as a\nPercentage of SAD (red>30%)", "impact", "dd",
  )

  inputs <- dplyr::tribble(
    ~Parameter,       ~Symbol,  ~Units, ~Value,
    "Transmissivity", "T", "m2/day", NA,
    "Storativity",    "S", "-", NA,
    "Pumping rate",   "Q", "m3/day", as.numeric(rate),
    "Duration",       "t", "days", as.numeric(duration),
    "Distance",       "r", "m", NA)

  # Organization
  space <- 1 + 1 + 1 # Title + well + space

  # Get static value locations
  locs <- dplyr::tibble(
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

  # EQ forumlas
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

  refs <- info |>
    dplyr::select(dplyr::all_of(nms$name[nms$sheet != "dd"])) |>
    dplyr::rename_with(\(x) nms$name_nice[match(x, nms$name)])
    #dplyr::filter(`Well Tag Number` %in% c(15173, 36730, 37353))

  # Drawdown columns and formulae
  dd <- info |>
    dplyr::arrange(dist) |>
    dplyr::select(dplyr::all_of(nms$name))
    #dplyr::select(dplyr::all_of(nms$name[nms$sheet != "ref"]))

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

  pump <- dplyr::filter(dd, `Distance to Well` == 0) |>
    dplyr::select(-dplyr::matches("Safe|Drawdown|Impact"))

  dd <- dplyr::filter(dd, `Distance to Well` != 0)

  # Apply formula class
  for(x in nms$name_nice[nms$name %in% c("drawdown", "safe", "impact")]) {
    class(dd[[x]]) <- c(class(dd[[x]]), "formula")
  }

  # TODO: NExt is add safe and impact foRmula, and get the screen vs. depth formula


    # TODO ---------------------------
    # dplyr::filter(well_tag_number %in% c(14862, 15173, 36730, 37353, 52011,
    #                                      53169, 56016, 84818, 94356, 94359,
    #                                      97015, 104589, 124191))


  wb <- openxlsx::createWorkbook()

  # Write inputs
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

  # Write drawdowns
  openxlsx::addWorksheet(wb, "Drawdown")

  openxlsx::writeData(wb, "Drawdown", x = "Pumping Well",     startRow = 1)
  openxlsx::writeData(wb, "Drawdown", x = pump,               startRow = 2)
  openxlsx::writeData(wb, "Drawdown", x = "",                 startRow = 4)
  openxlsx::writeData(wb, "Drawdown", x = "Wells within 1km", startRow = 5)
  openxlsx::writeData(wb, "Drawdown", x = dd,                 startRow = 6)
  openxlsx::setRowHeights(wb, "Drawdown", rows = c(2, 6), heights = 35)
  openxlsx::setColWidths(wb, "Drawdown", cols = seq_len(ncol(dd)), widths = "auto")

  # Save
  openxlsx::saveWorkbook(wb, "testing.xlsx", overwrite = TRUE)
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


#' Excel locations
eloc <- function(df, name, row = 7) {
  paste0(LETTERS[which(names(df) == name)], seq(row, dplyr::n() + row - 1))
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
