

ui_cross_sections <- function(id) {

  ns <- NS(id)

  fluidRow(
    column(
      width = 3,
      box(
        width = 12,
        title = "Explore Cross Sections",
        checkboxGroupInput(
          ns("cross_section_columns"), label = "Show columns",
          choices = list("Basic" = "min",
                         "Raw lithology" = "lith_raw",
                         "Flags" = "flags"),
          selected = "min"
        )
      )
    ),
    column(
      width = 9,
      box(
        width = 12,
        title = "Table",
        div(style = "overflow-x:scroll",
            DT::dataTableOutput(ns("cross_sections_table")))
      )
    )
  )
}

server_cross_sections <- function(id, wells) {

  moduleServer(id, function(input, output, session) {

    output$cross_sections_table <- DT::renderDataTable({
      show <- input$cross_section_columns
      cols <- c("well_tag_number")

      if("min" %in% show) cols <- c(
        cols,
        "elev", "well_depth_m", "lithology_from_m", "lithology_to_m",
        "well_yield_usgpm", "well_yield_unit_code",
        "depth", "yield", "yield_units")
      if("lith_raw" %in% show) cols <- c(cols, "lithology_raw_data")
      if("flags" %in% show) cols <- c(cols, "lith_flag")


      wells() %>%
        dplyr::select(dplyr::all_of(cols)) %>%
        sf::st_drop_geometry()
    })

  })

}
