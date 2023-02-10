ui_lithology <- function(id) {

  ns <- NS(id)

  fluidRow(
    column(
      width = 3,
      box(
        title = "Explore Lithology",
        width = 12,
        checkboxGroupInput(
          ns("lith_columns"), label = "Show columns",
          choices = list("Basic" = "min",
                         "Extra" = "extra",
                         "All from GWELLS" = "gwells",
                         "Lithology categorization" = "cats",
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
            DT::dataTableOutput(ns("lith_table"))),
      )
    )
  )
}

server_lithology <- function(id, wells) {

  moduleServer(id, function(input, output, session) {

    output$lith_table <- DT::renderDataTable({

      show <- input$lith_columns
      cols <- c("well_tag_number")

      if("min" %in% show) cols <- c(cols,
                                    "lithology_from_m", "lithology_to_m",
                                    "lith_clean", "lith_category")
      if("extra" %in% show) cols <- c(cols, "lith_extra")
      if("cats" %in% show) cols <- c(cols, "lith_primary", "lith_secondary",
                                     "lith_tertiary")
      if("flags" %in% show) cols <- c(cols, "lith_flag")
      if("gwells" %in% show) cols <- c(cols, fields_lith_gwells)

      wells() %>%
        dplyr::select(dplyr::all_of(cols)) %>%
        sf::st_drop_geometry()
    })

  })
}
