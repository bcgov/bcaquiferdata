

ui_flags <- function(id) {

  ns <- NS(id)

  fluidRow(
    column(
      width = 12,
      box(
        title = "Check Flags",
        width = 12,
        # Data table with flags,
        DT::dataTableOutput(ns("flags_table"))
      ),
      box(title = "Glossary",
          tableOutput(ns("flags_glossary"))

      )
    )
  )
}

server_flags <- function(id, wells) {

  moduleServer(id, function(input, output, session) {

    output$flags_table <- DT::renderDataTable({
      cols <- stringr::str_subset(names(wells()), "flag_")
      wells() %>%
        sf::st_drop_geometry() %>%
        dplyr::select("well_tag_number", "lithology_from_m", "lithology_to_m",
                      dplyr::starts_with("flag")) %>%
        dplyr::filter(dplyr::if_any(dplyr::starts_with("flag"))) %>%
        aq_dt() %>%
        DT::formatStyle(cols, backgroundColor = DT::styleEqual(TRUE, "#f8d7da"))
    })

    output$flags_glossary <- renderTable(flags)


  })

}
