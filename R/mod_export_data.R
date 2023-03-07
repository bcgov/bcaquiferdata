

ui_export_data <- function(id) {

  ns <- NS(id)

  fluidRow(
    column(
      width = 12,
      tabBox(width = 12,
             tabPanel(title = "Strater",
                      actionButton("export_strater", "Export"),
                      DT::dataTableOutput(ns("table_strater"))),
             tabPanel(title = "Voxler",
                      actionButton("export_voxler", "Export"),
                      DT::dataTableOutput(ns("table_voxler"))),
             tabPanel(title = "ArcHydro",
                      actionButton("export_archydro", "Export"),
                      DT::dataTableOutput(ns("table_archydro")))
      )
    )
  )
}

server_export_data <- function(id, wells) {

  moduleServer(id, function(input, output, session) {

    output$table_strater <- DT::renderDataTable(aq_dt(wells(), 12, buttons = FALSE))
    output$table_voxler <- DT::renderDataTable(aq_dt(wells(), 12, buttons = FALSE))
    output$table_archydro <- DT::renderDataTable(aq_dt(wells(), 12, buttons = FALSE))

  })

}
