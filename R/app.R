
#' @import shiny
#' @import shinydashboard
#' @import shinyFiles

aq_app <- function() {

  ui <- dashboardPage(
    header = dashboardHeader(title = "BC Aquifer Data"),
    sidebar = dashboardSidebar(aq_sidebar()),

    body = dashboardBody(
      shinyjs::useShinyjs(),
      tabItems(
        tabItem(tabName = "data", ui_data("data")),
        tabItem(tabName = "wells", ui_wells("wells")),
        tabItem(tabName = "lithology", ui_lithology("lithology")),
        tabItem(tabName = "hydrostratigraphy", ui_hydrostratigraphy("hydrostratigraphy")),
        tabItem(tabName = "flags", ui_flags("flags")),
        tabItem(tabName = "export_data", ui_export_data("export_data"))
      )
    )
  )

  server <- function(input, output, session) {
    have_data <- server_data("data")
    wells <- server_wells("wells", have_data)
    #wells <- reactive(readr::read_rds("mills.rds"))
    server_lithology("lithology", wells)
    server_hydrostratigraphy("hydrostratigraphy", wells)
    server_flags("flags", wells)
    server_export_data("export_data", wells)
  }

  shinyApp(ui, server)
}


aq_sidebar <- function() {
  sidebarMenu(
    menuItem("Download Data", tabName = "data"),
    menuItem("Prepare Data", tabName = "wells"),
    menuItem("Explore Lithology", tabName = "lithology"),
    menuItem("Explore Hydrostratigraphy", tabName = "hydrostratigraphy"),
    menuItem("Check Flags", tabName = "flags"),
    menuItem("Exports", tabName = "export_data")
  )
}
