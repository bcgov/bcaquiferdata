
#' @import shiny
#' @import shinydashboard

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
        tabItem(tabName = "cross_sections", ui_cross_sections("cross_sections"))
      #tabItem(tabName = "flags", ui_flags()),
      #tabItem(tabName = "exports", ui_exports())
      )
    )
  )

  server <- function(input, output, session) {
    server_data("data")
    wells <- server_wells("wells")
    #wells <- reactive(readr::read_rds("mills.rds"))
    server_lithology("lithology", wells)
    server_cross_sections("cross_sections", wells)
  }

  shinyApp(ui, server)
}


aq_sidebar <- function() {
  sidebarMenu(
    menuItem("Download Data", tabName = "data"),
    menuItem("Prepare Data", tabName = "wells"),
    menuItem("Explore Lithology", tabName = "lithology"),
    menuItem("Explore Cross sections", tabName = "cross_sections"),
    menuItem("Check Flags", tabName = "flags"),
    menuItem("Export", tabName = "export")
    #   menuItem("Fetch Wells Data", tabName = "wells")
  )
}
