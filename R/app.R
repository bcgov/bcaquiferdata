# Copyright 2023 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations under
# the License.

#' Launch Aquifer Data Shiny App
#'
#' This app allows you to load a shapefile and filter aquifer/well data
#' according to region, explore data, and export cleaned files.
#'
#' @import shiny
#' @import bslib
#' @import shinyFiles
#' @export
#'
#' @examplesIf interactive()
#' aq_app()
#'
aq_app <- function() {

  # Check for suggested packages
  rlang::check_installed(
    c("DT", "ggplot2", "ggthemes", "shinyjs"))

  ui <- tagList(
    page_navbar(
      title = "BC Aquifer Data",
      theme = aq_theme(),
      ui_data("data"),
      ui_wells("wells"),
      ui_lithology("lithology"),
      ui_hydrostratigraphy("hydrostratigraphy"),
      ui_flags("flags"),
      ui_export_data("export_data")
    ),
    shinyjs::useShinyjs()  # Set up shinyjs
  )

  server <- function(input, output, session) {
    have_data <- server_data("data")
    wells <- server_wells("wells", have_data)
    #wells <- reactive(readr::read_rds("misc/mills.rds"))
    server_lithology("lithology", wells)
    server_hydrostratigraphy("hydrostratigraphy", wells)
    server_flags("flags", wells)
    server_export_data("export_data", wells)
  }

  shinyApp(ui, server, options = list(launch.browser = TRUE))
}

aq_theme <- function() {
  bs_theme(
    fg = "#003366", # BC Gov blue
    bg = "#fff",
    primary = "#385a8a", # BC Gov light blue
    #"#fcba19", # BC Gov yellow
    secondary = "#AAB1B8",
    font_scale = 0.9,
    "nav-link-font-size" = "110%",
    ) %>%
    bs_add_variables(
      "nav-tabs-link-border-color" = "$primary", .where = "declarations"
    ) %>%
    bs_add_rules(
      list(
        "div.nopad .value-box-area { padding: 0; }",
        ".nav-pills .nav-link { background: #00336630; margin-left: 2px; margin-right: 2px;};"
        ))
}


mod_test <- function(which) {
  ui <- tagList(
    page_navbar(
      title = "BC Aquifer Data",
      theme = aq_theme(),
      get(paste0("ui_", which))(which)
    ),
    shinyjs::useShinyjs()  # Set up shinyjs
  )

  server <- function(input, output, session) {
    if(which == "data") {
      server_data("data")
    } else if(which == "wells") {
      server_wells("wells", reactive(TRUE))
    } else {
      wells <- reactive(readr::read_rds("misc/mills.rds"))
      get(paste0("server_", which))(which, wells)
    }
  }

  shinyApp(ui, server)
}

