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

ui_hydrostratigraphy <- function(id) {

  ns <- NS(id)

  fluidRow(
    column(
      width = 2,
      box(
        width = 12,
        title = "Explore Hydrostratigraphy",
        checkboxGroupInput(
          ns("hydrostratigraphy_columns"), label = "Show columns",
          choices = list("Basic" = "min",
                         "Raw lithology" = "lith_raw",
                         "Flags" = "flags"),
          selected = "min"
        )
      )
    ),
    column(
      width = 10,
      box(
        width = 12,
        title = "Table",
        DT::dataTableOutput(ns("hydrostratigraphy_table"))
      )
    )
  )
}

server_hydrostratigraphy <- function(id, wells) {

  moduleServer(id, function(input, output, session) {

    output$hydrostratigraphy_table <- DT::renderDataTable({
      show <- input$hydrostratigraphy_columns
      cols <- c("well_tag_number")

      if("min" %in% show) cols <- c(
        cols,
        "elev", "well_depth_m", "lithology_from_m", "lithology_to_m",
        #"well_yield_usgpm", "well_yield_unit_code",
        "depth", "depth_units", "yield", "yield_units")
      if("lith_raw" %in% show) cols <- c(cols, "lithology_raw_data")
      if("flags" %in% show) cols <- c(cols, "flag_extra_digits")

      wells() %>%
        wells_yield() %>%
        dplyr::select(dplyr::all_of(cols)) %>%
        aq_dt()
    })

  })

}
