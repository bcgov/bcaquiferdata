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

  nav_panel(
    title = "Explore Hydrostratigraphy",
    navset_card_pill(
      nav_panel(
        title = "Table",
        layout_sidebar(
          sidebar = sidebar(
            checkboxGroupInput(
              ns("hydrostratigraphy_columns"),
              label = h4(aq_tt("Columns", "Which types of columns to display")),
              choices = list("Basic" = "min",
                             "Raw lithology" = "lith_raw",
                             "Flags" = "flags"),
              selected = "min"
            ),
            markdown(paste0(
              "**Note:**<br>The `flag_yield_digits` column shows digits ",
              "extracted from raw lithology which could not be converted to ",
              "a yield or depth (see 'Info' tab for more details)."))
            ),
          DT::dataTableOutput(ns("hydrostratigraphy_table"))
        )
      ),
      nav_panel(title = "Info", includeMarkdown(
        system.file("extra_docs", "hydrostratigraphy_desc.md",
                    package = "bcaquiferdata"))
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
      if("lith_raw" %in% show) cols <- c(cols, "lithology_raw_combined")
      if("flags" %in% show) cols <- c(cols, "flag_yield_mismatch", "flag_yield_digits")

      wells() %>%
        sf::st_drop_geometry() %>%
        wells_yield() %>%
        dplyr::select(dplyr::all_of(cols)) %>%
        aq_dt(filename = "hydrostratigraphy")
    }, server = FALSE)

  })

}
