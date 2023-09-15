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

ui_lithology <- function(id) {

  ns <- NS(id)

  nav_panel(
    title = "Explore Lithology",
    navset_card_pill(
      nav_panel(title = "Table",
                layout_sidebar(
                  sidebar = sidebar(
                    checkboxGroupInput(
                      ns("lith_columns"), label = h4("Columns"),
                      choices = list("Basic" = "min",
                                     "Extra" = "extra",
                                     "All from GWELLS" = "gwells",
                                     "Intermediate categories" = "cats",
                                     "Flags" = "flags"),
                      selected = "min"
                    ),
                    p("See 'Info' for specific details regarding these columns")),
                  DT::dataTableOutput(ns("lith_table"))
                )
      ),
      nav_panel(title = "Info",
                includeMarkdown(
                  system.file("extra_docs", "lithology_desc.md",
                              package = "bcaquiferdata"))
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
                                    "lithology_raw_data",
                                    "lithology_clean", "lithology_category",
                                    "depth_to_bedrock")
      if("extra" %in% show) cols <- c(cols, "lithology_extra")
      if("cats" %in% show) cols <- c(cols, "lith_primary", "lith_secondary",
                                     "lith_tertiary")
      if("flags" %in% show) cols <- c(cols, stringr::str_subset(names(wells()), "flag_"))
      if("gwells" %in% show) cols <- c(cols, fields_lith_gwells)



      wells() %>%
        dplyr::select(dplyr::all_of(cols)) %>%
        sf::st_drop_geometry() %>%
        aq_dt()
    })

  })
}
