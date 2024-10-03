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

ui_flags <- function(id) {

  ns <- NS(id)

  nav_panel(
    title = "Check Flags",
    card(
      full_screen = TRUE,
      # Data table with flags,
      DT::dataTableOutput(ns("flags_table"))
    ),
    card(
      card_header("Glossary"),
      max_height = "25%",
      full_screen = TRUE,
      includeMarkdown(system.file("extra_docs", "flags.md",
                                  package = "bcaquiferdata")),
      tableOutput(ns("flags_glossary"))

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
        aq_dt(filename = "flags") %>%
        DT::formatStyle(cols, backgroundColor = DT::styleEqual(TRUE, "#f8d7da"))
    }, server = FALSE)

    output$flags_glossary <- renderTable(bcaquiferdata::flags)


  })

}
