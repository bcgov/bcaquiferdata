# Copyright 2024 Province of British Columbia
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

ui_about <- function(id) {

  ns <- NS(id)

  nav_panel(
    title = "About",
    card(
      card_header("About this Shiny app"),
      p("This app allows functions the R package bcaquiferdata (BC Aquifer Data Tools)",
        "to be used in a simpler, and interactive format."),
      p("BC Government GWELLS data is retrieved, filtered and matched to Lidar or TRIM elevation data.",
        "Lithology is cleaned, simplified and categorized, and flow yields are extracted"),
      p("As this data can be messy and error prone, we also flag potentially suspect data for review."),
      p("Data can be exported for use in a variety of external programs or as basic csv/xlsx files."),
      h3("Citing", code("bcaquiferdata")),
      p("To cite this software please use the following:"),
      markdown(paste0(utils::capture.output(print(utils::citation("bcaquiferdata"), style = "text")), collapse = "\n"))
    )
  )
}
