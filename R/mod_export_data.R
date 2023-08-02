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

ui_export_data <- function(id) {

  ns <- NS(id)

  nav_panel(
    title = "Exports",
    navset_card_pill(
      sidebar = sidebar(
        textInput(ns("export_id"), h4("ID for export files"), value = "xxx"),
        p(),
        h4("Output folder"),
        textOutput(ns("export_dir")),
        p(),
        shinyDirButton(ns("choose_export_dir"),
                       "Choose output folder",
                       "Choose where to save files")
      ),
      nav_panel(
        title = "Strater",
        actionButton(ns("export_strater"), "Export", width = 150),
        textOutput(ns("feedback_strater")),
        navset_card_tab(
          nav_panel(
            title = h5("Lithology (", textOutput(ns("strater_f1"), container = code), ")"),
            DT::dataTableOutput(ns("table_strater_f1"))),

          nav_panel(
            title = h5("Collars (", textOutput(ns("strater_f2"), container = code), ")"),
            DT::dataTableOutput(ns("table_strater_f2"))),

          nav_panel(
            title = h5("Wells (", textOutput(ns("strater_f3"), container = code), ")"),
            DT::dataTableOutput(ns("table_strater_f3"))))
      ),
      nav_panel(title = "Voxler",
                actionButton(ns("export_voxler"), "Export", width = 150),
                textOutput(ns("feedback_voxler")),

                h3("Voxler file (", textOutput(ns("voxler_f1"), container = code), ")"),
                DT::dataTableOutput(ns("table_voxler_f1"))),

      nav_panel(
        title = "ArcHydro",
        actionButton(ns("export_archydro"), "Export", width = 150),
        textOutput(ns("feedback_archydro")),
        navset_card_tab(
          nav_panel(
            title = h5("Wells (", textOutput(ns("archydro_f1"), container = code), ")"),
            DT::dataTableOutput(ns("table_archydro_f1"))),

          nav_panel(
            title = h5("HGU ID (lithology ", textOutput(ns("archydro_f2"), container = code), ")"),
            DT::dataTableOutput(ns("table_archydro_f2"))),

          nav_panel(
            title = h5("BH (lithology index ", textOutput(ns("archydro_f3"), container = code), ")"),
            DT::dataTableOutput(ns("table_archydro_f3")))
        )
      )
    )
  )
}

server_export_data <- function(id, wells) {

  moduleServer(id, function(input, output, session) {

    # Setup
    volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
    feedback <- reactiveValues(strater = "", voxler = "", archydro = "")

    # Functions
    observe_export <- function(type) {
      observe({
        req(!is.na(export_dir()))
        wells_export(wells(), id = export_id(), type = type, dir = export_dir())
        feedback[[type]] <- paste(stringr::str_to_title(type), "files exported")
      }) %>%
        bindEvent(input[[paste0("export_", type)]])
    }

    feedback_output <- function(type) {
      renderText({
        validate(need(export_dir() != "No output directory selected",
                      "Please choose an output directory"))
        validate(need(dir.exists(export_dir()),
                      "The choose output directory does not exist, please
                    choose another one"))
        feedback[[type]]
      }) %>%
        bindEvent(input[[paste0("export_", type)]], export_dir(), ignoreInit = TRUE)
    }


    # Setup Directory and File IDs
    export_id <- reactive(janitor::make_clean_names(input$export_id))

    shinyDirChoose(input, "choose_export_dir", session = session,
                   roots = volumes)

    export_dir <- reactive({
      if (is.integer(input$choose_export_dir)) {
        NA_character_
      } else {
        parseDirPath(volumes, input$choose_export_dir)
      }
    })

    output$export_dir <- renderText({
      if(is.na(export_dir())) "No output directory selected" else export_dir()
    })

    files <- reactive({
      paste0(export_id(),
             c("_lith.csv", "_collars.csv", "_wls.csv",
               "_vox",
               "_arc_well.csv", "_arc_hguid.csv", "_arc_bh.csv"))
    })

    # File headers
    output$strater_f1 <- renderText(files()[1])
    output$strater_f2 <- renderText(files()[2])
    output$strater_f3 <- renderText(files()[3])
    output$voxler_f1 <- renderText(files()[4])
    output$archydro_f1 <- renderText(files()[5])
    output$archydro_f2 <- renderText(files()[6])
    output$archydro_f3 <- renderText(files()[7])

    # Export previews
    exp_strater <- reactive(wells_export(wells(), type = "strater", preview = TRUE))
    exp_voxler <- reactive(wells_export(wells(), type = "voxler", preview = TRUE))
    exp_archydro <- reactive(wells_export(wells(), type = "archydro", preview = TRUE))

    output$table_strater_f1 <- DT::renderDataTable({
      aq_dt(exp_strater()[[1]], minimal = TRUE)})
    output$table_strater_f2 <- DT::renderDataTable(aq_dt(exp_strater()[[2]], minimal = TRUE))
    output$table_strater_f3 <- DT::renderDataTable(aq_dt(exp_strater()[[3]], minimal = TRUE))

    output$table_voxler_f1 <- DT::renderDataTable(aq_dt(exp_voxler()[[1]], minimal = TRUE))

    output$table_archydro_f1 <- DT::renderDataTable(aq_dt(exp_archydro()[[1]], minimal = TRUE))
    output$table_archydro_f2 <- DT::renderDataTable(aq_dt(exp_archydro()[[2]], minimal = TRUE))
    output$table_archydro_f3 <- DT::renderDataTable(aq_dt(exp_archydro()[[3]], minimal = TRUE))

    # Export files
    output$feedback_strater <- feedback_output("strater")
    observe_export("strater")

    output$feedback_voxler <- feedback_output("voxler")
    observe_export("voxler")

    output$feedback_archydro <- feedback_output("archydro")
    observe_export("archydro")

  })

}
