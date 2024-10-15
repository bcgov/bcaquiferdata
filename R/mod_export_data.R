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
        textInput(
          ns("export_id"),
          h4(aq_tt("File ID", "Prefix for the files to be exported")),
          value = "xxx"),
        p(),
        h4(aq_tt("Output folder", "Where should the exported files be saved?")),
        textOutput(ns("export_dir")),
        p(),
        shinyDirButton(ns("choose_export_dir"),
                       "Choose output folder",
                       "Choose where to save files"),
        uiOutput(ns("fixes"), inline = TRUE)
      ),

      # UI - Strater -------------
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

      # UI - Voxler -------------
      nav_panel(title = "Voxler",
                actionButton(ns("export_voxler"), "Export", width = 150),
                textOutput(ns("feedback_voxler")),

                h3("Voxler file (", textOutput(ns("voxler_f1"), container = code), ")"),
                DT::dataTableOutput(ns("table_voxler_f1"))),

      # UI - ArcHydro -------------
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
      ),

      # UI - Leapfrog -------------
      nav_panel(
        title = "Leapfrog",
        actionButton(ns("export_leapfrog"), "Export", width = 150),
        textOutput(ns("feedback_leapfrog")),
        navset_card_tab(
          nav_panel(
            title = h5("Collars (", textOutput(ns("leapfrog_f1"), container = code), ")"),
            DT::dataTableOutput(ns("table_leapfrog_f1"))),

          nav_panel(
            title = h5("Intervals (", textOutput(ns("leapfrog_f2"), container = code), ")"),
            DT::dataTableOutput(ns("table_leapfrog_f2")))
        )
      ),

      # UI - Surfer -------------
      nav_panel(
        title = "Surfer",
        actionButton(ns("export_surfer"), "Export", width = 150),
        textOutput(ns("feedback_surfer")),

        h3("Surfer file (", textOutput(ns("surfer_f1"), container = code), ")"),
        DT::dataTableOutput(ns("table_surfer_f1"))
      )


    )
  )
}

server_export_data <- function(id, wells) {

  moduleServer(id, function(input, output, session) {

    # Setup ----------------------
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


    # Messaging --------------------

    # If any things need to be fixed (and haven't already), let the user know
    output$fixes <- renderUI({

      f1 <- any(wells()$flag_int_bottom) & !any(wells()$fix_int_bottom)
      f2 <- any(wells()$flag_depth_mismatch)

      if(f1 | f2) {
        t <- tagList(strong("LeapFrog Export:"), br())
      } else {
        t <- tagList()
      }

      if(f1) {
        t <- tagList(
          t,
          p("Forcing thickness of bottom lithology intervals from 0m to 1m in wells:", br(),
            tags$ul(
              lapply(unique(wells()$well_tag_number[wells()$flag_int_bottom]),
                     htmltools::tags$li)
            )
          )
        )
      }

      if(f2) {
        t <- tagList(
          t,
          p("Forcing well depth to equal depth of the final lithology interval", br(),
            tags$ul(
              lapply(unique(wells()$well_tag_number[wells()$flag_depth_mismatch]),
                     htmltools::tags$li)
            )
          )
        )
      }

      t
    })

    # Setup Directory and File IDs ------------------
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
               "_voxler.csv",
               "_archydro_well.csv", "_archydro_hguid.csv", "_archydro_bh.csv",
               "_leapfrog_collars.csv", "_leapfrog_intervals.csv",
               "_surfer.csv"))
    })

    # File headers
    output$strater_f1 <- renderText(files()[1])
    output$strater_f2 <- renderText(files()[2])
    output$strater_f3 <- renderText(files()[3])
    output$voxler_f1 <- renderText(files()[4])
    output$archydro_f1 <- renderText(files()[5])
    output$archydro_f2 <- renderText(files()[6])
    output$archydro_f3 <- renderText(files()[7])
    output$leapfrog_f1 <- renderText(files()[8])
    output$leapfrog_f2 <- renderText(files()[9])
    output$surfer_f1 <- renderText(files()[10])

    # Export previews ---------------
    exp_strater <- reactive(wells_export(wells(), type = "strater", preview = TRUE))
    exp_voxler <- reactive(wells_export(wells(), type = "voxler", preview = TRUE))
    exp_archydro <- reactive(wells_export(wells(), type = "archydro", preview = TRUE))
    exp_leapfrog <- reactive(wells_export(wells(), type = "leapfrog", preview = TRUE))
    exp_surfer <- reactive(wells_export(wells(), type = "surfer", preview = TRUE))

    output$table_strater_f1 <- DT::renderDataTable({
      aq_dt(exp_strater()[[1]], minimal = TRUE)})
    output$table_strater_f2 <- DT::renderDataTable(aq_dt(exp_strater()[[2]], minimal = TRUE))
    output$table_strater_f3 <- DT::renderDataTable(aq_dt(exp_strater()[[3]], minimal = TRUE))

    output$table_voxler_f1 <- DT::renderDataTable(aq_dt(exp_voxler()[[1]], minimal = TRUE))

    output$table_archydro_f1 <- DT::renderDataTable(aq_dt(exp_archydro()[[1]], minimal = TRUE))
    output$table_archydro_f2 <- DT::renderDataTable(aq_dt(exp_archydro()[[2]], minimal = TRUE))
    output$table_archydro_f3 <- DT::renderDataTable(aq_dt(exp_archydro()[[3]], minimal = TRUE))

    output$table_leapfrog_f1 <- DT::renderDataTable(aq_dt(exp_leapfrog()[[1]], minimal = TRUE))
    output$table_leapfrog_f2 <- DT::renderDataTable(aq_dt(exp_leapfrog()[[2]], minimal = TRUE))

    output$table_surfer_f1 <- DT::renderDataTable(aq_dt(exp_surfer()[[1]], minimal = TRUE))

    # Export files ---------------------
    output$feedback_strater <- feedback_output("strater")
    observe_export("strater")

    output$feedback_voxler <- feedback_output("voxler")
    observe_export("voxler")

    output$feedback_archydro <- feedback_output("archydro")
    observe_export("archydro")

    output$feedback_leapfrog <- feedback_output("leapfrog")
    observe_export("leapfrog")

    output$feedback_surfer <- feedback_output("surfer")
    observe_export("surfer")

  })

}
